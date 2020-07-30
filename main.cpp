#include <SDL2/SDL.h>
#include <GL/glew.h>
#include <SDL2/SDL_opengl.h>
#include <GL/glu.h>
#include <glm/vec2.hpp>
#include <glm/mat4x4.hpp>
#include <glm/gtx/transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <chrono>
#include <cmath>
#include <cstdlib>
#include <iostream>

#include "glpp.h"
#include "graphics.h"

constexpr int WINDOW_HEIGHT = 800;
constexpr int WINDOW_WIDTH = 800;

// Represents the in-game understanding of user inputs.
struct ShipController {
  bool thruster = false;
  bool rotate_clockwise = false;
  bool rotate_counterclockwise = false;

  void reset() {
    thruster = rotate_clockwise = rotate_counterclockwise = false;
  }
};

glm::mat4x4 transformation(glm::vec2 pos, float angle, float scale) {
  glm::mat4x4 transform = glm::translate(glm::mat4(1.f),
                                         glm::vec3(pos.x, pos.y, 0));
  transform = glm::rotate(transform, angle, glm::vec3(0, 0, 1));
  transform *= glm::scale(glm::mat4(1.f), glm::vec3(scale));
  return transform;
}

Error run() {
  Graphics gfx;
  if (Error e = gfx.init(WINDOW_WIDTH, WINDOW_HEIGHT); !e.ok) return e;

  Shader verts(Shader::Type::VERTEX);
  verts.add_source(
		"#version 140\n"
    "in vec2 vertex_pos;"
    "in vec2 tex_coord;"
    "uniform mat4 transform;"
    "out vec2 TexCoord;"
    "void main() {"
      "gl_Position = transform * vec4(vertex_pos, 1, 1);"
      "TexCoord = tex_coord;"
    "}"
  );

  if (Error e = verts.compile(); !e.ok) return e;

  Shader frag(Shader::Type::FRAGMENT);
  frag.add_source(
    "#version 140\n"
    "in vec2 TexCoord;"
    "out vec4 FragColor;"
    "uniform sampler2D tex;"
    "void main() {"
      "FragColor = texture(tex, TexCoord);"
    "}"
  );

  if (Error e = frag.compile(); !e.ok) return e;

  GlProgram gl_program;
  gl_program.add_shader(verts);
  gl_program.add_shader(frag);
  if (Error e = gl_program.link(); !e.ok) return e;

  auto gl_vertext_pos = gl_program.attribute_location("vertex_pos");
  if (gl_vertext_pos == -1) return Error("vertex_pos is not a valid var.");
  auto gl_tex_coord = gl_program.attribute_location("tex_coord");
  if (gl_tex_coord == -1) return Error("tex_coord is not a valid var.");

  //Initialize clear color
  gl::clearColor(0.f, 0.f, 0.f, 1.f);

  SDL_Surface* ship_surface = SDL_LoadBMP("art/ship 512 RGBA8.bmp");
  if (ship_surface == nullptr) {
    return Error(concat_strings("Failed to load image: ", SDL_GetError()));
  }

  GLuint ship_texture = gl::genTexture();
  gl::bindTexture(GL_TEXTURE_2D, ship_texture);
  gl::texImage2D(GL_TEXTURE_2D, 0, GL_RGBA, ship_surface->w,
                 ship_surface->h, 0, GL_BGRA, GL_UNSIGNED_BYTE,
                 ship_surface->pixels);
  SDL_FreeSurface(ship_surface);

  if (auto e = glGetError(); e != GL_NO_ERROR) {
    return Error(concat_strings(
        "I'm too lazy to figure out if there's a function which maps this GL "
        "error number to a string so here's a number: ", std::to_string(e)));
  }

  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);

  //GLuint vao;
  //glGenVertexArrays(1, &vao);
  //glBindVertexArray(vao);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  struct Vertex {
    glm::vec2 pos;
    glm::vec2 tex_coord;
  };

  //VBO data
  Vertex vertecies[] = {
    // Position    TexCoords
    {{-0.5f, -0.5f},  {0.0f, 1.0f}},
    {{ 0.5f, -0.5f},  {1.0f, 1.0f}},
    {{ 0.5f,  0.5f},  {1.0f, 0.0f}},
    {{-0.5f,  0.5f},  {0.0f, 0.0f}}
  };

  GLuint vbo = gl::genBuffer();
  gl::bindBuffer(GL_ARRAY_BUFFER, vbo);
  gl::bufferData(GL_ARRAY_BUFFER, vertecies, GL_DYNAMIC_DRAW);

  //IBO data
  GLuint vbo_elems[] = {0, 1, 2,
                        2, 3, 0};

  GLuint vbo_elems_id = gl::genBuffer();
  gl::bindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_elems_id);
  gl::bufferData(GL_ELEMENT_ARRAY_BUFFER, vbo_elems, GL_STATIC_DRAW);

  ShipController ship_controller;
  float ship_speed = 0;
  float ship_acc = 0;
  float ship_rotation = 0;
  float ship_rotation_vel = 0;
  auto ship_pos = glm::vec2(0.0f, 0.0f);

  auto time = std::chrono::high_resolution_clock::now();
  std::chrono::milliseconds dtime;

  bool keep_going = true;
  SDL_Event e;

  auto ship_texture_uni = gl_program.uniform_location("tex");
  if (ship_texture_uni == -1) return Error("tex is not a valid uniform location.");

  auto ship_transform_uni = gl_program.uniform_location("transform");
  if (ship_transform_uni == -1) return Error("linear_transformation not valid.");

  while (keep_going) {
    while (SDL_PollEvent(&e) != 0) {
      switch (e.type) {
        case SDL_QUIT: keep_going = false; break;
        case SDL_KEYDOWN: case SDL_KEYUP:
          bool* control = nullptr;
          switch (e.key.keysym.sym) {
            case SDLK_UP: control = &ship_controller.thruster; break;
            case SDLK_LEFT:
              control = &ship_controller.rotate_counterclockwise; break;
            case SDLK_RIGHT:
              control = &ship_controller.rotate_clockwise; break;
            case 'q': keep_going = false; break;
          }

          if (control) *control = e.key.type == SDL_KEYDOWN;
      }
    }

    auto new_time = std::chrono::high_resolution_clock::now();
    dtime = std::chrono::duration_cast<std::chrono::milliseconds>(new_time - time);
    time = new_time;

    if (ship_controller.thruster) ship_acc = 0.000001f;
    if (ship_controller.rotate_clockwise) ship_rotation_vel += 0.001f;
    if (ship_controller.rotate_counterclockwise) ship_rotation_vel -= 0.001f;

    ship_rotation += ship_rotation_vel * dtime.count();
    ship_speed += ship_acc * dtime.count();
    auto pos_change = glm::vec2(std::cos(ship_rotation),
                                std::sin(ship_rotation)); 
    pos_change *= ship_speed * dtime.count();
    ship_pos = ship_pos + pos_change;
    ship_rotation_vel = 0;
    ship_acc = 0;

    gl::clear();

    gl_program.use();

    gl::uniform(ship_texture_uni, ship_texture);

    glm::mat4x4 transform = transformation(ship_pos, ship_rotation, 0.5f);
    glUniformMatrix4fv(ship_transform_uni, 1, GL_FALSE, glm::value_ptr(transform));

    gl::enableVertexAttribArray(gl_vertext_pos);
    gl::vertexAttribPointer<float>(gl_vertext_pos, 2, GL_FALSE, &Vertex::pos);

    gl::enableVertexAttribArray(gl_tex_coord);
    gl::vertexAttribPointer<float>(gl_tex_coord, 2, GL_FALSE, &Vertex::tex_coord);

    gl::drawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

    gl::useProgram(0);

    gfx.swap_buffers();
  }

  return Error();
}

int main() {
  if (Error e = run(); !e.ok) {
    std::cerr << e.reason << std::endl;
    return 1;
  }
  return 0;
}
