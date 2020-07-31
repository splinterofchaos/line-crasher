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

#include "ecs.h"
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
                                         glm::vec3(pos.x * scale, pos.y * scale, 0));
  transform *= glm::scale(glm::mat4(1.f), glm::vec3(scale));
  transform = glm::rotate(transform, angle, glm::vec3(0, 0, 1));
  return transform;
}

Error construct_ship_shader(GlProgram& ship_shader_program)
{
  Shader verts(Shader::Type::VERTEX);
  verts.add_source(R"(
		#version 140
    in vec2 vertex_pos;
    in vec2 tex_coord;
    uniform mat4 transform;
    out vec2 TexCoord;
    void main() {
      gl_Position = transform * vec4(vertex_pos, 1, 1);
      TexCoord = tex_coord;
    }
  )");
  if (Error e = verts.compile(); !e.ok) return e;

  Shader frag(Shader::Type::FRAGMENT);
  frag.add_source(R"(
    #version 140
    in vec2 TexCoord;
    out vec4 FragColor;
    uniform sampler2D tex;
    void main() {
      FragColor = texture(tex, TexCoord);
    }
  )");
  if (Error e = frag.compile(); !e.ok) return e;

  ship_shader_program.add_shader(verts);
  ship_shader_program.add_shader(frag);
  return ship_shader_program.link();
}

Error construct_line_shader(GlProgram& line_shader_program) {
  Shader verts(Shader::Type::VERTEX);
  verts.add_source(R"(
		#version 140
    in vec2 vertex_pos;
    uniform mat4 transform;
    uniform float length;
    void main() {
      gl_Position = transform * vec4(vertex_pos.x * length, vertex_pos.y, 1, 1);
    }
  )");
  if (Error e = verts.compile(); !e.ok) return e;

  Shader frag(Shader::Type::FRAGMENT);
  frag.add_source(R"(
    #version 140
    in vec2 TexCoord;
    out vec4 FragColor;
    void main() {
      FragColor = vec4(1);
    }
  )");
  if (Error e = frag.compile(); !e.ok) return e;

  line_shader_program.add_shader(verts);
  line_shader_program.add_shader(frag);
  return line_shader_program.link();
}

struct Transform {
  glm::vec2 pos;
  float rotation;  // ... in radians.

  // TODO: There might be a more appropriate component for this.
  float length;  // Used for lines in determining how long they are.
};

// References all shader uniforms and attribute bindings.
struct ShaderBindings {
  GlProgram* program;

  GLuint vbo;

  GLint texture_uniform = -1;
  GLint transform_uniform = -1;
  GLint length_uniform = -1;

  GLint vertex_pos_attrib = -1;
  GLint tex_coord_attrib = -1;

  ShaderBindings() { }
  ShaderBindings(GlProgram* program, GLuint vbo)
    : program(program), vbo(vbo) { }
};

Error set_uniform(const GlProgram& program, const char* const name,
                  GLint& out) {
  out = program.uniform_location(name);
  if (out == -1) {
    return Error(concat_strings(name, " is not a valid uniform location"));
  }
  return Error();
}

Error set_attribute(const GlProgram& program, const char* const name,
                    GLint& out) {
  out = program.attribute_location(name);
  if (out == -1) {
    return Error(concat_strings(name, " is not a valid attribute location"));
  }
  return Error();
}

Error run() {
  Graphics gfx;
  if (Error e = gfx.init(WINDOW_WIDTH, WINDOW_HEIGHT); !e.ok) return e;

  GlProgram ship_shader_program;
  if (Error e = construct_ship_shader(ship_shader_program); !e.ok) return e;

  GlProgram line_shader_program;
  if (Error e = construct_line_shader(line_shader_program); !e.ok) return e;

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

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  struct Vertex {
    glm::vec2 pos;
    glm::vec2 tex_coord;
  };

  //VBO data
  Vertex quad_vertecies[] = {
    // Position    TexCoords
    {{-0.5f, -0.5f},  {0.0f, 1.0f}},
    {{ 0.5f, -0.5f},  {1.0f, 1.0f}},
    {{ 0.5f,  0.5f},  {1.0f, 0.0f}},
    {{-0.5f,  0.5f},  {0.0f, 0.0f}}
  };

  GLuint quad_vbo = gl::genBuffer();
  gl::bindBuffer(GL_ARRAY_BUFFER, quad_vbo);
  gl::bufferData(GL_ARRAY_BUFFER, quad_vertecies, GL_DYNAMIC_DRAW);

  Vertex line_vertecies[] = {
    // Position    TexCoords
    {{-0.5f, -0.05f},  {0.0f, 0.0f}},
    {{ 0.5f, -0.05f},  {0.0f, 0.0f}},
    {{ 0.5f,  0.05f},  {0.0f, 0.0f}},
    {{-0.5f,  0.05f},  {0.0f, 0.0f}}
  };

  GLuint line_vbo = gl::genBuffer();
  gl::bindBuffer(GL_ARRAY_BUFFER, line_vbo);
  gl::bufferData(GL_ARRAY_BUFFER, line_vertecies, GL_DYNAMIC_DRAW);
  bool flip = false;

  //IBO data
  GLuint vbo_elems[] = {0, 1, 2,
                        2, 3, 0};

  GLuint vbo_elems_id = gl::genBuffer();
  gl::bindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_elems_id);
  gl::bufferData(GL_ELEMENT_ARRAY_BUFFER, vbo_elems, GL_STATIC_DRAW);

  EntityComponentSystem<Transform, ShaderBindings*> ecs;

  ShaderBindings player_shader_bindings(&ship_shader_program, quad_vbo);
  if (Error e =
      set_uniform(ship_shader_program, "tex",
                  player_shader_bindings.texture_uniform) &&
      set_uniform(ship_shader_program, "transform",
                  player_shader_bindings.transform_uniform) &&
      set_attribute(ship_shader_program, "vertex_pos",
                    player_shader_bindings.vertex_pos_attrib) &&
      set_attribute(ship_shader_program, "tex_coord",
                    player_shader_bindings.tex_coord_attrib);
      !e.ok) return e;

  auto player = ecs.write_new_entity(Transform{glm::vec2(0.0f), 0, 0},
                                     &player_shader_bindings);

  ShaderBindings line_shader_bindings(&line_shader_program, line_vbo);
  line_shader_program.use();
  if (Error e =
      set_uniform(line_shader_program, "transform",
                  line_shader_bindings.transform_uniform) &&
      set_uniform(line_shader_program, "length",
                  line_shader_bindings.length_uniform) &&
      set_attribute(line_shader_program, "vertex_pos",
                    line_shader_bindings.vertex_pos_attrib);
      !e.ok) return e;

  // One should be roughly the width of the player ship.
  ecs.write_new_entity(Transform{glm::vec2(1, 1), 3.14 / 2, 1},
                       &line_shader_bindings);
  ecs.write_new_entity(Transform{glm::vec2(1.5, 1), 3.14 / 1.5, 1.5},
                       &line_shader_bindings);
  ecs.write_new_entity(Transform{glm::vec2(1.5, 1), 3.14 / 1.0, 2},
                       &line_shader_bindings);
  ecs.write_new_entity(Transform{glm::vec2(1, 1.2), 3.14 / 2, 1},
                       &line_shader_bindings);
  ecs.write_new_entity(Transform{glm::vec2(1.5, 1.2), 3.14 / 1.5, 1.5},
                       &line_shader_bindings);
  ecs.write_new_entity(Transform{glm::vec2(1.5, 1.2), 3.14 / 1.0, 2},
                       &line_shader_bindings);

  // TODO: These should eventually be stored into components, too.
  ShipController ship_controller;
  float ship_speed = 0;
  float ship_acc = 0;
  float ship_rotation_vel = 0;

  auto time = std::chrono::high_resolution_clock::now();
  std::chrono::milliseconds dtime;

  bool keep_going = true;
  SDL_Event e;

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
            case ' ': flip = !flip; break;
          }

          if (control) *control = e.key.type == SDL_KEYDOWN;
      }
    }

    auto new_time = std::chrono::high_resolution_clock::now();
    dtime = std::chrono::duration_cast<std::chrono::milliseconds>(new_time - time);
    static auto highest_dtime = decltype(dtime.count())(0);
    if (dtime.count() > highest_dtime) {
      std::cout << "new highest ftime: " << dtime.count() << std::endl;
      highest_dtime = dtime.count();
    }
    time = new_time;

    if (ship_controller.thruster) ship_acc = 0.000001f;
    if (ship_controller.rotate_clockwise) ship_rotation_vel -= 0.001f;
    if (ship_controller.rotate_counterclockwise) ship_rotation_vel += 0.001f;

    Transform& ship_transform = ecs.read_or_panic<Transform>(player);

    ship_transform.rotation += ship_rotation_vel * dtime.count();
    ship_speed += ship_acc * dtime.count();
    auto pos_change = glm::vec2(std::cos(ship_transform.rotation),
                                std::sin(ship_transform.rotation));
    pos_change *= ship_speed * dtime.count();

    ship_transform.pos = ship_transform.pos + pos_change;
    ship_rotation_vel = 0;
    ship_acc = 0;

    // TODO: make less linear.
    float zoom = 0.25f; // - ship_speed * 50;

    gl::clear();

    ShaderBindings* last_bindings = nullptr;
    for (const auto& [id, transform, shader_bindings] :
         ecs.read_all<Transform, ShaderBindings*>()) {

      if (last_bindings != shader_bindings) {
        shader_bindings->program->use();

        gl::bindBuffer(GL_ARRAY_BUFFER, shader_bindings->vbo);

        gl::enableVertexAttribArray(shader_bindings->vertex_pos_attrib);
        gl::vertexAttribPointer<float>(shader_bindings->vertex_pos_attrib, 2,
                                       GL_FALSE, &Vertex::pos);

        if (shader_bindings->tex_coord_attrib != -1) {
          gl::enableVertexAttribArray(shader_bindings->tex_coord_attrib);
          gl::vertexAttribPointer<float>(shader_bindings->tex_coord_attrib, 2,
                                         GL_FALSE, &Vertex::tex_coord);
        }
      }
      last_bindings = shader_bindings;

      glUniformMatrix4fv(
          shader_bindings->transform_uniform, 1, GL_FALSE,
          glm::value_ptr(transformation(transform.pos, transform.rotation,
                                        zoom)));

      if (shader_bindings->length_uniform != -1) {
        gl::uniform(shader_bindings->length_uniform, transform.length);
      }

      gl::drawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
    }

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
