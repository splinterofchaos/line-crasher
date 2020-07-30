#include <SDL2/SDL.h>
#include <GL/glew.h>
#include <SDL2/SDL_opengl.h>
#include <GL/glu.h>

#include <cstdlib>
#include <iostream>

#include "glpp.h"
#include "graphics.h"
#include "vec.h"


constexpr int WINDOW_HEIGHT = 640;
constexpr int WINDOW_WIDTH = 480;

Error run() {
  Graphics gfx;
  if (Error e = gfx.init(WINDOW_WIDTH, WINDOW_HEIGHT); !e.ok) return e;


  Shader verts(Shader::Type::VERTEX);
  verts.add_source(
		"#version 140\n"
    "in vec2 vertex_pos;"
    "in vec2 tex_coord;"
    "out vec2 TexCoord;"
    "void main() {"
      "gl_Position = vec4(vertex_pos, 0, 1);"
      "TexCoord = tex_coord;"
    "}"
  );

  if (Error e = verts.compile(); !e.ok) return e;

  Shader frag(Shader::Type::FRAGMENT);
  frag.add_source(
    "#version 140\n"
    "in vec2 TexCoord;"
    "in vec2 tex_coord;"
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

  SDL_Surface* floor_surface = SDL_LoadBMP("art/goblin.bmp");
  if (floor_surface == nullptr) {
    return Error(concat_strings("Failed to load art/floor.bmp: ",
                                SDL_GetError()));
  }

  GLuint floor_texture = gl::genTexture();
  gl::bindTexture(GL_TEXTURE_2D, floor_texture);
  gl::texImage2D(GL_TEXTURE_2D, 0, GL_RGB, floor_surface->w,
                 floor_surface->h, 0, GL_RGB, GL_UNSIGNED_BYTE,
                 floor_surface->pixels);
  SDL_FreeSurface(floor_surface);

  if (auto e = glGetError(); e != GL_NO_ERROR) {
    return Error(concat_strings(
        "I'm too lazy to figure out if there's a function which maps this GL "
        "error number to a string so here's a number: ", std::to_string(e)));
  }

  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR,
                   {.8f, 0.5f, 0.8f, 1.f});

  //GLuint vao;
  //glGenVertexArrays(1, &vao);
  //glBindVertexArray(vao);

  struct Vertex {
    Vec<GLfloat, 2> pos;
    Vec<GLfloat, 2> tex_coord;
  };

  //VBO data
  Vertex vertecies[] = {
    // Position    TexCoords
    {{-0.5f, -0.5f},  {1.0f, 1.0f}},
    {{ 0.5f, -0.5f},  {0.0f, 1.0f}},
    {{ 0.5f,  0.5f},  {0.0f, 0.0f}},
    {{-0.5f,  0.5f},  {1.0f, 0.0f}}
  };

  GLuint vbo = gl::genBuffer();
  gl::bindBuffer(GL_ARRAY_BUFFER, vbo);
  gl::bufferData(GL_ARRAY_BUFFER, vertecies, GL_STATIC_DRAW);

  //IBO data
  GLuint vbo_elems[] = {0, 1, 2,
                        2, 3, 0};

  GLuint vbo_elems_id = gl::genBuffer();
  gl::bindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_elems_id);
  gl::bufferData(GL_ELEMENT_ARRAY_BUFFER, vbo_elems, GL_STATIC_DRAW);

  bool keep_going = true;
  SDL_Event e;
  while (keep_going) {
    while (SDL_PollEvent(&e) != 0) {
      if (e.type == SDL_QUIT) keep_going = false;
    }

    gl::clear();

    // This must happen before gl_program.use().
    auto uni = gl_program.uniform_location("tex");
    if (uni == -1) return Error("tex is not a valid uniform location.");
    gl::uniform(uni, floor_texture);

    gl_program.use();


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
