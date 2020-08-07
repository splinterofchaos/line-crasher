#pragma once

#include "graphics.h"
#include "util.h"

Error contruct_textured_shader(GlProgram& ship_shader_program);
Error construct_line_shader(GlProgram& line_shader_program);

// References all shader uniforms and attribute bindings.
struct ShaderBindings {
  GlProgram* program;

  GLuint vbo;
  GLuint texture = 0;

  GLint texture_uniform = -1;
  GLint transform_uniform = -1;
  GLint length_uniform = -1;
  GLint color_uniform = -1;

  GLint vertex_pos_attrib = -1;
  GLint tex_coord_attrib = -1;
};
