#include <sstream>

#include "graphics.h"

// TODO: use glpp for the gl* functions.

Error sdl_error(const char* const action) {
  std::ostringstream oss;
  oss << "SDL error while " << action << ": " << SDL_GetError();
  return Error(oss.str());
}

Shader::Shader(Shader::Type type) {
  id_ = glCreateShader(GLenum(type));
  type_ = type;
}

void Shader::add_source(std::string src) {
  sources_.push_back(std::move(src));
}

std::string Shader::log() const {
  if (!glIsShader(id_)) {
    return concat_strings("No shader with id ", std::to_string(id_));
  }

  GLint len;
  glGetShaderiv(id_, GL_INFO_LOG_LENGTH, &len);

  std::string log(len + 1, 0);
  GLint real_len;
  glGetShaderInfoLog(id_, len, &real_len, log.data());
  log.resize(real_len + 1);

  return log;
}

Error Shader::compile() {
  const char* sources[sources_.size()];
  for (unsigned int i = 0; i < sources_.size(); ++i) {
    sources[i] = sources_[i].c_str();
  }

  glShaderSource(id_, sources_.size(), sources, nullptr);
  glCompileShader(id_);

  GLint status;
  if (glGetShaderiv(id_, GL_COMPILE_STATUS, &status); status != GL_TRUE) {
    return Error(concat_strings("Unable to compile shader (id=",
                                std::to_string(id_), "):\n", log()));
  }

  return Error();
}

std::string GlProgram::log() const {
  if (!glIsProgram(id_)) {
    return concat_strings("No program with id ", std::to_string(id_));
  }

  GLint len;
  glGetProgramiv(id_, GL_INFO_LOG_LENGTH, &len);

  std::string log(len + 1, 0);
  GLint real_len;
  glGetProgramInfoLog(id_, len, &real_len, log.data());
  log.resize(real_len + 1);

  return log;
}

Error GlProgram::link() {
  glLinkProgram(id_);

  GLint link_status;
  glGetProgramiv(id_, GL_LINK_STATUS, &link_status);
  if (link_status != GL_TRUE) {
    return Error(concat_strings("Error linking GL program ",
                                std::to_string(id_), ";\n", log()));
  }

  return Error();
}

Error Graphics::init(int width, int height) {
  if (SDL_Init(SDL_INIT_VIDEO) < 0) return sdl_error("initializing");

  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);  // 4?
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
 
  width_ = width;
  height_ = height;
  win_ = SDL_CreateWindow(
      "SRPG", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width_,
      height_, SDL_WINDOW_OPENGL|SDL_WINDOW_SHOWN);
  if (win_ == nullptr) return sdl_error("creating window");

  gl_context_ = SDL_GL_CreateContext(win_);
  if (gl_context_ == nullptr) sdl_error("creating OpenGL context");
  
  if (GLenum glew_error = glewInit(); glew_error != GLEW_OK) {
    return Error(concat_strings("error initializing GLEW: ",
                                (char*)glewGetErrorString(glew_error)));
  }

  return Error();
}

void Graphics::swap_buffers() {
  SDL_GL_SwapWindow(win_);
}

Graphics::~Graphics() {
  SDL_GL_DeleteContext(gl_context_);
  SDL_DestroyWindow(win_);
  SDL_Quit();
}
