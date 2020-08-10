#include <sstream>

#include "glpp.h"
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

Error GlProgram::attribute_location(const char* const name, GLint& out) const {
  out = attribute_location(name);
  if (out == -1) {
    return Error(concat_strings(
            name, " is not a valid attribute location (program id: ",
            std::to_string(id_)));
  }
  return Error();
}

Error GlProgram::uniform_location(const char* const name, GLint& out) const {
  out = uniform_location(name);
  if (out == -1) {
    return Error(concat_strings(
            name, " is not a valid uniform location (program id: ",
            std::to_string(id_)));
  }
  return Error();
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
  if (SDL_Init(SDL_INIT_VIDEO|SDL_INIT_AUDIO) < 0)
    return sdl_error("initializing");

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

Error load_bmp_texture(const char* const filename, GLuint& texture) {
  SDL_Surface* surface = SDL_LoadBMP(filename);
  if (surface == nullptr) {
    return Error(concat_strings("Failed to load image: ", SDL_GetError()));
  }

  texture = gl::genTexture();
  gl::bindTexture(GL_TEXTURE_2D, texture);
  gl::texImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface->w, surface->h, 0, GL_BGRA,
                 GL_UNSIGNED_BYTE, surface->pixels);
  SDL_FreeSurface(surface);

  if (auto e = glGetError(); e != GL_NO_ERROR) {
    return Error(concat_strings(
        "I'm too lazy to figure out if there's a function which maps this GL "
        "error number to a string so here's a number: ", std::to_string(e)));
  }

  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);

  return Error();
}
