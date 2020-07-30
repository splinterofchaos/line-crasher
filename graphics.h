#include <SDL2/SDL.h>
#include <GL/glew.h>
#include <SDL2/SDL_opengl.h>
#include <GL/glu.h>

#include <string>
#include <vector>

#include "util.h"

class Shader {
public:
  enum class Type : GLenum {
    VERTEX = GL_VERTEX_SHADER,
    FRAGMENT = GL_FRAGMENT_SHADER
  };

private:
  std::vector<std::string> sources_;
  GLuint id_;
  Type type_;

public:
  Shader(Type type);
  Shader(const Shader&) = delete;

  void add_source(std::string src);
  Error compile();

  std::string log() const;

  GLuint id() const { return id_; }
};

class GlProgram {
  GLuint id_;

public:
  GlProgram() { id_ = glCreateProgram(); }
  ~GlProgram() { glDeleteProgram(id_); }

  GLuint id() const { return id_; }

  void add_shader(const Shader& s) { glAttachShader(id_, s.id()); }

  GLint attribute_location(const char* const name) {
    return glGetAttribLocation(id_, name);
  }

  GLint uniform_location(const char* const name) {
    return glGetUniformLocation(id_, name);
  }

  Error link();

  // Ask GL to use this program.
  void use() { glUseProgram(id_); }
  void unuse() { glUseProgram(0); }

  std::string log() const;
};

// This is a little POD for organizing SDL and OpenGL code.
class Graphics {
  SDL_Window* win_ = nullptr;
  int width_;
  int height_;

  SDL_GLContext gl_context_ = nullptr;

public:
  Graphics() { }

  Error init(int width, int height);

  void swap_buffers();

  ~Graphics();
};
