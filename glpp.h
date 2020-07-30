// This file is intedned to contain the lightest wrapper over OpenGL's C API
// possible. This seems like a great opportunity to use a third-party library,
// but most appear to have not been updated for  3 years or demand the use of
// them exclusively. For example, ClanLib wants to control the window and
// inputs, and certain others force all code to be written in Java-like
// classes.
//
// Functions shall map very clearly to OpenGL functions. For example,
// glClearColor is gl::clearColor. glTexParameteri, glTexParameterf, and most
// other variants just map to gl::texParameter. Likewise, if a type can be
// deduced from not converting an argument to a void*, there shall be no GLenum
// argument to differentiate.
//
// Enums may be provided for type safety, but in general the standard OpenGL
// constants are still used, so GL_TEXTURE_2D, not gl::Texture2d or some deeply
// nested hierarchical concept.
//
// Occasionally, default arguments may be supplied for functions where there is
// rarely a different argument. For example, GL_COLOR_BUFFER_BIT to glClear().

#include <utility>
#include <GL/glew.h>

#include <initializer_list>

namespace gl {

template<typename T>
struct GlTraits { };

template<>
struct GlTraits<GLint> {
  static constexpr auto glType = GL_INT;
  static constexpr auto texParameter = glTexParameteri;
  static constexpr auto texParameterv = glTexParameteriv;
  // Not usable in constexpr:
  static constexpr auto& uniform1 = glUniform1i;
  static constexpr auto& uniform2 = glUniform2i;
  static constexpr auto& uniform3 = glUniform3i;
  static constexpr auto& uniform4 = glUniform4i;
};

template<>
struct GlTraits<GLuint> {
  static constexpr auto glType = GL_UNSIGNED_INT;
  static constexpr auto texParameter = glTexParameteri;
  static constexpr auto texParameterv = glTexParameteriv;
  // Not usable in constexpr:
  static constexpr auto& uniform1 = glUniform1ui;
  static constexpr auto& uniform2 = glUniform2ui;
  static constexpr auto& uniform3 = glUniform3ui;
  static constexpr auto& uniform4 = glUniform4ui;
};

template<>
struct GlTraits<GLfloat> {
  static constexpr auto glType = GL_FLOAT;
  static constexpr auto texParameter = glTexParameterf;
  static constexpr auto texParameterv = glTexParameterfv;
  static constexpr auto& uniform1 = glUniform1f;
  static constexpr auto& uniform2 = glUniform2f;
  static constexpr auto& uniform3 = glUniform3f;
  static constexpr auto& uniform4 = glUniform4f;
};

inline void clearColor(GLclampf r, GLclampf g, GLclampf b, GLclampf a) {
  glClearColor(r, g, b, a);
}

inline GLuint genTexture() {
  GLuint id;
  glGenTextures(1, &id);
  return id;
}

inline void bindTexture(GLenum type, GLuint texture) {
  glBindTexture(type, texture);
}

// TODO: Maybe one overload could take a vector for width/height. glm?
inline void texImage2D(GLenum target, GLint level, GLint internalformat,
                       GLsizei width, GLsizei height, GLint border,
                       GLenum format, GLenum type, const void* data) {
  glTexImage2D(target, level, internalformat, width, height, border, format,
               type, data);
}

template<typename T>
inline void texImage2D(GLenum target, GLint level, GLint internalformat,
                       GLsizei width, GLsizei height, GLint border,
                       GLenum format, const T* data) {
  return texImage2D(target, level, internalformat, width, height, border,
                    format, GlTraits<T>::glType, (void*)data);
}

template<typename T>
void texParameter(GLenum target, GLenum pname, const T param) {
  if constexpr (std::is_pointer_v<T>) {
    GlTraits<std::decay_t<std::remove_pointer_t<T>>>::texParameterv(
        target, pname, param);
  } else {
    GlTraits<T>::texParameter(target, pname, param);
  }
}

template<typename T>
void texParameter(GLenum target, GLenum pname, const std::initializer_list<T> param) {
  texParameter(target, pname, param.begin());
}

inline GLuint genBuffer() {
  GLuint id;
  glGenBuffers(1, &id);
  return id;
}

inline void bindBuffer(GLenum type, GLuint id) {
  glBindBuffer(type, id);
}

inline void bufferData(GLuint buffer, GLsizeiptr size, const void* data, GLenum usage) {
  glBufferData(buffer, size, data, usage);
}

template<typename T, size_t N>
void bufferData(GLuint buffer, const T (&data)[N], GLenum usage) {
  bufferData(buffer, sizeof(T) * N, data, usage);
}

inline void clear(GLbitfield mask = GL_COLOR_BUFFER_BIT) {
  glClear(mask);
}

// TODO: support the "v" variants.
template<typename T>
inline void uniform(GLint location, T data) {
  GlTraits<T>::uniform1(location, data);
}

template<typename T>
inline void uniform(GLint location, T data, T data2) {
  GlTraits<T>::uniform2(location, data, data2);
}

template<typename T>
inline void uniform(GLint location, T data, T data2, T data3) {
  GlTraits<T>::uniform3(location, data, data2, data3);
}

template<typename T>
inline void uniform(GLint location, T data, T data2, T data3, T data4) {
  GlTraits<T>::uniform4(location, data, data2, data3, data4);
}

inline void enableVertexAttribArray(GLuint index) {
  glEnableVertexAttribArray(index);
}

inline void vertexAttribPointer(GLuint index, GLint size, GLenum type,
                                GLboolean normalized, GLsizei stride,
                                const void* pointer) {
  glVertexAttribPointer(index, size, type, normalized, stride, pointer);
}

template<typename T>
inline void vertexAttribPointer(GLuint index, GLint size, GLboolean normalized,
                                GLsizei stride, const T* pointer) {
  vertexAttribPointer(index, size, GlTraits<T>::glType, normalized, stride,
                      pointer);
}

template<typename RealType, typename T, typename Mem>
inline void vertexAttribPointer(GLuint index, GLint size, GLboolean normalized,
                                const Mem T::*mem) {
  // This is basically C's offsetof() macro generalized to member pointers.
  RealType* pointer = (RealType*)&(((T*)nullptr)->*mem);
  vertexAttribPointer(index, size, normalized, sizeof(T), pointer);
}

inline void drawElements(GLenum mode, GLsizei count, GLenum type, const void* indices) {
  glDrawElements(mode, count, type, indices);
}

inline void useProgram(GLuint target) {
  glUseProgram(target);
}
}
