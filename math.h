#pragma once

#include <glm/vec2.hpp>
#include <glm/mat4x4.hpp>

glm::mat4x4 transformation(glm::vec3 pos, float angle, float scale);

// Returns the Z-axis part of a 3D cross product.
float cross2(const glm::vec3& a, const glm::vec3& b);

// TODO: unused. Consider deleting.
bool barycentric_point_in_triangle(glm::vec3 point, glm::vec3 v0, glm::vec3 v1,
                                   glm::vec3 v2);

glm::vec3 radial_vec(float radians, float length = 1);

inline glm::vec3 vec_resize(const glm::vec3& v, float size) {
  return glm::normalize(v) * size;
}

inline glm::vec2 vec_resize(const glm::vec2& v, float size) {
  return glm::normalize(v) * size;
}

inline glm::vec2 clockwize(const glm::vec2& v) {
  return glm::vec2(-v.y, v.x);
}

inline glm::vec3 clockwize(const glm::vec3& v) {
  return glm::vec3(-v.y, v.x, v.z);
}

template<typename T>
inline auto plus_minus(T init, T operand) {
  return std::tuple(init + operand, init - operand);
}

bool segment_segment_intersection(glm::vec3 p1, glm::vec3 p2,
                                  glm::vec3 q1, glm::vec3 q2);


