#pragma once

#include <chrono>
#include <glm/vec3.hpp>

#include "ecs.h"
#include "line_breaker_shader.h"  // For ShaderBindings
#include "math.h"
#include "timer.h"

// The coefficient of negative acceleration proportionate to velocity.
// Similar to air resistance + friction.
inline constexpr float SHIP_RESISTENCE = 0.001f;
// The acceleration applied by side thrusters to keep the ship moving forward.
inline constexpr float SHIP_SIDE_THRUST = 0.0005;
inline constexpr float SHIP_THRUST       = 0.0000002f;
inline constexpr float SHIP_THRUST_DECAY = 0.000000075f;
inline constexpr float SHIP_ROTATE_SPEED = 0.005;

inline constexpr float SHIP_NOSE_LENGTH = 0.8f;
inline constexpr float SHIP_TAIL_LENGTH = 0.2f;
inline constexpr float SHIP_LENGTH = SHIP_NOSE_LENGTH + SHIP_TAIL_LENGTH;
inline constexpr float SHIP_HALF_WIDTH = 0.5f;

inline constexpr float PLANK_WIDTH = 0.05f;
inline constexpr auto BROKEN_PLANK_LIFETIME = std::chrono::seconds(1);
inline constexpr auto FLAME_LIFETIME = std::chrono::milliseconds(500);

// All entities that can be rendered have a transform that describes their
// position and shape.
struct Transform {
  glm::vec3 pos;
  float rotation;  // in radians.

  // TODO: this should really be in PlankData. The question is how to properly
  // pipe it to the render code.
  float length;  // Used for lines in determining how long they are.
};

// All things that move through the world.
struct Physics {
  static constexpr float MAX_SPEED = 0.04;

  glm::vec3 a, v;
  float rotation_velocity;

  // TODO: This is a nice implicit Euler integration, but consider RK4.
  // ref: https://gafferongames.com/post/integration_basics/#:~:text=Euler%20integration%20is%20the%20most,is%20constant%20over%20the%20timestep.&text=However%2C%20we%20are%20also%20integrating,error%20in%20the%20integrated%20position.
  inline void integrate(Transform& t, unsigned int time_step) {
    t.rotation += rotation_velocity * time_step;

    v += a * float(time_step);
    if (glm::length(v) > MAX_SPEED) v = vec_resize(v, MAX_SPEED);

    t.pos += v * float(time_step);
  }
};

struct Color {
  // The color can change over time. If colors.size() is one or t is zero,
  // the color is colors[0]. If t = 1, colors[colors.size-1], and so on. If
  // t is between to indexes, the colors are mixed.
  std::vector<glm::vec4> colors;
  float t = 0;

  // TODO: Debug why we get a segfault if we don't include this copy
  // constructor which should be auto-generated. Perhaps we're disabling a
  // buggy move or assignment operator?
  Color(const Color& c) {
    colors = c.colors;
  }

  Color(const glm::vec4& c) : colors({c}) { }
  Color(float r, float g, float b, float a=1) : Color(glm::vec4(r,g,b,a)) { }
  Color(std::initializer_list<glm::vec4> il) : colors(il) { }

  glm::vec4 get() const {
    assert(colors.size());
    if (!colors.size()) return glm::vec4(1, 0, 1, 1);
    // fi, or the "floating index" ranges between [0, colors.size - 1).
    float fi = t * (colors.size() - 1);
    // i and j represent the lower and upper index respectively.
    unsigned int i = std::floor(fi);
    unsigned int j = std::min(colors.size() - 1.f, std::ceil(fi));
    // Then u is the amount we should mix the preceeeding or proceeding color.
    float u = fi - i;
    return glm::mix(colors[i], colors[j], u);
  }
};

// Planks set a sort of gear for the ship to run in.
struct Gear {
  float thrust;
  glm::vec4 color;
  float thrust_particles_per_ms;
};

// Each higher gear gets brighter and offers more thrust.
constexpr std::array GEARS{
  Gear{1.500e-05, {0.1, 0.1, 0.50, 1.f}},
  Gear{2.050e-05, {0.1, 0.2, 0.55, 1.f}},
  Gear{2.500e-05, {0.2, 0.2, 0.60, 1.f}},
  Gear{3.000e-05, {0.2, 0.3, 0.80, 1.f}},
  Gear{3.500e-05, {0.3, 0.3, 0.80, 1.f}},
  Gear{4.000e-05, {0.4, 0.4, 0.80, 1.f}},
};

// Each plank points to the gear it sets the player's ship at.
struct PlankData {
  std::size_t gear;
};

using Ecs = EntityComponentSystem<Transform, Physics, Timer,
                                  ShaderBindings*, Color,
                                  PlankData>;

