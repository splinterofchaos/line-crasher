#pragma once

#include <chrono>
#include <glm/vec3.hpp>

#include "ecs.h"
#include "line_breaker_shader.h"  // For ShaderBindings
#include "math.h"

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

inline constexpr auto BROKEN_PLANK_LIFETIME = std::chrono::seconds(2);

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
  glm::vec4 color;

  Color() { }

  Color(const glm::vec4& c) : color(c) { }
  Color(float r, float g, float b, float a=1)
    : color(r, g, b, a) { }

  glm::vec4 get() const { return color; }
};

struct TimeToDie {
  std::chrono::high_resolution_clock::time_point time_to_die;
};

// Planks set a sort of gear for the ship to run in.
struct Gear {
  float thrust;
  glm::vec4 color;
};

// Each higher gear gets brighter and offers more thrust.
constexpr std::array GEARS{
  Gear{1.500e-05, {0.1, 0.1, 0.50, 1.f}},
  Gear{1.750e-05, {0.1, 0.2, 0.55, 1.f}},
  Gear{2.000e-05, {0.2, 0.2, 0.60, 1.f}},
  Gear{2.300e-05, {0.2, 0.3, 0.80, 1.f}},
  Gear{2.500e-05, {0.3, 0.4, 0.80, 1.f}},
  Gear{2.580e-05, {0.4, 0.4, 0.90, 1.f}},
  Gear{3.000e-05, {0.5, 0.5, 1.00, 1.f}},
  Gear{3.500e-05, {0.6, 0.6, 1.00, 1.f}},
  Gear{4.000e-05, {1.0, 1.0, 1.00, 1.f}}
};

// Each plank points to the gear it sets the player's ship at.
struct PlankData {
  std::size_t gear;
};

using Ecs = EntityComponentSystem<Transform, Physics, TimeToDie,
                                  ShaderBindings*, Color,
                                  PlankData>;

