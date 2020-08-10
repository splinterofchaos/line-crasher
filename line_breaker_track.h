#pragma once

#include <memory>

#include <glm/gtc/constants.hpp>
#include <glm/vec3.hpp>

#include "line_breaker_components.h"
#include "line_breaker_shader.h"
#include "math.h"
#include "random.h"

struct TrackHead {
  glm::vec3 pos;
  float rotation = 0;  // The direction of the track.
  float width;
};

// The spacing between different segments of road.
constexpr static float TRACK_SPACING = 0.5f;

class TrackGeneratorStrategy {
protected:
  TrackHead start_;
  float exit_width_;

  unsigned int count_ = 0;  // Number of planks laid so far.
  unsigned int length_;     // Total number of planks to lay.

public:
  TrackGeneratorStrategy(TrackHead pos, float exit_width,
                         unsigned int length = 0)
    : start_(pos), exit_width_(exit_width), length_(length) { }

  virtual TrackHead next_plank() = 0;
  virtual bool finished() const = 0;
};

class TrackGenerator {
  ShaderBindings* shader_bindings_;
  std::size_t current_gear_ = 0;

  enum DifficultyIncreaseReason { GEAR_UP, NARROW_TRACK };

  unsigned int plank_count_ = 0;
  unsigned int planks_destroyed_ = 0;
  unsigned int difficulty_increase_at_;
  DifficultyIncreaseReason difficulty_increase_reason_;

  // Planks that have been consumed, available to be re-used.
  EntityPool plank_pool_;

  std::unique_ptr<TrackGeneratorStrategy> strategy_;

public:
  TrackHead head_;

  constexpr static float MAX_WIDTH = SHIP_HALF_WIDTH * 2 * 10;
  constexpr static float MIN_WIDTH = SHIP_HALF_WIDTH * 2 * 1;
  constexpr static float SAFE_WIDTH = MIN_WIDTH * 3;
  constexpr static unsigned int MAX_PLANKS = 50 / TRACK_SPACING;

  enum Strategy {
    CIRCULAR_CURVE,
    CHANGE_WIDTH,
    N_STRAGEGIES
  };

  void reset(glm::vec3 pos) {
    head_.pos = pos;
    head_.rotation = 0;
    head_.width = SHIP_HALF_WIDTH * 2 * 3;
    plank_count_ = 0;
    planks_destroyed_ = 0;
    difficulty_increase_at_ = 200;
    difficulty_increase_reason_ = GEAR_UP;
    current_gear_ = 0;
    plank_pool_.clear();
  }

  TrackGenerator(ShaderBindings* bindings)
    : shader_bindings_(bindings) { }

  void set_heading(float h) { head_.rotation = h; }
  float heading() const { return head_.rotation; }

  const glm::vec3& pos() { return head_.pos; }

  void set_strategy(Ecs& ecs, Strategy strat);
  void extend_track(Ecs& ecs);

  void delete_plank(Ecs& ecs, EntityId id) {
    plank_count_--;
    planks_destroyed_++;
    plank_pool_.deactivate(ecs, id);
  }
};

