#include "line_breaker_track.h"

// When changing the width of the track, we take `len` planks to go from `old_`
// to `new_width`. This calculates the width of the i'th plank.
static float smooth_width(float old_width, float new_width,
                          unsigned int i, unsigned int half_phase) {
  const float theta = (float(i + 1) / half_phase) * glm::pi<float>();
  return glm::mix(new_width, old_width, std::cos(theta) / 2 + 0.5);
}

class TrackStrategyChangeWidth : public TrackGeneratorStrategy{
public:
  TrackStrategyChangeWidth(TrackHead pos, float exit_width,
                           unsigned int length)
    : TrackGeneratorStrategy(pos, exit_width, length) { }

  TrackHead next_plank() override {
    ++count_;
    TrackHead ret{
        .pos = start_.pos + radial_vec(start_.rotation,
                                       TRACK_SPACING * count_),
        .rotation = start_.rotation,
        .width = smooth_width(start_.width, exit_width_, count_, length_)
    };
    return ret;
  }

  bool finished() const override { return count_ >= length_; }
};

class TrackStrategyCircularCurve : public TrackGeneratorStrategy {
  glm::vec3 center_;        // Center of the circle.
  float radius_;
  float theta_;
  int dir_;

public:
  TrackStrategyCircularCurve(TrackHead pos, float exit_width,
                             float radius)
    : TrackGeneratorStrategy(pos, exit_width), radius_(radius) {
    dir_ = random_bool() ? 1 : -1;

    // We want to draw the next segment TRACK_SPACING further into the curve.
    // In other words, we want an arc length of TRACK_SPACING.
    //    arc length = r * theta.
    //    arc length / r = theta.
    // where theta is the rate of change.
    theta_ = (TRACK_SPACING / radius_) * dir_;

    // Each turn should have an angle of between 45 and 90 degrees.
    float angle = (random_int(50, 101) / 100.f) * glm::half_pi<float>();
    length_ = std::abs(angle / theta_) + 1;

    center_ = start_.pos +
      radial_vec(start_.rotation + glm::half_pi<float>() * dir_, radius_);
  }

  TrackHead next_plank() override {
    ++count_;
    float rotation = start_.rotation + theta_ * count_;
    TrackHead ret{
      .pos = center_ + radial_vec(rotation - glm::half_pi<float>() * dir_,
                                  radius_),
      .rotation = rotation,
      .width = smooth_width(start_.width, exit_width_, count_, length_)};
    return ret;
  }

  bool finished() const override { return count_ >= length_; }
      
};

void TrackGenerator::set_strategy(Ecs& ecs, Strategy strat) {
  // Disallow consecutive thin tracks to keep turns wider.
  const float min_width = head_.width < SAFE_WIDTH ? SAFE_WIDTH : MIN_WIDTH;
  const float new_track_width = random_int(min_width, MAX_WIDTH);

  switch (strat) {
    case TrackGenerator::CHANGE_WIDTH:
      strategy_.reset(new TrackStrategyChangeWidth(head_, new_track_width,
                                                   10.f / TRACK_SPACING));
      break;
    case TrackGenerator::CIRCULAR_CURVE: {
      float gear_turn_ratio = (current_gear_ + 1) * 0.5;
      float larger_width = std::max(head_.width, new_track_width);
      float radius = random_int(
          std::max(larger_width * 1.5f, larger_width * gear_turn_ratio),
          larger_width * 5);
      strategy_.reset(new TrackStrategyCircularCurve(head_, new_track_width,
                                                     radius));
      break;
    }
    case TrackGenerator::N_STRAGEGIES:
      std::cerr << "unhandled TrackGenerator::Strategy" << std::endl;
  }
}

void TrackGenerator::extend_track(Ecs& ecs) {
  for (; plank_count_ < MAX_PLANKS; ++plank_count_) {
    if (planks_destroyed_ >= difficulty_increase_at_) {
      if (current_gear_ + 1 < GEARS.size()) ++current_gear_;
      difficulty_increase_at_ *= 2.5;
    }

    if (!strategy_ || strategy_->finished()) 
      set_strategy(ecs, Strategy(random_int(N_STRAGEGIES)));

    plank_pool_.create_new(ecs,
                           Transform{head_.pos,
                                     head_.rotation + glm::half_pi<float>(),
                                     head_.width},
                           shader_bindings_,
                           Color(GEARS[current_gear_].color),
                           PlankData{current_gear_});
    head_ = strategy_->next_plank();
  }
}
