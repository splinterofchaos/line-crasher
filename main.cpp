#include <SDL2/SDL.h>
#include <GL/glew.h>
#include <SDL2/SDL_opengl.h>
#include <GL/glu.h>
#include <glm/vec2.hpp>
#include <glm/vec3.hpp>
#include <glm/mat4x4.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtx/closest_point.hpp>

#include <array>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <memory>

#include "line_breaker_shader.h"
#include "line_breaker_components.h"

#include "ecs.h"
#include "glpp.h"
#include "graphics.h"
#include "math.h"
#include "random.h"

constexpr int WINDOW_HEIGHT = 800;
constexpr int WINDOW_WIDTH = 800;

// Target 60 FPS and do physics updates four times as often.
constexpr auto TIME_STEP = std::chrono::milliseconds(1000) / (60 * 4);
constexpr auto TIME_STEP_MS = TIME_STEP.count();

constexpr auto BROKEN_PLANK_LIFETIME = std::chrono::seconds(2);

// The coefficient of negative acceleration proportionate to velocity.
// Similar to air resistance + friction.
constexpr float SHIP_RESISTENCE = 0.001f;
// The acceleration applied by side thrusters to keep the ship moving forward.
constexpr float SHIP_SIDE_THRUST = 0.0005;
constexpr float SHIP_THRUST       = 0.0000002f;
constexpr float SHIP_THRUST_DECAY = 0.000000075f;
constexpr float SHIP_ROTATE_SPEED = 0.005;

constexpr float SHIP_NOSE_LENGTH = 0.8f;
constexpr float SHIP_TAIL_LENGTH = 0.2f;
constexpr float SHIP_LENGTH = SHIP_NOSE_LENGTH + SHIP_TAIL_LENGTH;
constexpr float SHIP_HALF_WIDTH = 0.5f;

std::ostream& operator<<(std::ostream& os, const glm::vec3 v) {
  return os << '<' << v.x << ", " << v.y << ", " << v.z << '>';
}

// Represents the in-game understanding of user inputs.
struct ShipController {
  bool thruster = false;
  bool breaks = false;
  bool rotate_clockwise = false;
  bool rotate_counterclockwise = false;

  void reset() {
    thruster = rotate_clockwise = rotate_counterclockwise = false;
  }
};

struct ShipPoints {
  glm::vec3 center_of_gravity, nose, left_back, right_back;

  ShipPoints(const Transform& ship_transform) {
    center_of_gravity = ship_transform.pos;
    glm::vec3 to_nose = radial_vec(ship_transform.rotation,
                                   SHIP_NOSE_LENGTH);
    glm::vec3 ship_back = ship_transform.pos +
      vec_resize(to_nose, -SHIP_TAIL_LENGTH);
    glm::vec3 to_left = vec_resize(clockwize(to_nose), SHIP_HALF_WIDTH);

    nose = ship_transform.pos + to_nose;
    std::tie(left_back, right_back) = plus_minus(ship_back, to_left);
  }
};

struct LinePoints {
  glm::vec3 a, b;

  LinePoints(const Transform& line_transform) {
    glm::vec3 parallel = radial_vec(line_transform.rotation,
                                    line_transform.length / 2);
    std::tie(a, b) = plus_minus(line_transform.pos, parallel);
  }
};

// Check if the line segment, [a, b], intersects either side of the ship. Since
// it can't go backwards, we don't need to check the back side.
std::pair<float, bool> intersection(const ShipPoints& ship_points,
                                    const LinePoints& line_points) {
  if (auto [point, has_intersection] = segment_segment_intersection(
          ship_points.left_back, ship_points.nose,
          line_points.a, line_points.b);
      has_intersection)
    return {point, has_intersection};
  return segment_segment_intersection(ship_points.right_back, ship_points.nose,
                                      line_points.a, line_points.b);
}

struct Vertex {
  glm::vec3 pos;
  glm::vec2 tex_coord;
};

void draw_object(const Transform& transform,
                 const ShaderBindings* shader_bindings,
                 const Color& color,
                 glm::vec3 camera_offset,
                 float zoom) {
  static const ShaderBindings* last_bindings = nullptr;
  if (last_bindings != shader_bindings) {
    shader_bindings->program->use();

    gl::bindBuffer(GL_ARRAY_BUFFER, shader_bindings->vbo);

    gl::enableVertexAttribArray(shader_bindings->vertex_pos_attrib);
    gl::vertexAttribPointer<float>(shader_bindings->vertex_pos_attrib, 3,
                                   GL_FALSE, &Vertex::pos);

    if (shader_bindings->tex_coord_attrib != -1) {
      gl::enableVertexAttribArray(shader_bindings->tex_coord_attrib);
      gl::vertexAttribPointer<float>(shader_bindings->tex_coord_attrib, 2,
                                     GL_FALSE, &Vertex::tex_coord);
    }
  }
  last_bindings = shader_bindings;

  glm::vec3 visual_pos = transform.pos;
  visual_pos -= camera_offset;

  glUniformMatrix4fv(
      shader_bindings->transform_uniform, 1, GL_FALSE,
      glm::value_ptr(transformation(visual_pos, transform.rotation,
                                    zoom)));

  if (shader_bindings->length_uniform != -1) {
    gl::uniform(shader_bindings->length_uniform, transform.length);
  }

  if (shader_bindings->color_uniform != -1) {
    gl::uniform3v(shader_bindings->color_uniform, 1, glm::value_ptr(color.c));
  }

  gl::drawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

std::chrono::milliseconds time_diff(
    std::chrono::high_resolution_clock::time_point old_time,
    std::chrono::high_resolution_clock::time_point new_time) {
  if (old_time > new_time) return time_diff(new_time, old_time);
  return std::chrono::duration_cast<std::chrono::milliseconds>(
      new_time - old_time);
}

class TrackGeneratorStrategy;

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
  struct Head {
    glm::vec3 pos;
    float rotation = 0;  // The direction of the track.
    float width;
  } head_;

  // The spacing between different segments of road.
  constexpr static float SPACING = 0.5f;
  constexpr static float MAX_WIDTH = SHIP_HALF_WIDTH * 2 * 10;
  constexpr static float MIN_WIDTH = SHIP_HALF_WIDTH * 2 * 1;
  constexpr static float SAFE_WIDTH = MIN_WIDTH * 3;
  constexpr static unsigned int MAX_PLANKS = 200;

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

// When changing the width of the track, we take `len` planks to go from `old_`
// to `new_width`. This calculates the width of the i'th plank.
float smooth_width(float old_width, float new_width,
                   unsigned int i, unsigned int half_phase) {
  const float theta = (float(i + 1) / half_phase) * glm::pi<float>();
  return glm::mix(new_width, old_width, std::cos(theta) / 2 + 0.5);

}

class TrackGeneratorStrategy {
protected:
  TrackGenerator::Head start_;
  float exit_width_;

  unsigned int count_ = 0;  // Number of planks laid so far.
  unsigned int length_;     // Total number of planks to lay.

public:
  TrackGeneratorStrategy(TrackGenerator::Head pos, float exit_width,
                         unsigned int length = 0)
    : start_(pos), exit_width_(exit_width), length_(length) { }

  virtual TrackGenerator::Head next_plank() = 0;
  virtual bool finished() const = 0;
};


class TrackStrategyChangeWidth : public TrackGeneratorStrategy{
public:
  TrackStrategyChangeWidth(TrackGenerator::Head pos, float exit_width,
                           unsigned int length)
    : TrackGeneratorStrategy(pos, exit_width, length) { }

  TrackGenerator::Head next_plank() override {
    ++count_;
    TrackGenerator::Head ret{
        .pos = start_.pos + radial_vec(start_.rotation,
                                       TrackGenerator::SPACING * count_),
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
  TrackStrategyCircularCurve(TrackGenerator::Head pos, float exit_width,
                             float radius)
    : TrackGeneratorStrategy(pos, exit_width), radius_(radius) {
    dir_ = random_bool() ? 1 : -1;

    // We want to draw the next segment SPACING further into the curve. In
    // other words, we want an arc length of SPACING.
    //    arc length = r * theta.
    //    arc length / r = theta.
    // where theta is the rate of change.
    theta_ = (TrackGenerator::SPACING / radius_) * dir_;

    // Each turn should have an angle of between 45 and 90 degrees.
    float angle = (random_int(50, 101) / 100.f) * glm::half_pi<float>();
    length_ = std::abs(angle / theta_) + 1;

    center_ = start_.pos +
      radial_vec(start_.rotation + glm::half_pi<float>() * dir_, radius_);
  }

  TrackGenerator::Head next_plank() override {
    ++count_;
    float rotation = start_.rotation + theta_ * count_;
    TrackGenerator::Head ret{
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
                                                   10.f / SPACING));
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
                           Color{GEARS[current_gear_].color},
                           PlankData{current_gear_});
    head_ = strategy_->next_plank();
  }
}

// When the ship hits a plank, it will break it into two pieces. Adds one of
// them to the ECS.
void write_broken_plank(
    Ecs& ecs,
    EntityPool& pool,
    // Between a and b, one point of the plank and where the intersection
    // happened.
    const glm::vec3& a,
    const glm::vec3& b,
    float u,  // The ratio of the plank this piece came from.
    const Transform& plank_transform,
    const glm::vec3& incoming_velocity,
    ShaderBindings& shader_bindings,
    const Color& color,
    const std::chrono::high_resolution_clock::time_point& now) {
  // Rotate clockwise (-1) or counter clockwise?
  float dir = glm::sign(cross2(b - a, incoming_velocity));
  // If 100% of linear velocity were converted into rotational velocity at the
  // point of intersection, we would have
  //    v = X * r
  // where X is radians over time. If r is small, X is large; if r is large, X
  // is small.
  float rotational_vel = glm::length(incoming_velocity) * 0.5f /
    (plank_transform.length * u * 0.5f) *  // <- actual radius
    dir;
  glm::vec3 v = incoming_velocity * 0.3f +
                clockwize(incoming_velocity) * 0.2f * dir;
  pool.create_new(
      ecs,
      Transform{(a + b) / 2.f,
                plank_transform.rotation,
                plank_transform.length * u},
      Physics{glm::vec3(), v, rotational_vel},
      TimeToDie{now + BROKEN_PLANK_LIFETIME},
      &shader_bindings,
      Color(color));
}

// Holds much of the game logic state allowing us to do operations like reset
// it to the start.
class Game {
  Ecs ecs_;
  EntityId player_;
  // The acceleration the player ship always has in the direction it faces.
  float  player_thrust_;
  TrackGenerator track_gen_;

  // The pool used for managing broken planks. (Planks split in two when the
  // player hits them.)
  EntityPool broken_plank_pool_;

  ShaderBindings* player_shader_bindings_;
  ShaderBindings* line_shader_bindings_;

  // If true, the player may control their thrusters with up and down on the
  // arrow keys.
  bool manual_thrusters_enabled_ = true;

public:
  void reset();

  Game(ShaderBindings* player_shader_bindings,
       ShaderBindings* line_shader_bindings)
    : track_gen_(line_shader_bindings),
      player_shader_bindings_(player_shader_bindings),
      line_shader_bindings_(line_shader_bindings)
  {
    reset();
  }

  Ecs& ecs() { return ecs_; }
  const Ecs& ecs() const { return ecs_; }

  TrackGenerator& track_gen() { return track_gen_; }
  const TrackGenerator& track_gen() const { return track_gen_; }

  EntityId player() const { return player_; }

  void set_manual_thrusters_enabled(bool b) { manual_thrusters_enabled_ = b; }
  bool manual_thrusters_enabled() const { return manual_thrusters_enabled_; }

  void add_player_thrust(float thrust) {
    player_thrust_ = std::max(player_thrust_ + thrust, 0.f);
  }

  void set_player_thrust(float thrust) { player_thrust_ = thrust; }
  float player_thrust() const { return player_thrust_; }

  EntityPool& broken_plank_pool() { return broken_plank_pool_; }
  const EntityPool& broken_plank_pool() const { return broken_plank_pool_; }
};

void Game::reset() {
  ecs().clear();
  track_gen_.reset(glm::vec3(3, 0, 0));
  track_gen().set_strategy(ecs(), TrackGenerator::CHANGE_WIDTH);

  broken_plank_pool().clear();

  manual_thrusters_enabled_ = true;
  player_thrust_ = 0;
  player_ = ecs().write_new_entity(
      Transform{glm::vec3(0.0f), 0, 0},
      Physics{glm::vec3(), glm::vec3(), 0},
      Color{glm::vec3()},  // unused
      player_shader_bindings_);
}

Error run() {
  random_seed();

  Graphics gfx;
  if (Error e = gfx.init(WINDOW_WIDTH, WINDOW_HEIGHT); !e.ok) return e;

  GlProgram ship_shader_program;
  if (Error e = construct_ship_shader(ship_shader_program); !e.ok) return e;

  GlProgram line_shader_program;
  if (Error e = construct_line_shader(line_shader_program); !e.ok) return e;

  //Initialize clear color
  gl::clearColor(0.f, 0.f, 0.f, 1.f);

  GLuint ship_texture;
  if (Error e = load_bmp_texture("art/ship 512 RGBA8.bmp", ship_texture);
      !e.ok)
    return e;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  //VBO data
  Vertex quad_vertecies[] = {
    // Position              TexCoords
    {{-SHIP_TAIL_LENGTH, -SHIP_HALF_WIDTH, 0.0f},  {0.0f, 1.0f}},
    {{ SHIP_NOSE_LENGTH, -SHIP_HALF_WIDTH, 0.0f},  {1.0f, 1.0f}},
    {{ SHIP_NOSE_LENGTH,  SHIP_HALF_WIDTH, 0.0f},  {1.0f, 0.0f}},
    {{-SHIP_TAIL_LENGTH,  SHIP_HALF_WIDTH, 0.0f},  {0.0f, 0.0f}}
  };

  GLuint quad_vbo = gl::genBuffer();
  gl::bindBuffer(GL_ARRAY_BUFFER, quad_vbo);
  gl::bufferData(GL_ARRAY_BUFFER, quad_vertecies, GL_DYNAMIC_DRAW);

  Vertex line_vertecies[] = {
    // Position               TexCoords
    {{-0.5f, -0.05f, 0.0f},  {0.0f, 0.0f}},
    {{ 0.5f, -0.05f, 0.0f},  {0.0f, 0.0f}},
    {{ 0.5f,  0.05f, 0.0f},  {0.0f, 0.0f}},
    {{-0.5f,  0.05f, 0.0f},  {0.0f, 0.0f}}
  };

  GLuint line_vbo = gl::genBuffer();
  gl::bindBuffer(GL_ARRAY_BUFFER, line_vbo);
  gl::bufferData(GL_ARRAY_BUFFER, line_vertecies, GL_DYNAMIC_DRAW);
  bool flip = false;

  //IBO data
  GLuint vbo_elems[] = {0, 1, 2,
                        2, 3, 0};

  GLuint vbo_elems_id = gl::genBuffer();
  gl::bindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_elems_id);
  gl::bufferData(GL_ELEMENT_ARRAY_BUFFER, vbo_elems, GL_STATIC_DRAW);

  ShaderBindings player_shader_bindings(&ship_shader_program, quad_vbo);
  if (Error e =
      ship_shader_program.uniform_location(
          "tex", player_shader_bindings.texture_uniform) &&
      ship_shader_program.uniform_location(
          "transform", player_shader_bindings.transform_uniform) &&
      ship_shader_program.attribute_location(
          "vertex_pos", player_shader_bindings.vertex_pos_attrib) &&
      ship_shader_program.attribute_location(
          "tex_coord", player_shader_bindings.tex_coord_attrib);
      !e.ok) return e;

  ShaderBindings line_shader_bindings(&line_shader_program, line_vbo);
  line_shader_program.use();
  if (Error e =
      line_shader_program.uniform_location(
          "transform", line_shader_bindings.transform_uniform) &&
      line_shader_program.uniform_location(
          "length", line_shader_bindings.length_uniform) &&
      line_shader_program.uniform_location(
          "color", line_shader_bindings.color_uniform) &&
      line_shader_program.attribute_location(
          "vertex_pos", line_shader_bindings.vertex_pos_attrib);
      !e.ok) return e;

  Game game(&player_shader_bindings, &line_shader_bindings);
  game.reset();

  // TODO: These should eventually be stored into components, too.
  ShipController ship_controller;

  auto time = std::chrono::high_resolution_clock::now();
  std::chrono::high_resolution_clock::time_point last_physics_update =
    time - TIME_STEP;
  std::chrono::milliseconds dtime;

  bool keep_going = true;
  SDL_Event e;

  glm::vec3 camera_offset(0.f);
  // TODO: make less linear.
  float zoom = 0.25f;

  while (keep_going) {
    while (SDL_PollEvent(&e) != 0) {
      switch (e.type) {
        case SDL_QUIT: keep_going = false; break;
        case SDL_KEYDOWN: case SDL_KEYUP:
          bool* control = nullptr;
          switch (e.key.keysym.sym) {
            case SDLK_UP: control = &ship_controller.thruster; break;
            case SDLK_DOWN: control = &ship_controller.breaks; break;
            case SDLK_LEFT:
              control = &ship_controller.rotate_counterclockwise; break;
            case SDLK_RIGHT:
              control = &ship_controller.rotate_clockwise; break;
            case 'r': game.reset(); break;
            case 'q': keep_going = false; break;
            case ' ': flip = !flip; break;
          }

          if (control) *control = e.key.type == SDL_KEYDOWN;
      }
    }

    auto new_time = std::chrono::high_resolution_clock::now();
    dtime = time_diff(new_time, time);
    static auto highest_dtime = decltype(dtime.count())(0);
    if (dtime.count() > highest_dtime) {
      std::cout << "new highest ftime: " << dtime.count() << std::endl;
      highest_dtime = dtime.count();
    }

    // Perform physics updates.
    while (time_diff(last_physics_update, new_time) > TIME_STEP) {
      last_physics_update += TIME_STEP;

      Transform& ship_transform =
        game.ecs().read_or_panic<Transform>(game.player());
      Physics& ship_physics =
        game.ecs().read_or_panic<Physics>(game.player());

      ship_physics.rotation_velocity = 0;
      game.add_player_thrust(-SHIP_THRUST_DECAY);
      if (game.manual_thrusters_enabled()) {
        if (ship_controller.thruster) game.add_player_thrust(SHIP_THRUST);
        if (ship_controller.breaks) game.add_player_thrust(-SHIP_THRUST);
      }

      if (ship_controller.rotate_clockwise)
        ship_physics.rotation_velocity -= SHIP_ROTATE_SPEED;
      if (ship_controller.rotate_counterclockwise)
        ship_physics.rotation_velocity += SHIP_ROTATE_SPEED;

      ship_physics.a = glm::vec3();
      if (game.player_thrust() != 0)
        ship_physics.a = radial_vec(ship_transform.rotation,
                                    game.player_thrust());
      if (glm::length(ship_physics.v)) {
        glm::vec3 heading = radial_vec(ship_transform.rotation);
        ship_physics.a += vec_resize(
            clockwize(ship_physics.v),
            -cross2(heading, ship_physics.v) * SHIP_SIDE_THRUST);
      }

      ship_physics.a -= ship_physics.v * SHIP_RESISTENCE;

      for (auto [_, t, phys] : game.ecs().read_all<Transform, Physics>())
        phys.integrate(t, TIME_STEP_MS);

      ship_physics.rotation_velocity = 0;

      // TODO: This isn't very intelligent. If the offset factor is too large,
      // the player will be off screen and if too small, overly centered.
      camera_offset = ship_transform.pos +
                      ship_physics.v * (500.f / TIME_STEP_MS);

      zoom = 0.25f;
      if (glm::length(ship_physics.v) > 0.001)
        zoom -= std::log(glm::length(ship_physics.v) * 1000) * 0.06;

      ShipPoints ship_points(ship_transform);
      for (const auto& [id, line_transform, line_data, color] :
           game.ecs().read_all<Transform, PlankData, Color>()) {
        // Bounds check first.
        if (glm::distance(line_transform.pos, ship_points.center_of_gravity) <
            line_transform.length + SHIP_LENGTH &&
            !game.ecs().is_marked(id)) {
          LinePoints line_points(line_transform);
          if (auto [u, intersects] = intersection(ship_points, line_points);
              intersects) {
            game.set_manual_thrusters_enabled(false);
            game.track_gen().delete_plank(game.ecs(), id);
            game.set_player_thrust(GEARS[line_data.gear].thrust);

            auto crash_point =
              line_points.a + (line_points.b - line_points.a) * u;
            write_broken_plank(game.ecs(), game.broken_plank_pool(),
                               line_points.a, crash_point, u,
                               line_transform, ship_physics.v,
                               line_shader_bindings, color, time);
            write_broken_plank(game.ecs(), game.broken_plank_pool(),
                               line_points.b, crash_point, 1 - u,
                               line_transform, ship_physics.v,
                               line_shader_bindings, color, time);
            break;
          }
        }
      }
    }

    game.track_gen().extend_track(game.ecs());

    time = new_time;

    for (auto [id, ttd] : game.ecs().read_all<TimeToDie>())
      if (ttd.time_to_die <= time)
        game.broken_plank_pool().deactivate(game.ecs(), id);

    game.ecs().deleted_marked_ids();

    gl::clear();

    for (const auto& [id, transform, color, shader_bindings] :
         game.ecs().read_all<Transform, Color, ShaderBindings*>()) {
      draw_object(transform, shader_bindings, color, camera_offset, zoom);
    }

    gfx.swap_buffers();
  }

  return Error();
}

int main() {
  if (Error e = run(); !e.ok) {
    std::cerr << e.reason << std::endl;
    return 1;
  }
  return 0;
}

