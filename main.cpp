#include <SDL2/SDL.h>
#include <GL/glew.h>
#include <SDL2/SDL_opengl.h>
#include <GL/glu.h>
#include <glm/vec2.hpp>
#include <glm/mat4x4.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtx/closest_point.hpp>

#include <array>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <random>

#include "ecs.h"
#include "glpp.h"
#include "graphics.h"
#include "math.h"

constexpr int WINDOW_HEIGHT = 800;
constexpr int WINDOW_WIDTH = 800;

// Target 60 FPS and do physics updates four times as often.
constexpr auto TIME_STEP = std::chrono::milliseconds(1000) / (60 * 4);
constexpr auto TIME_STEP_MS = TIME_STEP.count();

constexpr auto BROKEN_PLANK_LIFETIME = std::chrono::seconds(2);

constexpr float MAX_SPEED = 0.04;
// The coefficient of negative acceleration proportionate to velocity.
// Similar to air resistance + friction.
constexpr float SHIP_RESISTENCE = 0.001f;
// The acceleration applied by side thrusters to keep the ship moving forward.
constexpr float SHIP_SIDE_THRUST = 0.0005;
constexpr float SHIP_TRUST = 0.0000001f;
constexpr float SHIP_ROTATE_SPEED = 0.005;

constexpr float SHIP_NOSE_LENGTH = 0.8f;
constexpr float SHIP_TAIL_LENGTH = 0.2f;
constexpr float SHIP_LENGTH = SHIP_NOSE_LENGTH + SHIP_TAIL_LENGTH;
constexpr float SHIP_HALF_WIDTH = 0.5f;

// Globals are bad, but random is good.
std::random_device rd;
std::mt19937 random_gen;

std::ostream& operator<<(std::ostream& os, const glm::vec3 v) {
  return os << '<' << v.x << ", " << v.y << ", " << v.z << '>';
}

int random_int(std::mt19937& gen, int min, int max) {
  auto distribution = std::uniform_int_distribution(min, max - 1);
  return distribution(gen);
}

int random_int(std::mt19937& gen, int max) {
  return random_int(gen, 0, max);
}

bool random_bool(std::mt19937& gen) {
  auto distribution = std::uniform_int_distribution<int>(false, true);
  return distribution(gen);
}

// All entities that can be rendered have a transform that describes their
// position and shape.
struct Transform {
  glm::vec3 pos;
  float rotation;  // in radians.

  // TODO: this should really be in LineData. The question is how to properly
  // pipe it to the render code.
  float length;  // Used for lines in determining how long they are.
};

// All things that move through the world.
struct Physics {
  glm::vec3 a, v;
  float rotation_velocity;

  // TODO: This is a nice implicit Euler integration, but consider RK4.
  // ref: https://gafferongames.com/post/integration_basics/#:~:text=Euler%20integration%20is%20the%20most,is%20constant%20over%20the%20timestep.&text=However%2C%20we%20are%20also%20integrating,error%20in%20the%20integrated%20position.
  void integrate(Transform& t) {
    t.rotation += rotation_velocity * TIME_STEP_MS;

    v += a * float(TIME_STEP_MS);
    if (glm::length(v) > MAX_SPEED) v = vec_resize(v, MAX_SPEED);

    t.pos += v * float(TIME_STEP_MS);
  }
};

struct TimeToDie {
  std::chrono::high_resolution_clock::time_point time_to_die;
};

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

Error construct_ship_shader(GlProgram& ship_shader_program)
{
  Shader verts(Shader::Type::VERTEX);
  verts.add_source(R"(
		#version 140
    in vec3 vertex_pos;
    in vec2 tex_coord;
    uniform mat4 transform;
    out vec2 TexCoord;
    void main() {
      gl_Position = transform * vec4(vertex_pos, 1);
      TexCoord = tex_coord;
    }
  )");
  if (Error e = verts.compile(); !e.ok) return e;

  Shader frag(Shader::Type::FRAGMENT);
  frag.add_source(R"(
    #version 140
    in vec2 TexCoord;
    out vec4 FragColor;
    uniform sampler2D tex;
    void main() {
      FragColor = texture(tex, TexCoord);
    }
  )");
  if (Error e = frag.compile(); !e.ok) return e;

  ship_shader_program.add_shader(verts);
  ship_shader_program.add_shader(frag);
  return ship_shader_program.link();
}

Error construct_line_shader(GlProgram& line_shader_program) {
  Shader verts(Shader::Type::VERTEX);
  verts.add_source(R"(
		#version 140
    in vec3 vertex_pos;
    uniform mat4 transform;
    uniform float length;
    void main() {
      gl_Position = transform * vec4(vertex_pos.x * length, vertex_pos.y,
                                     vertex_pos.z, 1);
    }
  )");
  if (Error e = verts.compile(); !e.ok) return e;

  Shader frag(Shader::Type::FRAGMENT);
  frag.add_source(R"(
    #version 140
    in vec2 TexCoord;
    uniform vec3 color;
    out vec4 FragColor;
    void main() {
      FragColor = vec4(color, 0.5);
    }
  )");
  if (Error e = frag.compile(); !e.ok) return e;

  line_shader_program.add_shader(verts);
  line_shader_program.add_shader(frag);
  return line_shader_program.link();
}

struct Gear {
  float thrust;
  glm::vec3 color;
};

constexpr std::array GEARS{
  Gear{1.500e-05, {0.1, 0.1, 0.5}},
  Gear{1.750e-05, {0.1, 0.2, 0.55}},
  Gear{2.000e-05, {0.2, 0.2, 0.6}},
  Gear{2.300e-05, {0.2, 0.3, 0.8}},
  Gear{2.500e-05, {0.3, 0.4, 0.8}},
  Gear{2.580e-05, {0.4, 0.4, 0.9}},
  Gear{3.000e-05, {0.5, 0.5, 1.0}},
  Gear{3.500e-05, {0.6, 0.6, 1.0}},
  Gear{4.000e-05, {1.0, 1.0, 1.0}}
};

struct LineData {
  std::size_t gear;
};

// References all shader uniforms and attribute bindings.
struct ShaderBindings {
  GlProgram* program;

  GLuint vbo;

  GLint texture_uniform = -1;
  GLint transform_uniform = -1;
  GLint length_uniform = -1;
  GLint color_uniform = -1;

  GLint vertex_pos_attrib = -1;
  GLint tex_coord_attrib = -1;

  ShaderBindings() { }
  ShaderBindings(GlProgram* program, GLuint vbo)
    : program(program), vbo(vbo) { }
};

struct Vertex {
  glm::vec3 pos;
  glm::vec2 tex_coord;
};

struct Color {
  glm::vec3 c;
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

using Ecs = EntityComponentSystem<Transform, Physics, TimeToDie,
                                  ShaderBindings*, Color,
                                  LineData>;

// Please excuse the bad name. TODO: Make a better one.
//
// Maintains a free list of entities of a specific type. When that entity has
// expired, instead of deleting it, this pool deactivates it. When an entity is
// created, this pool can reactivate it with new parameters or create a new one
// entirely.
class EntityPool {
  SortedVector<EntityId> free_list_;

public:
  EntityPool() { }

  void deactivate(Ecs& ecs, EntityId id) {
    free_list_.insert_if_not_present(id);
    ecs.deactivate(id);
  }

  template<typename...Args>
  void create_new(Ecs& ecs, Args&&...args) {
    bool made_new = false;
    if (free_list_.size()) {
      EcsError e = ecs.write(free_list_.back(), std::forward<Args>(args)...);

      if (e == EcsError::OK) {
        ecs.activate(free_list_.back());
        made_new = true;
      } else if (e == EcsError::NOT_FOUND) {
        std::cerr << "WARNING: We're holding onto ID's in our free list that "
                     "may have been garbage collected." << std::endl;
      } else {
        std::cerr << "EntityPool: unhandled error on write." << std::endl;
      }

      free_list_.pop_back();
    }

    if (!made_new) {
      auto id = ecs.write_new_entity(std::forward<Args>(args)...);
      std::cout << "New entity id: " << id.id << std::endl;
    }
  }
};

class TrackGeneratorStrategy;

class TrackGenerator {
  ShaderBindings* shader_bindings_;
  std::size_t current_gear_ = 0;

  enum DifficultyIncreaseReason { GEAR_UP, NARROW_TRACK };

  unsigned int plank_count_ = 0;
  unsigned int planks_destroyed_ = 0;
  unsigned int difficulty_increase_at_ = 200;
  DifficultyIncreaseReason difficulty_increase_reason_ = GEAR_UP;

  // Planks that have been consumed, available to be re-used.
  EntityPool plank_pool_;

  std::unique_ptr<TrackGeneratorStrategy> strategy_;

public:
  struct Head {
    glm::vec3 pos;
    float rotation = 0;  // The direction of the track.
    float width = SHIP_HALF_WIDTH * 2 * 3;
  } head_;

  // The spacing between different segments of road.
  constexpr static float SPACING = 1.f;
  constexpr static float MAX_WIDTH = SHIP_HALF_WIDTH * 2 * 10;
  constexpr static float MIN_WIDTH = SHIP_HALF_WIDTH * 2 * 1;
  constexpr static float SAFE_WIDTH = MIN_WIDTH * 3;
  constexpr static unsigned int MAX_PLANKS = 200;

  enum Strategy {
    CIRCULAR_CURVE,
    CHANGE_WIDTH,
    N_STRAGEGIES
  };

  TrackGenerator(glm::vec3 pos, ShaderBindings* bindings)
    : shader_bindings_(bindings) {
    head_.pos = pos;
  }

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
    dir_ = random_bool(random_gen) ? 1 : -1;

    // We want to draw the next segment SPACING further into the curve. In
    // other words, we want an arc length of SPACING.
    //    arc length = r * theta.
    //    arc length / r = theta.
    // where theta is the rate of change.
    theta_ = (TrackGenerator::SPACING / radius_) * dir_;

    // Each turn should have an angle of between 45 and 90 degrees.
    float angle =
      (random_int(random_gen, 50, 101) / 100.f) * glm::half_pi<float>();
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
  const float new_track_width = random_int(random_gen, min_width, MAX_WIDTH);

  switch (strat) {
    case TrackGenerator::CHANGE_WIDTH:
      strategy_.reset(new TrackStrategyChangeWidth(head_, new_track_width,
                                                   10.f / SPACING));
      break;
    case TrackGenerator::CIRCULAR_CURVE: {
      float gear_turn_ratio = (current_gear_ + 1) * 0.5;
      float larger_width = std::max(head_.width, new_track_width);
      float radius = random_int(
          random_gen,
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
      set_strategy(ecs, Strategy(random_int(random_gen, N_STRAGEGIES)));

    plank_pool_.create_new(ecs,
                           Transform{head_.pos,
                                     head_.rotation + glm::half_pi<float>(),
                                     head_.width},
                           shader_bindings_,
                           Color{GEARS[current_gear_].color},
                           LineData{current_gear_});
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

Error run() {
  random_gen.seed(rd());

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

  Ecs ecs;

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

  auto player = ecs.write_new_entity(Transform{glm::vec3(0.0f), 0, 0},
                                     Physics{glm::vec3(), glm::vec3(), 0},
                                     Color{glm::vec3()},  // unused
                                     &player_shader_bindings);

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

  // One should be roughly the width of the player ship.
  TrackGenerator track_gen(glm::vec3(1, 0, 0), &line_shader_bindings);
  track_gen.set_strategy(ecs, TrackGenerator::CHANGE_WIDTH);

  // TODO: These should eventually be stored into components, too.
  ShipController ship_controller;
  // The acceleration the ship always has in the direction it faces.
  float ship_thrust = 0;
  std::size_t ship_gear = 0;
  // If true, the player may control their thrusters with up and down on the
  // arrow keys.
  bool manual_thrusters_enabled = true;

  auto time = std::chrono::high_resolution_clock::now();
  std::chrono::high_resolution_clock::time_point last_physics_update =
    time - TIME_STEP;
  std::chrono::milliseconds dtime;

  bool keep_going = true;
  SDL_Event e;

  glm::vec3 camera_offset(0.f);
  // TODO: make less linear.
  float zoom = 0.25f;

  EntityPool broken_plank_pool;

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

      Transform& ship_transform = ecs.read_or_panic<Transform>(player);
      Physics& ship_physics = ecs.read_or_panic<Physics>(player);

      ship_physics.rotation_velocity = 0;
      if (manual_thrusters_enabled) {
        if (ship_controller.thruster) ship_thrust += SHIP_TRUST;
        if (ship_controller.breaks)
          ship_thrust = std::max(ship_thrust - SHIP_TRUST, 0.f);
      } else {
        ship_thrust = GEARS[ship_gear].thrust;
      }
      if (ship_controller.rotate_clockwise)
        ship_physics.rotation_velocity -= SHIP_ROTATE_SPEED;
      if (ship_controller.rotate_counterclockwise)
        ship_physics.rotation_velocity += SHIP_ROTATE_SPEED;

      ship_physics.a = glm::vec3();
      if (ship_thrust != 0)
        ship_physics.a = radial_vec(ship_transform.rotation, ship_thrust);
      if (glm::length(ship_physics.v)) {
        glm::vec3 heading = radial_vec(ship_transform.rotation);
        ship_physics.a += vec_resize(
            clockwize(ship_physics.v),
            -cross2(heading, ship_physics.v) * SHIP_SIDE_THRUST);
      }

      ship_physics.a -= ship_physics.v * SHIP_RESISTENCE;

      for (auto [_, t, phys] : ecs.read_all<Transform, Physics>())
        phys.integrate(t);

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
           ecs.read_all<Transform, LineData, Color>()) {
        // Bounds check first.
        if (glm::distance(line_transform.pos, ship_points.center_of_gravity) <
            line_transform.length + SHIP_LENGTH &&
            !ecs.is_marked(id)) {
          LinePoints line_points(line_transform);
          if (auto [u, intersects] = intersection(ship_points, line_points);
              intersects) {
            manual_thrusters_enabled = false;
            track_gen.delete_plank(ecs, id);
            ship_gear = line_data.gear;

            auto crash_point =
              line_points.a + (line_points.b - line_points.a) * u;
            write_broken_plank(ecs, broken_plank_pool,
                               line_points.a, crash_point, u,
                               line_transform, ship_physics.v,
                               line_shader_bindings, color, time);
            write_broken_plank(ecs, broken_plank_pool,
                               line_points.b, crash_point, 1 - u,
                               line_transform, ship_physics.v,
                               line_shader_bindings, color, time);
            break;
          }
        }
      }
    }

    track_gen.extend_track(ecs);

    time = new_time;

    for (auto [id, ttd] : ecs.read_all<TimeToDie>())
      if (ttd.time_to_die <= time) broken_plank_pool.deactivate(ecs, id);

    ecs.deleted_marked_ids();

    gl::clear();

    for (const auto& [id, transform, color, shader_bindings] :
         ecs.read_all<Transform, Color, ShaderBindings*>()) {
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

