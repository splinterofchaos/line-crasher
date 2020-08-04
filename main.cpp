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
  if (auto [u, has_intersection] = segment_segment_intersection(
          ship_points.left_back, ship_points.nose,
          line_points.a, line_points.b);
      has_intersection)
    return {u, has_intersection};
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
      FragColor = vec4(color, 1);
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
  Gear{1.500e-05, {0.2, 0.2, 0.5}},
  Gear{1.750e-05, {0.2, 0.2, 0.55}},
  Gear{2.000e-05, {0.2, 0.2, 0.6}},
  Gear{2.300e-05, {0.2, 0.2, 0.8}},
  Gear{2.500e-05, {0.3, 0.3, 0.8}},
  Gear{2.580e-05, {0.3, 0.3, 0.9}},
  Gear{3.000e-05, {0.5, 0.5, 1.0}}
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

class TrackGenerator {
  glm::vec3 start_;
  float heading_ = 0;  // The direction of the track.
  ShaderBindings* shader_bindings_;
  std::size_t current_gear_ = 0;
  float track_width_ = 2;

  // The spacing between different segments of road.
  constexpr static float SPACING = 1.f;
  constexpr static float MAX_WIDTH = 10;
  constexpr static float MIN_WIDTH = 1;
  constexpr static float WIDTH_CHANGE = 4;

public:
  enum Strategy {
    LONG_STRAIGHT,
    CIRCULAR_CURVE,
    GEAR_UP,
    GEAR_DOWN,
    CHANGE_WIDTH,
    N_STRAGEGIES
  };

  TrackGenerator(glm::vec3 start, ShaderBindings* bindings)
    : start_(start), shader_bindings_(bindings) { }

  void set_heading(float h) { heading_ = h; }
  float heading() const { return heading_; }

  const glm::vec3& start() { return start_; }

  void write_track(Ecs& ecs, Strategy strat);

  void write_plank(Ecs& ecs, float width);
};

void TrackGenerator::write_plank(Ecs& ecs, float width) {
  ecs.write_new_entity(
      Transform{start_, heading_ + glm::half_pi<float>(), width},
      shader_bindings_, Color{GEARS[current_gear_].color},
      LineData{current_gear_});
}

// When changing the width of the track, we take `len` planks to go from `old_`
// to `new_width`. This calculates the width of the i'th plank.
float smooth_width(float old_width, float new_width,
                   unsigned int i, unsigned int len) {
  const float theta = (float(i + 1) / len) * glm::pi<float>();
  return glm::mix(new_width, old_width, std::cos(theta) / 2 + 0.5);

}

void TrackGenerator::write_track(Ecs& ecs, Strategy strat) {
  switch (strat) {
    case TrackGenerator::LONG_STRAIGHT:
      for (unsigned int i = 0; i < 1; ++i) {
        write_plank(ecs, track_width_);
        start_ += radial_vec(heading_, SPACING);
      }
      break;
    case TrackGenerator::GEAR_UP:
      if (current_gear_ + 1 < GEARS.size()) current_gear_ += 1;
      break;
    case TrackGenerator::GEAR_DOWN:
      if (current_gear_ > 0) current_gear_ -= 1;
      break;
    case TrackGenerator::CHANGE_WIDTH: {
      float diff = 4;
      if (track_width_ >= MAX_WIDTH ||
          (track_width_ - WIDTH_CHANGE > MIN_WIDTH &&
           random_bool(random_gen))) {
        diff = -diff;
      }
      float new_track_width = track_width_ + diff;
      constexpr int LENGTH = 10;
      for (unsigned int i = 0; i < LENGTH; ++i) {
        write_plank(ecs,
                    smooth_width(track_width_, new_track_width, i, LENGTH));
        start_ += radial_vec(heading_, SPACING);
      }
      track_width_ = new_track_width;
      break;
    }
    case TrackGenerator::CIRCULAR_CURVE: {
      float gear_turn_ratio = (current_gear_ + 1) * 0.5;
      float radius = random_int(
          random_gen,
          std::max(track_width_ * 2, track_width_ * gear_turn_ratio),
          track_width_ * 5 * gear_turn_ratio);
      int dir = random_bool(random_gen) ? 1 : -1;
      // Each turn should have an angle of between 45 and 90 degrees.
      auto angle =
        (random_int(random_gen, 50, 101) / 100.f) * glm::half_pi<float>();

      float new_heading = heading_ + angle * dir;
      glm::vec3 center = start_ +
        radial_vec(heading_ + glm::half_pi<float>() * dir, radius);

      while (dir > 0 ? heading_ < new_heading : heading_ > new_heading) {
        write_plank(ecs, track_width_);
        // We want to draw the next segment SPACING further into the curve. In
        // other words, we want an arc length of SPACING.
        //    arc length = r * theta.
        //    arc length / r = theta.
        heading_ += (SPACING / radius) * dir;
        start_ = center + radial_vec(
            heading_ - glm::half_pi<float>() * dir, radius);
      }
      heading_ = new_heading;
      break;
    }
    case TrackGenerator::N_STRAGEGIES:
      std::cerr << "unhandled TrackGenerator::Strategy" << std::endl;
  }
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
  track_gen.write_track(ecs, TrackGenerator::LONG_STRAIGHT);

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

      std::cout << "|v| = " << glm::length(ship_physics.v) << std::endl;
      std::cout << "thrust = " << ship_thrust << std::endl;

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
            ecs.mark_to_delete(id);
            ship_gear = line_data.gear;

            glm::vec3 ab = line_points.b - line_points.a;

            // If 100% of linear velocity were converted into rotational
            // velocity at the point of intersection, we would have
            //    v = X * r
            // where X is radians over time. If r is small, X is large; if r is
            // large, X is small.
            float rotation_a = glm::length(ship_physics.v) * 0.5 /
                               (line_transform.length * u * 0.5);
            float rotation_b = -glm::length(ship_physics.v) * 0.5 /
                               (line_transform.length * (1 - u) * 0.5);

            ecs.write_new_entity(
                Transform{line_points.a + ab * u * 0.5f,
                          line_transform.rotation,
                          line_transform.length * u},
                Physics{glm::vec3(), ship_physics.v / 2.f, rotation_a},
                TimeToDie{last_physics_update + BROKEN_PLANK_LIFETIME},
                &line_shader_bindings,
                Color(color));

            ecs.write_new_entity(
                Transform{line_points.b - ab * (1 - u) * 0.5f,
                          line_transform.rotation + glm::pi<float>(),
                          line_transform.length * (1 - u)},
                Physics{glm::vec3(), ship_physics.v / 2.f, rotation_b},
                TimeToDie{last_physics_update + BROKEN_PLANK_LIFETIME},
                &line_shader_bindings,
                Color(color));
            break;
          }
        }
      }

      if (glm::distance(track_gen.start(), camera_offset) < 2.f / zoom) {
        auto strat = (TrackGenerator::Strategy)
          random_int(random_gen, TrackGenerator::N_STRAGEGIES);
        track_gen.write_track(ecs, strat);
      }
    }

    time = new_time;

    for (auto [id, ttd] : ecs.read_all<TimeToDie>())
      if (ttd.time_to_die <= time) ecs.mark_to_delete(id);

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

