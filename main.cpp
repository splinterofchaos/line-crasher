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

constexpr float SHIP_MAX_SPEED = 0.04;
// The coefficient of negative acceleration proportionate to velocity.
// Similar to air resistance + friction.
constexpr float SHIP_RESISTENCE = 0.001f;
// The acceleration applied by side thrusters to keep the ship moving forward.
constexpr float SHIP_SIDE_THRUST = 0.0005;
constexpr float SHIP_TRUST = 0.0000001f;
constexpr float SHIP_ROTATE_SPEED = 0.005;

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

constexpr float SHIP_HALF_LENGTH = 0.5f;
constexpr float SHIP_HALF_WIDTH = 0.5f;

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
    out vec4 FragColor;
    void main() {
      FragColor = vec4(1);
    }
  )");
  if (Error e = frag.compile(); !e.ok) return e;

  line_shader_program.add_shader(verts);
  line_shader_program.add_shader(frag);
  return line_shader_program.link();
}

struct Transform {
  glm::vec3 pos;
  float rotation;  // ... in radians.

  // TODO: There might be a more appropriate component for this.
  float length;  // Used for lines in determining how long they are.
};

// References all shader uniforms and attribute bindings.
struct ShaderBindings {
  GlProgram* program;

  GLuint vbo;

  GLint texture_uniform = -1;
  GLint transform_uniform = -1;
  GLint length_uniform = -1;

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

void draw_object(const Transform& transform,
                 const ShaderBindings* shader_bindings,
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

  gl::drawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

std::chrono::milliseconds time_diff(
    std::chrono::high_resolution_clock::time_point old_time,
    std::chrono::high_resolution_clock::time_point new_time) {
  if (old_time > new_time) return time_diff(new_time, old_time);
  return std::chrono::duration_cast<std::chrono::milliseconds>(
      new_time - old_time);
}

struct Gear {
  float thrust;
};

constexpr std::array GEARS{
  Gear{   0.59e-05},
  Gear{   2.00e-05},
  Gear{2.58001e-05}
};

struct LineData {
  std::size_t gear;
};

using Ecs = EntityComponentSystem<Transform, ShaderBindings*, LineData>;

class TrackGenerator {
  glm::vec3 start_;
  float heading_ = 0;  // The direction of the track.
  ShaderBindings* shader_bindings_;
  std::size_t current_gear_ = 0;

  // The spacing between different segments of road.
  constexpr static float SPACING = 1.f;

public:
  enum Strategy {
    LONG_STRAIGHT,
    CIRCULAR_CURVE,
    GEAR_UP,
    GEAR_DOWN,
    N_STRAGEGIES
  };

  TrackGenerator(glm::vec3 start, ShaderBindings* bindings)
    : start_(start), shader_bindings_(bindings) { }

  void set_heading(float h) { heading_ = h; }
  float heading() const { return heading_; }

  const glm::vec3& start() { return start_; }

  void write_track(Ecs& ecs, Strategy strat);

  void write_plank(Ecs& ecs);
};

void TrackGenerator::write_plank(Ecs& ecs) {
  ecs.write_new_entity(
      Transform{start_, heading_ + glm::half_pi<float>(), 3},
      shader_bindings_, LineData{current_gear_});
}

void TrackGenerator::write_track(Ecs& ecs, Strategy strat) {
  switch (strat) {
    case TrackGenerator::LONG_STRAIGHT:
      for (unsigned int i = 0; i < 5; ++i) {
        write_plank(ecs);
        start_ += radial_vec(heading_, SPACING);
      }
      break;
    case TrackGenerator::GEAR_UP:
      if (current_gear_ + 1 < GEARS.size()) current_gear_ += 1;
      write_track(ecs, TrackGenerator::LONG_STRAIGHT);
      break;
    case TrackGenerator::GEAR_DOWN:
      if (current_gear_ > 0) current_gear_ -= 1;
      write_track(ecs, TrackGenerator::LONG_STRAIGHT);
      break;
    case TrackGenerator::CIRCULAR_CURVE: {
      float radius = random_int(random_gen, 6, 20);
      int dir = random_bool(random_gen) ? 1 : -1;
      // Each turn should have an angle of between 45 and 90 degrees.
      auto angle =
        (random_int(random_gen, 50, 101) / 100.f) * glm::half_pi<float>();

      float new_heading = heading_ + angle * dir;
      glm::vec3 center = start_ +
        radial_vec(heading_ + glm::half_pi<float>() * dir, radius);

      while (dir > 0 ? heading_ < new_heading : heading_ > new_heading) {
        write_plank(ecs);
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

  SDL_Surface* ship_surface = SDL_LoadBMP("art/ship 512 RGBA8.bmp");
  if (ship_surface == nullptr) {
    return Error(concat_strings("Failed to load image: ", SDL_GetError()));
  }

  GLuint ship_texture = gl::genTexture();
  gl::bindTexture(GL_TEXTURE_2D, ship_texture);
  gl::texImage2D(GL_TEXTURE_2D, 0, GL_RGBA, ship_surface->w,
                 ship_surface->h, 0, GL_BGRA, GL_UNSIGNED_BYTE,
                 ship_surface->pixels);
  SDL_FreeSurface(ship_surface);

  if (auto e = glGetError(); e != GL_NO_ERROR) {
    return Error(concat_strings(
        "I'm too lazy to figure out if there's a function which maps this GL "
        "error number to a string so here's a number: ", std::to_string(e)));
  }

  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  gl::texParameter(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  //VBO data
  Vertex quad_vertecies[] = {
    // Position              TexCoords
    {{-0.5f, -0.5f, 0.0f},  {0.0f, 1.0f}},
    {{ 0.5f, -0.5f, 0.0f},  {1.0f, 1.0f}},
    {{ 0.5f,  0.5f, 0.0f},  {1.0f, 0.0f}},
    {{-0.5f,  0.5f, 0.0f},  {0.0f, 0.0f}}
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
                                     &player_shader_bindings);

  ShaderBindings line_shader_bindings(&line_shader_program, line_vbo);
  line_shader_program.use();
  if (Error e =
      line_shader_program.uniform_location(
          "transform", line_shader_bindings.transform_uniform) &&
      line_shader_program.uniform_location(
          "length", line_shader_bindings.length_uniform) &&
      line_shader_program.attribute_location(
          "vertex_pos", line_shader_bindings.vertex_pos_attrib);
      !e.ok) return e;

  // One should be roughly the width of the player ship.
  TrackGenerator track_gen(glm::vec3(1, 0, 0), &line_shader_bindings);
  track_gen.write_track(ecs, TrackGenerator::LONG_STRAIGHT);

  // TODO: These should eventually be stored into components, too.
  ShipController ship_controller;
  glm::vec3 ship_velocity(0.f);
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

      float ship_rotation_vel = 0;
      if (manual_thrusters_enabled) {
        if (ship_controller.thruster) ship_thrust += SHIP_TRUST;
        if (ship_controller.breaks)
          ship_thrust = std::max(ship_thrust - SHIP_TRUST, 0.f);
      } else {
        ship_thrust = GEARS[ship_gear].thrust;
      }
      if (ship_controller.rotate_clockwise)
        ship_rotation_vel -= SHIP_ROTATE_SPEED;
      if (ship_controller.rotate_counterclockwise)
        ship_rotation_vel += SHIP_ROTATE_SPEED;

      // TODO: This is a nice implicit Euler integration, but consider RK4.
      // ref: https://gafferongames.com/post/integration_basics/#:~:text=Euler%20integration%20is%20the%20most,is%20constant%20over%20the%20timestep.&text=However%2C%20we%20are%20also%20integrating,error%20in%20the%20integrated%20position.
      Transform& ship_transform = ecs.read_or_panic<Transform>(player);

      ship_transform.rotation += ship_rotation_vel * TIME_STEP_MS;
      ship_rotation_vel = 0;

      glm::vec3 heading = radial_vec(ship_transform.rotation);

      glm::vec3 a(0);
      if (ship_thrust != 0)
        a = radial_vec(ship_transform.rotation, ship_thrust);
      if (glm::length(ship_velocity)) {
        a += vec_resize(clockwize(ship_velocity),
                        -cross2(heading, ship_velocity) * SHIP_SIDE_THRUST);
      }

      a -= ship_velocity * SHIP_RESISTENCE;

      ship_velocity += a * float(TIME_STEP_MS);
      if (glm::length(ship_velocity) > SHIP_MAX_SPEED)
        ship_velocity = vec_resize(ship_velocity, SHIP_MAX_SPEED);

      ship_transform.pos += ship_velocity * float(TIME_STEP_MS);


      std::cout << "|v| = " << glm::length(ship_velocity) << std::endl;
      std::cout << "thrust = " << ship_thrust << std::endl;

      camera_offset = ship_velocity;
      // TODO: This isn't very intelligent. If the offset factor is too large,
      // the player will be off screen and if too small, overly centered.
      camera_offset *= 500.f / TIME_STEP_MS;
      camera_offset += ship_transform.pos;

      zoom = 0.25f;
      if (glm::length(ship_velocity) > 0.001) zoom -= std::log(glm::length(ship_velocity) * 1000) * 0.06;

      glm::vec3 to_nose = radial_vec(ship_transform.rotation,
                                     SHIP_HALF_LENGTH);
      glm::vec3 nose = ship_transform.pos + to_nose;
      glm::vec3 ship_back = ship_transform.pos +
                            vec_resize(to_nose, -SHIP_HALF_LENGTH);
      glm::vec3 to_left = vec_resize(clockwize(to_nose), SHIP_HALF_WIDTH);
      auto [left_back, right_back] = plus_minus(ship_back, to_left);
      for (const auto& [id, line_transform, line_data] :
           ecs.read_all<Transform, LineData>()) {
        // Bounds check first.
        if (glm::distance(line_transform.pos, ship_transform.pos) <
            line_transform.length + SHIP_HALF_LENGTH) {
          // Check if the line segment, [a, b], intersects either side of the
          // ship. Since it can't go backwards, we don't need to check the
          // back side.
          glm::vec3 parallel = radial_vec(line_transform.rotation,
                                          line_transform.length / 2);
          auto [a, b] = plus_minus(line_transform.pos, parallel);
          if (segment_segment_intersection(left_back, nose, a, b) ||
              segment_segment_intersection(right_back, nose, a, b)) {
            manual_thrusters_enabled = false;
            ecs.mark_to_delete(id);
            ship_gear = line_data.gear;
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

    ecs.deleted_marked_ids();

    gl::clear();

    for (const auto& [id, transform, shader_bindings] :
         ecs.read_all<Transform, ShaderBindings*>()) {
      draw_object(transform, shader_bindings, camera_offset, zoom);
    }

    gfx.swap_buffers();

    time = new_time;
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
