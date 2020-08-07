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

#include "line_breaker_track.h"
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
    gl::bindTexture(GL_TEXTURE_2D, shader_bindings->texture);

    gl::bindBuffer(GL_ARRAY_BUFFER, shader_bindings->vbo);

    gl::enableVertexAttribArray(shader_bindings->vertex_pos_attrib);
    gl::vertexAttribPointer<float>(shader_bindings->vertex_pos_attrib, 3,
                                   GL_FALSE, &Vertex::pos);

    if (shader_bindings->tex_coord_attrib != -1 && shader_bindings->texture) {
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
    gl::uniform4v(shader_bindings->color_uniform, 1,
                  glm::value_ptr(color.get()));
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
      TimeToDie{now, now + BROKEN_PLANK_LIFETIME},
      &shader_bindings,
      Color({color.get(), glm::vec4()}));
}

void write_flame(
    Ecs& ecs,
    EntityPool& pool,
    const Transform& ship_transform,
    const ShipPoints& ship_points,
    const Physics& ship_physics,
    float ship_thrust,
    ShaderBindings& shader_bindings,
    const std::chrono::high_resolution_clock::time_point& now) {
  // Modify the rotation by up to 30 degrees (pi / 6)
  float dir_mod = random_float(0, glm::pi<float>() / 12.f, 100) * random_sign();
  glm::vec3 v = ship_physics.v + radial_vec(
      ship_transform.rotation + glm::pi<float>() + dir_mod,
      ship_thrust * random_int(200, 500));
  pool.create_new(
      ecs,
      Transform{(ship_points.left_back + ship_points.right_back) / 2.f,
                ship_transform.rotation + glm::pi<float>(),
                0.05f},
      Physics{glm::vec3(), v, ship_physics.rotation_velocity},
      TimeToDie{now, now + FLAME_LIFETIME},
      &shader_bindings,
      Color({glm::vec4(1.0f, 1.0f, 1.0f, 1.0f),
             glm::vec4(1.0f, 0.3f, 0.3f, 1.0f),
             glm::vec4(0.8f, 0.8f, 0.2f, 1.0f),
             glm::vec4(0.2f, 0.1f, 0.0f, 1.0f),
             glm::vec4()}));
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

  float score_ = 0;

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

  void add_score(float x) { score_ += x; }
  unsigned int score() const { return score_; }
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
      Color(),  // unused
      player_shader_bindings_);

  score_ = 0;
}

glm::vec2 flip_x(const glm::vec2& v) { return glm::vec2(-v.x, v.y); }
glm::vec2 flip_y(const glm::vec2& v) { return glm::vec2(v.x, -v.y); }
glm::vec2 flip_xy(const glm::vec2& v) { return flip_x(flip_y(v)); }

glm::vec3 flip_x(const glm::vec3& v) { return glm::vec3(-v.x, v.y, v.z); }
glm::vec3 flip_y(const glm::vec3& v) { return glm::vec3(v.x, -v.y, v.z); }
glm::vec3 flip_xy(const glm::vec3& v) { return flip_x(flip_y(v)); }

GLuint rectangle_vbo(glm::vec3 dimensions = glm::vec3(1.f, 1.f, 0.f),
                     glm::vec2 tex_pos = glm::vec2(0.5f, 0.5f),
                     glm::vec2 tex_size = glm::vec2(1.f, 1.f)) {
  tex_size.y = -tex_size.y;
  Vertex vertecies[] = {
    {-dimensions / 2.f,         tex_pos - tex_size/2.f},
    {flip_x(-dimensions / 2.f), tex_pos - flip_x(tex_size/2.f)},
    {dimensions / 2.f,          tex_pos + tex_size/2.f},
    {flip_x(dimensions) / 2.f,  tex_pos + flip_x(tex_size/2.f)}
  };
  GLuint vbo = gl::genBuffer();
  gl::bindBuffer(GL_ARRAY_BUFFER, vbo);
  gl::bufferData(GL_ARRAY_BUFFER, vertecies, GL_STATIC_DRAW);

  return vbo;
}

// Fills `score_digits` with the digits of `score` from least significant to
// most. For example, if `score = 1234`, `score_digits = [4, 3, 2, 1]`.
void fill_score_digits(std::vector<unsigned int>& score_digits,
                       unsigned int score) {
  for (unsigned int& d : score_digits) {
    d = score % 10;
    score = score / 10;
  }
}

Error run(bool show_thrust) {
  random_seed();

  Graphics gfx;
  if (Error e = gfx.init(WINDOW_WIDTH, WINDOW_HEIGHT); !e.ok) return e;

  GlProgram ship_shader_program;
  if (Error e = construct_ship_shader(ship_shader_program); !e.ok) return e;

  GlProgram line_shader_program;
  if (Error e = construct_line_shader(line_shader_program); !e.ok) return e;

  //Initialize clear color
  gl::clearColor(0.f, 0.f, 0.f, 1.f);

  GLuint zero123456789score_texture;
  if (Error  e = load_bmp_texture("art/0123456789score.bmp",
                                  zero123456789score_texture);
      !e.ok)
    return e;

  GLuint ship_texture;
  if (Error e = load_bmp_texture("art/ship 512 RGBA8.bmp", ship_texture);
      !e.ok)
    return e;


  ShaderBindings score_bindings[10];
  for (unsigned int i = 0; i < 10; ++i) {
    score_bindings[i].program = &ship_shader_program;
    score_bindings[i].texture = zero123456789score_texture;
    score_bindings[i].vbo = rectangle_vbo(
        glm::vec3(1.f, 1.f, 2.f),
        glm::vec2(((14.f/2.f) + 14.f * i) / 256.f, 0.5f),
        glm::vec2(14.f / 256.f, 1.f));
    if (Error e =
      ship_shader_program.uniform_location(
          "tex", score_bindings[i].texture_uniform) &&
      ship_shader_program.uniform_location(
          "transform", score_bindings[i].transform_uniform) &&
      ship_shader_program.attribute_location(
          "vertex_pos", score_bindings[i].vertex_pos_attrib) &&
      ship_shader_program.attribute_location(
          "tex_coord", score_bindings[i].tex_coord_attrib);
        !e.ok)
      return e;
  }
  std::vector<unsigned int> score_digits = {0, 0, 0, 0, 0};

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  // Unfortunately, for the player ship VBO, its center of rendering isn't the
  // center of the texutre so we can't use rectangle_vbo().
  //VBO data
  Vertex player_ship_verts[] = {
    // Position              TexCoords
    {{-SHIP_TAIL_LENGTH, -SHIP_HALF_WIDTH, 0.0f},  {0.0f, 1.0f}},
    {{ SHIP_NOSE_LENGTH, -SHIP_HALF_WIDTH, 0.0f},  {1.0f, 1.0f}},
    {{ SHIP_NOSE_LENGTH,  SHIP_HALF_WIDTH, 0.0f},  {1.0f, 0.0f}},
    {{-SHIP_TAIL_LENGTH,  SHIP_HALF_WIDTH, 0.0f},  {0.0f, 0.0f}}
  };

  GLuint player_ship_vbo = gl::genBuffer();
  gl::bindBuffer(GL_ARRAY_BUFFER, player_ship_vbo);
  gl::bufferData(GL_ARRAY_BUFFER, player_ship_verts, GL_STATIC_DRAW);

  //IBO data
  GLuint vbo_elems[] = {0, 1, 2,
                        2, 3, 0};

  GLuint vbo_elems_id = gl::genBuffer();
  gl::bindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_elems_id);
  gl::bufferData(GL_ELEMENT_ARRAY_BUFFER, vbo_elems, GL_STATIC_DRAW);

  ShaderBindings player_shader_bindings{
      .program = &ship_shader_program,
      .vbo = player_ship_vbo,
      .texture = ship_texture};
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

  ShaderBindings line_shader_bindings{
    .program = &line_shader_program,
    .vbo = rectangle_vbo(glm::vec3(1.f, 0.05f, 0.f))};
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

      static float flames_expected_for_thrust = 0;
      flames_expected_for_thrust += game.player_thrust();
      for (;
           show_thrust && flames_expected_for_thrust > GEARS[0].thrust / 2.f;
           flames_expected_for_thrust =
              std::max(flames_expected_for_thrust - GEARS[0].thrust, 0.f)) {
        write_flame(game.ecs(), game.broken_plank_pool(), ship_transform,
                    ship_points, ship_physics, game.player_thrust(),
                    line_shader_bindings, time);
      }

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
            game.add_score(TRACK_SPACING);
            break;
          }
        }
      }
    }

    game.track_gen().extend_track(game.ecs());

    time = new_time;

    for (auto [id, ttd, color] : game.ecs().read_all<TimeToDie, Color>()) {
      if (ttd.time_to_die > time) {
        color.t = float((time - ttd.time_born).count()) /
                  float((ttd.time_to_die - ttd.time_born).count());
      } else {
        game.broken_plank_pool().deactivate(game.ecs(), id);
      }
    }

    game.ecs().deleted_marked_ids();

    gl::clear();

    for (const auto& [_, transform, color, shader_bindings] :
         game.ecs().read_all<Transform, Color, ShaderBindings*>()) {
      draw_object(transform, shader_bindings, color, camera_offset, zoom);
    }

    // Draw the score.
    fill_score_digits(score_digits, game.score());
    for (unsigned int i = 0; i < score_digits.size(); ++i) {
      Transform score_trans{.pos = glm::vec3(9.f - i, 9.f, 0.f)};
      draw_object(score_trans, &score_bindings[score_digits[i]],
                  Color(1.f, 1.f, 1.f), glm::vec3(), 0.1f);
    }

    gfx.swap_buffers();
  }

  return Error();
}

int main(int argc, char** argv) {
  bool show_thrust = argc == 2 && strcmp(argv[1], "--show_thrust") == 0;
  if (Error e = run(show_thrust); !e.ok) {
    std::cerr << e.reason << std::endl;
    return 1;
  }
  return 0;
}

