#include <SDL2/SDL.h>
#include <GL/glew.h>
#include <SDL2/SDL_opengl.h>
#include <GL/glu.h>
#include <glm/vec2.hpp>
#include <glm/mat4x4.hpp>
#include <glm/gtx/transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <chrono>
#include <cmath>
#include <cstdlib>
#include <iostream>

#include "ecs.h"
#include "glpp.h"
#include "graphics.h"

constexpr int WINDOW_HEIGHT = 800;
constexpr int WINDOW_WIDTH = 800;

// Target 60 FPS and do physics updates four times as often.
constexpr auto TIME_STEP = std::chrono::milliseconds(1000) / (60 * 4);
constexpr auto TIME_STEP_MS = TIME_STEP.count();

// Represents the in-game understanding of user inputs.
struct ShipController {
  bool thruster = false;
  bool rotate_clockwise = false;
  bool rotate_counterclockwise = false;

  void reset() {
    thruster = rotate_clockwise = rotate_counterclockwise = false;
  }
};

glm::mat4x4 transformation(glm::vec2 pos, float angle, float scale) {
  glm::mat4x4 transform = glm::translate(glm::mat4(1.f),
                                         glm::vec3(pos.x * scale, pos.y * scale, 0));
  transform *= glm::scale(glm::mat4(1.f), glm::vec3(scale));
  transform = glm::rotate(transform, angle, glm::vec3(0, 0, 1));
  return transform;
}

Error construct_ship_shader(GlProgram& ship_shader_program)
{
  Shader verts(Shader::Type::VERTEX);
  verts.add_source(R"(
		#version 140
    in vec2 vertex_pos;
    in vec2 tex_coord;
    uniform mat4 transform;
    out vec2 TexCoord;
    void main() {
      gl_Position = transform * vec4(vertex_pos, 1, 1);
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
    in vec2 vertex_pos;
    uniform mat4 transform;
    uniform float length;
    void main() {
      gl_Position = transform * vec4(vertex_pos.x * length, vertex_pos.y, 1, 1);
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
  glm::vec2 pos;
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
  glm::vec2 pos;
  glm::vec2 tex_coord;
};

Error set_uniform(const GlProgram& program, const char* const name,
                  GLint& out) {
  out = program.uniform_location(name);
  if (out == -1) {
    return Error(concat_strings(name, " is not a valid uniform location"));
  }
  return Error();
}

Error set_attribute(const GlProgram& program, const char* const name,
                    GLint& out) {
  out = program.attribute_location(name);
  if (out == -1) {
    return Error(concat_strings(name, " is not a valid attribute location"));
  }
  return Error();
}

void draw_object(const Transform& transform,
                 const ShaderBindings* shader_bindings,
                 glm::vec2 camera_offset,
                 float zoom) {
  static const ShaderBindings* last_bindings = nullptr;
  if (last_bindings != shader_bindings) {
    shader_bindings->program->use();

    gl::bindBuffer(GL_ARRAY_BUFFER, shader_bindings->vbo);

    gl::enableVertexAttribArray(shader_bindings->vertex_pos_attrib);
    gl::vertexAttribPointer<float>(shader_bindings->vertex_pos_attrib, 2,
                                   GL_FALSE, &Vertex::pos);

    if (shader_bindings->tex_coord_attrib != -1) {
      gl::enableVertexAttribArray(shader_bindings->tex_coord_attrib);
      gl::vertexAttribPointer<float>(shader_bindings->tex_coord_attrib, 2,
                                     GL_FALSE, &Vertex::tex_coord);
    }
  }
  last_bindings = shader_bindings;

  glm::vec2 visual_pos = transform.pos;
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

// This little tag just identifies an entity as a line segment as opposed to a
// player or UI element.
struct LineTag { };

glm::vec2 sin_cos_vector(float radians, float length = 1) {
  return glm::vec2(std::cos(radians) * length, std::sin(radians) * length);
}

using Ecs = EntityComponentSystem<Transform, ShaderBindings*, LineTag>;

class TrackGenerator {
  glm::vec2 start_;
  float heading_ = 0;  // The direction of the track.
  ShaderBindings* shader_bindings_;

public:
  TrackGenerator(glm::vec2 start, ShaderBindings* bindings)
    : start_(start), shader_bindings_(bindings) { }

  void set_heading(float h) { heading_ = h; }
  float heading() const { return heading_; }

  const glm::vec2& start() { return start_; }

  void write_track(Ecs& ecs);
};

void TrackGenerator::write_track(Ecs& ecs) {
  for (unsigned int i = 0; i < 10; ++i) {
    ecs.write_new_entity(
        Transform{start_, heading_ + glm::half_pi<float>(), 1},
        shader_bindings_, LineTag{});
    start_ += sin_cos_vector(heading_);
  }
}

Error run() {
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
    // Position    TexCoords
    {{-0.5f, -0.5f},  {0.0f, 1.0f}},
    {{ 0.5f, -0.5f},  {1.0f, 1.0f}},
    {{ 0.5f,  0.5f},  {1.0f, 0.0f}},
    {{-0.5f,  0.5f},  {0.0f, 0.0f}}
  };

  GLuint quad_vbo = gl::genBuffer();
  gl::bindBuffer(GL_ARRAY_BUFFER, quad_vbo);
  gl::bufferData(GL_ARRAY_BUFFER, quad_vertecies, GL_DYNAMIC_DRAW);

  Vertex line_vertecies[] = {
    // Position    TexCoords
    {{-0.5f, -0.05f},  {0.0f, 0.0f}},
    {{ 0.5f, -0.05f},  {0.0f, 0.0f}},
    {{ 0.5f,  0.05f},  {0.0f, 0.0f}},
    {{-0.5f,  0.05f},  {0.0f, 0.0f}}
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
      set_uniform(ship_shader_program, "tex",
                  player_shader_bindings.texture_uniform) &&
      set_uniform(ship_shader_program, "transform",
                  player_shader_bindings.transform_uniform) &&
      set_attribute(ship_shader_program, "vertex_pos",
                    player_shader_bindings.vertex_pos_attrib) &&
      set_attribute(ship_shader_program, "tex_coord",
                    player_shader_bindings.tex_coord_attrib);
      !e.ok) return e;

  auto player = ecs.write_new_entity(Transform{glm::vec2(0.0f), 0, 0},
                                     &player_shader_bindings);

  ShaderBindings line_shader_bindings(&line_shader_program, line_vbo);
  line_shader_program.use();
  if (Error e =
      set_uniform(line_shader_program, "transform",
                  line_shader_bindings.transform_uniform) &&
      set_uniform(line_shader_program, "length",
                  line_shader_bindings.length_uniform) &&
      set_attribute(line_shader_program, "vertex_pos",
                    line_shader_bindings.vertex_pos_attrib);
      !e.ok) return e;

  // One should be roughly the width of the player ship.
  TrackGenerator track_gen(glm::vec2(1, 0), &line_shader_bindings);
  track_gen.write_track(ecs);

  // TODO: These should eventually be stored into components, too.
  ShipController ship_controller;
  float ship_speed = 0;
  float ship_acc = 0;
  float ship_rotation_vel = 0;

  auto time = std::chrono::high_resolution_clock::now();
  std::chrono::high_resolution_clock::time_point last_physics_update =
    time - TIME_STEP;
  std::chrono::milliseconds dtime;

  bool keep_going = true;
  SDL_Event e;

  glm::vec2 camera_offset(0.f);
  // TODO: make less linear.
  float zoom = 0.25f; // - ship_speed * 50;

  while (keep_going) {
    while (SDL_PollEvent(&e) != 0) {
      switch (e.type) {
        case SDL_QUIT: keep_going = false; break;
        case SDL_KEYDOWN: case SDL_KEYUP:
          bool* control = nullptr;
          switch (e.key.keysym.sym) {
            case SDLK_UP: control = &ship_controller.thruster; break;
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

      if (ship_controller.thruster) ship_acc = 0.00001f;
      if (ship_controller.rotate_clockwise) ship_rotation_vel -= 0.001f;
      if (ship_controller.rotate_counterclockwise) ship_rotation_vel += 0.001f;

      // TODO: This is a nice implicit Euler integration, but consider RK4.
      // ref: https://gafferongames.com/post/integration_basics/#:~:text=Euler%20integration%20is%20the%20most,is%20constant%20over%20the%20timestep.&text=However%2C%20we%20are%20also%20integrating,error%20in%20the%20integrated%20position.
      Transform& ship_transform = ecs.read_or_panic<Transform>(player);

      ship_transform.rotation += ship_rotation_vel * TIME_STEP_MS;
      ship_speed += ship_acc * TIME_STEP_MS;
      auto pos_change = sin_cos_vector(ship_transform.rotation, ship_speed * TIME_STEP_MS);
      ship_transform.pos = ship_transform.pos + pos_change;
      ship_rotation_vel = 0;
      ship_acc = 0;

      camera_offset = pos_change;
      // TODO: This isn't very intelligent. If the offset factor is too large,
      // the player will be off screen and if too small, overly centered.
      camera_offset *= 500.f / TIME_STEP_MS;
      camera_offset += ship_transform.pos;

      zoom = 0.25f;
      if (ship_speed > 0.001) zoom -= std::log(ship_speed * 1000) * 0.06;

      // TODO: Destroy lines we collide with. We do this inside each physics
      // update to minimize the chance we go past a line entirely in one
      // iteration. Obviously, we still have to implement that at some point.
      // TODO: Actual triangle-line collision.
      for (const auto& [id, line_transform, _] : ecs.read_all<Transform, LineTag>()) {
        if (glm::distance(line_transform.pos, ship_transform.pos) < 0.75) {
          ecs.mark_to_delete(id);
        }
      }

      if (glm::distance(track_gen.start(), camera_offset) < 2.f / zoom) {
        track_gen.write_track(ecs);
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
