#include "ecs.h"

#include "test.h"

int main() {
  EntityComponentSystem<int, char> ecs_ic;
  TEST_WITH(
      EntityComponentSystem<int> ecs;
      auto id = ecs.write_new_entity(4),
      ecs.read_or_panic<int>(id), 4);

  // Funny story: macros can't have the commas from type lists in them.
  EntityComponentSystem<int, unsigned> iu;
  TEST_WITH(
      iu.write_new_entity(1, 1u);
      iu.write_new_entity(1);  // Ignored since it doesn't have both.
      iu.write_new_entity(1, 1u);
      int sum = 0;
      for (auto [id, i, u] : iu.read_all<int, unsigned>()) sum += i + u,
      sum, 4);

  TEST_WITH(
      const auto& const_iu = iu;
      int sum = 0;
      for (auto [id, i, u] : const_iu.read_all<int, unsigned>()) sum += i + u,
      sum, 4);

  EntityComponentSystem<int, unsigned> iu2;
  TEST_WITH(
      iu2.write_new_entity(1, 1u);
      iu2.write_new_entity(1, 1u);
      auto id = iu2.write_new_entity(1, 1u);
      iu2.deactivate(id);  // This is now ignored.
      int sum = 0;
      for (auto [id, i, u] : iu2.read_all<int, unsigned>()) sum += i + u,
      sum, 4);
}
