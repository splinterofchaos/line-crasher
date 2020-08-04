#pragma once

#include <vector>
#include <tuple>
#include <iostream>

#include "util.h"

// Note: One source of knowledge:
// https://www.gamasutra.com/blogs/TobiasStein/20171122/310172/The_EntityComponentSystem__An_awesome_gamedesign_pattern_in_C_Part_1.php

enum class EcsError {
  OK,
  ALREADY_EXISTS,
  NOT_FOUND
};

// In this ECS, all objects are uniquely identified by an integer.
struct EntityId {
  static constexpr unsigned int NOT_AN_ID = 0;
  unsigned int id = NOT_AN_ID;
};

constexpr bool operator < (EntityId a, EntityId b) { return a.id < b.id; }
constexpr bool operator == (EntityId a, EntityId b) { return a.id == b.id; }
constexpr bool operator != (EntityId a, EntityId b) { return a.id != b.id; }

// Each component is stored with a pointer to its entity and some data.
template<typename T>
struct ComponentData {
  EntityId id;
  T data;

  ComponentData(EntityId id, T data)
    : id(id), data(std::move(data)) { }
};

// Each series of components is also manually sorted.
template<typename T>
using Store = std::vector<ComponentData<T>>;

// A range abstraction that allows multiple component data series to be iterated
// lazily over in a range-based for loop.
template<typename StoreTuple>
class ComponentRange {
  StoreTuple stores_;

protected:
  template<typename IteratorTuple>
  struct Iterator {
    IteratorTuple its;
    IteratorTuple ends;

    // To know if all iterators are pointing to the same entity, we'll neet
    // to remember what entity that is.
    EntityId max_id;

    bool sentinel = false;

    template<typename It>
    bool at_end(It it) const {
      return it == std::get<It>(ends);
    }
    // Note that the alternative to the above is
    // std::get<std::decay_t<decltype(it)>>(ends).

    bool any_at_end() const {
      auto any = [](auto...args) { return (args || ...); };
      auto pred = [this](const auto& it) { return at_end(it); };
      return std::apply(any, tuple_map(pred, its));
    }

    // True if all iterators point to the same ID.
    bool all_same() const {
      auto all = [](auto...args) { return (args && ...); };
      auto equals_max_id =
        [this](const auto& it) { return it->id == max_id; };
      return std::apply(all, tuple_map(equals_max_id, its));
    }

    template<typename Iter>
    void increment_iter(Iter& it) {
      if (!at_end(it)) ++it;
      if (!at_end(it)) max_id = std::max(it->id, max_id);
    }

    // A helper to catch up iterators that are behind.
    void increment_if_lower_than_max_id() {
      auto impl = [&](auto& it) {
        if (!at_end(it) && it->id < max_id) increment_iter(it);
      };
      tuple_foreach(impl, its);
    }

    // After iteration, or on initialization, increment any iterators that
    // are behind.
    void catch_up() {
      while (!any_at_end() && !all_same()) increment_if_lower_than_max_id();
      if (any_at_end()) its = ends;
    }


    Iterator& operator++() {
      // Increment at least once.
      tuple_foreach([this](auto& it) { increment_iter(it); }, its);
      catch_up();

      return *this;
    }

    Iterator operator++(int) {
      Iterator old = *this;
      ++(*this);
      return old;
    }

    Iterator(IteratorTuple its, IteratorTuple ends)
      : its(std::move(its)), ends(std::move(ends)) {
        max_id = EntityId{0};
        tuple_foreach(
            [&](auto it) {
              if (!at_end(it)) max_id = std::max(max_id, it->id);
            }, its);
        catch_up();  // Ensure a valid initial starting position.
      }

    Iterator() { sentinel = true; }

    bool operator==(const Iterator& other) const {
      return sentinel ? other == *this : any_at_end();
    }
    bool operator!=(const Iterator& other) const {
      return !(*this == other);
    }

    auto operator*() const {
      auto data = [](const auto& it) -> auto& { return it->data; };
      return std::tuple_cat(std::tuple(max_id), tuple_map_forward(data, its));
    }
  };

public:
  explicit ComponentRange(StoreTuple stores)
    : stores_(stores) { }

  auto begin() const {
    auto b = [](auto&& store) { return store.begin(); };
    auto e = [](auto&& store) { return store.end(); };
    return Iterator(tuple_map(b, stores_), tuple_map(e, stores_));
  }

  auto end() const {
    return decltype(begin())();
  }
};

// A single object manages components of all types. The entities themselves
// store no data. No two components may be of the same type.
template<typename...Components>
class EntityComponentSystem {
  // The series of ID's will be contiguously stored (frequently iterated
  // through) and manually sorted.
  std::vector<EntityId> entity_ids_;

  // Entities to be deleted.
  std::vector<EntityId> garbage_ids_;

  // We assign the ID's to ensure their uniqueness.
  EntityId next_id_ = { 1 };

  // And so each series of components may be stored by their type.
  std::tuple<Store<Components>...> components_;

  template<typename T>
  const Store<T>& get_store() const { return std::get<Store<T>>(components_); }
  template<typename T>
  Store<T>& get_store() { return std::get<Store<T>>(components_); }

  template<typename T>
  using FindResultConst = std::pair<bool, typename Store<T>::const_iterator>;

  template<typename T>
  using FindResult = std::pair<bool, typename Store<T>::iterator>;

  template<typename T>
  FindResultConst<T> find_component(EntityId id) const {
    const Store<T>& series = get_store<T>();
    auto pred =
      [](const ComponentData<T>& c, EntityId id) { return c.id < id; };
    auto it = std::lower_bound(series.begin(), series.end(), id, pred);
    return std::make_pair(it != series.end() && it->id == id, it);
  }

  template<typename T>
  FindResult<T> find_component(EntityId id) {
    Store<T>& series = get_store<T>();
    auto pred =
      [](const ComponentData<T>& c, EntityId id) { return c.id < id; };
    auto it = std::lower_bound(series.begin(), series.end(), id, pred);
    return std::make_pair(it != series.end() && it->id == id, it);
  }

  template<typename Iterator, typename T>
  void emplace_at(EntityId id, Iterator it, T data) {
    get_store<T>().emplace(it, id, std::move(data));
  }

  const EntityComponentSystem<Components...>* const_this() const {
    return this;
  }

  public:
  EntityComponentSystem() = default;

  EntityId new_entity() {
    entity_ids_.push_back(next_id_);
    next_id_.id++;
    return entity_ids_.back();
  }

  void mark_to_delete(EntityId id) {
    garbage_ids_.push_back(id);
  }

  bool is_marked(EntityId id) {
    return std::binary_search(garbage_ids_.begin(), garbage_ids_.end(), id);
  }

  template<typename U>
  void delete_marked_component() {
    auto garbage_it = garbage_ids_.begin();
    auto pred = [&](const auto& component_data) {
      const EntityId& id = component_data.id;
      while (garbage_it != garbage_ids_.end() && *garbage_it < id)
        ++garbage_it;
      return (garbage_it != garbage_ids_.end() && id == *garbage_it);
    };
    std::erase_if(get_store<U>(), pred);
  }

  void deleted_marked_ids() {
    std::sort(garbage_ids_.begin(), garbage_ids_.end());
    auto garbage_it = garbage_ids_.begin();
    auto pred = [&](const EntityId id) {
      while (garbage_it != garbage_ids_.end() && *garbage_it < id)
        ++garbage_it;
      return (garbage_it != garbage_ids_.end() && id == *garbage_it);
    };
    std::erase_if(entity_ids_, pred);
    (delete_marked_component<Components>(), ...);
    garbage_ids_.clear();
  }

  enum WriteAction { CREATE_ENTRY, CREATE_OR_UPDATE };

  // Adds data to a component, although that the entity exists is taken for
  // granted.
  template<typename T>
  EcsError write(EntityId id, T data,
                 WriteAction action = WriteAction::CREATE_ENTRY) {
    auto [found, insert] = find_component<T>(id);
    if (found && insert->id == id && action == WriteAction::CREATE_ENTRY) {
      return EcsError::ALREADY_EXISTS;
    }
    emplace_at(id, insert, std::move(data));
    return EcsError::OK;
  }

  // Adds data to a component, although that the entity exists is taken for
  // granted.
  template<typename T, typename U, typename...V>
  EcsError write(EntityId id, T data, U next, V...rest) {
    EcsError e = write(id, std::move(data));
    return e != EcsError::OK ?
      e : write(id, std::move(next), std::move(rest)...);
  }

  template<typename...T>
  EntityId write_new_entity(T...components) {
    EntityId id = new_entity();
    write(id, std::move(components)...);
    return id;
  }

  template<typename T>
  EcsError read(EntityId id, const T** out) const {
    auto [found, it] = find_component<T>(id);
    if (!found) return EcsError::NOT_FOUND;
    *out = &it->data;
    return EcsError::OK;
  }

  template<typename T>
  void read_or_panic(EntityId id, const T** out) const {
    *out = &read_or_panic<T>(id);
  }

  // Unsafe version of read that ignores NOT_FOUND errors.
  template<typename T>
  const T& read_or_panic(EntityId id) const {
    auto [found, it] = find_component<T>(id);
    if (!found) {
      std::cerr << "Exiting because of entity not found." << std::endl;
      *(char*)nullptr = '0';
    }
    return it->data;
  }

  template<typename T>
  EcsError read(EntityId id, T** out) {
    return const_this()->read(id, const_cast<const T**>(out));
  }

  template<typename T>
  void read_or_panic(EntityId id, T** out) {
    *out = &read_or_panic<T>(id);
  }

  // Unsafe version of read that ignores NOT_FOUND errors.
  template<typename T>
  T& read_or_panic(EntityId id) {
    auto [found, it] = find_component<T>(id);
    if (!found) {
      std::cerr << "Exiting because of entity not found." << std::endl;
      *(char*)nullptr = '0';
    }
    return it->data;
  }

  template<typename...T>
  EcsError read(EntityId id, const T**...out) const {
    std::vector<EcsError> errors{ read(id, out)... };
    for (EcsError e : errors) if (e != EcsError::OK) return e;
    return EcsError::OK;
  }

  template<typename...T>
  void read_or_panic(EntityId id, const T**...out) const {
    (read_or_panic(id, out), ...);
  }

  template<typename...T>
  EcsError read(EntityId id, T**...out) {
    return const_this()->read(id, const_cast<const T**>(out)...);
  }

  template<typename...T>
  void read_or_panic(EntityId id, T**...out) {
    (read_or_panic(id, out), ...);
  }

  template<typename...U>
  auto read_all() const {
    return ComponentRange(std::tuple<const Store<U>&...>(get_store<U>()...));
  }

  template<typename...U>
  auto read_all() {
    return ComponentRange(std::tuple<Store<U>&...>(get_store<U>()...));
  }

  bool has_entity(EntityId id) const {
    auto it = std::lower_bound(entity_ids_.begin(), entity_ids_.end(), id);
    return it != entity_ids_.end() && *it == id;
  }

  template<typename U>
  void erase_component(EntityId id) {
    auto [found, it] = find_component<U>(id);
    if (found) get_store<U>().erase(it);
  }

  void erase(EntityId id) {
    (erase_component<Components>(id), ...);
    auto it = std::lower_bound(entity_ids_.begin(), entity_ids_.end(), id);
    if (it == entity_ids_.end()) return;
    entity_ids_.erase(it);
  }
};
