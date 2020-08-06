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

// In the ECS, an entity can be deactivated and it will generally be skipped.
struct EntityData {
  EntityId id;
  bool active = true;
};

using EntityStore = SortedVector<EntityData>;

constexpr bool operator < (EntityData a, EntityData b) { return a.id < b.id; }
constexpr bool operator == (EntityData a, EntityData b) { return a.id == b.id; }
constexpr bool operator != (EntityData a, EntityData b) { return a.id != b.id; }

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
using Store = SortedVector<ComponentData<T>>;

// A range abstraction that allows multiple component data series to be iterated
// lazily over in a range-based for loop.
template<typename StoreTuple>
class ComponentRange {
  const EntityStore& ids_;
  StoreTuple stores_;

protected:
  template<typename EntityIdIterator, typename IteratorTuple>
  struct Iterator {
    EntityIdIterator entity_iterator;
    EntityIdIterator entity_iterator_end;
    IteratorTuple its;
    IteratorTuple ends;

    bool sentinel = false;

    template<typename It>
    bool at_end(It it) const {
      return it == std::get<It>(ends);
    }
    // Note that the alternative to the above is
    // std::get<std::decay_t<decltype(it)>>(ends).

    bool any_at_end() const {
      if (entity_iterator == entity_iterator_end) return true;
      auto any = [](auto...args) { return (args || ...); };
      auto pred = [this](const auto& it) { return at_end(it); };
      return std::apply(any, tuple_map(pred, its));
    }

    // True if all iterators point to the same ID.
    bool all_same() const {
      auto all = [](auto...args) { return (args && ...); };
      auto equals_max_id =
        [this](const auto& it) { return it->id == entity_iterator->id; };
      return std::apply(all, tuple_map(equals_max_id, its));
    }

    template<typename Iter>
    void catch_up_entity_iter(const Iter& it) {
      while (entity_iterator != entity_iterator_end &&
             (!entity_iterator->active || entity_iterator->id < it->id))
        ++entity_iterator;
    }

    template<typename Iter>
    void increment_iter(Iter& it) {
      if (!at_end(it)) ++it;
      if (!at_end(it)) catch_up_entity_iter(it);
    }

    // A helper to catch up iterators that are behind.
    void increment_if_lower_than_max_id() {
      auto impl = [&](auto& it) {
        if (!at_end(it) && it->id < entity_iterator->id) increment_iter(it);
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

    Iterator(EntityIdIterator it, EntityIdIterator end,
             IteratorTuple its, IteratorTuple ends)
      : entity_iterator(it), entity_iterator_end(end),
        its(std::move(its)), ends(std::move(ends)) {
        if (!any_at_end())
          tuple_foreach([&](auto it) { catch_up_entity_iter(it); }, its);
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
      return std::tuple_cat(std::tuple(entity_iterator->id),
                            tuple_map_forward(data, its));
    }
  };

public:
  explicit ComponentRange(const EntityStore& ids, StoreTuple stores)
    : ids_(ids), stores_(stores) { }

  auto begin() const {
    auto b = [](auto&& store) { return store.begin(); };
    auto e = [](auto&& store) { return store.end(); };
    return Iterator(ids_.begin(), ids_.end(),
                    tuple_map(b, stores_), tuple_map(e, stores_));
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
  EntityStore entity_ids_;

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

  auto find_id(EntityId id) {
    auto get_id = [](EntityData d) { return d.id; };
    return entity_ids_.find(id, get_id);
  }

  auto find_id(EntityId id) const {
    auto get_id = [](EntityData d) { return d.id; };
    return entity_ids_.find(id, get_id);
  }

public:
  EntityComponentSystem() = default;

  void clear() {
    entity_ids_.clear();
    garbage_ids_.clear();
    (get_store<Components>().clear(), ...);
    next_id_ = {0};
  }

  EntityId new_entity() {
    entity_ids_.push_back({next_id_, true});
    next_id_.id++;
    return entity_ids_.back().id;
  }

  void deactivate(EntityId id) {
    if (auto [it, found] = find_id(id); found) it->active = false;
  }

  void activate(EntityId id) {
    if (auto [it, found] = find_id(id); found) it->active = true;
  }

  bool is_active(EntityId id) {
    auto [it, found] = find_id(id);
    return found && it->active;
  }

  void mark_to_delete(EntityId id) {
    garbage_ids_.push_back(id);
    std::sort(garbage_ids_.begin(), garbage_ids_.end());
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
    get_store<U>().erase_if(pred);
  }

  void deleted_marked_ids() {
    std::sort(garbage_ids_.begin(), garbage_ids_.end());
    auto garbage_it = garbage_ids_.begin();
    auto pred = [&](const EntityData data) {
      while (garbage_it != garbage_ids_.end() && *garbage_it < data.id)
        ++garbage_it;
      return (garbage_it != garbage_ids_.end() && data.id == *garbage_it);
    };
    entity_ids_.erase_if(pred);
    (delete_marked_component<Components>(), ...);
    garbage_ids_.clear();
  }

  enum WriteAction { CREATE_ENTRY, CREATE_OR_UPDATE, UPDATE_ONLY };

  template<typename T, typename...U>
  static constexpr bool any_is() { return (std::is_same_v<T, U> || ...); }

  template<typename T>
  static constexpr void assert_has_type() {
    static_assert(any_is<T, Components...>());
  }

  // Adds data to a component, although that the entity exists is taken for
  // granted.
  template<typename T>
  EcsError write(EntityId id, T data,
                 WriteAction action = WriteAction::UPDATE_ONLY) {
    assert_has_type<T>();
    auto [found, insert] = find_component<T>(id);
    if (found && insert->id == id && action == WriteAction::CREATE_ENTRY) {
      return EcsError::ALREADY_EXISTS;
    }
    if (!found && action == WriteAction::UPDATE_ONLY) {
      return EcsError::NOT_FOUND;
    }
    if (!found) {
      emplace_at(id, insert, std::move(data));
    } else {
      insert->data = std::move(data);
    }
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
    // TODO: Check for errors. Should only matter on ID overflow.
    (write(id, std::move(components), WriteAction::CREATE_ENTRY), ...);
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
    return ComponentRange(entity_ids_,
                          std::forward_as_tuple(get_store<U>()...));
  }

  template<typename...U>
  auto read_all() {
    return ComponentRange(entity_ids_,
                          std::forward_as_tuple(get_store<U>()...));
  }

  bool has_entity(EntityId id) const {
    return entity_ids_.contains(id, &EntityData::id);
  }

  template<typename U>
  void erase_component(EntityId id) {
    get_store<U>().find_erase(id, &ComponentData<U>::id);
  }

  void erase(EntityId id) {
    (erase_component<Components>(id), ...);
    entity_ids_.find_erase(id, &EntityData::id);
  }
};

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

  void clear() { free_list_.clear(); }

  template<typename...Components>
  void deactivate(EntityComponentSystem<Components...>& ecs, EntityId id) {
    free_list_.insert_if_not_present(id);
    ecs.deactivate(id);
  }

  template<typename...Components, typename...Args>
  void create_new(EntityComponentSystem<Components...>& ecs, Args&&...args) {
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
      ecs.write_new_entity(std::forward<Args>(args)...);
    }
  }
};
