#pragma once

#include <optional>

#include "avl_node.h"

namespace intrusive {
namespace compare_traits {
template <class Compare, typename LeftT, typename RightT>
constexpr inline bool is_nothrow_comparable_ex_v = noexcept(
    std::declval<Compare>()(std::declval<LeftT>(), std::declval<RightT>()));

template <class Compare, typename T>
constexpr inline bool is_nothrow_comparable_v =
    is_nothrow_comparable_ex_v<Compare, const T &, const T &>;

template <class Compare, typename NodeT, typename T, typename Key>
constexpr inline bool is_nothrow_comparable_key_v =
    is_nothrow_comparable_ex_v<Compare, const Key &, const T &>
        &&is_nothrow_comparable_ex_v<Compare, const T &, const Key &>;
template <class Compare, typename NodeT, typename T>
constexpr inline bool is_nothrow_comparable_key_v<Compare, NodeT, T, NodeT> =
    is_nothrow_comparable_v<Compare, T>;
} // namespace compare_traits

template <typename T, class Compare = std::less<T>, typename Tag = default_tag>
class avl_tree {
private:
  using node_t = binary_tree_element<Tag>;
  using node_view_t = node_view<Tag>;
  using const_node_view_t = const_node_view<Tag>;
  node_t end_ = {nullptr, nullptr, nullptr};
  node_t *root = &end_;
  node_t *min = root;
  size_t size_ = 0;

  mutable struct comparer : public Compare {
    static constexpr node_t *neg_inf = nullptr;
    const node_t *inf;

    static constexpr bool is_nothrow_comparable_v =
        compare_traits::is_nothrow_comparable_v<Compare, T>;

    template <class Key>
    static constexpr bool is_nothrow_comparable_key_v =
        compare_traits::is_nothrow_comparable_key_v<Compare, node_t, T, Key>;

    template <class LeftT, class RightT>
    static constexpr bool is_nothrow_comparable_ex_v =
        compare_traits::is_nothrow_comparable_ex_v<Compare, LeftT, RightT>;

    comparer(Compare less, const node_t *inf)
        : Compare(std::move(less)), inf(inf) {}

    bool operator()(const node_t *left, const node_t *right) const
        noexcept(is_nothrow_comparable_v) {
      return left == neg_inf
                 ? right != neg_inf
                 : (right == inf ? left != inf
                                 : left != inf && right != neg_inf &&
                                       Compare::operator()(
                                           static_cast<const T &>(*left),
                                           static_cast<const T &>(*right)));
    }
    template <class Key>
    std::enable_if_t<!std::is_same_v<Key, node_t>, bool>
    operator()(const node_t *left, const Key *right) const
        noexcept(is_nothrow_comparable_ex_v<const T &, const Key &>) {
      return left == neg_inf ||
             (left != inf &&
              Compare::operator()(static_cast<const T &>(*left), *right));
    }
    template <class Key>
    std::enable_if_t<!std::is_same_v<Key, node_t>, bool>
    operator()(const Key *left, const node_t *right) const
        noexcept(is_nothrow_comparable_ex_v<const Key &, const T &>) {
      return right == inf ||
             (right != neg_inf &&
              Compare::operator()(*left, static_cast<const T &>(*right)));
    }
    template <class Key>
    std::enable_if_t<!std::is_same_v<Key, node_t>, bool>
    operator()(const Key *left, const T *right) const
        noexcept(is_nothrow_comparable_ex_v<const T &, const T &>) {
      return Compare::operator()(*left, *right);
    }
  } less;

public:
  static constexpr bool is_nothrow_searchable_v =
      comparer::is_nothrow_comparable_v;

  class iterator {
    friend class avl_tree;

  protected:
    const node_t *data;

    explicit iterator(const node_t *data) noexcept : data(data) {}

  public:
    using value_type = const T;
    using difference_type = std::ptrdiff_t;
    using pointer = const T *;
    using reference = const T &;
    using iterator_category = std::bidirectional_iterator_tag;

  protected:
    reference get() const noexcept { return static_cast<reference>(*data); }
    pointer get_ptr() const noexcept { return &get(); }

  public:
    iterator() = default;

    reference operator*() const noexcept { return get(); }
    pointer operator->() const noexcept { return get_ptr(); }

    bool operator==(iterator other) const noexcept {
      return data == other.data;
    }
    bool operator!=(iterator other) const noexcept {
      return data != other.data;
    }

    iterator &operator--() noexcept {
      data = node_t::prev(std::move(data));
      return *this;
    }
    iterator &operator++() noexcept {
      data = node_t::next(std::move(data));
      return *this;
    }
    iterator operator--(int) noexcept {
      iterator res(data);
      operator--();
      return res;
    }
    iterator operator++(int) noexcept {
      iterator res(data);
      operator++();
      return res;
    }
  };

private:
  const_node_view_t view_node(iterator pos) const noexcept {
    const node_t *const *holder =
        pos.data == root ? &root : &node_t::compare_child(pos.data);
    return const_node_view_t(*holder);
  }
  node_view_t view_node(iterator pos) noexcept {
    node_t *data = const_cast<node_t *>(pos.data);
    node_t **holder = pos.data == root ? &root : &node_t::compare_child(data);
    return node_view_t(*holder);
  }

  const_node_view_t view_root() const noexcept {
    return view_node(iterator(root));
  }
  node_view_t view_root() noexcept { return view_node(iterator(root)); }

  iterator iter_from_found(const node_t *node) const noexcept {
    return node == nullptr ? end() : iterator(node);
  }

  void print() const { node_t::template print<T>(root, &end_); }

  void check_tree() const {
    if (IS_DEBUG)
      assert(size_ ==
             node_t::check_tree(root, less.neg_inf, less.inf, less).second - 1);
  }

public:
  static_assert(std::is_convertible_v<T &, binary_tree_element<Tag> &>,
                "value type is not convertible to binary_tree_element");

  avl_tree(Compare less = Compare())
      noexcept(std::is_nothrow_move_constructible_v<Compare>)
      : less{std::move(less), &end_} {}
  avl_tree(avl_tree &&other) noexcept(
      std::is_nothrow_move_constructible_v<Compare>)
      : root(other.root), less{std::move(other.less.default_less), &end_} {
    view_node(other.end()).replace(&end_);
    other.clear();
  }
  avl_tree(const avl_tree &other) = delete;
  avl_tree &operator=(avl_tree &&other)
      noexcept(std::is_nothrow_move_assignable_v<Compare>) {
    root = other.root;
    less.Compare::operator=(std::move(other.less));
    view_node(other.end()).replace(&end_);
    other.clear();
    return *this;
  }
  avl_tree &operator=(const avl_tree &other) = delete;

  iterator iterator_from_ref(const T &el) const noexcept {
    return iterator(&el);
  }
  // pre: view views an existing element
  iterator iterator_from_view(const_node_view_t view) const noexcept {
    assert(view.holder != nullptr);
    return iterator(view.holder);
  }

  void clear() noexcept {
    root = &end_;
    root->left = root->right = root->parent = nullptr;
    min = root;
    size_ = 0;
  }

  template <class Key>
  std::pair<const_node_view_t, bool> try_insert(iterator hint,
                                                const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return view_root().try_insert(&el, view_node(hint), less);
  }
  template <class Key>
  std::pair<const_node_view_t, bool> try_insert(const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return try_insert(iterator(root), el);
  }

  // if comparator throws, insertion has no effect
  std::pair<iterator, bool>
  insert(T &el) noexcept(comparer::is_nothrow_comparable_v) {
    return insert(view_root(), el);
  }

  // `hint`: iterator, used as a place to start the search
  // behaviour is undefined if the right place to insert is not in subtree of
  // `hint` if compare throws, the insertion has no effect
  std::pair<iterator, bool>
  insert(iterator hint, T &el) noexcept(comparer::is_nothrow_comparable_v) {
    return insert(view_node(hint), el);
  }

  // `hint`: node_view, used as a place to start the search
  // behaviour is undefined if the right place to insert is not in subtree of
  // `hint` if compare throws, the insertion has no effect if `hint` points to a
  // place to insert (for example, result of `try_insert`), function cannot
  // throw
  std::pair<iterator, bool>
  insert(const_node_view_t hint,
         T &el) noexcept(comparer::is_nothrow_comparable_v) {
    size_++;
    if (less(&el, min))
      min = &el;

    node_t *&holder = *const_cast<node_t **>(&hint.holder);
    node_t *parent = const_cast<node_t *>(hint.parent);
    auto [res, inserted] =
        view_root().insert(&el, node_view_t(parent, holder), less);
    return {iterator(res), inserted};
  }

  template <class Key>
  iterator lower_bound(const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return iter_from_found(view_root().lower_bound(&el, view_root(), less));
  }

  template <class Key>
  iterator upper_bound(const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return iter_from_found(view_root().upper_bound(&el, view_root(), less));
  }

  template <class Key>
  iterator find(const Key &el) const
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return iter_from_found(view_root().find(&el, view_root(), less));
  }

  // returns: iterator to the next element in order
  iterator erase(iterator pos) noexcept {
    return erase(pos, static_cast<const T &>(*pos.data))
        .value()
        .first; // no compares will be executed
  }

  // if comparator throws, erasure has no effect
  // returns:
  //   if element is found, {iterator to next element, removed node}
  //   otherwise, {empty, nullptr}
  template <class Key>
  std::enable_if_t<!std::is_convertible_v<Key, iterator>,
                   std::optional<std::pair<iterator, T &>>>
  erase(const Key &el)
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    return erase(iterator(root), el);
  }

  // `hint`: iterator, used as a suggestion as to where to start the search
  // behaviour is undefined if `el` is `end()`
  // if comparator throws, erasure has no effect
  // returns:
  //   if element is found, {iterator to next element, removed node}
  //   otherwise, {empty, nullptr}
  template <class Key>
  std::optional<std::pair<iterator, T &>>
  erase(iterator hint, const Key &el)
      noexcept(comparer::template is_nothrow_comparable_key_v<Key>) {
    auto res = view_root().erase(&el, view_node(hint), less);
    if (res.first == nullptr && hint.data != root)
      // try erase from root instead
      res = view_root().erase(&el, view_root(), less);
    if (res.first != nullptr) {
      size_--;
      if (!less(min, &el) && !less(&el, min))
        min = res.second;
      return std::pair<iterator, T &>(iterator(res.second),
                                      static_cast<T &>(*res.first));
    }
    return {};
  }

  iterator begin() noexcept { return iterator(min); }
  iterator begin() const noexcept { return iterator(min); }
  iterator end() noexcept { return iterator(&end_); }
  iterator end() const noexcept { return iterator(&end_); }

  const Compare &get_comparator() const noexcept { return less; }

  template <typename Left, typename Right>
  bool call_comparator(Left &&a, Right &&b) const
      noexcept(comparer::template is_nothrow_comparable_ex_v<Left &&, Right &&>) {
    return static_cast<Compare &>(less)(std::forward<Left>(a),
                                        std::forward<Right>(b));
  }

  // pre: tree is empty
  void copy_comparator_from(const avl_tree &other)
      noexcept(std::is_nothrow_copy_assignable_v<Compare>) {
    assert(size_ == 0);
    less = comparer(other.less, &end_);
  }

  size_t size() const noexcept { return size_; }
  bool empty() const noexcept { return size_ == 0; }

  template <class Callback> void walk(Callback &&callback) const
      noexcept(noexcept(callback(std::declval<const T *>()))) {
    node_t::const_walk(root, &end_, [&](const node_t *node) {
      callback(static_cast<const T *>(node));
    });
  }

  template <class Callback> void walk(Callback &&callback)
      noexcept(noexcept(callback(std::declval<T *>()))) {
    node_t::walk(root, &end_,
                 [&](node_t *node) { callback(static_cast<T *>(node)); });
  }
};
} // namespace intrusive
