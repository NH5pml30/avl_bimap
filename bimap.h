#pragma once

#include <cstddef>
#include <exception>
#include <memory>
#include <stdexcept>

#include "avl_tree.h"

template <typename Left, typename Right, typename CompareLeft = std::less<Left>,
          typename CompareRight = std::less<Right>>
class bimap {
private:
  using left_t = Left;
  using right_t = Right;

  struct left_tag;
  struct right_tag;

  struct node_t : intrusive::binary_tree_element<left_tag>,
                  intrusive::binary_tree_element<right_tag> {
    left_t left_key;
    right_t right_key;

    template <typename LeftT, typename RightT>
    node_t(LeftT &&left_key, RightT &&right_key)
      noexcept(noexcept(left_t(std::forward<LeftT>(left_key))) &&
               noexcept(right_t(std::forward<RightT>(right_key))))
        : left_key(std::forward<LeftT>(left_key)),
          right_key(std::forward<RightT>(right_key)) {}
  };

  template <typename tag, auto if_left, auto if_right>
  static constexpr auto compare_tag_v =
      my_type_traits::conditional_v<std::is_same_v<tag, left_tag>, if_left,
                                    if_right>;

  template <typename tag, typename if_left, typename if_right>
  using compare_tag_t =
      std::conditional_t<std::is_same_v<tag, left_tag>, if_left, if_right>;

  template <typename tag>
  using tag_flipped = compare_tag_t<tag, right_tag, left_tag>;

  template <typename tag>
  using comparator_t = compare_tag_t<tag, CompareLeft, CompareRight>;

  template <typename tag>
  static constexpr auto node2key =
      compare_tag_v<tag, &node_t::left_key, &node_t::right_key>;

  template <typename tag>
  static constexpr auto node2key_flipped =
      compare_tag_v<tag, &node_t::right_key, &node_t::left_key>;

  template <typename tag> using side_t = compare_tag_t<tag, left_t, right_t>;

  template <typename tag> struct compare_holder : comparator_t<tag> {
    bool operator()(const node_t &left, const node_t &right) const
        noexcept(noexcept(std::declval<comparator_t<tag>>()(left.*node2key<tag>,
                                                            right.*node2key<tag>))) {
      return comparator_t<tag>::operator()(left.*node2key<tag>,
                                           right.*node2key<tag>);
    }
    bool operator()(const node_t &left, const side_t<tag> &right) const
        noexcept(noexcept(std::declval<comparator_t<tag>>()(left.*node2key<tag>,
                                                            right))) {
      return comparator_t<tag>::operator()(left.*node2key<tag>, right);
    }
    bool operator()(const side_t<tag> &left, const node_t &right) const
        noexcept(noexcept(std::declval<comparator_t<tag>>()(left, right.*node2key<tag>))) {
      return comparator_t<tag>::operator()(left, right.*node2key<tag>);
    }
    bool operator()(const side_t<tag> &left, const side_t<tag> &right) const
        noexcept(noexcept(std::declval<comparator_t<tag>>()(left, right))) {
      return comparator_t<tag>::operator()(left, right);
    }
  };

  template <typename tag>
  using tree_t = intrusive::avl_tree<node_t, compare_holder<tag>, tag>;

  tree_t<left_tag> left_tree = tree_t<left_tag>(compare_holder<left_tag>{{}});
  tree_t<right_tag> right_tree =
      tree_t<right_tag>(compare_holder<right_tag>{{}});

  template <typename tag>
  static constexpr auto bimap::*tree =
      compare_tag_v<tag, &bimap::left_tree, &bimap::right_tree>;

  template <typename tag> class iterator_ex : public tree_t<tag>::iterator {
    friend class bimap;

  private:
    using base_t = typename tree_t<tag>::iterator;
    static constexpr auto node2key = node2key<tag>;
    static constexpr auto node2key_flipped = node2key_flipped<tag>;
    using flipped = tag_flipped<tag>;

    const bimap *enclosing;

    explicit iterator_ex(const bimap *enclosing, base_t data) : base_t(data), enclosing(enclosing) {}
    explicit iterator_ex(const bimap *enclosing, const node_t *data) : base_t(data), enclosing(enclosing) {}

  public:
    using value_type = const std::remove_reference_t<decltype(
        std::declval<node_t>().*node2key)>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type *;
    using reference = value_type &;
    using iterator_category = std::bidirectional_iterator_tag;

    iterator_ex() = default;

    operator base_t() const noexcept { return *this; }

    reference operator*() const noexcept { return base_t::get().*node2key; }

    pointer operator->() const noexcept { return &(base_t::get().*node2key); }

    iterator_ex &operator++() noexcept {
      base_t::operator++();
      return *this;
    }
    iterator_ex operator++(int) noexcept {
      return iterator_ex(enclosing, base_t::operator++(0));
    }

    iterator_ex &operator--() noexcept {
      base_t::operator--();
      return *this;
    }
    iterator_ex operator--(int) noexcept {
      return iterator_ex(enclosing, base_t::operator--(0));
    }

    bool operator==(iterator_ex other) const noexcept {
      return base_t::operator==(other);
    }
    bool operator!=(iterator_ex other) const noexcept {
      return base_t::operator!=(other);
    }

    iterator_ex<flipped> flip() const noexcept {
      return base_t::operator==((enclosing->*tree<tag>).end())
                 ? iterator_ex<flipped>(
                       enclosing, (enclosing->*tree<tag_flipped<tag>>).end())
                 : iterator_ex<flipped>(enclosing, base_t::get_ptr());
    }
  };

  void copy_from(bimap const &other) {
    for (const node_t &el : other.left_tree)
      insert_ex(el.left_key, el.right_key,
                left_tree.end()); // el is biggest in `left_tree` so far
  }

public:
  using left_iterator = iterator_ex<left_tag>;
  using right_iterator = iterator_ex<right_tag>;

private:
  // insert node constructed from `left` and `right` into places `place_l` and
  // `place_r` in avl trees pre: places are correct
  template <typename LeftT, typename RightT>
  left_iterator
  insert_ex(LeftT &&left, RightT &&right,
            intrusive::const_node_view<left_tag> place_l,
            intrusive::const_node_view<right_tag> place_r)
      noexcept(noexcept(left_t(std::forward<LeftT>(left))) &&
               noexcept(right_t(std::forward<RightT>(right))) &&
               tree_t<left_tag>::is_nothrow_searchable_v &&
               tree_t<right_tag>::is_nothrow_searchable_v) {
    node_t *node =
        new node_t(std::forward<LeftT>(left), std::forward<RightT>(right));
    auto res =
        left_tree.insert(place_l, *node).first; // no comparisons => nothrow
    right_tree.insert(place_r, *node);          // no comparisons => nothrow
    return left_iterator(this, res);
  }

  template <typename LeftT, typename RightT>
  left_iterator insert_ex(LeftT &&left, RightT &&right)
      noexcept(noexcept(std::declval<bimap>().insert_ex(
                            std::forward<LeftT>(left),
                            std::forward<RightT>(right),
                            std::declval<intrusive::const_node_view<left_tag>>(),
                            std::declval<intrusive::const_node_view<right_tag>>()))) {
    if (auto [place_l, inserted_l] = left_tree.try_insert(left); inserted_l)
      if (auto [place_r, inserted_r] = right_tree.try_insert(right); inserted_r)
        return insert_ex(std::forward<LeftT>(left), std::forward<RightT>(right),
                         place_l, place_r);
    return end_left();
  }

  template <typename LeftT, typename RightT>
  left_iterator insert_ex(
      LeftT &&left, RightT &&right,
      typename tree_t<left_tag>::iterator left_hint)
          noexcept(noexcept(std::declval<bimap>()
                                .insert_ex(
                                    std::forward<LeftT>(left),
                                    std::forward<RightT>(right),
                                    std::declval<intrusive::const_node_view<left_tag>>(),
                                    std::declval<intrusive::const_node_view<right_tag>>()))) {
    if (auto [place_l, inserted_l] = left_tree.try_insert(left_hint, left);
        inserted_l)
      if (auto [place_r, inserted_r] = right_tree.try_insert(right); inserted_r)
        return insert_ex(std::forward<LeftT>(left), std::forward<RightT>(right),
                         place_l, place_r);
    return end_left();
  }

  template <typename tag>
  iterator_ex<tag> erase_ex(iterator_ex<tag> it)
      noexcept(tree_t<tag>::is_nothrow_searchable_v &&
               tree_t<tag_flipped<tag>>::is_nothrow_searchable_v) {
    auto flipped = it.flip();
    const node_t *saved_mem = it.get_ptr();
    auto res = (this->*tree<tag>).erase(it);
    (this->*tree<tag_flipped<tag>>).erase(flipped);
    delete saved_mem;
    return iterator_ex<tag>(this, res);
  }

  template <typename tag>
  bool erase_ex(const side_t<tag> &el)
      noexcept(tree_t<tag>::is_nothrow_searchable_v &&
               tree_t<tag_flipped<tag>>::is_nothrow_searchable_v) {
    auto res = (this->*tree<tag>).erase(el);
    if (res.has_value()) {
      auto iter =
          (this->*tree<tag_flipped<tag>>).iterator_from_ref(res.value().second);
      (this->*tree<tag_flipped<tag>>).erase(iter);
      delete &res.value().second;
    }
    return res.has_value();
  }

  template <typename tag>
  iterator_ex<tag>
  erase_ex(iterator_ex<tag> first, iterator_ex<tag> last)
      noexcept(noexcept(std::declval<bimap>().template erase_ex<tag>(first))) {
    auto it = first;
    while (it != last)
      it = erase_ex<tag>(it);
    return it;
  }

  template <typename tag>
  iterator_ex<tag> find_ex(const side_t<tag> &el) const
      noexcept(tree_t<tag>::is_nothrow_searchable_v) {
    return iterator_ex<tag>(this, (this->*tree<tag>).find(el));
  }

  // get flipped element from key
  // throws `std::out_of_range` if key is not in the corresponing keys
  template <typename tag>
  const side_t<tag_flipped<tag>> &at_side(const side_t<tag> &key) const
      noexcept(tree_t<tag>::is_nothrow_searchable_v) {
    auto it = find_ex<tag>(key);
    if (it == iterator_ex<tag>(this, (this->*tree<tag>).end()))
      throw std::out_of_range("Bimap: no such element");
    return it.get().*node2key_flipped<tag>;
  }

  template <typename tag>
  const side_t<tag_flipped<tag>> &
  at_side_or_default(const side_t<tag> &key)
      noexcept(std::is_nothrow_default_constructible_v<side_t<tag>> &&
               tree_t<left_tag>::is_nothrow_searchable_v &&
               tree_t<right_tag>::is_nothrow_searchable_v &&
               std::is_nothrow_copy_constructible_v<side_t<tag>> &&
               std::is_nothrow_move_constructible_v<side_t<tag_flipped<tag>>>) {
    const node_t *result;
    if (auto [place_a, inserted] = (this->*tree<tag>).try_insert(key);
        inserted) {
      // `key` not found, try to insert pair from `key` & default value
      side_t<tag_flipped<tag>> default_val{};
      if (auto [place_b, inserted] =
              (this->*tree<tag_flipped<tag>>).try_insert(default_val);
          inserted) {
        left_iterator res;
        // can insert `key` & default value
        if constexpr (std::is_same_v<tag, left_tag>)
          res = insert_ex(key, std::move(default_val), place_a, place_b);
        else
          res = insert_ex(std::move(default_val), key, place_b, place_a);
        result = &res.get();
      } else {
        // default on that side already exists, replace its pair with key
        auto default_place =
            (this->*tree<tag_flipped<tag>>).iterator_from_view(place_b);
        iterator_ex<tag_flipped<tag>> default_place_bi(this, default_place);
        node_t *node = const_cast<node_t *>(default_place_bi.get_ptr());
        (this->*tree<tag>).erase(default_place_bi.flip()); // nothrow
        node->*node2key<tag> = key;
        try {
          (this->*tree<tag>).insert(*node);
        } catch (...) {
          // insertion threw. erase node from other tree as well
          (this->*tree<tag_flipped<tag>>).erase(default_place); // nothrow
          delete node;
          throw;
        }
        // successfully finished. Return result
        result = node;
      }
    } else {
      // element is found, return its pair
      result = &*(this->*tree<tag>).iterator_from_view(place_a);
    }

    return result->*node2key_flipped<tag>;
  }

public:
  bimap(CompareLeft compare_left = CompareLeft(),
        CompareRight compare_right = CompareRight())
      : left_tree(compare_holder<left_tag>{std::move(compare_left)}),
        right_tree(compare_holder<right_tag>{std::move(compare_right)}) {}

  bimap(bimap const &other)
      : left_tree(other.left_tree.get_comparator()),
        right_tree(other.right_tree.get_comparator()) {
    copy_from(other);
  }
  bimap(bimap &&other) = default;

  bimap &operator=(bimap const &other) {
    clear();
    left_tree.copy_comparator_from(other.left_tree);
    right_tree.copy_comparator_from(other.right_tree);
    copy_from(other);
    return *this;
  }

  bimap &operator=(bimap &&other) noexcept {
    clear();
    left_tree = std::move(other.left_tree);
    right_tree = std::move(other.right_tree);
    return *this;
  }

  void clear() noexcept {
    right_tree.clear();
    left_tree.walk([](node_t *node) { delete node; });
    left_tree.clear();
  }

  ~bimap() { clear(); }

  left_iterator insert(left_t const &left, right_t const &right)
      noexcept(noexcept(std::declval<bimap>().insert_ex(left, right))) {
    return insert_ex(left, right);
  }
  left_iterator insert(left_t const &left, right_t &&right)
      noexcept(noexcept(std::declval<bimap>().insert_ex(left, std::move(right)))) {
    return insert_ex(left, std::move(right));
  }
  left_iterator insert(left_t &&left, right_t const &right)
      noexcept(noexcept(std::declval<bimap>().insert_ex(std::move(left), right))) {
    return insert_ex(std::move(left), right);
  }
  left_iterator insert(left_t &&left, right_t &&right)
      noexcept(noexcept(std::declval<bimap>().insert_ex(std::move(left), std::move(right)))) {
    return insert_ex(std::move(left), std::move(right));
  }

  left_iterator erase_left(left_iterator it)
      noexcept(noexcept(std::declval<bimap>().template erase_ex<left_tag>(it))) {
    return erase_ex<left_tag>(it);
  }
  right_iterator erase_right(right_iterator it)
      noexcept(noexcept(std::declval<bimap>().template erase_ex<right_tag>(it))) {
    return erase_ex<right_tag>(it);
  }

  bool erase_left(const left_t &left)
      noexcept(noexcept(std::declval<bimap>().template erase_ex<left_tag>(left))) {
    return erase_ex<left_tag>(left);
  }
  bool erase_right(const right_t &right)
      noexcept(noexcept(std::declval<bimap>().template erase_ex<right_tag>(right))) {
    return erase_ex<right_tag>(right);
  }

  left_iterator erase_left(left_iterator first, left_iterator last)
      noexcept(noexcept(std::declval<bimap>().template erase_ex<left_tag>(first, last))) {
    return erase_ex<left_tag>(first, last);
  }
  right_iterator
  erase_right(right_iterator first, right_iterator last)
      noexcept(noexcept(std::declval<bimap>().template erase_ex<right_tag>(first, last))) {
    return erase_ex<right_tag>(first, last);
  }

  left_iterator find_left(left_t const &left) const
      noexcept(tree_t<left_tag>::is_nothrow_searchable_v) {
    return find_ex<left_tag>(left);
  }
  right_iterator find_right(right_t const &right) const
      noexcept(tree_t<right_tag>::is_nothrow_searchable_v) {
    return find_ex<right_tag>(right);
  }

  // throws `std::out_of_range` if key is not in left keys
  right_t const &at_left(left_t const &key) const
      noexcept(tree_t<left_tag>::is_nothrow_searchable_v) {
    return at_side<left_tag>(key);
  }
  // throws `std::out_of_range` if key is not in right keys
  left_t const &at_right(right_t const &key) const
      noexcept(tree_t<right_tag>::is_nothrow_searchable_v) {
    return at_side<right_tag>(key);
  }

  // if `key` is in left keys:
  //   returns its right pair
  // otherwise:
  //   if default value of right key exists in right keys:
  //     replaces its left pair to `key`, returns the right key reference
  //     if exception occures during replacement, the map is left in a valid
  //     state (the pair is removed)
  //   otherwise:
  //     inserts pair {`key`, default value} into the map (for exception
  //     guarantess, see insert) returns reference to inserted right key
  template<typename U = right_t, typename = std::enable_if_t<std::is_default_constructible_v<U>>>
  right_t const &at_left_or_default(left_t const &key)
      noexcept(noexcept(std::declval<bimap>().template at_side_or_default<left_tag>(key))) {
    return at_side_or_default<left_tag>(key);
  }
  // if `key` is in right keys:
  //   returns its left pair
  // otherwise:
  //   if default value of left key exists in left keys:
  //     replaces its right pair to `key`, returns the left key reference
  //     if exception occures during replacement, the map is left in a valid
  //     state (the pair is removed)
  //   otherwise:
  //     inserts pair {default value, `key`} into the map (for exception
  //     guarantess, see insert) returns reference to inserted left key
  template<typename U = left_t, typename = std::enable_if_t<std::is_default_constructible_v<U>>>
  left_t const &at_right_or_default(right_t const &key)
      noexcept(noexcept(std::declval<bimap>().template at_side_or_default<right_tag>(key))) {
    return at_side_or_default<right_tag>(key);
  }

  left_iterator lower_bound_left(const left_t &left) const
      noexcept(tree_t<left_tag>::is_nothrow_searchable_v) {
    return left_iterator(this, left_tree.lower_bound(left));
  }
  left_iterator upper_bound_left(const left_t &left) const
      noexcept(tree_t<left_tag>::is_nothrow_searchable_v) {
    return left_iterator(this, left_tree.upper_bound(left));
  }

  right_iterator lower_bound_right(const right_t &right) const
      noexcept(tree_t<right_tag>::is_nothrow_searchable_v) {
    return right_iterator(this, right_tree.lower_bound(right));
  }
  right_iterator upper_bound_right(const right_t &right) const
      noexcept(tree_t<right_tag>::is_nothrow_searchable_v) {
    return right_iterator(this, right_tree.upper_bound(right));
  }

  left_iterator begin_left() const noexcept {
    return left_iterator(this, left_tree.begin());
  }
  left_iterator end_left() const noexcept {
    return left_iterator(this, left_tree.end());
  }

  right_iterator begin_right() const noexcept {
    return right_iterator(this, right_tree.begin());
  }
  right_iterator end_right() const noexcept {
    return right_iterator(this, right_tree.end());
  }

  bool empty() const noexcept { return left_tree.empty(); }

  std::size_t size() const noexcept { return left_tree.size(); }

  friend bool operator==(bimap const &a, bimap const &b)
      noexcept(noexcept(a.left_tree.call_comparator(*a.begin_left(), *a.begin_left())) &&
               noexcept(a.right_tree.call_comparator(*a.begin_right(), *a.begin_right()))) {
    if (a.size() != b.size())
      return false;
    auto &a_lt = a.left_tree;
    auto &a_rt = a.right_tree;
    for (auto it_la = a.begin_left(), it_lb = b.begin_left();
         it_la != a.end_left(); it_la++, it_lb++) {
      if (a_lt.call_comparator(*it_la, *it_lb) ||
          a_lt.call_comparator(*it_lb, *it_la))
        return false;
      auto it_ra = it_la.flip(), it_rb = it_lb.flip();
      if (a_rt.call_comparator(*it_ra, *it_rb) ||
          a_rt.call_comparator(*it_rb, *it_ra))
        return false;
    }
    return true;
  }

  friend bool operator!=(bimap const &a,
                         bimap const &b) noexcept(noexcept(a == b)) {
    return !(a == b);
  }
};
