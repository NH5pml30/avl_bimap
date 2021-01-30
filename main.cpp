#include "bimap.h"

#include "gtest/gtest.h"

// no tests because they were not mine

TEST(avl_bimap, dummy) {
  bimap<int, int> b;
  b.insert(1, 2);
  b.insert(2, 1);
}