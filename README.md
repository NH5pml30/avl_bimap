# avl_bimap
Template bimap implementation. Underlying search data strucutre is intrusive AVL tree.

Allows for logarithmic in container size lookup of left or right side of contained key-value pairs.

`Compare` requirements are the same as C++ standard `Compare` (including viable assignment operator for container assignment).
