
# Thalloc

Allocate a class of students to a subset of groups, based on some rules.

Each student is defined by the following:

- A name
- Senior or junior
- A list of 6 preferred choices for groups, by rank

The allocation rules:

- Each group should be between 3 and 5 students inclusive
- Each group should have at least one junior and one senior student;
  preferably at least two seniors students
- Take into account the preferences of students,
  according to their group choices

The trick is that the total number of groups is larger than the final
selected group; the algorithm should pick the optimal subset of groups
(and given that the number of groups in the subset can vary as the group
can contain between 3 and 5 students).

To understand how the program works, read the source.

My first program in Rust, be lenient on the coding style.

By the way, rust rocks!

No licence given, do whatever you feel with this little piece of code.
