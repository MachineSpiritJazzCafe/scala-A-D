# Scala Algorithms & Data Structures


## Structure

```
src/
├── main/scala/
│   ├── datastructures/     # Core data structures (LinkedList, Tree, Graph, etc.)
│   └── problems/           # LeetCode problem solutions organized by topic
└── test/scala/
    ├── datastructures/     # Data structure tests
    └── problems/           # Problem solution tests
```

## Running Tests

```bash
# Run all tests
sbt test

# Run specific test suite
sbt "testOnly problems.LinkedListProblemsSpec"

# Run tests in watch mode (auto-rerun on save)
sbt ~test

# Run tests with detailed output
sbt "testOnly * -- -oD"
```

## Progress Tracking

### Data Structures Implemented
- [x] LinkedList
- [ ] DoublyLinkedList
- [ ] Stack & Queue
- [ ] Binary Search Tree
- [ ] Hash Table
- [ ] Graphs
- [ ] Heap

### Problems Solved by Topic

#### Linked Lists

#### Arrays & Strings

#### Trees & Graphs

#### Dynamic Programming

