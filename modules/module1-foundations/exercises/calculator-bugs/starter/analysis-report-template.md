# Analysis Report: Calculator Bugs

## Student Name: Flavia Daniels
## Date: March 12

---

## Part 1: Static Analysis Findings (ESLint)

Run `npx eslint calculator.js` and record all findings below.

| # | Line | Rule                  | Description             | Severity |
| - | ---- | --------------------- | ----------------------- | -------- |
| 1 | 13   | no-undef              | reslt not defined       | Error    |
| 2 | 20   | no-unreachable        | code after return       | Error    |
| 3 | 30   | no-fallthrough        | missing break in switch | Warning  |
| 4 | 61   | no-unused-vars        | temp variable unused    | Warning  |
| 5 | 73   | no-constant-condition | if(true) always true    | Warning  |


**Total static analysis issues found:** 5

---

## Part 2: Dynamic Analysis Findings (Test Suite)

Run `node test-calculator.js` and record all test failures below.

| # | Test Name             | Error Message      | Root Cause          |
| - | --------------------- | ------------------ | ------------------- |
| 1 | add(2,3)              | reslt not defined  | wrong variable name |
| 2 | divide(10,0)          | division by zero   | missing check       |
| 3 | calculate('add',10,5) | expected 15 got 5  | switch fallthrough  |
| 4 | factorial(-1)         | infinite recursion | missing base case   |
| 5 | absolute(5)           | expected 5 got -5  | constant condition  |


**Total dynamic analysis issues found:** 5

---

## Part 3: Comparison

### Which bugs did ONLY static analysis catch?
<!-- List bugs found by ESLint but NOT by running tests -->

1. Unused variable temp
2. Unreachable console.log

### Which bugs did ONLY dynamic analysis catch?
<!-- List bugs found by tests but NOT by ESLint -->

1. Division by zero
2. Infinite recursion in factorial

### Which bugs were found by BOTH approaches?
<!-- List bugs caught by both ESLint and test failures -->

1. Switch fallthrough in calculate

---

## Part 4: Reflection

### Why can't static analysis catch all bugs?
Static analysis only examines the code without running it. Bugs that depend on runtime inputs or execution, like division by zero or infinite recursion, may not be detected.


### Why can't dynamic analysis catch all bugs?
Dynamic analysis only tests the code paths executed during testing. If some paths aren’t tested, certain bugs will not appear, even if they exist in the code.


### When would you prioritize one approach over the other?
Static analysis is useful early to catch syntax errors and code quality issues. Dynamic analysis is important for testing real program behavior and runtime errors. Using both together provides the most complete coverage.
