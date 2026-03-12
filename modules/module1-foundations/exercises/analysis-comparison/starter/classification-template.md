# Analysis Classification Exercise

## Instructions
For each code snippet in `code-samples.md`, fill in the table below.

**Objective categories:** Correctness, Security, Performance
**Detection method:** Static, Dynamic, Both

---

| Snippet | Issue | Objective | Detection | Explanation |
|--------|-------|-----------|-----------|-------------|
| 1 | SQL injection | Security | Static | User input in SQL query |
| 2 | Unreachable code | Correctness | Static | Code after return |
| 3 | Divide by zero | Correctness | Both | Empty list possible |
| 4 | Buffer overflow | Security | Static | No bounds check |
| 5 | Off-by-one loop | Correctness | Static | Loop goes past length |
| 6 | Slow recursion | Performance | Static | Exponential Fibonacci |
| 7 | File not closed | Performance | Static | Resource leak |
| 8 | Command injection | Security | Static | User input in system command |
| 9 | Memory growth | Performance | Dynamic | Cache never cleared |
| 10 | Unreachable code | Correctness | Static | Code after return |
| 11 | Inconsistent state | Correctness | Both | Exception risk |
| 12 | Inefficient loop | Performance | Static | Unnecessary nested loops |
| 13 | XSS | Security | Static | User input in innerHTML |
| 14 | Divide by zero | Correctness | Both | Divisor may be 0 |
| 15 | Return local pointer | Correctness | Static | Local variable returned |

---

## Summary Questions

### How many snippets had Correctness issues? ___
7

### How many had Security issues? __
4
_
### How many had Performance issues? ___
4

### Which issues are best caught by static analysis? Why?
SQL injection patterns
buffer overflow risks
unreachable code
resource leaks
returning pointers to local variables

### Which issues require dynamic analysis? Why?
memory growth over time (Snippet 9)
division by zero depending on input (Snippet 3, 14)
failures due to exceptions during execution (Snippet 11)
