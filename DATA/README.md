# `BUDGET.CSV`
Put a `BUDGET.CSV` file here.
## Structure of `BUDGET.CSV`
* Line 1: begins with `#`, followed by your initial budget, e.g. `#599.99`.
* Lines 2..1000: added/subtracted value, e.g. `12.34`, `-52.25`, `+77.77`.
  Values can have an associated comment, separated from the value with `;`.

Empty lines are quietly ignored and so is all leading/trailing spacing.

### Example
```CSV
#543.21
-12.34; oh no, I lost a few...
101.01; yay, gains!
-432;   bye money...
+66.6;  deal with the devil...
```
