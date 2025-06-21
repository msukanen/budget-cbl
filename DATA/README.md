# `BUDGET.DTA`
Put a `BUDGET.DTA` file here.
## Structure of `BUDGET.DTA`
Each line is as follows:
* 9 bytes numeric value with (or without) a sign: S00000.00
* 1 byte for "mode": `I` (initial starting point), `P` (potential, but not necessarily happening), `M` (is late!).
* 30 bytes for a comment of some sort.
### Example
```DTA
   -14.16I
   595.36 Some pay #1
   299.25 Some pay #2
   -20.22 Burgers
   -57.61 Pizzas
   -30.30MPhone bill
   -30.26 Phone bill
   -57.48 Electricity
   -33.24 Internet
  -605.00 Rent
    -6.00PMisc #1
    -6.00PMisc #2
    36.00 Random pay #1
   -22.99 Google Gemini Pro
    -9.99 Anime
    -9.99 Discord
```
