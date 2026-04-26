# Enter the team lineups at the start of a set

Enter the team lineups at the start of a set

## Usage

``` r
dv_set_lineups(x, set_number, lineups, setter_positions, setters)
```

## Arguments

- x:

  datavolley: a datavolley object

- set_number:

  integer: set number, 1–3 for beach or 1–5 for indoor

- lineups:

  list: two-element list with numeric vectors of player numbers. Each
  lineup is

  - for indoor, of length 6, 7, or 8 (first 6 are player jersey numbers
    in positions 1–6, elements 7 and 8 are optionally the libero jersey
    numbers)

  - for beach, of length 2

- setter_positions:

  integer: two-element integer vector giving the position on court of
  the two setters. At least one of `setter_positions` or `setters` must
  be provided for indoor. Ignored for beach

- setters:

  integer: two-element integer vector giving the jersey numbers of the
  two setters. At least one of `setter_positions` or `setters` must be
  provided for indoor. Ignored for beach

## Value

A modified version of `x`
