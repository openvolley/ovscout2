# Enter scout codes from the console

Probably only useful for testing.

## Usage

``` r
dv_scout_from_console(
  x,
  prompt = "SCOUT> ",
  compound_table = ov_default_compound_table(),
  default_scouting_table = ov_default_scouting_table()
)
```

## Arguments

- x:

  datavolley: a datavolley object as returned by
  [`dv_create()`](dv_create.md)

- prompt:

  string: the prompt to show

- compound_table:

  tibble: the table of default compound codes

- default_scouting_table:

  tibble: the table of scouting defaults (skill type and evaluation)

## Value

A modified version of `x`, with rows added to the plays2 component
