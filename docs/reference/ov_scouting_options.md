# Scouting options

Scouting options

## Usage

``` r
ov_scouting_options(
  end_convention = "actual",
  nblockers = TRUE,
  default_nblockers = NA,
  transition_sets = FALSE,
  set_quality = FALSE,
  attacks_by = "codes",
  zones_cones = "Z",
  team_system = "SHM3",
  setter_dump_code = "PP",
  second_ball_attack_code = "P2",
  overpass_attack_code = "PR",
  default_scouting_table = ov_default_scouting_table(),
  compound_table = ov_default_compound_table(),
  attack_table = ov_simplified_attack_table(),
  setter_calls = "none",
  setter_calls_table = ov_default_setter_calls_table()
)
```

## Arguments

- end_convention:

  string: either "actual" or "intended". Is the end coordinate of an
  attack or serve the actual end location (where the ball contacted the
  floor or out of bounds area), or the intended one. The actual might
  differ from the intended if there is a block touch or the ball hit the
  net. If "actual", and a block touch is recorded, then the end location
  of the attack will not be used for the dig location (the dig location
  will be missing)

- nblockers:

  logical: scout the number of blockers on each attack?

- default_nblockers:

  integer: if `nblockers` is TRUE, what number of blockers should we
  default to? If `NA`, no default. Having a default value might speed up
  the data entry process very slightly but it is not recommended because
  it's easy to forget to update it on each attack and then your data
  will include default-valued entries that you can't easily distinguish
  from genuine ones that you actually entered

- transition_sets:

  logical: scout sets in transition? If `FALSE`, just the endpoint of
  each attack (i.e. the dig) and the subsequent counter-attack are
  scouted

- set_quality:

  logical: are we assessing set quality? If `FALSE` sets will be
  classified as "error", "overpass set", or "positive" for all other
  sets

- attacks_by:

  string: "codes" (X5, V5, etc) or "tempo" (high, medium, quick)

- zones_cones:

  string: record attack directions as "Z"ones or "C"ones (ignored when
  scouting in 'click' mode, which always uses zones. But after
  click-scouting you can export your dvw with attack directions as
  cones, if you wish)

- team_system:

  string: the assumed system that teams are using to assign e.g. passing
  and hitting responsibilities

  - "SHM3" - a setter-hitter-middle rotation, with 3 passers (the libero
    and two outside hitters)

- setter_dump_code:

  string: the attack combination code for a setter dump

- second_ball_attack_code:

  string: the attack combination code for a second-ball attack

- overpass_attack_code:

  string: the attack combination code for an attack on an overpass

- default_scouting_table:

  tibble: the table of scouting defaults (skill type and evaluation)

- compound_table:

  tibble: the table of compound codes

- attack_table:

  tibble: table of attack codes (X5, V5, etc) as returned by
  [`ov_default_attack_table()`](ov_default_attack_table.md) or
  [`ov_simplified_attack_table()`](ov_default_attack_table.md)

- setter_calls:

  string: either "none", "reception" (setter calls in reception phase
  only), or "both" (setter calls in reception and transition)

- setter_calls_table:

  tibble: table of setter calls (where the setter instructs the middle
  to run) as returned by
  [`ov_default_setter_calls_table()`](ov_default_setter_calls_table.md)

## Value

A named list
