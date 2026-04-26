# Create a new datavolley match object

Create a new datavolley match object

## Usage

``` r
dv_create(
  match,
  more,
  teams,
  players_h,
  players_v,
  video_file,
  attacks = ov_simplified_attack_table(),
  setter_calls = ov_default_setter_calls_table(),
  winning_symbols = ov_default_winning_symbols(),
  zones_or_cones = "Z",
  regulation = "indoor rally point",
  comments
)
```

## Arguments

- match:

  list or single-row data.frame: (optional) with components `date`
  (defaults to current date), `time` (defaults to current time),
  `season`, `league`, `phase`, `home_away`, `day_number`,
  `match_number`, `regulation`, `zones_or_cones`. `zones_or_cones` can
  also be provided directly

- more:

  list or single-row data.frame: (optional) with components `referees`,
  `spectators`, `receipts`, `city`, `arena`, `scout`

- teams:

  data.frame: a 2-row data frame describing the home and visiting teams,
  with required columns `team_id`, `team` and optional columns `coach`,
  `assistant`, `shirt_colour`. The home team must be in the first row of
  this data frame

- players_h, players_v:

  data.frame: with required columns `number`, `firstname`, `lastname`,
  and optional columns `player_id`, `role` (character vector with
  "outside", "opposite", "middle", "libero", "setter"), `nickname`,
  `special_role` (character vector with "L", "C", or NA), `foreign`
  (logical, defaults to `FALSE`)

- video_file:

  string: (optional) path to video file

- attacks:

  data.frame: as returned by
  [`ov_simplified_attack_table()`](ov_default_attack_table.md) or
  [`ov_default_attack_table()`](ov_default_attack_table.md)

- setter_calls:

  data.frame: as returned by
  [`ov_default_setter_calls_table()`](ov_default_setter_calls_table.md)

- winning_symbols:

  data.frame: as returned by
  [`ov_default_winning_symbols()`](ov_default_winning_symbols.md)

- zones_or_cones:

  string: "Z" or "C". Will be ignored if `zones_or_cones` is provided in
  the `match` parameter

- regulation:

  string: "indoor rally point", "beach rally point", or "indoor
  sideout". Will be ignored if `regulation` is provided in the `match`
  parameter

- comments:

  character: optional vector of length up to 5, of comments

## Value

A datavolley object

## Examples

``` r
x <- dv_create(teams = data.frame(team_id = c("TM1", "TM2"), team = c("Team 1", "Team 2")),
               comments = "Test file",
               players_h = data.frame(firstname = toupper(letters[1:7]), lastname = "Player",
                                      number = 1:7),
               players_v = data.frame(firstname = letters[10:15], lastname = "VisPlayer",
                                      number = 10:15))

## enter the team lineups for set 1
x <- dv_set_lineups(x, set_number = 1, lineups = list(6:1, 15:10), setter_positions = c(2, 1))
```
