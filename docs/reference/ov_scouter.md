# Launch a Shiny app for scouting

Launch a Shiny app for scouting

## Usage

``` r
ov_scouter(
  dvw,
  video_file,
  court_ref,
  season_dir,
  auto_save_dir,
  scout_mode = "click",
  pause_on_type = 500,
  scoreboard = TRUE,
  ball_path = FALSE,
  playlist_display_option = "dv_codes",
  review_pane = TRUE,
  playback_rate = 1,
  scouting_options = ov_scouting_options(),
  app_styling = ov_app_styling(),
  click_shortcuts = ov_default_click_shortcuts(),
  type_shortcuts = ov_default_type_shortcuts(),
  playstable_shortcuts = ov_default_playstable_shortcuts(),
  key_remapping,
  scout_name = "",
  show_courtref = FALSE,
  dash = FALSE,
  host,
  launch_browser = TRUE,
  prompt_for_files = interactive(),
  ...
)
```

## Arguments

- dvw:

  string or datavolley: either the path to a dvw or ovs file or a
  datavolley object (e.g. as returned by [`dv_create()`](dv_create.md).
  Passing the file name (not the datavolley object) is required if any
  extra arguments are passed via `...`. `dvw` can also be an object as
  saved by `ov_scouter()` in ovs format. If `dvw` is "demo", the app
  will be started with a demonstration data set

- video_file:

  string or `FALSE`: optionally, the path to the video file. If `FALSE`
  and `scout_mode` is "type", no video will be used (use this for e.g.
  live scouting). If `video_file` is not supplied or is `NULL`, the
  video file specified in the dvw file will be used. `video_file` can
  also be a URL (including a YouTube URL or video ID)

- court_ref:

  data.frame or string: data.frame with the court reference (as returned
  by
  [`ovideo::ov_shiny_court_ref()`](https://rdrr.io/pkg/ovideo/man/ov_shiny_court_ref.html))
  or the path to the rds file containing the output from this

- season_dir:

  string: optional path to a directory with other dvw/ovs files from
  this season

- auto_save_dir:

  string: optional path to a directory where the dvw will be saved
  automatically after each rally

- scout_mode:

  string: either "click" for the guided point-and-click scouting
  interface, or "type" for the typing-based interface

- pause_on_type:

  numeric: if greater than 0 and using `scout_mode = "type"`, pause the
  video for this many milliseconds after each key press in the scouting
  bar

- scoreboard:

  logical: if `TRUE`, show a scoreboard in the top-right of the video
  pane

- ball_path:

  logical: if `TRUE`, show the ball path on the court inset diagram.
  Note that this will slow the app down slightly

- playlist_display_option:

  string: what to show in the plays table? Either "dv_codes" (scouted
  codes) or "commentary" (a plain-language interpretation of the
  touches)

- review_pane:

  logical: if `TRUE`, entry popups will be accompanied by a small video
  pane that shows a loop of the video of the action in question

- playback_rate:

  numeric: starting playback rate of the video (1.0 is normal speed,
  higher is faster)

- scouting_options:

  list: a named list with entries as per
  [`ov_scouting_options()`](ov_scouting_options.md). See Details, below

- app_styling:

  list: named list of styling options, as returned by
  [`ov_app_styling()`](ov_app_styling.md)

- click_shortcuts:

  list: named list of keyboard shortcuts, as returned by
  [`ov_default_click_shortcuts()`](ov_default_click_shortcuts.md)

- type_shortcuts:

  list: named list of keyboard shortcuts, as returned by
  [`ov_default_type_shortcuts()`](ov_default_click_shortcuts.md)

- playstable_shortcuts:

  list: named list of keyboard shortcuts that apply when in the plays
  table, as returned by
  [`ov_default_playstable_shortcuts()`](ov_default_click_shortcuts.md)

- key_remapping:

  list: a named list of key remappings, with entries as per
  [`ov_default_key_remapping()`](ov_default_key_remapping.md). If not
  provided, it will default to `ov_default_key_remapping(scout_mode)`
  (for whatever `scout_mode` is being used)

- scout_name:

  string: the name of the scout (your name)

- show_courtref:

  logical: if `TRUE`, show the court reference lines overlaid on the
  video

- dash:

  logical: support live MPEG DASH streams? If not specified, will
  default to `TRUE` if `video_file` is a `*.mpd` stream. Note that DASH
  support is fragile at best. HLS streams are automatically supported
  and likely to be more reliable

- host:

  string: the IP address of this machine. Only required if you intend to
  run the app on this machine but connect to it from a different machine
  (in which case use
  `ov_scouter(..., host = "www.xxx.yyy.zzz", launch_browser = FALSE)` on
  the other machine, where www.xxx.yyy.zzz is the IP address of this
  machine, i.e. the machine running the app)

- launch_browser:

  logical: if `TRUE`, launch the app in the system's default web browser
  (passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)'s
  `launch.browser` parameter)

- prompt_for_files:

  logical: if `dvw` was not specified, prompt the user to select the dvw
  file

- ...:

  : extra parameters passed to
  [`datavolley::dv_read()`](https://datavolley.openvolley.org/reference/dv_read.html)
  (if `dvw` is a provided as a string) and/or to the shiny server and UI
  functions

## Details

A note on `scouting_options`

If a \*.ovs file (i.e. a partially-scouted file, that was previously
scouted using this app) has been provided in the `dvw` argument, then it
will contain the scouting options used during the previous scouting
session. Those options will be re-used EXCEPT if `scouting_options` are
also provided here. Any scouting options provided here via the
`scouting_options` argument will override options saved in the .ovs
file. Thus, it is recommended that `scouting_options` not be provided
here along with a .ovs file unless absolutely necessary. If necessary,
only the specific, relevant elements of the `scouting_options` list
should be provided. Note that \*.dvw files do not contain saved options,
only .ovs files that were scouted with this app.

## Examples

``` r
if (FALSE) { # \dontrun{
  ov_scouter("demo")
} # }
```
