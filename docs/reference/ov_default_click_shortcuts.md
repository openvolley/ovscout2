# Default keyboard shortcuts for ov_scouter

`ov_default_click_shortcuts` apply when `using scout_mode = "click"`,
and `ov_default_type_shortcuts` apply when `using scout_mode = "type"`

## Usage

``` r
ov_default_click_shortcuts()

ov_default_type_shortcuts()

ov_default_playstable_shortcuts()
```

## Value

A named list

## Details

Shortcuts should be defined in terms of the printable representation of
the key (e.g. "a", "\$", "H", "Escape", "Enter"). See
<https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values>
for guidance. Shortcuts can optionally use modifier keys: "Ctrl-x" means
pressing the control key and x simultaneously; similarly Alt-x, Meta-x,
Shift-x. Be aware that some keys are hard-coded for specific
functionality and might cause problems if you use them as shortcuts
(e.g. "Enter", "Tab"), and some keys have browser-level or
operating-system-level handling that cannot be overridden.
