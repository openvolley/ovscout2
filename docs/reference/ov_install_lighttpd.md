# Install lighttpd

This is a helper function to install lighttpd. Currently it only works
on Windows platforms. The lighttpd bundle will be downloaded from
<http://lighttpd.dtech.hu/> and saved to your user appdata directory.

## Usage

``` r
ov_install_lighttpd(force = FALSE)
```

## Arguments

- force:

  logical: force reinstallation if lighttpd already exists

## Value

the path to the installed executable

## References

<http://lighttpd.dtech.hu/>

## Examples

``` r
if (FALSE) { # \dontrun{
  ov_install_lighttpd()
} # }
```
