# phr_utils — Core Public Health Utilities

**phr_utils** is the repository for the **phr** R package, focused on shared
utility layers used across public health workflows:

- `utils_errors` — standardized error, warning, message, and safe-try wrappers
- `utils_validators` — type, schema, and value validation helpers
- `utils_language` — lightweight translation/text lookup helper
- `utils_color_palettes` — reusable visualization color palettes

## Installation

The package is not yet on CRAN. Install the development version directly from
GitHub using the [remotes](https://remotes.r-lib.org/) package:

```r
# install.packages("remotes")
remotes::install_github("SaeedR1987/phr_utils")
```

Then load the package:

```r
library(phr)
```

### System requirements

- R ≥ 4.1.0
- The package imports several CRAN packages (see `DESCRIPTION` for the full list). These are installed automatically by `remotes::install_github()`.

## Quick start

```r
library(phr)

# Validation
phr_validate_columns(df, c("id", "value"), soft = FALSE)

# Error handling
phr_try({
  phr_assert(nrow(df) > 0, "Input data is empty")
}, on_error = "abort")

# Colors
palette <- get_color_palette("water", n = 5)
```

## Documentation

Function documentation is available in-package (for example `?phr_error`,
`?phr_validate_numeric`, `?get_color_palette`).

## License

MIT — see [LICENSE.md](LICENSE.md).
