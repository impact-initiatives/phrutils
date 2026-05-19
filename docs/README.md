# phrutils Documentation

This `docs/` folder contains package-level guidance for the **phrutils**
package and its current utility-only scope.

For package overview and installation, see the root
[`README.md`](../README.md).

## Current codebase scope

The package currently provides shared utility modules:

- `utils_errors` — structured error, warning, message, assertion, and safe-try helpers
- `utils_validators` — type/dataframe/schema/value validation and date-time conversion helpers
- `utils_language` — translation/text lookup helper (`phr_txt`)
- `utils_color_palettes` — reusable palette functions and grouped palette accessors

## Function reference

Use standard R help for authoritative API docs:

```r
?phr_error
?phr_validate_columns
?phr_txt
?get_color_palette
```

## Conventions

- [`coding_conventions.md`](coding_conventions.md)
- [`naming_conventions.md`](naming_conventions.md)
