# Coding Conventions in phr

This repository currently contains utility-focused R code. Contributors should
follow these conventions:

## 1) Function design

- Prefer small, reusable functions in `R/utils_*.R` files.
- Keep exported APIs stable and documented with roxygen2.
- Keep internal helpers unexported (leading `.` allowed).

## 2) Error handling

- Use `phr_error()`, `phr_warning()`, and `phr_message()` for standardized user-facing signaling.
- Use `phr_try()` / `phr_try_step()` for structured recovery flows.
- Include meaningful `origin`/`step` text when available.

## 3) Validation behavior

- Validation helpers should support strict mode (`soft = FALSE`) and soft mode (`soft = TRUE`) consistently.
- Prefer clear, actionable error/warning messages.

## 4) Naming conventions (summary)

- Functions: `snake_case`
- Exported package utilities: `phr_*` prefix when package-scoped
- Internal helpers: optional leading dot (`.helper_name`)
- Files: `snake_case` (e.g., `utils_validators.R`)
- Tests: `test-` prefix aligned to module names (e.g., `test-utils_errors.R`)

## 5) Documentation and tests

- Add roxygen2 docs for exported functions (`@description`, `@param`, `@return`, `@export`).
- Add/maintain `testthat` coverage for behavior changes.
- Keep docs aligned with the actual implemented utility modules.
