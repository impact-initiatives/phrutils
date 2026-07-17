#' Check whether an expression is a valid logical expression (static analysis)
#'
#' @description
#' `is_logical_expression()` performs a **purely static (non-evaluating)**
#' inspection of an R expression to determine whether it represents a
#' syntactically valid logical statement.
#'
#' This function is intended for **schema- or configuration-time validation**
#' (e.g. validating user-supplied logical expressions that will later be
#' evaluated against a dataset). It does **not** evaluate the expression and
#' does not require referenced objects to exist.
#'
#' Supported constructs include:
#' \itemize{
#'   \item Logical operators: `&`, `|`, `!`, `&&`, `||`
#'   \item Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`, `%in%`
#'   \item Logical-returning functions: `is.na()`, `grepl()`, `is_safely_coercible()`
#'   \item Base type check functions: `is.numeric()`, `is.character()`, `is.logical()`,
#'         `is.integer()`, `is.double()`, `is.factor()`, `is.list()`, `is.vector()`,
#'         `is.data.frame()`, `is.matrix()`, `is.null()`, `is.atomic()`, `is.recursive()`
#'   \item Symbols (e.g. dataset column names)
#'   \item Literal `TRUE` / `FALSE`
#' }
#'
#' Unsupported constructs (and therefore rejected) include:
#' \itemize{
#'   \item Character or numeric literals as standalone expressions
#'   \item Non-logical functions (e.g. `mean()`, `paste()`)
#'   \item Arbitrary function calls or side-effect expressions
#' }
#'
#' @param expr_chr A single character string containing a logical expression
#'   (typically the text of an expression to be parsed and evaluated).
#'
#' @return Logical scalar:
#' \itemize{
#'   \item `TRUE` if the expression represents a valid logical statement
#'   \item `FALSE` otherwise
#' }
#'
#' @examples
#' \dontrun{
#' is_logical_expression("age > 5 & !is.na(sex)")
#'
#' is_logical_expression("grepl('a', name)")
#'
#' is_logical_expression("x %in% c('a', 'b', 'c')")
#'
#' is_logical_expression("is.numeric(age)")
#'
#' is_logical_expression("'my name is Jack'")
#' }
#'
#' @keywords internal
#' @export
is_logical_expression <- function(expr_chr) {

  # Must be a single character string
  if (!is.character(expr_chr) || length(expr_chr) != 1L || is.na(expr_chr)) {
    return(FALSE)
  }

  # Must parse
  parsed <- try(parse(text = expr_chr), silent = TRUE)
  if (inherits(parsed, "try-error") || length(parsed) != 1L) {
    return(FALSE)
  }

  expr <- parsed[[1]]

  # Allowed logical operators and functions
  logical_ops <- c("&", "|", "!", "&&", "||", "==", "!=", "<", "<=", ">", ">=", "%in%")
  logical_fns <- c(
    "is.na", "grepl",
    "is_safely_coercible",
    # Base R type checking functions
    "is.numeric", "is.character", "is.logical", "is.integer", "is.double",
    "is.factor", "is.list", "is.vector", "is.data.frame", "is.matrix",
    "is.null", "is.atomic", "is.recursive", "is_greater_than_one_selection", "is_greater_than_three_selection"
  )

  # Case 1: direct logical literal
  if (is.logical(expr)) {
    return(TRUE)
  }

  # Case 2: expression contains logical operator
  if (is.call(expr) && as.character(expr[[1]]) %in% logical_ops) {
    return(TRUE)
  }

  # Case 3: expression contains logical-returning function
  if (is.call(expr) && as.character(expr[[1]]) %in% logical_fns) {
    return(TRUE)
  }

  FALSE
}
