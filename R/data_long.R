#' Pivot data from wide to long
#'
#' `data_long()` "lengthens" data, increasing the number of rows and decreasing the number of columns. The inverse
#' transformation is [pivot_wider()].
#'
#' @param data `data.frame`. The data to pivot.
#' @param cols <[`poor-select`][select_helpers]>. Columns to pivot into longer format.
#' @param values_to `character(n)`. The name of the new column(s) that will contain the values of the pivoted variables.
#' @param ... Additional arguments passed on to methods.
#'
#' @return A `data.frame`.
#'
#' @examples
#' wide_data <- data.frame(replicate(5, rnorm(10)))
#' # Customizing the names
#' data_long(
#'   data = wide_data,
#'   cols = c(1, 2),
#'   names_to = "Column",
#'   values_to = "Numbers"
#' )
#'
#' @export
data_long <- function(data, cols = "group", names_to = "name", names_prefix = NULL,
    names_sep = NULL, names_pattern = NULL, values_to = "value",
    values_drop_na = FALSE, ...) {
  if (missing(cols)) stop("`cols` must select at least one column.")
  if (length(cols) == 1 && cols == "group") {
    cols <- colnames(data)[!"group" == colnames(data)]
  }
  cols <- names(eval_select_pos(data, substitute(cols)))
  if (any(names_to %in% setdiff(names(data), cols))) {
    stop(
      paste0(
        "Some values of the columns specified in 'names_to' are already present
        as column names. Either use another value in `names_to` or rename the
        following columns: ",
        paste(names_to[which(names_to %in% setdiff(names(data), cols))], sep = ", ")
      ),
      call. = FALSE)
  }
  if (length(cols) == 0L) stop("No columns found for reshaping data.", call. = FALSE)
  # Create Index column as needed by reshape
  data[["_Row"]] <- as.numeric(rownames(data))
  # Create a new index for cases with length(names_to) > 1
  names_to_2 <- paste(names_to, collapse = "_")
  # Reshape
  long <- stats::reshape(data, varying = cols, idvar = "_Row", v.names = values_to,
    timevar = names_to_2, direction = "long")
  # Sort the dataframe (to match pivot_longer's output)
  long <- long[do.call(order, long[, c("_Row", names_to_2)]), ]
  long[["_Row"]] <- NULL
  # Re-insert col names as levels
  long[[names_to_2]] <- cols[long[[names_to_2]]]
  # reorder
  long <- relocate(.data = long, values_to, .after = -1)
  # Reset row names
  rownames(long) <- NULL
  # Remove reshape attributes
  attributes(long)$reshapeLong <- NULL
  long
}

#' Get integer column positions
#'
#' Within the context of a selection function (`select`, `relocate`, `rename`), return the integer position of the
#' columns selected.
#'
#' @details
#' This function accepts
#'
#' * `integer()`s
#' * `numeric()`s
#' * `character()`s
#' * `symbol()`s
#' * `call()`s
#'
#' Each type is handled separately.
#' @param group_pos `logical(1)`. Should grouping variable positions be returned (default: `FALSE`)?
#'
#' @return
#' A vector of named positive `integer`s.
#'
#' @examples
#' select_positions(mtcars, mpg)
#' select_positions(mtcars, "mpg")
#' select_positions(mtcars, starts_with("m"))
#' select_positions(mtcars, -mpg)
#' select_positions(mtcars, mpg:drat)
#'
#' @noRd
select_positions <- function(.data, ..., .group_pos = FALSE) {
  cols <- dotdotdot(...)
  cols <- cols[!vapply(cols, is.null, FALSE)]
  if (length(cols) == 0L) return(integer(0))
  select_env$setup(.data = .data, calling_frame = parent.frame(2L))
  on.exit(select_env$clean(), add = TRUE)
  data_names <- select_env$get_colnames()
  pos <- unlist(lapply(cols, eval_expr))
  if (length(pos) > 0) pos <- if (pos[1] >= 0) pos[pos >= 0] else pos[pos < 0]
  col_len <- select_env$get_ncol()
  if (any(pos > col_len)) {
    oor <- pos[which(pos > col_len)]
    oor_len <- length(oor)
    stop(
      "Location", if (oor_len > 1) "s " else " ", collapse_to_sentence(oor),
      if (oor_len > 1) " don't " else " doesn't ", "exist. There are only ", col_len, " columns."
    )
  }
  if (isTRUE(.group_pos)) {
    groups <- group_vars(.data)
    missing_groups <- !(groups %in% cols)
    if (any(missing_groups)) {
      sel_missing <- groups[missing_groups]
      readd <- match(sel_missing, data_names)
      readd <- readd[!(readd %in% pos)]
      if (length(readd) > 0L) {
        message("Adding missing grouping variables: `", paste(sel_missing, collapse = "`, `"), "`")
        if (length(names(cols)) > 0L) names(readd) <- data_names[readd]
        pos <- c(readd, pos)
      }
    }
  }
  if (length(data_names[pos]) != 0L) {
    nm_pos <- names(pos)
    if (any(nm_pos == "")) {
      names(pos)[which(nm_pos == "")] <- data_names[pos[which(nm_pos == "")]]
    }
    if (is.null(nm_pos)) {
      names(pos) <- data_names[abs(pos)]
    }
  }
  uniques <- pos[!duplicated(pos)]
  res_nms <- data_names[uniques]
  res <- match(res_nms, data_names)
  if (length(res) != 0L) {
    res <- if (length(setdiff(names(uniques), data_names)) > 0L) {
      if (all(uniques > 0L)) structure(res, .Names = names(uniques)) else structure(res, .Names = res_nms)
    } else {
      structure(res, .Names = res_nms)
    }
  }
  res
}

eval_expr <- function(x) {
  type <- typeof(x)
  switch(
    type,
    "integer" = x,
    "double" = as.integer(x),
    "character" = select_char(x),
    "symbol" = select_symbol(x),
    "language" = eval_call(x),
    stop("Expressions of type <", typeof(x), "> cannot be evaluated for use when subsetting.")
  )
}

select_char <- function(expr) {
  pos <- match(expr, select_env$get_colnames())
  if (any(is.na(pos))) stop("The following columns do not exist:\n    ", paste(expr, collapse = "\n    "))
  pos
}

select_symbol <- function(expr) {
  expr_name <- as.character(expr)
  if (grepl("^is\\.", expr_name) && is.function(expr)) {
    stop(
      "Predicate functions must be wrapped in `where()`.\n\n",
      sprintf("  data %%>%% select(where(%s))", expr_name)
    )
  }
  res <- try(select_char(as.character(expr)), silent = TRUE)
  if (inherits(res, "try-error")) {
    res <- tryCatch(
      unlist(lapply(eval(expr, envir = select_env$calling_frame), eval_expr)),
      error = function(e) stop("Column ", expr, " does not exist.")
    )
  }
  res
}

eval_call <- function(x) {
  type <- as.character(x[[1]])
  if (length(type) > 1L) {
    # This helps when pkg::fn is used in a select helper
    type <- "context"
  }
  switch(
    type,
    `:` = select_seq(x),
    `!` = select_negate(x),
    `-` = select_minus(x),
    `c` = select_c(x),
    `(` = select_bracket(x),
    `&` = select_and(x),
    select_context(x)
  )
}

select_and <- function(expr) {
  exprs <- as.list(expr)[-1]
  res <- do.call(c, lapply(exprs, eval_expr))
  if (all(res > 0) || all(res < 0)) return(unique(res))
  res <- res[!(duplicated(abs(res)) | duplicated(abs(res), fromLast = TRUE))]
  res[res > 0]
}

select_seq <- function(expr) {
  x <- eval_expr(expr[[2]])
  y <- eval_expr(expr[[3]])
  x:y
}

select_negate <- function(expr) {
  x <- if (is_negated_colon(expr)) {
    expr <- call(":", expr[[2]][[2]], expr[[2]][[3]][[2]])
    eval_expr(expr)
  } else {
    eval_expr(expr[[2]])
  }
  x * -1L
}

is_negated_colon <- function(expr) {
  expr[[1]] == "!" && length(expr[[2]]) > 1L && expr[[2]][[1]] == ":" && expr[[2]][[3]][[1]] == "!"
}

select_minus <- function(expr) {
  x <- eval_expr(expr[[2]])
  x * -1L
}

select_c <- function(expr) {
  lst_expr <- as.list(expr)
  lst_expr[[1]] <- NULL
  unlist(lapply(lst_expr, eval_expr))
}

select_bracket <- function(expr) {
  eval_expr(expr[[2]])
}

select_context <- function(expr) {
  eval(expr, envir = select_env$.data)
}

# -- Environment ---------------------------------------------------------------

select_env <- new.env()
select_env$setup <- function(.data, calling_frame) {
  select_env$.data <- .data
  select_env$calling_frame <- calling_frame
}
select_env$clean <- function() {
  rm(list = c(".data", "calling_frame"), envir = select_env)
}
select_env$get_colnames <- function() colnames(select_env$.data)
select_env$get_nrow <- function() nrow(select_env$.data)
select_env$get_ncol <- function() ncol(select_env$.data)

# -- Helpers -------------------------------------------------------------------

#' A cleaner interface to evaluating select_positions when column names are not passed via ...
#' @noRd
eval_select_pos <- function(.data, .cols, .group_pos = FALSE) {
  do.call(select_positions, list(.data = .data, .cols, .group_pos = .group_pos))
}

#' Capture unevaluated dots
#' Gather the unevaluated dots into a list, storing them as is.
#' @param ... Arguments to be stored in the list.
#' @param .impute_names `logical(1)`. Whether to fill any missing names of the list.
#' @noRd
dotdotdot <- function(..., .impute_names = FALSE) {
  dots <- eval(substitute(alist(...)))
  if (isTRUE(.impute_names)) {
    deparse_dots <- lapply(dots, deparse)
    names_dots <- names(dots)
    unnamed <- if (is.null(names_dots)) rep(TRUE, length(dots)) else nchar(names_dots) == 0L
    names(dots)[unnamed] <- deparse_dots[unnamed]
  }
  dots
}

#' Capture unevaluated dots
#' Gather the unevaluated dots into a list, storing them as characters.
#' @param ... Arguments to be stored in the list.
#' @noRd
deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}

# relocate ----
relocate <- function(.data, ..., .before = NULL, .after = NULL) {
  UseMethod("relocate")
}

relocate.data.frame <- function(.data, ..., .before = NULL, .after = NULL) {
  data_names <- colnames(.data)
  col_pos <- select_positions(.data, ...)

  if (!missing(.before)) .before <- colnames(.data)[eval_select_pos(.data, substitute(.before))]
  if (!missing(.after)) .after <- colnames(.data)[eval_select_pos(.data, substitute(.after))]

  has_before <- !is.null(.before)
  has_after <- !is.null(.after)

  if (has_before && has_after) {
    stop("You must supply only one of `.before` and `.after`")
  } else if (has_before) {
    where <- min(match(.before, data_names))
    col_pos <- c(setdiff(col_pos, where), where)
  } else if (has_after) {
    where <- max(match(.after, data_names))
    col_pos <- c(where, setdiff(col_pos, where))
  } else {
    where <- 1L
    col_pos <- union(col_pos, where)
  }
  lhs <- setdiff(seq(1L, where - 1L), col_pos)
  rhs <- setdiff(seq(where + 1L, ncol(.data)), col_pos)
  col_pos <- unique(c(lhs, col_pos, rhs))
  col_pos <- col_pos[col_pos <= length(data_names)]

  res <- .data[col_pos]
  if (has_groups(.data)) res <- groups_set(res, group_vars(.data))
  res
}

has_groups <- function (x) {
  groups <- group_vars(x)
  if (length(groups) == 0L)
    FALSE
  else TRUE
}

group_vars <- function (x) {
  groups <- attr(x, "groups", exact = TRUE)
  if (is.null(groups))
    character(0)
  else colnames(groups)[!colnames(groups) %in% c(".group_id", ".rows")]
}
