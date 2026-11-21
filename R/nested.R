#' Split and recombine a tibble with a nested column
#'
#' @description
#' `split_nested()` splits a tibble with a column of nested tibbles into a
#' parent table and a detail table that can be stored separately,
#' e.g. as two tables in a data warehouse.
#' `recombine_nested()` restores the original tibble from the two tables.
#'
#' Each cell of the nested column must be a tibble with a schema shared by
#' all cells, a zero-row tibble, or `NULL`.
#'
#' @section Split layout:
#' The parent table stores all remaining columns and, in the position of the
#' nested column, three metadata columns:
#'
#' - `.parent_id`: a surrogate key that locates each parent row independently,
#'   even if all business columns are duplicated.
#' - `.detail_status`: `"rows"` for a detail with at least one row,
#'   `"empty"` for a zero-row tibble, `"null"` for `NULL`.
#' - `.detail_count`: the number of detail rows.
#'
#' The detail table stores the columns of the nested tibbles, plus:
#'
#' - `.parent_id`: the parent row each detail row belongs to.
#' - `.row_no`: the row number within its parent, used to restore the
#'   original order even if the detail table is read back in random order.
#'
#' An all-empty batch (all cells `NULL` or zero-row tibbles) still yields a
#' fully typed zero-row detail table, generated from the detail schema.
#'
#' @section Recombination:
#' `recombine_nested()` restores the nested column at its original position,
#' with the original column types, duplicate details, and the distinction
#' between zero-row and `NULL` cells intact.
#' Parent rows appear in the order of the parent table,
#' detail rows are ordered by `.row_no` within each parent.
#'
#' Recombination is refused, naming the offending keys or row numbers,
#' if a detail row references an unknown parent key,
#' a row number is duplicated within a parent,
#' expected rows are missing,
#' or a row number is out of bounds.
#'
#' @param x A tibble with a column of nested tibbles.
#' @param col The name of the nested column, as a string or symbol.
#'   For `recombine_nested()`, the name of the restored nested column.
#'   Can be omitted if `parent` is the result of `split_nested()`.
#' @param ptype An optional template (a tibble, ideally zero-row) that defines
#'   the detail schema. Required if all cells of the nested column are `NULL`.
#' @param parent A parent table created by `split_nested()`,
#'   or the split object itself.
#' @param detail A detail table created by `split_nested()`.
#'   Can be omitted if `parent` is the split object.
#'
#' @return `split_nested()` returns a list of class `"tibble_nested_split"`
#'   with components `parent` and `detail`.
#'   `recombine_nested()` returns a tibble with the nested column restored.
#' @export
#'
#' @examples
#' orders <- tibble(
#'   id = 1:3,
#'   details = list(
#'     tibble(item = c("a", "b"), qty = 1:2),
#'     tibble(item = character(), qty = integer()),
#'     NULL
#'   )
#' )
#'
#' split <- split_nested(orders, details)
#' split$parent
#' split$detail
#'
#' # Recombination restores the original tibble,
#' # even if the detail table is read back in a different order
#' recombine_nested(split$parent, split$detail[2:1, ], col = "details")
split_nested <- function(x, col, ptype = NULL) {
  if (!is.data.frame(x)) {
    abort_nested_must_be_data_frame("x")
  }

  col_name <- as_name(ensym(col))

  if (!has_name(x, col_name)) {
    abort_nested_unknown_column(col_name)
  }

  cells <- x[[col_name]]
  if (!is.list(cells) || is.data.frame(cells)) {
    abort_nested_column_must_be_list(col_name)
  }

  is_null_cell <- map_lgl(cells, is.null)
  is_df_cell <- map_lgl(cells, is.data.frame)
  bad_cell <- which(!is_null_cell & !is_df_cell)
  if (has_length(bad_cell)) {
    abort_nested_cell_must_be_data_frame(col_name, bad_cell)
  }

  data_idx <- which(!is_null_cell)

  if (!is.null(ptype)) {
    if (!is.data.frame(ptype)) {
      abort_nested_ptype_must_be_data_frame()
    }
    template_names <- names(ptype)
  } else {
    if (!has_length(data_idx)) {
      abort_nested_schema_unknown(col_name)
    }
    template_names <- names(cells[[data_idx[[1]]]])
  }

  reserved <- intersect(template_names, nested_detail_meta)
  if (has_length(reserved)) {
    abort_nested_schema_reserved(reserved)
  }

  # All cells must provide exactly the template's columns,
  # a different column order is repaired by the cast below
  for (i in data_idx) {
    cell_names <- names(cells[[i]])
    missing <- setdiff(template_names, cell_names)
    extra <- setdiff(cell_names, template_names)
    if (has_length(missing) || has_length(extra)) {
      abort_nested_cell_schema_mismatch(col_name, i, missing, extra)
    }
  }

  if (!is.null(ptype)) {
    schema <- vec_ptype(as_tibble(ptype))
  } else {
    schema <- as_tibble(vec_ptype_common(!!!cells[data_idx]))
  }

  cells[data_idx] <- map(cells[data_idx], vec_cast, to = schema)

  sizes <- map_int(cells, function(cell) {
    if (is.null(cell)) 0L else vec_size(cell)
  })
  status <- rep("rows", length(cells))
  status[sizes == 0L] <- "empty"
  status[is_null_cell] <- "null"

  clash <- intersect(setdiff(names(x), col_name), nested_parent_meta)
  if (has_length(clash)) {
    abort_nested_parent_reserved(clash)
  }

  # The metadata columns take the place of the nested column
  meta <- list(seq_len(nrow(x)), status, sizes)
  names(meta) <- nested_parent_meta
  pos <- match(col_name, names(x))
  parent <- new_tibble(
    append(unclass(x)[-pos], meta, after = pos - 1L),
    nrow = nrow(x)
  )

  has_rows <- which(status == "rows")
  if (has_length(has_rows)) {
    detail <- vec_rbind(!!!map2(cells[has_rows], has_rows, nested_detail_piece))
  } else {
    # An all-empty batch still yields a fully typed detail table
    detail_meta <- list(integer(), integer())
    names(detail_meta) <- nested_detail_meta
    detail <- new_tibble(c(detail_meta, unclass(schema)), nrow = 0L)
  }

  structure(
    list(parent = parent, detail = detail),
    class = "tibble_nested_split",
    col = col_name,
    ptype = schema
  )
}

#' @rdname split_nested
#' @export
recombine_nested <- function(parent, detail = NULL, col = NULL) {
  ptype <- NULL
  if (inherits(parent, "tibble_nested_split")) {
    split <- parent
    parent <- split$parent
    if (is.null(detail)) {
      detail <- split$detail
    }
    if (is.null(col)) {
      col <- attr(split, "col")
    }
    ptype <- attr(split, "ptype")
  }

  if (is.null(col)) {
    abort_recombine_nested_need_col()
  }
  col_name <- as_name(ensym(col))

  if (!is.data.frame(parent)) {
    abort_nested_must_be_data_frame("parent")
  }
  if (is.null(detail)) {
    abort_recombine_nested_need_detail()
  }
  if (!is.data.frame(detail)) {
    abort_nested_must_be_data_frame("detail")
  }

  missing_meta <- setdiff(nested_parent_meta, names(parent))
  if (has_length(missing_meta)) {
    abort_recombine_nested_missing_meta("parent", missing_meta)
  }

  keys <- parent[[nested_key_col]]
  if (!is.numeric(keys) || anyNA(keys) || !is_integerish(keys)) {
    abort_recombine_nested_bad_key()
  }
  keys <- as.integer(keys)

  dup_keys <- unique(keys[duplicated(keys)])
  if (has_length(dup_keys)) {
    abort_recombine_nested_key_duplicated(dup_keys)
  }

  status <- parent[[nested_status_col]]
  if (
    !is.character(status) ||
      anyNA(status) ||
      any(!(status %in% nested_status_values))
  ) {
    bad <- if (is.character(status)) {
      which(is.na(status) | !(status %in% nested_status_values))
    } else {
      seq_along(keys)
    }
    abort_recombine_nested_bad_status(keys[bad])
  }

  count <- parent[[nested_count_col]]
  if (
    !is.numeric(count) ||
      anyNA(count) ||
      !is_integerish(count) ||
      any(count < 0)
  ) {
    bad <- if (is.numeric(count) && !anyNA(count) && is_integerish(count)) {
      which(count < 0)
    } else {
      seq_along(keys)
    }
    abort_recombine_nested_bad_count(keys[bad])
  }
  count <- as.integer(count)

  mismatch <- (status == "rows" & count == 0L) |
    (status != "rows" & count != 0L)
  if (any(mismatch)) {
    abort_recombine_nested_status_count(
      keys[mismatch],
      status[mismatch],
      count[mismatch]
    )
  }

  missing_meta <- setdiff(nested_detail_meta, names(detail))
  if (has_length(missing_meta)) {
    abort_recombine_nested_missing_meta("detail", missing_meta)
  }

  dkey <- detail[[nested_key_col]]
  if (!is.numeric(dkey) || anyNA(dkey) || !is_integerish(dkey)) {
    abort_recombine_nested_bad_detail_key()
  }
  dkey <- as.integer(dkey)

  row_no <- detail[[nested_row_no_col]]
  if (!is.numeric(row_no) || anyNA(row_no) || !is_integerish(row_no)) {
    abort_recombine_nested_bad_row_no()
  }
  row_no <- as.integer(row_no)

  schema_names <- setdiff(names(detail), nested_detail_meta)
  if (!is.null(ptype)) {
    missing_cols <- setdiff(names(ptype), schema_names)
    extra_cols <- setdiff(schema_names, names(ptype))
    if (has_length(missing_cols) || has_length(extra_cols)) {
      abort_recombine_nested_schema_mismatch(missing_cols, extra_cols)
    }
    schema <- ptype
  } else {
    schema <- vec_ptype(as_tibble(detail)[schema_names])
  }

  detail <- as_tibble(detail)
  detail[names(schema)] <- vec_cast(detail[names(schema)], schema)

  # Reconcile the detail rows with the parent keys
  pos <- match(dkey, keys)
  orphan <- unique(dkey[is.na(pos)])
  if (has_length(orphan)) {
    abort_recombine_nested_orphan_key(orphan)
  }

  dup <- duplicated(data.frame(pos, row_no))
  if (any(dup)) {
    abort_recombine_nested_row_no_duplicated(dkey[dup], row_no[dup])
  }

  oob <- row_no < 1L | row_no > count[pos]
  if (any(oob)) {
    abort_recombine_nested_row_no_out_of_bounds(dkey[oob], row_no[oob])
  }

  row_no_split <- split(row_no, pos)
  missing_keys <- integer()
  missing_row_nos <- list()
  for (i in which(count > 0L)) {
    actual <- row_no_split[[as.character(i)]]
    missing <- if (is.null(actual)) {
      seq_len(count[[i]])
    } else {
      setdiff(seq_len(count[[i]]), actual)
    }
    if (has_length(missing)) {
      missing_keys <- c(missing_keys, keys[[i]])
      missing_row_nos <- c(missing_row_nos, list(missing))
    }
  }
  if (has_length(missing_keys)) {
    abort_recombine_nested_row_missing(missing_keys, missing_row_nos)
  }

  # Rebuild the nested column, restoring the within-parent row order
  ord <- order(pos, row_no)
  groups <- split(seq_along(ord), pos[ord])
  detail_data <- vec_slice(detail[names(schema)], ord)

  cells <- vector("list", nrow(parent))
  cells[status == "null"] <- list(NULL)
  cells[status == "empty"] <- rep(list(schema), sum(status == "empty"))
  for (i in which(status == "rows")) {
    cells[[i]] <- vec_slice(detail_data, groups[[as.character(i)]])
  }

  # Restore the nested column at the position of the metadata columns
  if (col_name %in% setdiff(names(parent), nested_parent_meta)) {
    abort_recombine_nested_col_clash(col_name)
  }
  key_pos <- match(nested_key_col, names(parent))
  before <- names(parent)[seq_len(key_pos - 1L)]
  n_before <- sum(!(before %in% nested_parent_meta))
  rest <- unclass(parent)[setdiff(names(parent), nested_parent_meta)]
  out <- append(rest, set_names(list(cells), col_name), after = n_before)
  new_tibble(out, nrow = nrow(parent))
}

# Metadata columns added by split_nested() and consumed by recombine_nested()
nested_key_col <- ".parent_id"
nested_status_col <- ".detail_status"
nested_count_col <- ".detail_count"
nested_row_no_col <- ".row_no"

nested_parent_meta <- c(nested_key_col, nested_status_col, nested_count_col)
nested_detail_meta <- c(nested_key_col, nested_row_no_col)

nested_status_values <- c("rows", "empty", "null")

nested_detail_piece <- function(cell, id) {
  n <- vec_size(cell)
  meta <- list(rep.int(id, n), seq_len(n))
  names(meta) <- nested_detail_meta
  new_tibble(c(meta, unclass(cell)), nrow = n)
}

# Errors ------------------------------------------------------------------

abort_nested_must_be_data_frame <- function(arg, call = caller_env()) {
  tibble_abort(call = call, paste0("`", arg, "` must be a data frame."))
}

abort_nested_unknown_column <- function(col_name, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("Can't find column ", tick(col_name), " in `x`.")
  )
}

abort_nested_column_must_be_list <- function(col_name, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Column ",
      tick(col_name),
      " must be a list column of nested tibbles."
    )
  )
}

abort_nested_cell_must_be_data_frame <- function(
  col_name,
  pos,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    problems(
      paste0(
        "Each cell of column ",
        tick(col_name),
        " must be a tibble or `NULL`:"
      ),
      paste0("Row ", pos)
    ),
    pos = pos
  )
}

abort_nested_ptype_must_be_data_frame <- function(call = caller_env()) {
  tibble_abort(call = call, "`ptype` must be a data frame.")
}

abort_nested_schema_unknown <- function(col_name, call = caller_env()) {
  tibble_abort(
    call = call,
    bullets(
      paste0(
        "Can't determine the detail schema of column ",
        tick(col_name),
        " because all cells are `NULL`:"
      ),
      i = "Supply a template via the `ptype` argument."
    )
  )
}

abort_nested_schema_reserved <- function(cols, call = caller_env()) {
  tibble_abort(
    call = call,
    pluralise_commas(
      "The detail schema uses reserved column name(s) ",
      tick(cols),
      "."
    )
  )
}

abort_nested_cell_schema_mismatch <- function(
  col_name,
  pos,
  missing,
  extra,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    bullets(
      paste0(
        "Cell ",
        pos,
        " of column ",
        tick(col_name),
        " doesn't match the detail schema:"
      ),
      x = if (has_length(missing)) {
        pluralise_commas("Missing column(s) ", tick(missing))
      },
      x = if (has_length(extra)) {
        pluralise_commas("Unexpected column(s) ", tick(extra))
      }
    ),
    pos = pos
  )
}

abort_nested_parent_reserved <- function(cols, call = caller_env()) {
  tibble_abort(
    call = call,
    pluralise_commas(
      "`x` already contains reserved column name(s) ",
      tick(cols),
      "."
    )
  )
}

abort_recombine_nested_need_col <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    bullets(
      "Can't determine the name of the nested column:",
      i = "Supply `col`, or pass the object returned by `split_nested()`."
    )
  )
}

abort_recombine_nested_need_detail <- function(call = caller_env()) {
  tibble_abort(call = call, "Must supply a `detail` table.")
}

abort_recombine_nested_missing_meta <- function(
  which,
  cols,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    pluralise_commas(
      paste0("The `", which, "` table is missing metadata column(s) "),
      tick(cols),
      "."
    )
  )
}

abort_recombine_nested_bad_key <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Column ",
      tick(nested_key_col),
      " of the parent table must be an integerish vector without missing values."
    )
  )
}

abort_recombine_nested_key_duplicated <- function(keys, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      paste0(
        "Column ",
        tick(nested_key_col),
        " of the parent table must be unique:"
      ),
      paste0("Key ", keys)
    ),
    keys = keys
  )
}

abort_recombine_nested_bad_status <- function(keys, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      paste0(
        "Column ",
        tick(nested_status_col),
        ' must be one of "rows", "empty", or "null":'
      ),
      paste0("Key ", keys)
    ),
    keys = keys
  )
}

abort_recombine_nested_bad_count <- function(keys, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      paste0(
        "Column ",
        tick(nested_count_col),
        " must contain non-negative whole numbers:"
      ),
      paste0("Key ", keys)
    ),
    keys = keys
  )
}

abort_recombine_nested_status_count <- function(
  keys,
  status,
  count,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    problems(
      paste0(
        "Columns ",
        tick(nested_status_col),
        " and ",
        tick(nested_count_col),
        " are inconsistent:"
      ),
      paste0("Key ", keys, ': status "', status, '" with count ', count)
    ),
    keys = keys
  )
}

abort_recombine_nested_bad_detail_key <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Column ",
      tick(nested_key_col),
      " of the detail table must be an integerish vector without missing values."
    )
  )
}

abort_recombine_nested_bad_row_no <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Column ",
      tick(nested_row_no_col),
      " of the detail table must be an integerish vector without missing values."
    )
  )
}

abort_recombine_nested_schema_mismatch <- function(
  missing,
  extra,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    bullets(
      "The detail table doesn't match the schema recorded by `split_nested()`:",
      x = if (has_length(missing)) {
        pluralise_commas("Missing column(s) ", tick(missing))
      },
      x = if (has_length(extra)) {
        pluralise_commas("Unexpected column(s) ", tick(extra))
      }
    )
  )
}

abort_recombine_nested_orphan_key <- function(keys, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      "Detail rows reference parent keys that are absent from the parent table:",
      paste0("Key ", keys)
    ),
    keys = keys
  )
}

abort_recombine_nested_row_no_duplicated <- function(
  keys,
  row_nos,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    problems(
      "Detail rows have duplicated row numbers within their parent:",
      paste0("Key ", keys, ", row number ", row_nos)
    ),
    keys = keys,
    row_nos = row_nos
  )
}

abort_recombine_nested_row_no_out_of_bounds <- function(
  keys,
  row_nos,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    problems(
      "Detail rows have row numbers outside the range implied by the parent table:",
      paste0("Key ", keys, ", row number ", row_nos)
    ),
    keys = keys,
    row_nos = row_nos
  )
}

abort_recombine_nested_row_missing <- function(
  keys,
  row_nos,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    problems(
      "Detail rows are missing for the following parent keys:",
      paste0(
        "Key ",
        keys,
        ", row number(s) ",
        map_chr(row_nos, paste, collapse = ", ")
      )
    ),
    keys = keys,
    row_nos = row_nos
  )
}

abort_recombine_nested_col_clash <- function(col_name, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Can't restore column ",
      tick(col_name),
      ", already present in the parent table."
    )
  )
}
