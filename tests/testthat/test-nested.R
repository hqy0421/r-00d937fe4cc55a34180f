# split_nested -------------------------------------------------------------

test_that("split_nested() splits into parent and detail", {
  x <- tibble(
    id = 1:3,
    details = list(
      tibble(item = c("a", "b"), qty = 1:2),
      tibble(item = "c", qty = 3L),
      NULL
    )
  )

  split <- split_nested(x, details)

  expect_s3_class(split, "tibble_nested_split")
  expect_named(split, c("parent", "detail"))

  expect_identical(
    split$parent,
    tibble(
      id = 1:3,
      .parent_id = 1:3,
      .detail_status = c("rows", "rows", "null"),
      .detail_count = c(2L, 1L, 0L)
    )
  )

  expect_identical(
    split$detail,
    tibble(
      .parent_id = c(1L, 1L, 2L),
      .row_no = c(1L, 2L, 1L),
      item = c("a", "b", "c"),
      qty = 1:3
    )
  )
})

test_that("split_nested() accepts a string column name", {
  x <- tibble(details = list(tibble(v = 1)))
  expect_identical(split_nested(x, "details"), split_nested(x, details))
})

test_that("split_nested() inserts metadata at the position of the nested column", {
  x <- tibble(a = 1, details = list(tibble(v = 1)), b = 2)
  split <- split_nested(x, details)
  expect_named(
    split$parent,
    c("a", ".parent_id", ".detail_status", ".detail_count", "b")
  )
})

test_that("split_nested() distinguishes zero-row tibbles from NULL", {
  empty <- tibble(item = character(), qty = integer())
  x <- tibble(id = 1:2, details = list(empty, NULL))

  split <- split_nested(x, details)

  expect_identical(split$parent$.detail_status, c("empty", "null"))
  expect_identical(split$parent$.detail_count, c(0L, 0L))
  expect_identical(
    split$detail,
    tibble(
      .parent_id = integer(),
      .row_no = integer(),
      item = character(),
      qty = integer()
    )
  )
})

test_that("split_nested() generates an empty detail table from the schema for all-empty batches", {
  x <- tibble(id = 1:2, details = list(NULL, NULL))
  ptype <- tibble(item = character(), qty = integer())

  split <- split_nested(x, details, ptype = ptype)

  expect_identical(
    split$detail,
    tibble(
      .parent_id = integer(),
      .row_no = integer(),
      item = character(),
      qty = integer()
    )
  )
})

test_that("split_nested() derives the schema from zero-row cells", {
  empty <- tibble(item = character(), qty = integer())
  x <- tibble(id = 1:2, details = list(empty, empty))

  split <- split_nested(x, details)

  expect_identical(nrow(split$detail), 0L)
  expect_named(split$detail, c(".parent_id", ".row_no", "item", "qty"))
  expect_identical(split$detail$qty, integer())
})

test_that("split_nested() handles a zero-row input", {
  x <- tibble(id = integer(), details = list())
  split <- split_nested(x, details, ptype = tibble(qty = integer()))

  expect_identical(nrow(split$parent), 0L)
  expect_identical(split$parent$.parent_id, integer())
  expect_identical(nrow(split$detail), 0L)
})

test_that("split_nested() unifies compatible cell types", {
  x <- tibble(details = list(tibble(qty = 1L), tibble(qty = 2.5)))
  split <- split_nested(x, details)
  expect_identical(split$detail$qty, c(1, 2.5))
})

test_that("split_nested() validates its input", {
  expect_tibble_abort(
    split_nested(1:3, details),
    abort_nested_must_be_data_frame("x")
  )
  expect_tibble_abort(
    split_nested(tibble(a = 1), details),
    abort_nested_unknown_column("details")
  )
  expect_tibble_abort(
    split_nested(tibble(details = 1:3), details),
    abort_nested_column_must_be_list("details")
  )
  expect_tibble_abort(
    split_nested(tibble(details = list(1, "a")), details),
    abort_nested_cell_must_be_data_frame("details", 1:2)
  )
  expect_tibble_abort(
    split_nested(tibble(details = list(NULL)), details),
    abort_nested_schema_unknown("details")
  )
  expect_tibble_abort(
    split_nested(tibble(details = list(NULL)), details, ptype = 1:3),
    abort_nested_ptype_must_be_data_frame()
  )
  expect_tibble_abort(
    split_nested(tibble(details = list(tibble(.row_no = 1))), details),
    abort_nested_schema_reserved(".row_no")
  )
  expect_tibble_abort(
    split_nested(
      tibble(.parent_id = 1, details = list(tibble(v = 1))),
      details
    ),
    abort_nested_parent_reserved(".parent_id")
  )
})

test_that("split_nested() refuses cells with a different schema", {
  x <- tibble(details = list(tibble(a = 1), tibble(b = 2)))
  expect_tibble_abort(
    split_nested(x, details),
    abort_nested_cell_schema_mismatch("details", 2L, "a", "b")
  )

  x <- tibble(details = list(tibble(a = 1), tibble(a = 2, b = 3)))
  expect_tibble_abort(
    split_nested(x, details),
    abort_nested_cell_schema_mismatch("details", 2L, character(), "b")
  )
})

# recombine_nested ---------------------------------------------------------

test_that("roundtrip restores the original tibble", {
  x <- tibble(
    id = c(1L, 1L, 2L, 3L),
    details = list(
      tibble(item = c("a", "a"), qty = c(1L, 1L)),
      tibble(item = character(), qty = integer()),
      NULL,
      tibble(item = "z", qty = 9L)
    )
  )

  expect_identical(recombine_nested(split_nested(x, details)), x)
})

test_that("roundtrip preserves duplicated parent rows and duplicated details", {
  x <- tibble(
    id = c(1L, 1L),
    details = list(
      tibble(item = c("a", "a"), qty = c(1L, 1L)),
      tibble(item = c("a", "a"), qty = c(1L, 1L))
    )
  )

  expect_identical(recombine_nested(split_nested(x, details)), x)
})

test_that("roundtrip preserves column positions and types", {
  x <- tibble(
    a = 1L,
    details = list(tibble(
      i = 1L,
      d = 1.5,
      c = "a",
      l = TRUE,
      t = as.POSIXct("2024-01-01", tz = "UTC"),
      v = list(1)
    )),
    b = "x"
  )

  out <- recombine_nested(split_nested(x, details))
  expect_named(out, c("a", "details", "b"))
  expect_identical(out, x)
})

test_that("recombine_nested() restores the detail order from .row_no", {
  x <- tibble(
    id = 1:2,
    details = list(
      tibble(item = c("a", "b", "c"), qty = 1:3),
      tibble(item = "d", qty = 4L)
    )
  )
  split <- split_nested(x, details)

  shuffled <- split$detail[c(3, 1, 4, 2), ]
  expect_identical(recombine_nested(split$parent, shuffled, col = "details"), x)
})

test_that("recombine_nested() keeps the read-back order of the parent rows", {
  x <- tibble(
    id = 1:3,
    details = list(tibble(v = 1L), tibble(v = 2L), NULL)
  )
  split <- split_nested(x, details)

  parent_late <- split$parent[c(3, 1, 2), ]
  out <- recombine_nested(parent_late, split$detail, col = "details")

  expect_identical(out$id, c(3L, 1L, 2L))
  expect_identical(out$details[[1]], NULL)
  expect_identical(out$details[[2]], tibble(v = 1L))
  expect_identical(out$details[[3]], tibble(v = 2L))
})

test_that("recombine_nested() accepts the split object and can rename the column", {
  x <- tibble(id = 1, details = list(tibble(v = 1L)))
  split <- split_nested(x, details)

  expect_identical(recombine_nested(split), x)
  expect_named(recombine_nested(split, col = "items"), c("id", "items"))
})

test_that("recombine_nested() roundtrips an all-empty batch", {
  empty <- tibble(item = character(), qty = integer())
  x <- tibble(id = 1:3, details = list(empty, NULL, empty))

  expect_identical(recombine_nested(split_nested(x, details)), x)
})

test_that("recombine_nested() roundtrips a zero-row input", {
  x <- tibble(id = integer(), details = list())
  split <- split_nested(x, details, ptype = tibble(qty = integer()))

  expect_identical(recombine_nested(split), x)
})

test_that("recombine_nested() needs col and detail for separate tables", {
  split <- split_nested(tibble(details = list(tibble(v = 1))), details)

  expect_tibble_abort(
    recombine_nested(split$parent, split$detail),
    abort_recombine_nested_need_col()
  )
  expect_tibble_abort(
    recombine_nested(split$parent, col = "details"),
    abort_recombine_nested_need_detail()
  )
  expect_tibble_abort(
    recombine_nested(split$parent, "not a table", col = "details"),
    abort_nested_must_be_data_frame("detail")
  )
})

test_that("recombine_nested() validates the metadata columns", {
  split <- split_nested(tibble(details = list(tibble(v = 1))), details)

  expect_tibble_abort(
    recombine_nested(split$parent[-1], split$detail, col = "details"),
    abort_recombine_nested_missing_meta("parent", ".parent_id")
  )
  expect_tibble_abort(
    recombine_nested(split$parent[-2], split$detail, col = "details"),
    abort_recombine_nested_missing_meta("parent", ".detail_status")
  )
  expect_tibble_abort(
    recombine_nested(split$parent[-3], split$detail, col = "details"),
    abort_recombine_nested_missing_meta("parent", ".detail_count")
  )
  expect_tibble_abort(
    recombine_nested(split$parent, split$detail[-1], col = "details"),
    abort_recombine_nested_missing_meta("detail", ".parent_id")
  )
  expect_tibble_abort(
    recombine_nested(split$parent, split$detail[-2], col = "details"),
    abort_recombine_nested_missing_meta("detail", ".row_no")
  )
})

test_that("recombine_nested() refuses duplicated parent keys", {
  x <- tibble(id = 1:2, details = list(tibble(v = 1L), tibble(v = 2L)))
  split <- split_nested(x, details)

  parent_bad <- split$parent
  parent_bad$.parent_id <- c(1L, 1L)
  expect_tibble_abort(
    recombine_nested(parent_bad, split$detail, col = "details"),
    abort_recombine_nested_key_duplicated(1L)
  )
})

test_that("recombine_nested() refuses inconsistent status and count", {
  x <- tibble(id = 1:2, details = list(tibble(v = 1L), NULL))
  split <- split_nested(x, details)

  parent_bad <- split$parent
  parent_bad$.detail_status <- c("empty", "null")
  expect_tibble_abort(
    recombine_nested(parent_bad, split$detail, col = "details"),
    abort_recombine_nested_status_count(1L, "empty", 1L)
  )

  parent_bad <- split$parent
  parent_bad$.detail_count <- c(1L, 2L)
  expect_tibble_abort(
    recombine_nested(parent_bad, split$detail, col = "details"),
    abort_recombine_nested_status_count(2L, "null", 2L)
  )

  parent_bad <- split$parent
  parent_bad$.detail_status <- c("rows", "typo")
  expect_tibble_abort(
    recombine_nested(parent_bad, split$detail, col = "details"),
    abort_recombine_nested_bad_status(2L)
  )
})

test_that("recombine_nested() refuses unknown parent keys", {
  x <- tibble(id = 1:2, details = list(tibble(v = 1L), tibble(v = 2L)))
  split <- split_nested(x, details)

  detail_bad <- split$detail
  detail_bad$.parent_id <- c(1L, 99L)
  expect_tibble_abort(
    recombine_nested(split$parent, detail_bad, col = "details"),
    abort_recombine_nested_orphan_key(99L)
  )
})

test_that("recombine_nested() refuses duplicated row numbers", {
  x <- tibble(id = 1, details = list(tibble(v = 1:2)))
  split <- split_nested(x, details)

  detail_bad <- split$detail
  detail_bad$.row_no <- c(1L, 1L)
  expect_tibble_abort(
    recombine_nested(split$parent, detail_bad, col = "details"),
    abort_recombine_nested_row_no_duplicated(1L, 1L)
  )
})

test_that("recombine_nested() refuses missing rows", {
  x <- tibble(id = 1, details = list(tibble(v = 1:3)))
  split <- split_nested(x, details)

  detail_bad <- split$detail[-2, ]
  expect_tibble_abort(
    recombine_nested(split$parent, detail_bad, col = "details"),
    abort_recombine_nested_row_missing(1L, list(2L))
  )

  detail_bad <- split$detail[1, ]
  expect_tibble_abort(
    recombine_nested(split$parent, detail_bad, col = "details"),
    abort_recombine_nested_row_missing(1L, list(2:3))
  )
})

test_that("recombine_nested() refuses out-of-bounds row numbers", {
  x <- tibble(id = 1:2, details = list(tibble(v = 1:2), NULL))
  split <- split_nested(x, details)

  detail_bad <- split$detail
  detail_bad$.row_no <- c(1L, 3L)
  expect_tibble_abort(
    recombine_nested(split$parent, detail_bad, col = "details"),
    abort_recombine_nested_row_no_out_of_bounds(1L, 3L)
  )

  detail_bad <- split$detail
  detail_bad$.row_no <- c(0L, 2L)
  expect_tibble_abort(
    recombine_nested(split$parent, detail_bad, col = "details"),
    abort_recombine_nested_row_no_out_of_bounds(1L, 0L)
  )

  # a detail row for a NULL parent is out of bounds, too
  detail_bad <- vec_slice(split$detail, c(1, 1, 2))
  detail_bad$.parent_id <- c(1L, 2L, 2L)
  detail_bad$.row_no <- c(1L, 1L, 2L)
  expect_tibble_abort(
    recombine_nested(split$parent, detail_bad, col = "details"),
    abort_recombine_nested_row_no_out_of_bounds(c(2L, 2L), c(1L, 2L))
  )
})

test_that("recombine_nested() refuses a detail table that doesn't match the recorded schema", {
  x <- tibble(id = 1, details = list(tibble(v = 1L)))
  split <- split_nested(x, details)

  split_bad <- split
  split_bad$detail$extra <- 1
  expect_tibble_abort(
    recombine_nested(split_bad),
    abort_recombine_nested_schema_mismatch(character(), "extra")
  )

  split_bad <- split
  split_bad$detail <- split$detail[-3]
  expect_tibble_abort(
    recombine_nested(split_bad),
    abort_recombine_nested_schema_mismatch("v", character())
  )
})

test_that("recombine_nested() restores the types recorded in the split object", {
  x <- tibble(id = 1, details = list(tibble(v = 1L)))
  split <- split_nested(x, details)

  # a warehouse may return a different type for the same column
  split_lossy <- split
  split_lossy$detail$v <- 1.5
  expect_error(
    recombine_nested(split_lossy),
    class = "vctrs_error_cast_lossy"
  )

  split_dbl <- split
  split_dbl$detail$v <- 1
  expect_identical(recombine_nested(split_dbl), x)
})

# output -------------------------------------------------------------------

test_that("output test", {
  x <- tibble(
    id = 1:2,
    details = list(tibble(v = 1:2), tibble(v = 3L))
  )
  split <- split_nested(x, details)

  detail_orphan <- split$detail
  detail_orphan$.parent_id <- c(1L, 1L, 99L)

  detail_dup <- split$detail
  detail_dup$.row_no <- c(1L, 1L, 1L)

  detail_oob <- split$detail
  detail_oob$.row_no <- c(1L, 5L, 1L)

  expect_snapshot(error = TRUE, {
    split_nested(tibble(details = list(NULL)), details)
    split_nested(tibble(details = list(tibble(a = 1), tibble(b = 2))), details)
    recombine_nested(split$parent, detail_orphan, col = "details")
    recombine_nested(split$parent, detail_dup, col = "details")
    recombine_nested(split$parent, split$detail[-2, ], col = "details")
    recombine_nested(split$parent, detail_oob, col = "details")
  })
})
