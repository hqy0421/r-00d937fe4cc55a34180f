# output test

    Code
      split_nested(tibble(details = list(NULL)), details)
    Condition
      Error in `split_nested()`:
      ! Can't determine the detail schema of column `details` because all cells are `NULL`.
      i Supply a template via the `ptype` argument.
    Code
      split_nested(tibble(details = list(tibble(a = 1), tibble(b = 2))), details)
    Condition
      Error in `split_nested()`:
      ! Cell 2 of column `details` doesn't match the detail schema.
      x Missing column `a`.
      x Unexpected column `b`.
    Code
      recombine_nested(split$parent, detail_orphan, col = "details")
    Condition
      Error in `recombine_nested()`:
      ! Detail rows reference parent keys that are absent from the parent table.
      x Key 99.
    Code
      recombine_nested(split$parent, detail_dup, col = "details")
    Condition
      Error in `recombine_nested()`:
      ! Detail rows have duplicated row numbers within their parent.
      x Key 1, row number 1.
    Code
      recombine_nested(split$parent, split$detail[-2, ], col = "details")
    Condition
      Error in `recombine_nested()`:
      ! Detail rows are missing for the following parent keys.
      x Key 1, row number(s) 2.
    Code
      recombine_nested(split$parent, detail_oob, col = "details")
    Condition
      Error in `recombine_nested()`:
      ! Detail rows have row numbers outside the range implied by the parent table.
      x Key 1, row number 5.

