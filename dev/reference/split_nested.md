# Split and recombine a tibble with a nested column

`split_nested()` splits a tibble with a column of nested tibbles into a
parent table and a detail table that can be stored separately, e.g. as
two tables in a data warehouse. `recombine_nested()` restores the
original tibble from the two tables.

Each cell of the nested column must be a tibble with a schema shared by
all cells, a zero-row tibble, or `NULL`.

## Usage

``` r
split_nested(x, col, ptype = NULL)

recombine_nested(parent, detail = NULL, col = NULL)
```

## Arguments

- x:

  A tibble with a column of nested tibbles.

- col:

  The name of the nested column, as a string or symbol. For
  `recombine_nested()`, the name of the restored nested column. Can be
  omitted if `parent` is the result of `split_nested()`.

- ptype:

  An optional template (a tibble, ideally zero-row) that defines the
  detail schema. Required if all cells of the nested column are `NULL`.

- parent:

  A parent table created by `split_nested()`, or the split object
  itself.

- detail:

  A detail table created by `split_nested()`. Can be omitted if `parent`
  is the split object.

## Value

`split_nested()` returns a list of class `"tibble_nested_split"` with
components `parent` and `detail`. `recombine_nested()` returns a tibble
with the nested column restored.

## Split layout

The parent table stores all remaining columns and, in the position of
the nested column, three metadata columns:

- `.parent_id`: a surrogate key that locates each parent row
  independently, even if all business columns are duplicated.

- `.detail_status`: `"rows"` for a detail with at least one row,
  `"empty"` for a zero-row tibble, `"null"` for `NULL`.

- `.detail_count`: the number of detail rows.

The detail table stores the columns of the nested tibbles, plus:

- `.parent_id`: the parent row each detail row belongs to.

- `.row_no`: the row number within its parent, used to restore the
  original order even if the detail table is read back in random order.

An all-empty batch (all cells `NULL` or zero-row tibbles) still yields a
fully typed zero-row detail table, generated from the detail schema.

## Recombination

`recombine_nested()` restores the nested column at its original
position, with the original column types, duplicate details, and the
distinction between zero-row and `NULL` cells intact. Parent rows appear
in the order of the parent table, detail rows are ordered by `.row_no`
within each parent.

Recombination is refused, naming the offending keys or row numbers, if a
detail row references an unknown parent key, a row number is duplicated
within a parent, expected rows are missing, or a row number is out of
bounds.

## Examples

``` r
orders <- tibble(
  id = 1:3,
  details = list(
    tibble(item = c("a", "b"), qty = 1:2),
    tibble(item = character(), qty = integer()),
    NULL
  )
)

split <- split_nested(orders, details)
split$parent
#> # A tibble: 3 × 4
#>      id .parent_id .detail_status .detail_count
#>   <int>      <int> <chr>                  <int>
#> 1     1          1 rows                       2
#> 2     2          2 empty                      0
#> 3     3          3 null                       0
split$detail
#> # A tibble: 2 × 4
#>   .parent_id .row_no item    qty
#>        <int>   <int> <chr> <int>
#> 1          1       1 a         1
#> 2          1       2 b         2

# Recombination restores the original tibble,
# even if the detail table is read back in a different order
recombine_nested(split$parent, split$detail[2:1, ], col = "details")
#> # A tibble: 3 × 2
#>      id details         
#>   <int> <list>          
#> 1     1 <tibble [2 × 2]>
#> 2     2 <tibble [0 × 2]>
#> 3     3 <NULL>          
```
