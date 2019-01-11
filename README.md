# reviewr
### Purpose and Overview

The reviewr package makes performing noisy checks of how your data has changed after implementing a `dplyr` function easy to implement and read. By simply adding the suffix `_qc` to the end of several core `dplyr` functions, you will get the exact same result as you would had you used the normal `dplyr` functions, but  additional information about what just happened will automatically print. For example, using `filter_qc` instead of `filter` will filter the data exactly as `filter` will, but will also report how many (and what percent) of rows were dropped. This makes using  reviewr easy and allows you to quickly toggle back and forth between regular `dplyr` functions and their reviewr counterparts. No wrappers around your code or any other set-up; just add `_qc`.

There are currently four sets of dplyr functions to which you can add `_qc` suffix and get a reviewr equivalent

* Filter. `filter_qc`, `filter_all_qc`, `filter_at_qc`, and `filter_if_qc`
* Mutate. `mutate_qc`, `mutate_all_qc`, `mutate_at_qc`, and `mutate_if_qc`. And `transmute_qc`, `transmute_all_qc`, `transmute_at_qc`, and `transmute_if_qc`
* Summarize. `summarize_qc`, `summarize_all_qc`, `summarize_at_qc`, and `summarize_if_qc`
* Joins. `full_join_qc`, `inner_join_qc`, `left_join_qc`, `right_join_qc`, `semi_join_qc`, and `anti_join_qc`

Although, some `reviewr` functions have additional options (especially for grouped data), all `reviewr` functions will work by simply using them as you would dplyr. This means you can start using them now: if you know how to use their `dplyr` versions, the reviewr functions will be implemented identically.

`reviewr`, therefore, allows you to get under the hood of data cleaning with dplyr. It does not attempt or intend to tell if you  `dplyr` “worked”, but rather whether how you used the `dplyr` function(s) matched what you intended. (Does it make sense that you  dropped 20 rows when performing the filter? Did you expect the join to not be a perfect 1-1 join? etc.)

### Installing
Currently, `reviewr` lives only on github:
```
devtools::install_github("adamMaier/reviewr", build_opts = c("--no-resave-data", "--no-manual"))
```

### Learning more
All `reviewr` functions have help documentation with examples. For the fullest description, see the vignette:
```
vignette("reviewr")
```
