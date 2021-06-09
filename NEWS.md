# listcompr 0.3.0

* modified internal conversion from lists to data frames in `gen.data.frame` such that non-fundamental types are accepted
  (fixes https://github.com/patrickroocks/listcompr/issues/5)
* added a check to ensure that the inner expression for `gen.(named).(data.frame|matrix)` must be exactly one row
* some output format fixes (data frames and matrices where sometimes mixed up)
* added `byrow` parameter to `gen.(named).(data.frame|matrix)` which applies the Cartesian product by column to the expression
* fixed that `gen.named.(vector|list)` can be used as an inner expression of other `gen.`-functions
* `gen.list` and similar functions accept characters with placeholders, removed `gen.list.char` and `gen.vector.char`
* changed order of dimensions in `gen.matrix` for auto-detected 2-dim matrix
* `gen.matrix` automatically searches for two dimensions in the ... arguments
* fixed missing column names in `gen.named.data.frame`
* added `gen.matrix` and `gen.named.matrix` for generating (named) matrices

# listcompr 0.2.0

* variable ranges can also be of character type (fixes https://github.com/patrickroocks/listcompr/issues/1)
* fixed sorting of free variables (fixes https://github.com/patrickroocks/listcompr/issues/2)
* auto-detect column names when generating data frames where a symbol is in the base expression
* fixed missing export of `gen.logical.or`, added example to the vignette
* allow substitutions in the list comprehension arguments

# listcompr 0.1.0

* first version of the listcompr package