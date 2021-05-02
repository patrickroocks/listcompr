# listcompr 0.2.4

* `gen.list` and similar functions accept characters with placeholders, removed `gen.list.char` and `gen.vector.char`

# listcompr 0.2.3

* changed order of dimensions in `gen.matrix` for auto-detected 2-dim matrix

# listcompr 0.2.2

* `gen.matrix` automatically searches for two dimensions in the ... arguments

# listcompr 0.2.1

* fixed missing colnames in `gen.named.data.frame`
* added `gen.matrix` and `gen.named.matrix` for generating (named) matrices

# listcompr 0.2.0

* variable ranges can also be of character type (fixes https://github.com/patrickroocks/listcompr/issues/1)
* fixed sorting of free variables (fixes https://github.com/patrickroocks/listcompr/issues/2)
* auto-detect column names when generating data frames where a symbol is in the base expression
* fixed missing export of `gen.logical.or`, added example to the vignette
* allow substitutions in the list comprehension arguments

# listcompr 0.1.0

* First version of the listcompr package