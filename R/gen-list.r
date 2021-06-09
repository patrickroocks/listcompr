# ----- List, Vectors, Data Frames, Matrices -----

#' Generate Lists, Vectors, Data Frames and Matrices with List Comprehension
#' 
#' @description
#' 
#' Functions to transform a base expression containing free variables into a list, a vector, a data frame, or a matrix
#' based on variable ranges and additional conditions.
#'
#' @name gen.list
#' @param expr A base expression containing free variables which is evaluated for all combinations of variables, 
#'   where the combinations of variables are given by the ranges and conditions (see \code{...} parameters).
#' 
#' Expected structure of \code{expr}:
#'  \itemize{
#'    \item For \code{gen.list} it may have arbitrary structure (including a list).
#'    \item For \code{gen.vector} a value (i.e., a vector of length 1) is expected.
#'    \item For \code{gen.data.frame} a (named) vector or list is expected which describes exactly one row of the data frame.
#'          Use \code{list(name = val)} if \code{val} is a non-fundamental type like \code{difftime}.
#'    \item For \code{gen.matrix} either a (named) vector/list (like \code{gen.data.frame}) or a scalar is expected.
#'          In the first case, we expect the same as for \code{gen.data.frame}.
#'          In the latter case we expect exactly two variables (inducing rows and columns where the order depends on \code{byrow}) within the \code{...} arguments.
#'   }
#'   Within \code{expr} it is allowed to use functions and predefined constants from the parent environment.
#' @param ... Arbitrary many variable ranges and conditions.
#'   For all free variables occurring in \code{expr} a range must be assigned, e.g., \code{x = 1:3, y = 1:5} for an expression \code{x + y}. 
#'   At least one variable range is required.
#'   The ranges may depend on each other, e.g., \code{x = 1:3, y = x:3} or a substitution like \code{x = 1:3, y = 2 * x} is allowed.
#'   The generated values can be further restricted by conditions like \code{x <= y}.
#' @param byrow Logical. If \code{FALSE} (the default), the elements of a vector within \code{expr} are taken as columns. 
#'   Otherwise, they are taken as rows.
#'   
#' @return 
#' 
#' The result of \code{gen.list} is a list (a vector for \code{gen.vector}) containing an entry for each combination of the free variables (i.e., the Cartesian product), 
#' where all the free variables in \code{expr} are substituted.
#' The function \code{gen.vector} returns a vector while \code{gen.list} may contain also more complex substructures (like vectors or lists).
#' 
#' The output of \code{gen.data.frame} is a data frame where each substituted \code{expr} entry is one row.
#' The base expression \code{expr} should contain a (named) vector or list, such that each entry of this vector becomes a column of the returned data frame.
#' If the vector contains a single literal without a name, this is taken as column name. For instance, \code{gen.data.frame(a, a = 1:5)} returns the same as \code{gen.data.frame(c(a = a), a = 1:5)}.
#' Default names 'V1', 'V2', ... are used, if no names are given and names can't be automatically detected.
#' 
#' The result of \code{gen.matrix}:
#' \itemize{
#'   \item It's similar to \code{gen.data.frame}, if \code{expr} evaluates to a vector of length > 1, or row/column names are given.
#'         Each substituted \code{expr} entry is one row of the matrix.
#'         In contrast to \code{gen.data.frame}, column names are not auto-generated, e.g., \code{gen.matrix(c(a_1, a_2), a_ = 1:2)} is an unnamed matrix.
#'         If the \code{expr} argument has explicit names (e.g., \code{c(a_1 = a_1, a_2 = a_2)}), these column names are assigned to the resulting matrix.
#'   \item It's a matrix where the rows and columns are induced by the two variables within \code{...}, if \code{expr} is a scalar, and no names or conditions are given.
#'         If \code{byrow} is \code{FALSE}, the second variable (i.e., the inner loop) refers to the columns, otherwise it refers to the rows.
#'         For instance, \code{gen.matrix(i + j, i = 1:3, j = 1:2)} is a matrix with 3 rows and 2 columns.
#'         For \code{gen.matrix(i + j, i = 1:3, j = 1:2, byrow = TRUE)} we get 2 rows and 3 columns.
#' }
#' 
#' All expressions and conditions are applied to each combination of the free variables separately, i.e., they are applied row-wise and not vector-wise. 
#' For instance, the term \code{sum(x,y)} (within \code{expr} or a condition) is equivalent to \code{x+y}.
#' 
#' @section Indices for variables: 
#' 
#' A range for a variable ending with an underscore (like \code{x_}) defines a set of ranges affecting all variables named \code{{varname}_{index}}, e.g. \code{x_1}.
#' For instance, in \code{gen.vector(x_1 + x_2 + x_3, x_ = 1:5)} the variables \code{x_1, x_2, x_3} are all ranging in \code{1:5}.
#' This can be overwritten for each single \code{x_i}, e.g., an additional argument \code{x_3 = 1:3} assigns the range \code{1:3} to \code{x_3} while \code{x_1} and \code{x_2} keep the range \code{1:5}.
#' A group of indexed variables is kept always sorted according to the position of the main variable \code{{varname}_}. 
#' For instance, the two following statements produce the same results:
#'   
#' \itemize{
#'   \item \code{gen.vector(x_1 + x_2 + a, x_ = 1:5, a = 1:2, x_1 = 1:2)}
#'   \item \code{gen.vector(x_1 + x_2 + a, x_1 = 1:2, x_2 = 1:5, a = 1:2)}
#' }
#' 
#' @section Folded expressions:
#' 
#' Expressions and conditions support a \code{...}-notation which works as follows:
#' 
#' \itemize{
#'   \item A vector like \code{c(x_1, ..., x_4)} is a shortcut for \code{c(x_1, x_2, x_3, x_4)}. 
#'   \item A named vector like \code{c(a_1 = x_1, ..., a_3 = x_3)} is a shortcut for \code{c(a_1 = x_1, a_2 = x_2, a_3 = x_3)}. 
#'   \item A n-ary function argument like \code{sum(x_1, ..., x_4)} is a shortcut for \code{sum(x_1, x_2, x_3, x_4)}.
#'   \item Repeated expressions of binary operators can be abbreviated with the \code{...} expressions as follows:
#'     \code{x_1 + ... + x_4} is a shortcut for \code{x_1 + x_2 + x_3 + x_4}. 
#'     Note that, due to operator precedence, \code{1 + x_1 + ... + x_4} will not work, but \code{1 + (x_1 + ... + x_4)} works as expected.
#'   \item For non-commutative operators, \code{x_1 - ... - x_4} is a shortcut for \code{x_1 - x_2 - x_3 - x_4} which is evaluated as \code{((x_1 - x_2) - x_3) - x_4}.
#' }
#' 
#' The conditions may contain itself list comprehension expressions, e.g., \code{\link{gen.logical.and}} to compose and-connected logical expressions.
#' 
#' @section Character patterns:
#' 
#' In expression there may occur characters with \code{\{\}}-placeholders. 
#' The content of these placeholders is evaluated like any other part of an expression and converted to a character.
#' For example, \code{"a{x}"} is transformed into \code{"a1"} for \code{x = 1}. 
#' Double brackets are transformed into a single bracket without evaluating the inner expression.
#' For instance, \code{"var{x + 1}_{{a}}"} is transformed into \code{"var2_{a}"} for \code{x = 1}.
#' 
#' 
#' @seealso
#'   \code{\link{gen.named.list}} to generate named structures, 
#'   \code{\link{gen.list.expr}} to generate expressions to be evaluated later, 
#'   \code{\link{gen.logical.and}} to generate logical and/or conditions,
#'   and \link{listcompr} for an overview of all list comprehension functions.
#' 
#' 
#' @examples 
#' # Sum of 1:x
#' gen.vector(sum(1:x), x = 1:10)
#' 
#' # Same as above, but return as text
#' gen.list("sum of 1 to {x} is {sum(1:x)}", x = 1:5)
#' 
#' # A list containing vectors [1], [1, 2], [1, 2, 3], ...
#' gen.list(gen.vector(i, i = 1:n), n = 1:10)
#' 
#' # A data frame of tuples (x_1, x_2, x_3) summing up to 10
#' gen.data.frame(c(x_1, ..., x_3), x_ = 1:10, x_1 + ... + x_3 == 10)
#' 
#' # Same as above, but restrict to ascending tuples with x_i <= x_(i+1)
#' gen.data.frame(c(x_1, ..., x_3), x_1 = 1:10, x_2 = x_1:10, x_3 = x_2:10,
#'                x_1 + ... + x_3 == 10)
#' 
#' # A data.frame containing the numbers in 2:20, the sum of their divisors
#' # and a flag if they are "perfect" (sum of divisors equals the number)
#' gen.data.frame(list(n, sumdiv, perfect = (n == sumdiv)), n = 2:20, 
#'                sumdiv = sum(gen.vector(x, x = 1:(n-1), n %% x == 0)))
#'                
#' # A diagonal matrix with (1, ..., 5) on the diagonal
#' gen.matrix(if (i == j) i else 0, i = 1:5, j = 1:5)
#' 
#' @export
gen.list <- function(expr, ...) {
  expr <- substitute(expr)
  l <- substitute(list(...))
  return(gen_list_internal(expr, l, OUTPUT_FORMAT[["LST"]], NULL, parent.frame()))
}

#' @rdname gen.list
#' @export
gen.vector <- function(expr, ...) {
  expr <- substitute(expr)
  l <- substitute(list(...))
  return(gen_list_internal(expr, l, OUTPUT_FORMAT[["VEC"]], NULL, parent.frame()))
}

#' @rdname gen.list
#' @export
gen.data.frame <- function(expr, ..., byrow = FALSE) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, if (byrow) OUTPUT_FORMAT[["DF_ROW"]] else OUTPUT_FORMAT[["DF"]], NULL, parent.frame()))
}

#' @rdname gen.list
#' @export
gen.matrix <- function(expr, ..., byrow = FALSE) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, if (byrow) OUTPUT_FORMAT[["MTX_ROW"]] else OUTPUT_FORMAT[["MTX"]], NULL, parent.frame()))
}

# ----- Named Structures -----

#' Generate Named Lists, Vectors, Data Frames, and Matrices with List Comprehension
#' 
#' @description
#' 
#' Functions to transform patterns with placeholders into characters or into names of lists, vectors, data frames or matrices,
#' based on variable ranges and additional conditions.
#' 
#' @name gen.named.list
#' 
#' @param str A character, containing expressions to be evaluated in \code{\{\}}-brackets, e.g., \code{"a{x}"} is transformed into \code{"a1"} for \code{x = 1}. 
#'   Double brackets are transformed into a single bracket without evaluating the inner expression.
#'   For instance, \code{"var{x + 1}_{{a}}"} is transformed into \code{"var2_{a}"} for \code{x = 1}.
#' @param expr A base expression containing free variables which is evaluated for all combinations of variables. 
#' @param ... Arbitrary many variable ranges and conditions.
#' @param byrow Logical. If \code{FALSE} (the default), the elements of an \code{expr} vector are taken as columns. 
#'   Otherwise, they are taken as rows.
#' 
#' @details 
#' 
#' The free variables in the inner expressions (i.e., the content of the \code{\{\}}-brackets) of \code{expr} are evaluated in the same way as expressions in \code{\link{gen.list}}.
#' 
#' See \code{\link{gen.list}} for more details on the \code{expr} and \code{...} parameters.
#' 
#' @return 
#' 
#' These functions return lists, vectors, data frames, and matrices.
#' They work very similar to their counterparts without ".named".
#' Additionally the vector of characters, induced by \code{str}, serves as a vector of names for the generated structures. 
#' In case of lists or vectors, the result is a named list or a named vector. For data frames and matrices, the names are taken as row names.
#' 
#' @seealso \code{\link{gen.list}} for explanations on list and vector comprehension,
#'   and \link{listcompr} for an overview of all list comprehension functions.
#' 
#' @examples 
#' # sum up 1:i for i in 1:5
#' gen.named.list("sum_to_{x}", sum(1:x), x = 1:5)
#' 
#' # matrix with named columns and rows
#' gen.named.matrix("row{i}", gen.named.vector("col{j}", i+j, j = 1:3), i = 1:3)
#' 
#' # a matrix where the expression refers to the rows and not the columns
#' gen.named.matrix("col{i}", c(row1 = i, row2 = 10 * i, row3 = 100 * i), i = 1:10,
#'                  byrow = TRUE)
#' 
#' @export
gen.named.list <- function(str, expr, ...) {
  l <- substitute(list(...))
  str <- substitute(str)
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, OUTPUT_FORMAT[["LST"]], str, parent.frame()))
}

#' @rdname gen.named.list
#' @export
gen.named.vector <- function(str, expr, ...) {
  l <- substitute(list(...))
  str <- substitute(str)
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, OUTPUT_FORMAT[["VEC"]], str, parent.frame()))
}

#' @rdname gen.named.list
#' @export
gen.named.data.frame <- function(str, expr, ..., byrow = FALSE) {
  l <- substitute(list(...))
  str <- substitute(str)
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, if (byrow) OUTPUT_FORMAT[["DF_ROW"]] else OUTPUT_FORMAT[["DF"]], str, parent.frame()))
}

#' @rdname gen.named.list
#' @export
gen.named.matrix <- function(str, expr, ..., byrow = FALSE) {
  l <- substitute(list(...))
  str <- substitute(str)
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, if (byrow) OUTPUT_FORMAT[["MTX_ROW"]] else OUTPUT_FORMAT[["MTX"]], str, parent.frame()))
}

# ----- Expressions -----

#' Generate List and Vector Expressions with List Comprehension
#' 
#' @description 
#' 
#' Functions to transform a base expression containing free variables into a list or a vector of expressions,
#' based on variable ranges and additional conditions. 
#' 
#' @name gen.list.expr
#' @param expr A base expression which is partially evaluated for all combinations of variables. 
#'   It may still contain free variables.
#' @param str A character pattern, containing expressions to be evaluated in \{\}-brackets. 
#' @param ... Arbitrary many variable ranges and conditions.
#' 
#' @details 
#' 
#' See \code{\link{gen.list}} for more details on the \code{expr} and \code{...} parameters.
#' 
#' See \code{\link{gen.named.list}} for more details on the \code{str} parameter.
#' 
#' For variables with underscores additionally the evaluation of indices in \code{()}-brackets is supported.
#' For example, an expression \code{x_(i+1)} is evaluated as \code{x_3} for \code{i = 2}.
#' 
#' @return
#' 
#' Returns an expression containing a list or a vector which might be evaluated later.
#' The argument \code{expr} is partially evaluated, where all free variables are substituted for which a range is given.
#' The other variables remain untouched.
#' 
#' @seealso \code{\link{gen.list}} to generate lists,
#'   \code{\link{gen.named.list}} to generate named lists,  
#'   and \link{listcompr} for an overview of all list comprehension functions.
#' 
#' @examples
#' # An expression which is partially evaluated
#' gen.list.expr(a_i + 2 * i, i = 1:4)
#' 
#' # Generate an expression with placeholders a_i,
#' # generate data for a_1, ..., a_4 and finally evaluate it
#' expr <- gen.vector.expr(a_i + a_(j+1), i = 1:3, j = 1:3, i != j)
#' data <- gen.data.frame(c(a_1 = a_1, ..., a_4 = a_4), a_ = 1:2)
#' eval(expr, data)
#' 
#' @export
gen.list.expr <- function(expr, ...) {
  expr <- substitute(expr)
  l <- substitute(list(...))
  return(gen_list_internal(expr, l, OUTPUT_FORMAT[["LST_EXPR"]], NULL, parent.frame()))
}

#' @rdname gen.list.expr
#' @export
gen.vector.expr <- function(expr, ...) {
  expr <- substitute(expr)
  l <- substitute(list(...))
  return(gen_list_internal(expr, l, OUTPUT_FORMAT[["VEC_EXPR"]], NULL, parent.frame()))
}

#' @rdname gen.list.expr
#' @export
gen.named.list.expr <- function(str, expr, ...) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, OUTPUT_FORMAT[["LST_EXPR"]], str, parent.frame()))
}

#' @rdname gen.list.expr
#' @export
gen.named.vector.expr <- function(str, expr, ...) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, OUTPUT_FORMAT[["VEC_EXPR"]], str, parent.frame()))
}


#' Generate Logical Conditions with List Comprehension
#' 
#' @description 
#' 
#' Functions to compose and-/or-connected logical conditions, based on variable ranges and additional conditions.
#'
#' @param expr A base expression which is partially evaluated for all combinations of variables. It may still contain free variables.
#' @param ... Arbitrary many variable ranges and conditions.
#' 
#' @details
#' 
#' See \code{\link{gen.list}} for more details on the \code{expr} and \code{...} parameters.
#' 
#' For variables with underscores additionally the evaluation of indices in \code{()}-brackets is supported. For example, an expression \code{x_(i+1)} is evaluated as \code{x_3} for \code{i = 2}.
#' 
#' @return
#' 
#' Returns an expression \code{expr_1 & ... & expr_n} or \code{expr_1 | ... | expr_n} where \code{expr_i} is generated from \code{expr},
#' where all free variables are substituted for which a range is given. The other variables remain untouched.
#' 
#' The generated condition may be used within the the conditions of \code{\link{gen.list}} and similar functions from this package.
#' 
#' @seealso \code{\link{gen.list}} to generate lists and thereby make use of the generated logical conditions,
#'   and \link{listcompr} for an overview of all list comprehension functions.
#' 
#' @examples
#' # Returns a_1 == 1 & a_2 == 2 & a_3 == 3
#' gen.logical.and(a_i == i, i = 1:3)
#' 
#' # A data frame of tuples (x_1, x_2, x_3, x_4) summing up to 10 with x_i <= x_(i+1)
#' gen.data.frame(c(x_1, ..., x_4), x_ = 1:10, x_1 + ... + x_4 == 10, 
#'                gen.logical.and(x_i <= x_(i+1), i = 1:3))
#' 
#' # Get all permutations of 1:4
#' gen.data.frame(c(a_1, ..., a_4), a_ = 1:4, 
#'                gen.logical.and(a_i != a_j, i = 1:4, j = (i+1):4))
#'                
#' # Get again the permutations of 1:4, using filter from dplyr 
#' df <- gen.data.frame(c(a_1, ..., a_4), a_ = 1:4)
#' dplyr::filter(df, !!gen.logical.and(a_i != a_j, i = 1:3, j = (i+1):4))
#' 
#' @export
gen.logical.and <- function(expr, ...) {
  expr <- substitute(expr)
  l <- substitute(list(...))
  return(gen_logical_internal(expr, l, TRUE, parent.frame()))
}

#' @rdname gen.logical.and
#' @export
gen.logical.or <- function(expr, ...) {
  expr <- substitute(expr)
  l <- substitute(list(...))
  return(gen_logical_internal(expr, l, FALSE, parent.frame()))
}

