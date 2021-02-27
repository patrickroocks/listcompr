# ----------------------------- List, Vectors, Data Frames ----------------------------------------

#' Generate Lists, Vectors and Data Frames with List Comprehension
#' 
#' @description
#' 
#' Functions to transform a base expression containing free variables into a list, a vector, or a data frame
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
#'    \item For \code{gen.data.frame} a (named) vector or list is expected which describes one row of the data frame.
#'   }
#'   Within \code{expr} it is allowed to use functions and predefined constants from the parent environment.
#' @param ... Arbitrary many variable ranges and conditions.
#'   For all free variables occurring in \code{expr} a range must be assigned, e.g., \code{x = 1:3, y = 1:5} for an expression \code{x + y}. 
#'   At least one variable range is required.
#'   The ranges may depend on each other, e.g., \code{x = 1:3, y = x:3} is allowed.
#'   The generated values can be further restricted by conditions (like \code{x <= y}).
#' 
#' @return 
#' 
#' The result of \code{gen.list} is a list (a vector for \code{gen.vector}) containing an entry for each combination of the free variables (i.e., the Cartesian product), where all the free variables in \code{expr} are substituted.
#' The function \code{gen.vector} returns a vector while \code{gen.list} may contain also more complex substructures (like vectors or lists).
#' 
#' The output of \code{gen.data.frame} is a data frame where each substituted \code{expr} entry is one row.
#' The base expression \code{expr} should contain a (named) vector or list, such that each entry of this vector becomes a column of the returned data frame.
#' If the vector contains a single literal without a name, this is taken as column name. For instance, \code{gen.data.frame(a, a = 1:5)} returns the same as \code{gen.data.frame(c(a = a), a = 1:5)}.
#' Default names 'V1', 'V2', ... are used, if no names are given and names can't be automatically detected.
#' 
#' All expressions and conditions are applied to each combination of the free variables separately, i.e., they are applied row-wise and not vector-wise. 
#' For instance, the term \code{sum(x,y)} (within \code{expr} or a condition) is equivalent to \code{x+y}.
#' 
#' @section Syntactic Features: 
#' 
#' There are several syntactic features to be used in variable ranges, conditions, and expressions.
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
#' @seealso \code{\link{gen.list.expr}} to generate expressions to be evaluated later, 
#'   \code{\link{gen.list.char}} to generate lists of characters, 
#'   and \link{listcompr} for an overview of all list comprehension functions.
#' 
#' 
#' @examples 
#' # Compose 10, 11, 20, 21, 22, 30, ..., 33, ..., 90, ..., 99 into a vector
#' gen.vector(x * 10 + y, x = 1:9, y = 1:x)
#' 
#' # A data frame of all tuples (x_1, x_2, x_3) of whole positive numbers, summing up to 10
#' gen.data.frame(c(x_1, ..., x_3), x_ = 1:10, x_1 + ... + x_3 == 10)
#' 
#' # A data.frame containing the numbers in 2:20 and the sum of their divisors
#' gen.data.frame(c(num = a, sumdiv = sum(gen.vector(x, x = 1:(a-1), a %% x == 0))), 
#'                a = 2:20)
#' 
#' # Return perfect numbers between 2 and 100 (number equals the sum of divisors)
#' gen.vector(a, a = 2:100, a == sum(gen.vector(x, x = 1:(a-1), a %% x == 0)))
#' 
#' @export
gen.list <- function(expr, ...) {
  expr <- substitute(expr)
  l <- substitute(list(...))
  return(gen_list_internal(expr, l, FALSE, OUTPUT_FORMAT$NUM, NULL, parent.frame()))
}

#' @rdname gen.list
#' @export
gen.vector <- function(expr, ...) {
  expr <- substitute(expr)
  l <- substitute(list(...))
  return(gen_list_internal(expr, l, TRUE, OUTPUT_FORMAT$NUM, NULL, parent.frame()))
}
#' @rdname gen.list
#' @export
gen.data.frame <- function(expr, ...) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, FALSE, OUTPUT_FORMAT$DF, NULL, parent.frame()))
}

# ----------------------------- Characters / Named Structures ----------------------------------------

#' Generate Characters and Named Lists, Vectors and Data Frames with List Comprehension
#' 
#' @description
#' 
#' Functions to transform patterns with placeholders into characters or into names of lists, vectors, or data frames,
#' based on variable ranges and additional conditions.
#' 
#' @name gen.list.char
#' 
#' @param str A character, containing expressions to be evaluated in \code{\{\}}-brackets, e.g., \code{"a{x}"} is transformed into \code{"a1"} for \code{x = 1}. 
#'   Double brackets are transformed into a single bracket without evaluating the inner expression.
#'   For instance, \code{"var{x + 1}_{{a}}"} is transformed into \code{"var2_{a}"} for \code{x = 1}.
#' @param expr A base expression containing free variables which is evaluated for all combinations of variables. 
#' @param ... Arbitrary many variable ranges and conditions.
#' 
#' @details 
#' 
#' The free variables in the inner expressions (i.e., the content of the \code{\{\}}-brackets) of \code{expr} are evaluated in the same way as expressions in \code{\link{gen.list}}.
#' 
#' See \code{\link{gen.list}} for more details on the \code{expr} and \code{...} parameters.
#' 
#' @return 
#' 
#' The functions \code{gen.list.char} and \code{gen.vector.char} return lists and vectors of characters.
#' 
#' The functions \code{gen.named.list}, \code{gen.named.vector}, \code{gen.named.data.frame} return lists, vectors, and data frames.
#' The work very similar to their counterparts without ".named".
#' Additionally the vector of characters, induced by \code{str}, serves as a vector of names for the generated structures. 
#' In case of lists or vectors, the result is a named list or a named vector. For data frames, the names are taken as row names.
#' 
#' @seealso \code{\link{gen.list}} for explanations on list and vector comprehension,
#'   and \link{listcompr} for an overview of all list comprehension functions.
#' 
#' @examples 
#' # sum up 1:i for i in 1:5
#' gen.named.list("sum_to_{x}", sum(1:x), x = 1:5)
#' 
#' # same as above, but return as text
#' gen.list.char("sum of 1 to {x} is {sum(1:x)}", x = 1:5)
#' 
#' @export
gen.list.char <- function(str, ...) {
  l <- substitute(list(...))
  return(gen_list_internal(str, l, FALSE, OUTPUT_FORMAT$CHAR, NULL, parent.frame()))
}

#' @rdname gen.list.char
#' @export
gen.vector.char <- function(str, ...) {
  l <- substitute(list(...))
  return(gen_list_internal(str, l, TRUE, OUTPUT_FORMAT$CHAR, NULL, parent.frame()))
}

#' @rdname gen.list.char
#' @export
gen.named.list <- function(str, expr, ...) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, FALSE, OUTPUT_FORMAT$NUM, str, parent.frame()))
}

#' @rdname gen.list.char
#' @export
gen.named.vector <- function(str, expr, ...) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, TRUE, OUTPUT_FORMAT$NUM, str, parent.frame()))
}

#' @rdname gen.list.char
#' @export
gen.named.data.frame <- function(str, expr, ...) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, FALSE, OUTPUT_FORMAT$DF, str, parent.frame()))
}

# ----------------------------- Expressions ----------------------------------------

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
#' @seealso \code{\link{gen.data.frame}} to generate data frames, 
#'   \code{\link{gen.list}} to generate lists, 
#'   \code{\link{gen.list.char}} to generate lists of characters, 
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
  return(gen_list_internal(expr, l, FALSE, OUTPUT_FORMAT$EXPR, NULL, parent.frame()))
}

#' @rdname gen.list.expr
#' @export
gen.vector.expr <- function(expr, ...) {
  expr <- substitute(expr)
  l <- substitute(list(...))
  return(gen_list_internal(expr, l, TRUE, OUTPUT_FORMAT$EXPR, NULL, parent.frame()))
}

#' @rdname gen.list.expr
#' @export
gen.named.list.expr <- function(str, expr, ...) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, FALSE, OUTPUT_FORMAT$EXPR, str, parent.frame()))
}

#' @rdname gen.list.expr
#' @export
gen.named.vector.expr <- function(str, expr, ...) {
  l <- substitute(list(...))
  expr <- substitute(expr)
  return(gen_list_internal(expr, l, TRUE, OUTPUT_FORMAT$EXPR, str, parent.frame()))
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
#' # Get all permutations of 1:4
#' gen.data.frame(c(a_1, ..., a_4), a_ = 1:4, gen.logical.and(a_i != a_j, i = 1:4, j = (i+1):4))
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



# ----- Expression expansion & helpers -------

# helper function for expand_expr, search for x_1, return list("x", 1)
match_var_num_ind <- function(str) {
  res <- regmatches(str, regexec("^([[:alpha:]][[:alnum:]]*)_([[:digit:]]+)$", str))[[1]]
  if (length(res) != 3 || is.na(!as.numeric(res[3]))) return(NULL)
  return(list(res[2], as.numeric(res[3])))
}

# helper for partial_eval, search for x_i, return list("x", "i")
match_var_expr_ind <- function(str) {
  res <- regmatches(str, regexec("^([[:alpha:]][[:alnum:]]*)_([[:alpha:]][[:alnum:]]*)$", str))[[1]]
  if (length(res) != 3) return(NULL)
  return(list(res[2], res[3]))
}

# check lhs/rhs parts of "x_1" (start) and "x_4" (end). Symbol must be the same, numbers must be different
check_start_end <- function(start, end) {
  return(!is.null(start) && !is.null(end) 
         && start[[1]] == end[[1]]
         && start[[2]] != end[[2]])
}

# if a_ has a range X (e.g. a_ = 1:4), assign the same range to a_1, a_2, ..., a_n
fill_vars_by_range <- function(vars, varname, varname_prefix, req_var_ranges) {
  if (!(varname %in% names(vars) || varname_prefix %in% names(vars))) {
    if (req_var_ranges) {
      stop(paste0("no range for variable '", varname, "' found (also looked for '", varname_prefix, "')"), call. = FALSE)
    } else {
      return(vars)
    }
  }
  if (!(varname %in% names(vars))) {
    vars[[varname]] <- vars[[varname_prefix]]
  }
  return(vars)
}

# convert "a_1 + ... + a_4" into "a_1 + a_2 + a_3 + a_4"
match_binary_dot_expr <- function(expr) {
  # check for valid expression, return NULL if not
  if (is.atomic(expr)) return(NULL)
  if (length(expr) != 3) return(NULL) # not binary
  op1 <- expr[1][[1]]
  if (!is.symbol(op1)) return(NULL)
  subexpr <- expr[2][[1]] # (a_1 + ...)()
  if (length(subexpr) != 3) return(NULL)
  if (subexpr[3] != quote(...())) return(NULL)
  op2 <- expr[1][[1]]
  if (!is.symbol(op2)) return(NULL)
  
  # from now on we assume a "..." expression! (and return errors according to this)
  if (op1 != op2) {
    stop(paste0("different operators '", as.character(op1), "' and '", as.character(op2), " on both side of '...'"), call. = FALSE)
  }
  
  # get start/end
  start <- match_var_num_ind(as.character(subexpr[2]))
  end   <- match_var_num_ind(as.character(expr[3]))
  if (!check_start_end(start, end)) {
    stop(paste0("left hand side '", as.character(subexpr[2]), "' and right hand side '", as.character(expr[3]), "' of '", op1, " ... ", op1, "' expression are not valid for expansion"), call. = FALSE)
  }
  varname_prefix <- paste0(start[[1]], "_")
  
  # compose the folded expression, "a_1 + a_2 + a_3 + a_4" -> Reduce("+", list(a_2, a_3, a_4), a_1)
  res_expr <- quote(Reduce(NULL, list(), NULL))
  res_expr[[2]] <- op1
  res_expr[[4]] <- as.name(paste0(varname_prefix, start[[2]]))
  count <- 0
  varnames <- character(0)
  for (i in start[[2]]:end[[2]]) {
    count <- count + 1
    varname <- paste0(varname_prefix, as.character(i))
    varnames <- c(varnames, varname)
    if (count >= 2) res_expr[[3]][count][[1]] <- as.name(varname)
  }
  return(list(res_expr, varname_prefix, varnames))
}

# put i_ = a:b into (i_1 = a:b, ..., i_n = a:b) for all occurrences of i_j
# put FUNC(a_1, ..., a_n) into FUNC(a_1, a_2, ..., a_n) (fully expanded n-ary expressions)
# put also a_1 + ... + a_n into Reduce('+', list(a_2, a_3), a_1) (fully expanded binary repeated expressions)
# gets expression and vars (name -> range), returns tuple(modified expression, modified vars)
# ctx is a list of "parent_frame" and "req_var_ranges"
expand_expr <- function(expr, vars, ctx) {
  if (is.symbol(expr)) {
    varname <- as.character(expr)
    match_result <- match_var_num_ind(varname)
    if (!is.null(match_result)) {
      varname_prefix <- paste0(match_result[[1]], "_")
      vars <- fill_vars_by_range(vars, varname, varname_prefix, ctx$req_var_ranges)
    }
  } else if (!is.atomic(expr)) {
    match_result <- match_binary_dot_expr(expr)
    if (!is.null(match_result)) {
      expr <- match_result[[1]]
      varname_prefix <- match_result[[2]]
      subvars <- match_result[[3]]
      for (sub_var_name in subvars) {
        vars <- fill_vars_by_range(vars, sub_var_name, varname_prefix, ctx$req_var_ranges)
      }
    } else if (expr[1] == quote(gen.logical.and()) || expr[1] == quote(gen.logical.or()) ||
               expr[1] == quote(gen.vector.expr()) || expr[1] == quote(gen.named.vector.expr())  ||
               expr[1] == quote(gen.list.expr())   || expr[1] == quote(gen.named.list.expr())) {
      # call expression-generating list comprehension function
      if (length(expr) == 1) stop(paste0("missing arguments in '", as.character(as.expression(expr)), "'"), call. = FALSE)
      lst_args <- expr
      lst_args[1] <- quote(list())
      lst_args[2] <- NULL # remove base expression / name expression
      if (expr[1] == quote(gen.logical.and()) || expr[1] == quote(gen.logical.or())) {
        is_and <- (expr[1] == quote(gen.logical.and()))
        expr <- gen_logical_internal(expr[2][[1]], lst_args, is_and, ctx$parent_frame)
        res <- expand_expr(expr, vars, ctx)
        expr <- res[[1]]
        vars <- res[[2]]
      } else {
        use_vec <- (expr[1] == quote(gen.vector.expr()) || expr[1] == quote(gen.named.vector.expr()))
        has_names <- (expr[1] == quote(gen.named.list.expr()) || expr[1] == quote(gen.named.vector.expr()))
        base_expr <- if (has_names) expr[3][[1]] else expr[2][[1]]
        str_expr <- if (has_names) expr[2][[1]] else NULL
        if (has_names) lst_args[2] <- NULL
        
        expr <- gen_list_internal(base_expr, lst_args, use_vec, OUTPUT_FORMAT$EXPR, str_expr, ctx$parent_frame)
        res <- expand_expr(expr, vars, ctx)
        expr <- res[[1]]
        vars <- res[[2]]
      }
    } else {
      i <- 1 # start at 2, increment immediately
      while (i < length(expr)) { # expr is changed while iterating!
        i <- i + 1
        if (expr[i] == quote(...()) && i >= 3 && i <= length(expr) - 1) { # search for FUNC(a_1, ..., a_n)
          start <- match_var_num_ind(as.character(expr[i-1]))
          end   <- match_var_num_ind(as.character(expr[i+1]))
          if (!check_start_end(start, end)) {
            stop(paste0("left hand side '", as.character(expr[i-1]), "' and right hand side '", as.character(expr[i+1]), "' of ', ...,' expression are not valid for expansion"), call. = FALSE)
          }
          varname_names_prefix <- NULL
          if (!is.null(names(expr)) && names(expr)[i-1] != "" && names(expr)[i+1] != "") {
            start_names <- match_var_num_ind(names(expr)[i-1])
            end_names   <- match_var_num_ind(names(expr)[i+1])
            if (!check_start_end(start_names, end_names)) {
              stop(paste0("left side '", names(expr)[i-1], "' and right side '", names(expr)[i+1], "' of the names of ', ...,' expression are not valid for expansion"), call. = FALSE)
            }
            if (abs(end_names[[2]] - start_names[[2]]) != abs(end[[2]] - start[[2]])) {
              stop(paste0("the name range '", names(expr)[i-1], ", ..., ", names(expr)[i+1], "' has a different length ",
                          "than the expression range '", as.character(expr[i-1]), ", ..., ", as.character(expr[i+1]), "'"), call. = FALSE)
            }
            varname_names_prefix <- paste0(start_names[[1]], "_")
          }
          
          if (abs(end[[2]] - start[[2]]) == 1) { # silly case, "x_1, ..., x_2", just remove the dots
            expr[i] <- NULL
            i <- i - 1 # loop control
          } else { # expansion
            # expand inner
            varname_prefix <- paste0(start[[1]], "_")
            new_expr <- expr
            count <- i
            inner_range <- start[[2]]:end[[2]] # may be "-3, -2, -1"
            inner_range <- inner_range[2:(length(inner_range)-1)]
            if (!is.null(varname_names_prefix)) {
              inner_range_names <- start_names[[2]]:end_names[[2]]
              inner_range_names <- inner_range_names[2:(length(inner_range_names)-1)]
            }
            for (k in 1:length(inner_range)) {
              varname <- paste0(varname_prefix, as.character(inner_range[k]))
              new_expr[[count]] <- as.name(varname)
              if (!is.null(varname_names_prefix)) names(new_expr)[count] <- paste0(varname_names_prefix, as.character(inner_range_names[k]))
              vars <- fill_vars_by_range(vars, varname, varname_prefix, ctx$req_var_ranges)
              count <- count + 1
            }
            # append remainder (including right side of ... expression)
            for (k in (i+1):length(expr)) {
              new_expr[count] <- expr[k]
              names(new_expr)[count] <- names(expr)[k]
              count <- count + 1
            }
            expr <- new_expr
            # re-adjust loop control
            i <- i + length(inner_range) - 1 # skip "..." (thus -1), insert length of inner_range
          }
        } else if (!is.atomic(expr[i][[1]])) { # go into recursion, merge vars
          res <- expand_expr(expr[i][[1]], vars, ctx)
          expr[i][[1]] <- res[[1]]
          vars <- res[[2]]
        }
      }
    }
  }
  return(list(expr, vars))
}

# add default names, e.g. convert quote(c(a = 1, b)) to quote(c(a = 1, b = 2))
insert_names <- function(expr) {
  if (is.symbol(expr)) { # single symbol
    new_expr <- quote(c(X = X))
    names(new_expr)[2] <- as.character(expr)
    new_expr[2][[1]] <- expr
    return(new_expr)
  }
  if (length(expr) <= 1 || (expr[[1]] != quote(c) && expr[[1]] != quote(list))) return(expr) # nothing to detect
  if (length(names(expr)) == length(expr) && sum(names(expr) == "") == 1) return(expr) # all names set
  
  # already set names
  res_names <- names(expr)
  if (is.null(res_names)) res_names <- rep("", length(expr))
  
  # derive names from expr: accept only something like "a", not "1" (would be converted to "X1")
  # ensure that set names are persisted, quote(a = 1, a) stays at it is (final columns "a" and "V2" are intended)
  tmp_names <- as.character(expr)
  tmp_names[res_names != ""] <- res_names[res_names != ""]
  tmp_names[tmp_names != make.names(tmp_names, TRUE)] <- ""
  tmp_names[1] <- ""
  
  # fill unset names
  res_names[res_names == ""] <- tmp_names[res_names == ""]
  names(expr) <- res_names
  return(expr)
}

# ----- Variable range, conditions and Cartesian product -------

# prepare variable limits for Cartesian product. turns "x=1:n, y=x:m" into "x=1:n,y=1:m, y>=x"
adjust_limits <- function(vars, parent_frame) {
  
  if (length(vars) == 0) return(list(list(), list()))
  
  # check if some of the variable names is contained as a symbol in the function
  contains_varnames <- function(expr, local_varnames) {
    if (length(expr) == 1) {
      return(is.symbol(expr) && as.character(expr) %in% local_varnames)
    } else {
      # assume function, evaluate arguments recursively
      for (i in 1:length(expr)) {
        if (contains_varnames(expr[i][[1]], local_varnames)) return(TRUE)
      }
      return(FALSE)
    }
  }
  
  # evaluate expr, don't accept free vars
  try_evaluate <- function(expr, local_varnames, dataframe) {
    if (contains_varnames(expr, local_varnames)) return(NULL)
    return(tryCatch(eval(expr, dataframe, parent_frame), error = function(e) NULL))
  }
  
  create_range_expr <- function(startval, stopval) {
    range_expr <- quote(NULL:NULL)
    range_expr[2][[1]] <- startval
    range_expr[3][[1]] <- stopval
    return(range_expr)
  }
  
  varnames <- names(vars)
  additional_conditions <- list()
  final_vars <- list()
  fixed_vals <- list()
  starts <- list()
  stops <- list()
  for (i in 1:length(vars)) { # i <- 2
    var <- vars[i]
    varname <- names(var)
    expr <- var[[1]]
    
    show_err <- function(detail_err) {
      stop(paste0("could not evaluate variable range of '", varname, "', got '", as.character(as.expression(expr)), 
                  "', ", detail_err), call. = FALSE)
    }
    
    # returns NULL if it cannot be evaluated yet
    evaled_expr <- try_evaluate(expr, varnames, fixed_vals)
    if (!is.null(evaled_expr)) {
      if (length(evaled_expr) == 1) { # another fixed val, add to fixed_vals AND final_vars
        fixed_vals[[varname]] <- evaled_expr
        final_vars[[varname]] <- evaled_expr
        varnames <- setdiff(varnames, varname) # can be savely evaluated now!
      } else {
        if (is.numeric(evaled_expr)) {
          # check for i=start:end (valid for start/stop)
          # evaluation in baseenv suffices, as operands are already evaluated!
          range_expr <- create_range_expr(evaled_expr[1], evaled_expr[length(evaled_expr)])
          if (identical(eval(range_expr, baseenv()), evaled_expr)) {
            # don't accept i=3:1 (== [3,2,1]), then j=i:2 could not be transformed to conditions. accept seq(1,5,2) (== [1,3,5])
            starts[varname] <- evaled_expr[1]
            stops[varname] <- evaled_expr[length(evaled_expr)]
          }
        }
        final_vars[[varname]] <- expr # don't replace expression by 1:n
      } 
    } else if (length(expr) == 3 && expr[[1]] == quote(`:`)) {
      # do not replace in seq(...), step distance 2 can't be translated in conditions for cartesian product
      # no var range found yet, assume that the expression contains free vars
      lhs <- expr[[2]]
      rhs <- expr[[3]]
      # lhs
      evaled_start_expr <- try_evaluate(lhs, varnames, fixed_vals)
      if (is.null(evaled_start_expr)) {
        evaled_start_expr <- try_evaluate(lhs, setdiff(varnames, names(starts)), c(fixed_vals, starts))
        if (is.null(evaled_start_expr)) show_err('evaluation of left hand side failed')
        cond_expr <- quote(NULL <= NULL)
        cond_expr[2][[1]] <- lhs
        cond_expr[3][[1]] <- as.symbol(varname)
        additional_conditions <- c(additional_conditions, list(cond_expr))
      }
      # rhs
      evaled_stop_expr <- try_evaluate(rhs, varnames, fixed_vals)
      if (is.null(evaled_stop_expr)) {
        evaled_stop_expr <- try_evaluate(rhs, setdiff(varnames, names(stops)), c(fixed_vals, stops))
        if (is.null(evaled_stop_expr)) show_err('evaluation of right hand side failed')
        cond_expr <- quote(NULL <= NULL)
        cond_expr[2][[1]] <- as.symbol(varname)
        cond_expr[3][[1]] <- rhs
        additional_conditions <- c(additional_conditions, list(cond_expr))
      }
      # generate start/stop
      if (evaled_stop_expr < evaled_start_expr) show_err("start <= end violated")
      starts[varname] <- evaled_start_expr
      stops[varname]  <- evaled_stop_expr
      final_vars[[varname]] <- create_range_expr(evaled_start_expr, evaled_stop_expr)
    } else {
      show_err("did not find ':' operator")
    }
  }
  return(list(final_vars, additional_conditions))
}

get_cartesian_df_after_expansion <- function(vars_lst, cond_lst, parent_frame) {
  
  # sort (x_, a, b, x_1, x_2) to (x_1, x_2, a, b), i.e., the "x_" variable defines the position
  # and the generic matching free vars like "x_" are removed
  while (TRUE) {
    # pick one generic (like "x_") if existing
    inds <- which(substr(names(vars_lst), nchar(names(vars_lst)), nchar(names(vars_lst))) == '_')
    if (length(inds) == 0) break
    cur_ind <- inds[1]
    generic_name <- names(vars_lst)[cur_ind]
    # preserve all others left/right of it, sort the generics
    ind_no_generic <- which(substr(names(vars_lst), 1, nchar(generic_name)) != generic_name)
    ind_no_generic_left  <- ind_no_generic[ind_no_generic < cur_ind]
    ind_no_generic_right <- ind_no_generic[ind_no_generic > cur_ind]
    generic_vars <- vars_lst[setdiff(which(substr(names(vars_lst), 1, nchar(generic_name)) == generic_name), cur_ind)]
    vars_lst <- c(vars_lst[ind_no_generic_left], generic_vars[sort(names(generic_vars))], vars_lst[ind_no_generic_right])
  }
  
  # adjust the limits (x = a:b, y = x:c) to (x = a:b, y = a:b, y <= x)
  res <- adjust_limits(vars_lst, parent_frame)
  vars_lst <- res[[1]]
  extra_conditions <- res[[2]]
  
  vars_lst[["stringsAsFactors"]] <- FALSE # for non-numeric vars

  # Cartesian product of free vars via expand.grid
  cartesian_df <- do.call(expand.grid, vars_lst, envir = parent_frame)
  
  # Non vector-wise applier of a single condition
  applier <- function(expr) {
    vapply(1:nrow(cartesian_df), function(i) eval(expr, cartesian_df[i,,drop=FALSE], parent_frame), TRUE)
  }
  
  # Apply all conditions (given conditions + from adjusted limits), and-connect them
  lst_args <- c(cond_lst, extra_conditions)
  if (length(lst_args) > 0) {
    cartesian_df <- cartesian_df[Reduce("&", lapply(lst_args, applier), TRUE),,drop = FALSE]
  }
  
  return(cartesian_df) 
}

# Helper for evaluating expressions partially (in gen_logical_internal and gen_list_internal, if output is expr)
# (no parent_frame needed, evaluating "x" within "x_i" would be wrong!)
# evaluates only base operations on given "data", no lookup in parent environments!
# evaluates "x_(i+1)" to "x_2", preserves "x_((i+1))" as "x_(i+1)"
eval_partial <- function(expr, data) {
  # baseenv() is the right choice: contains operators like "+" (in contrast to emptyenv()) but no data sets like "iris" (contained in globalenv())
  # returns NULL if can't be evaluated yet
  res <- tryCatch(eval(expr, data, baseenv()), error = function(e) NULL)
  if (!is.null(res)) return(res)
  if (length(expr) == 1) {
    if (is.symbol(expr)) {
      res <- match_var_expr_ind(as.character(expr))
      if (!is.null(res)) {
        sub_res <- eval_partial(as.symbol(res[[2]]), data)
        expr <- as.symbol(paste0(res[[1]], "_", as.character(sub_res)))
      }
    }
    return(expr) # assume evaluation is done
  }
  
  # protect function calls like "x_((i))"
  is_underscore_index <- FALSE
  if (length(expr) == 2) {
    callstr <- as.character(expr[1][[1]])
    index_expr <- expr[2][[1]]
    is_embraced_index <- length(index_expr) == 2 && index_expr[1][[1]] == quote(`(`)
    if (!is_embraced_index && nchar(callstr) >= 2 && substring(callstr, nchar(callstr)) == "_") {
      is_underscore_index <- TRUE
    }
  }
  
  # assume function, evaluate arguments recursively
  for (i in 2:length(expr)) {
    expr[i][[1]] <- eval_partial(expr[i][[1]], data)
  }
  
  # transform "function call" x_(1) into x_1, index expression was evaluated above
  if (is_underscore_index) {
    index_expr <- expr[2][[1]]
    if (is.numeric(index_expr)) {
      expr <- as.symbol(paste0(callstr, as.character(index_expr)))
    }
  }
  
  return(expr)
}

# extract conditions and variables based on logical vectors which give the positions of conds/vars within "l"
# note that an "x_3" within a condition is allowed even it is not used in the main expression!
get_vars_and_conditions <- function(l, is_var, is_cond, ctx) {
  vars_lst <- lapply(which(is_var), function(i) l[i][[1]])
  names(vars_lst) <- names(l)[is_var]
  cond_lst <- list()
  j <- 0
  for (i in which(is_cond)) {
    j <- j + 1
    res <- expand_expr(l[[i]], vars_lst, ctx)
    cond_lst[[j]] <- res[[1]]
    vars_lst <- res[[2]]
  }
  return(list(vars_lst, cond_lst))
}

fold.and <- function(lst) {
  if (length(lst) == 0) return(TRUE)
  if (length(lst) == 1) return(lst[[1]])
  expr <- quote(NULL & NULL)
  expr[2][[1]] <- lst[[1]]
  expr[3][[1]] <- fold.and(lst[-1])
  return(expr)
}

fold.or <- function(lst) {
  if (length(lst) == 0) return(FALSE)
  if (length(lst) == 1) return(lst[[1]])
  expr <- quote(NULL | NULL)
  expr[2][[1]] <- lst[[1]]
  expr[3][[1]] <- fold.or(lst[-1])
  return(expr)
}

# ------------- Char compositions by patterns--------------------

prepare_char_pattern <- function(char, vars, ctx) { 

  if (!is.character(char) || length(char) != 1) {
    stop(paste0("expected character of length 1, got type '", typeof(char), "' of length ", length(char)), call. = FALSE)
  }
  
  segments <- list()
  pos <- 1
  len_char <- nchar(char)
  next_char <- substr(char, 1, 1)
  text_mode <- TRUE
  segment_begin <- 1
  count_open_brackets <- 0
  
  add_segment <- function() {
    segment_end <- pos - 1
    if (segment_begin <= segment_end) {
      segment <- substr(char, segment_begin, segment_end)
      # put brackets around code to allow "return"
      if (!text_mode) segment <- parse(text = paste0('{', segment, '}'))[[1]]
      res <- expand_expr(segment, vars, ctx)
      vars <- res[[2]]
      segments <- c(segments, list(res[[1]]))
    }
    segment_begin <- pos + 1
    text_mode <- !text_mode
    return(list(vars, segments, segment_begin, text_mode))
  }
  
  show_err <- function(details, pos) {
    stop(paste0("could not parse pattern '", char, "', ", details, " at position ", pos), call. = FALSE)
  }
  
  while(pos <= len_char) {
    cur_char <- next_char
    next_char <- substr(char, pos + 1, pos + 1)
    if (text_mode) {
      if (cur_char == '{') {
        if (next_char == '{') {
          pos <- pos + 1
          next_char <- substr(char, pos + 1, pos + 1)
        } else {
          res <- add_segment()
          vars <- res[[1]]
          segments <- res[[2]]
          segment_begin <- res[[3]]
          text_mode <- res[[4]]
        }
      } else if (cur_char == '}') {
        if (next_char == '}') {
          pos <- pos + 1
          next_char <- substr(char, pos + 1, pos + 1)
        } else {
          show_err("found unexpected '}'", pos)
        }
      }
    } else { # expr mode
      if (cur_char == '{') {
        count_open_brackets <- count_open_brackets + 1
      } else if (cur_char == '}') {
        if (count_open_brackets > 0) {
          count_open_brackets <- count_open_brackets - 1
        } else {
          res <- add_segment()
          vars <- res[[1]]
          segments <- res[[2]]
          segment_begin <- res[[3]]
          text_mode <- res[[4]]
        }
      }
    }
    pos <- pos + 1
  }
  if (!text_mode) show_err("did not find end of expression starting", segment_begin - 1)
  res <- add_segment()
  vars <- res[[1]]
  segments <- res[[2]]
  segment_begin <- res[[3]]
  text_mode <- res[[4]]
  
  return(list(segments, vars))
}

eval_char_pattern <- function(char_pattern, data, parent_frame) {
  return(paste(vapply(char_pattern, function(segment) {
      if (is.character(segment)) return(segment)
      evaled <- eval(segment, data, parent_frame)
      evaled_char <- as.character(evaled)
      if (length(evaled_char) == 1) return(evaled_char)
      deparsed <- deparse(evaled, width.cutoff = 500, control = c("keepNA", "niceNames", "showAttributes"))
      if (length(deparsed) != 1) stop("could not convert expression into char (too complex or too long expression?)", call. = FALSE)
      return(deparsed)
    }, ''), collapse = ''))
}


# ------------- Main functions (called by interface) --------------------

OUTPUT_FORMAT <- list(NUM = 1, EXPR = 2, CHAR = 3, DF = 4)

gen_list_internal <- function(expr, l, use_vec, output_format, name_str, parent_frame) {
  
  # preliminary checks
  expr <- expr # raise error if not existing
  if (is.null(names(l))) {
    stop("no named variables are given, expected at least one named variable for creating range in the '...' parameters", call. = FALSE)
  }

  # Find vars/conditions
  is_cond_or_var <- c(FALSE, rep(TRUE, length(names(l)) - 1))
  is_cond <- is_cond_or_var & (names(l) == "")
  is_var <- is_cond_or_var & !is_cond
  ctx <- list(parent_frame = parent_frame, req_var_ranges = TRUE)
  res <- get_vars_and_conditions(l, is_var, is_cond, ctx)
  vars_lst <- res[[1]]
  cond_lst <- res[[2]]
  
  # expand expression based on vars (will expand vars)
  is_expr_input <- output_format == OUTPUT_FORMAT$EXPR
  ctx <- list(parent_frame = parent_frame, req_var_ranges = !is_expr_input)
  if (output_format == OUTPUT_FORMAT$CHAR) {
    res <- prepare_char_pattern(expr, vars_lst, ctx)
    char_pattern_expr <- res[[1]]
    vars_lst <- res[[2]]
  } else {
    res <- expand_expr(expr, vars_lst, ctx)
    expr <- res[[1]]
    vars_lst <- res[[2]]
  }
  if (!is.null(name_str)) {
    res <- prepare_char_pattern(name_str, vars_lst, ctx)
    char_pattern_name <- res[[1]]
    vars_lst <- res[[2]]
  }
  
  # get Cartesian df for those vars/conditions
  cartesian_df <- get_cartesian_df_after_expansion(vars_lst, cond_lst, parent_frame)
  if (nrow(cartesian_df) == 0) {
    warning("no variable ranges detected, returning empty result", call. = FALSE)
    if (output_format == OUTPUT_FORMAT$DF) return(data.frame())
    else {
      if (use_vec) return(numeric(0)) else return(list())
    }
  }
  
  # generate names
  if (!is.null(name_str)) {
    name_vec <- vapply(1:nrow(cartesian_df), function(i) eval_char_pattern(char_pattern_name, cartesian_df[i,,drop=FALSE], parent_frame), '')
  }
  
  # * Apply expression and return
  if (output_format == OUTPUT_FORMAT$NUM) {
    if (use_vec) {
      rv <- eval(expr, cartesian_df[1,,drop=FALSE], parent_frame)
      if (nrow(cartesian_df) > 1) {
        rv <- c(rv, vapply(2:nrow(cartesian_df), function(i) eval(expr, cartesian_df[i,,drop=FALSE], parent_frame), rv))
      }
    } else {
      rv <- lapply(1:nrow(cartesian_df), function(i) eval(expr, cartesian_df[i,,drop=FALSE], parent_frame))
    }
    if (!is.null(name_str)) names(rv) <- name_vec
    return(rv)
    
  } else if (output_format == OUTPUT_FORMAT$EXPR) {
    rv <- if (use_vec) quote(c()) else quote(list())
    for (i in 1:nrow(cartesian_df)) {
      rv[[i+1]] <- eval_partial(expr, cartesian_df[i,,drop=FALSE])
    }
    if (!is.null(name_str)) names(rv) <- c("", name_vec)
    return(rv)
    
  } else if (output_format == OUTPUT_FORMAT$CHAR) {
    apply_func <- { if (use_vec) function(X, FUN) vapply(X, FUN, '') 
                    else         function(X, FUN) lapply(X, FUN) }
    return(apply_func(1:nrow(cartesian_df), function(i) eval_char_pattern(char_pattern_expr, cartesian_df[i,,drop=FALSE], parent_frame)))
    
  } else if (output_format == OUTPUT_FORMAT$DF) {
    if (is.null(name_str)) expr <- insert_names(expr)
    rv_list <- lapply(1:nrow(cartesian_df), function(i) eval(expr, cartesian_df[i,,drop=FALSE], parent_frame))
    if (!is.null(name_str)) names(rv_list) <- name_vec
    return(as.data.frame(do.call("rbind", rv_list)))
  }
}


# return something with fold.and(...) or fold.or(...)
gen_logical_internal <- function(expr, l, is_and, parent_frame) { 
  
  # * checks and extract data
  if (is.null(names(l))) {
    if (length(l) == 1) {
      # Return expr (boring...), no parent_frame in expr
      return(expr)
    } else {
      stop("no named variables are given, expected at least one named variable for creating range in the '...' parameters", call. = FALSE)
    }
  }
  
  ctx <- list(parent_frame = parent_frame, req_var_ranges = FALSE)
  
  # Find vars/conditions
  is_cond_or_var <- c(FALSE, rep(TRUE, length(names(l)) - 1))
  is_cond <- is_cond_or_var & (names(l) == "")
  is_var <- is_cond_or_var & !is_cond
  res <- get_vars_and_conditions(l, is_var, is_cond, ctx)
  vars_lst <- res[[1]]
  cond_lst <- res[[2]]
  
  # * start calculations
  res <- expand_expr(expr, vars_lst, ctx)
  expr <- res[[1]]
  vars_lst <- res[[2]]
  cartesian_df <- get_cartesian_df_after_expansion(vars_lst, cond_lst, parent_frame)
  if (nrow(cartesian_df) == 0) return(expression())
  
  # Apply expression and return, no parent_frame in eval_partial (evaluating "x" in "x_i" would be wrong!)
  lst_res <- lapply(1:nrow(cartesian_df), function(i) eval_partial(expr, cartesian_df[i,,drop=FALSE]))
  if (is_and) return(fold.and(lst_res)) else return(fold.or(lst_res))
}

