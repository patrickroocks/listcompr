# ----- Variable range, conditions and Cartesian product -----

# helper for partial_eval, search for x_i, return list("x", "i")
match_var_expr_ind <- function(str) {
  res <- regmatches(str, regexec("^([[:alpha:]][[:alnum:]]*)_([[:alpha:]][[:alnum:]]*)$", str))[[1]]
  if (length(res) != 3) return(NULL)
  return(list(literal = res[2], index = res[3]))
}

# apply expr to Cartesian product, independent of return type (e.g. char or num)
apply_to_cartesian <- function(expr, cartesian_df, parent_frame) {
  rv <- eval(expr, cartesian_df[1,,drop=FALSE], parent_frame)
  if (nrow(cartesian_df) > 1) {
    rv <- c(rv, vapply(2:nrow(cartesian_df), function(i) eval(expr, cartesian_df[i,,drop=FALSE], parent_frame), rv))
  }
  return(rv)
}

# prepare variable limits for Cartesian product. turns "x=1:n, y=x:m" into "x=1:n,y=1:m, y>=x"
# allow substitutions like "y" in "x=1:n, y=2*x"
adjust_limits <- function(vars, parent_frame) {
  if (length(vars) == 0) return(list(vars = list(), add_conds = list(), sub_vars = list()))
  
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
  sub_vars <- list()
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
      # assume a substitution
      sub_vars[varname] <- as.expression(expr)
    }
  }
  return(list(vars = final_vars, add_conds = additional_conditions, sub_vars = sub_vars))
}

# ---- Cartesian product -----

# expands the variables+conditions to the Cartesian product
# expressions must have been expanded before
get_cartesian_df_after_expansion <- function(vars, conds, parent_frame) {
  
  # sort (x_, a, b, x_1, x_2) to (x_1, x_2, a, b), i.e., the "x_" variable defines the position
  # and the generic matching free vars like "x_" are removed
  while (TRUE) {
    # pick one generic (like "x_") if existing
    inds <- which(substr(names(vars), nchar(names(vars)), nchar(names(vars))) == '_')
    if (length(inds) == 0) break
    cur_ind <- inds[1]
    generic_name <- names(vars)[cur_ind]
    # preserve all others left/right of it, sort the generics
    ind_no_generic <- which(substr(names(vars), 1, nchar(generic_name)) != generic_name)
    ind_no_generic_left  <- ind_no_generic[ind_no_generic < cur_ind]
    ind_no_generic_right <- ind_no_generic[ind_no_generic > cur_ind]
    generic_vars <- vars[setdiff(which(substr(names(vars), 1, nchar(generic_name)) == generic_name), cur_ind)]
    vars <- c(vars[ind_no_generic_left], generic_vars[sort(names(generic_vars))], vars[ind_no_generic_right])
  }
  
  # adjust the limits (x = a:b, y = x:c) to (x = a:b, y = a:b, y <= x)
  res <- adjust_limits(vars, parent_frame)
  vars <- res[[1]]
  extra_conditions <- res[[2]]
  sub_vars <- res[[3]]
  
  expand_grid_args <- vars
  expand_grid_args[["stringsAsFactors"]] <- FALSE # for non-numeric vars
  
  # Cartesian product of free vars via expand.grid
  cartesian_df <- do.call(expand.grid, expand_grid_args, envir = parent_frame)
  
  # quick exit?
  if (nrow(cartesian_df) == 0) return(cartesian_df)
  
  # apply substitutions first, add them to the data frame
  if (length(sub_vars) >= 1) {
    for (i in 1:length(sub_vars)) {
      cartesian_df[names(sub_vars)[i]] <- apply_to_cartesian(sub_vars[[i]], cartesian_df, parent_frame)
    }
  }
  
  # Apply all conditions (given conditions + from adjusted limits), and-connect them
  conds <- c(conds, extra_conditions)
  if (length(conds) > 0) {
    # Non-vector-wise applier of a single condition
    condition_applier <- function(expr) {
      vapply(1:nrow(cartesian_df), function(i) eval(expr, cartesian_df[i,,drop=FALSE], parent_frame), TRUE)
    }
    cartesian_df <- cartesian_df[Reduce("&", lapply(conds, condition_applier), TRUE),,drop = FALSE]
  }
  
  return(list(cartesian_df = cartesian_df, vars = vars, conds = conds))
}

# Helper for evaluating expressions partially (in gen_logical_internal and gen_list_internal, if output is expr)
# (no parent_frame needed, evaluating "x" within "x_i" would be wrong!)
# evaluates only base operations on given "data", no lookup in parent environments!
# evaluates "x_(i+1)" to "x_2", preserves "x_((i+1))" as "x_(i+1)"
eval_partial <- function(expr, data) {
  # baseenv() is the right choice: contains operators like "+" (in contrast to emptyenv()) 
  # but no data sets like "iris" (contained in globalenv())
  # returns NULL if can't be evaluated yet
  res <- tryCatch(eval(expr, data, baseenv()), error = function(e) NULL)
  if (!is.null(res)) return(res)
  if (length(expr) == 1) {
    if (is.symbol(expr)) {
      res <- match_var_expr_ind(as.character(expr))
      if (!is.null(res)) {
        sub_res <- eval_partial(as.symbol(res[["index"]]), data)
        expr <- as.symbol(paste0(res[["literal"]], "_", as.character(sub_res)))
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

# ---- Small helper for gen_list_internal -----

# extract conditions and variables based on logical vectors which give the positions of conds/vars within "l"
# note that an "x_3" within a condition is allowed even it is not used in the main expression!
get_vars_and_conditions <- function(l, is_var, is_cond, ctx) {
  vars <- lapply(which(is_var), function(i) l[i][[1]])
  names(vars) <- names(l)[is_var]
  conds <- list()
  j <- 0
  for (i in which(is_cond)) {
    j <- j + 1
    res <- expand_expr(l[[i]], vars, ctx)
    conds[[j]] <- res[["expr"]]
    vars <- res[["vars"]]
  }
  return(list(vars = vars, conds = conds))
}

# searches for 2-dim matrix without names/conditions, e.g. gen.matrix(i+j, i=1:3, j=1:4)
check_2d_matrix <- function(first_row, is_by_row, vars, conds, parent_frame) {
  if (!(    is.null(colnames(first_row))
         && is.null(rownames(first_row))
         && length(first_row) == 1
         && length(vars) == 2 && length(conds) == 0)) return(NULL)
  
  rowindex <- if (is_by_row) 2 else 1
  nrow <- tryCatch(eval(vars[[rowindex]], parent_frame), error = function(e) NULL)
  if (is.null(row)) return(NULL)
  
  colindex <- if (is_by_row) 1 else 2
  ncol <- tryCatch(eval(vars[[colindex]], parent_frame), error = function(e) NULL)
  if (is.null(ncol)) return(NULL)
  
  return(list(nrow = length(nrow), ncol = length(ncol)))
}

check_one_row <- function(val)  {
  if (nrow(val) > 1) {
    stop(paste0("the inner expression was evaluated to a data frame with ", nrow(val), " rows, but expected exactly one row"), call. = FALSE)
  }
}

# convert vector/list to data.frame
make_df_row <- function(val, byrow) {
  if (!is.null(nrow(val))) {
    if (is.data.frame(val)) {
      res <- val
    } else {
      # assume matrix or similar
      res <- as.data.frame(val)
    }
  } else {
    # assume vector, list, or similar
    res_names <- names(val)
    if (is.null(res_names)) res_names <- rep("", length(val))
    if (!any(res_names == "")) {
      res <- as.list(val)
    } else { # insert V{i} names
      mask_unset <- (res_names == "")
      res_names[mask_unset] <- paste0("V", which(mask_unset))
      names(val) <- res_names
      res <- as.list(val)
    }
    res <- as.data.frame(res, stringsAsFactors = FALSE)
  }
  
  # check if we have just one row after conversion to data frame
  check_one_row(res)
  
  # transpose if inner elements of val shall be rows
  if (byrow) res <- as.data.frame(t(res), stringsAsFactors = FALSE)
  
  return(res)
}

make_mtx_row <- function(val, is_by_row) {
  if (!is.null(nrow(val))) {
    check_one_row(val)
    res <- val[1,,drop=FALSE]
  } else {
    res <- val
  }
  
  # depending on list/vector/data frame we get either a column or row vector...
  res <- as.matrix(unlist(res), rownames.force = FALSE)
  
  # ... which we bring into the correct format now
  if (   ( is_by_row && (ncol(res) > 1 || !is.null(colnames(res))))
      || (!is_by_row && (nrow(res) > 1 || !is.null(rownames(res)))) ) res <- t(res)
  
  return(res)
}

# ---- Fold executors -----

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

# ----- Main functions (called by interface) -----

OUTPUT_FORMAT <- list(LST = 1, VEC = 2, LST_EXPR = 3, VEC_EXPR = 4, DF = 5, DF_ROW = 6, MTX = 7, MTX_ROW = 8)

gen_list_internal <- function(expr, l, output_format, name_str_expr, parent_frame) {
  
  # * preliminary checks, find vars/conditions
  
  expr <- expr # raise error if not existing
  if (is.null(names(l))) {
    stop("no named variables are given, expected at least one named variable in the '...' parameters", call. = FALSE)
  }
  
  is_cond_or_var <- c(FALSE, rep(TRUE, length(names(l)) - 1))
  is_cond <- is_cond_or_var & (names(l) == "")
  is_var <- is_cond_or_var & !is_cond
  ctx <- list(parent_frame = parent_frame, req_var_ranges = TRUE)
  res <- get_vars_and_conditions(l, is_var, is_cond, ctx)
  vars <- res[["vars"]]
  conds <- res[["conds"]]
  
  is_format_expr <- output_format %in% c(OUTPUT_FORMAT[["LST_EXPR"]], OUTPUT_FORMAT[["VEC_EXPR"]])
  is_format_vec  <- output_format %in% c(OUTPUT_FORMAT[["VEC"]],      OUTPUT_FORMAT[["VEC_EXPR"]])
  is_format_lst  <- output_format %in% c(OUTPUT_FORMAT[["LST"]],      OUTPUT_FORMAT[["LST_EXPR"]])
  is_format_mtx  <- output_format %in% c(OUTPUT_FORMAT[["MTX"]],      OUTPUT_FORMAT[["MTX_ROW"]])
  is_format_df   <- output_format %in% c(OUTPUT_FORMAT[["DF"]],       OUTPUT_FORMAT[["DF_ROW"]])
  is_by_row      <- output_format %in% c(OUTPUT_FORMAT[["MTX_ROW"]],  OUTPUT_FORMAT[["DF_ROW"]])
  
  # * Expression expansions
  
  ctx <- list(parent_frame = parent_frame, req_var_ranges = !is_format_expr)
  res <- expand_expr(expr, vars, ctx)
  expr <- res[["expr"]]
  vars <- res[["vars"]]
  
  # * Name expansions
  
  has_row_names <- !is.null(name_str_expr)
  if (has_row_names) { # for named list/vectors/...
    if (is.symbol(name_str_expr)) name_str_expr <- eval(name_str_expr, parent_frame)
    res <- expand_expr(name_str_expr, vars, ctx)
    name_str_expr <- res[["expr"]]
    vars          <- res[["vars"]]
  }
  
  # * Get Cartesian product
  
  res_cart <- get_cartesian_df_after_expansion(vars, conds, parent_frame)
  cartesian_df <- res_cart[["cartesian_df"]]
  vars         <- res_cart[["vars"]]
  conds        <- res_cart[["conds"]]
  
  if (nrow(cartesian_df) == 0) {
    warning("no variable ranges detected, returning empty result", call. = FALSE)
    if      (is_format_df)  return(data.frame())
    else if (is_format_mtx) return(matrix())
    else if (is_format_lst) return(list())
    else                    return(NULL)
  }
  
  # * Get names and create lambda for setting names later
  
  if (has_row_names) {
    name_vec <- vapply(1:nrow(cartesian_df), function(i) eval(name_str_expr, cartesian_df[i,,drop=FALSE], parent_frame), "")
  }
  insert_outer_names <- function(res) {
    if (has_row_names) {
      if (is_by_row) colnames(res) <- name_vec
      else           rownames(res) <- name_vec
    }
    return(res)
  }
  
  # * Apply expression and return
  if (output_format %in% c(OUTPUT_FORMAT[["LST"]], OUTPUT_FORMAT[["VEC"]])) {
    if (is_format_vec) {
      rv <- apply_to_cartesian(expr, cartesian_df, parent_frame)
    } else {
      rv <- lapply(1:nrow(cartesian_df), function(i) eval(expr, cartesian_df[i,,drop=FALSE], parent_frame))
    }
    if (has_row_names) names(rv) <- name_vec
    return(rv)
    
  } else if (is_format_expr) {
    rv <- if (is_format_vec) quote(c()) else quote(list())
    for (i in 1:nrow(cartesian_df)) {
      rv[[i+1]] <- eval_partial(expr, cartesian_df[i,,drop=FALSE])
    }
    if (has_row_names) names(rv) <- c("", name_vec)
    return(rv)
    
  } else if (is_format_df || is_format_mtx) {
    expr <- insert_inner_names(expr, is_format_df)
    
    if (is_format_df) {
      rv_df <- lapply(1:nrow(cartesian_df), function(i) make_df_row(eval(expr, cartesian_df[i,,drop=FALSE], parent_frame), is_by_row))
      rv_df <- do.call((if (is_by_row) "cbind" else "rbind"), rv_df)
      return(insert_outer_names(rv_df))
    } else { # matrix
      rv_list <- lapply(1:nrow(cartesian_df), function(i) make_mtx_row(eval(expr, cartesian_df[i,,drop=FALSE], parent_frame), is_by_row))
      rv_list_begin <- rv_list[[1]]
      rv_mtx <- do.call((if (is_by_row) "cbind" else "rbind"), rv_list)
      res_mtx <- if (!has_row_names) check_2d_matrix(rv_list_begin, is_by_row, vars, conds, parent_frame) else NULL
      if (is.null(res_mtx)) {
        return(insert_outer_names(rv_mtx))
      } else {
        return(matrix(rv_mtx, nrow = res_mtx[["nrow"]], ncol = res_mtx[["ncol"]], byrow = is_by_row))
      }
    }
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
      stop("no named variables are given, expected at least one named variable in the '...' parameters", call. = FALSE)
    }
  }
  
  ctx <- list(parent_frame = parent_frame, req_var_ranges = FALSE)
  
  # Find vars/conditions
  is_cond_or_var <- c(FALSE, rep(TRUE, length(names(l)) - 1))
  is_cond <- is_cond_or_var & (names(l) == "")
  is_var <- is_cond_or_var & !is_cond
  res <- get_vars_and_conditions(l, is_var, is_cond, ctx)
  vars  <- res[["vars"]]
  conds <- res[["conds"]]
  
  # * start calculations
  res <- expand_expr(expr, vars, ctx)
  expr <- res[["expr"]]
  vars <- res[["vars"]]
  cartesian_df <- get_cartesian_df_after_expansion(vars, conds, parent_frame)[["cartesian_df"]]
  if (nrow(cartesian_df) == 0) return(expression())
  
  # Apply expression and return, no parent_frame in eval_partial (evaluating "x" in "x_i" would be wrong!)
  lst_res <- lapply(1:nrow(cartesian_df), function(i) eval_partial(expr, cartesian_df[i,,drop=FALSE]))
  if (is_and) {
    return(fold.and(lst_res))
  } else {
    return(fold.or(lst_res))
  }
}
