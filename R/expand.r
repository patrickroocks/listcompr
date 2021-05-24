# ----- Expression expansion helpers -------

# helper function for expand_expr, search for x_1, return list("x", 1)
match_var_num_ind <- function(str) {
  res <- regmatches(str, regexec("^([[:alpha:]][[:alnum:]]*)_([[:digit:]]+)$", str))[[1]]
  if (length(res) != 3 || is.na(!as.numeric(res[3]))) return(NULL)
  return(list(res[2], as.numeric(res[3])))
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
    stop(paste0("left hand side '", as.character(subexpr[2]), "' and right hand side '", as.character(expr[3]),
                "' of '", op1, " ... ", op1, "' expression are not valid for expansion"), call. = FALSE)
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
  return(list(expr = res_expr, varname_prefix = varname_prefix, varnames = varnames))
}


# convert f(a_1, ..., a_3) to f(a_1, a_2, a_3)
# call expand_expr recursively for f(g(...), h(...))
expand_nested_expr <- function(expr, vars, ctx) {
  i <- 1 # start at 2, increment immediately
  while (i < length(expr)) { # expr is changed while iterating!
    i <- i + 1
    if (expr[i] == quote(...()) && i >= 3 && i <= length(expr) - 1) { # search for FUNC(a_1, ..., a_n)
      start <- match_var_num_ind(as.character(expr[i-1]))
      end   <- match_var_num_ind(as.character(expr[i+1]))
      if (!check_start_end(start, end)) {
        stop(paste0("left hand side '", as.character(expr[i-1]), "' and right hand side '", as.character(expr[i+1]), 
                    "' of ', ...,' expression are not valid for expansion"), call. = FALSE)
      }
      varname_names_prefix <- NULL
      if (!is.null(names(expr)) && names(expr)[i-1] != "" && names(expr)[i+1] != "") {
        start_names <- match_var_num_ind(names(expr)[i-1])
        end_names   <- match_var_num_ind(names(expr)[i+1])
        if (!check_start_end(start_names, end_names)) {
          stop(paste0("left side '", names(expr)[i-1], "' and right side '", names(expr)[i+1], 
                      "' of the names of ', ...,' expression are not valid for expansion"), call. = FALSE)
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
          vars <- fill_vars_by_range(vars, varname, varname_prefix, ctx[["req_var_ranges"]])
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
    } else { # go into recursion, merge vars
      res <- expand_expr(expr[i][[1]], vars, ctx)
      expr[i][[1]] <- res[["expr"]]
      vars <- res[["vars"]]
    }
  }
  return(list(expr = expr, vars = vars))
}

# ----- Char compositions by patterns -----

# convert "x{a}" to paste0("x", a)
expand_char <- function(char, vars, ctx) { 
  
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
      if (!text_mode) {
        segment <- parse(text = paste0('{', segment, '}'))[[1]]
        res_expand <- expand_expr(segment, vars, ctx)
        vars    <- res_expand[["vars"]]
        segment <- res_expand[["expr"]]
      }
      segments <- c(segments, segment)
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
  
  # compose expression (single segment is converted to character)
  expr <- as.call(c(list(quote(paste0)), segments))
  return(list(expr = expr, vars = vars))
}

# ----- Main expr expansion -----

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
      vars <- fill_vars_by_range(vars, varname, varname_prefix, ctx[["req_var_ranges"]])
    }
  } else if (is.character(expr)) {
    if (grepl("{", expr, fixed = TRUE)) {
      return(expand_char(expr, vars, ctx))
    }
  } else if (!is.atomic(expr)) {
    match_result <- match_binary_dot_expr(expr)
    if (!is.null(match_result)) {
      expr <- match_result[["expr"]]
      varname_prefix <- match_result[["varname_prefix"]]
      subvars <- match_result[["subvars"]]
      for (sub_var_name in subvars) {
        vars <- fill_vars_by_range(vars, sub_var_name, varname_prefix, ctx[["req_var_ranges"]])
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
        expr <- gen_logical_internal(expr[2][[1]], lst_args, is_and, ctx[["parent_frame"]])
        res <- expand_expr(expr, vars, ctx)
        expr <- res[[1]]
        vars <- res[[2]]
      } else {
        if (expr[1] == quote(gen.vector.expr()) || expr[1] == quote(gen.named.vector.expr())) {
          output_format <- OUTPUT_FORMAT[["VEC_EXPR"]] 
        } else {
          output_format <- OUTPUT_FORMAT[["LST_EXPR"]]
        }
        has_names <- (expr[1] == quote(gen.named.list.expr()) || expr[1] == quote(gen.named.vector.expr()))
        base_expr <- if (has_names) expr[3][[1]] else expr[2][[1]]
        str_expr <- if (has_names) expr[2][[1]] else NULL
        if (has_names) lst_args[2] <- NULL

        expr <- gen_list_internal(base_expr, lst_args, output_format, str_expr, ctx[["parent_frame"]])
        res <- expand_expr(expr, vars, ctx)
        expr <- res[[1]]
        vars <- res[[2]]
      }
    } else {
      return(expand_nested_expr(expr, vars, ctx))
    }
  }
  return(list(expr = expr, vars = vars))
}

# ----- Insert names in lists and vectors ----- 

# add default names, e.g. convert quote(c(a = 1, b)) to quote(c(a = 1, b = 2))
insert_inner_names <- function(expr, is_format_df) {
  
  if (!is_format_df) {
    # no "auto names" for unnamed matrices, but fill the gaps in existing names (or in data frames - where we want to avoid auto names like "x..y")
    if (!(!is.null(names(expr)) || (length(expr) > 1 && as.character(expr[[1]]) == "data.frame"))) return(expr)
  }
  
  if (is.symbol(expr)) { # single symbol
    new_expr <- quote(c(X = X))
    names(new_expr)[2] <- as.character(expr)
    new_expr[2][[1]] <- expr
    return(new_expr)
  }
  if (length(expr) <= 1) return(expr) # nothing to detect
  if (!(as.character(expr[[1]]) %in% c("c", "list", "data.frame"))) return(expr)
  
  if (length(names(expr)) == length(expr) && sum(names(expr) == "") == 1) return(expr) # all names set
  
  # already set names (fill with empty strings if null)
  res_names <- names(expr)
  if (is.null(res_names)) res_names <- rep("", length(expr))
  
  # derive names from expr: accept only something like "a", not "1" (would be converted to "X1")
  # ensure that set names are persisted, quote(a = 1, a) stays at it is (final columns "a" and "V2" are intended)
  tmp_names <- as.character(expr)
  tmp_names[res_names != ""] <- res_names[res_names != ""]
  tmp_names[tmp_names != make.names(tmp_names, TRUE)] <- ""
  tmp_names[1] <- ""
  res_names[res_names == ""] <- tmp_names[res_names == ""]
  
  # remaining names are V1, V2, ...
  mask_unset_relevant <- (res_names == "")[2:length(res_names)]
  res_names[c(FALSE, mask_unset_relevant)] <- paste0("V", which(mask_unset_relevant))
  
  # final result
  names(expr) <- res_names
  return(expr)
}