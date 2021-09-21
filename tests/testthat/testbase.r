suppressPackageStartupMessages(library(dplyr))

test_that("Trivial tests", {
  expect_error(gen.list(), "argument \"expr\" is missing")
  expect_error(gen.vector(), "argument \"expr\" is missing")
  expect_error(gen.data.frame(), "argument \"expr\" is missing")
  expect_error(gen.list(1), "no named variables are given")
  expect_error(gen.vector("a"), "no named variables are given")
  expect_error(gen.data.frame(1), "no named variables are given")
  x <- 1
  expect_equal(gen.list(c(x, y), y = 2), list(c(1, 2)))
  expect_equal(gen.list(x, x = 2), list(2))
  expect_equal(gen.list(a, a = 1, b = NULL), list(1))
})


test_that("Empty result tests", {
  expect_warning(gen.vector(x + y, x = 1, y = 2:3, x > y), "result is empty, conditions are too restrictive")
  expect_warning(gen.data.frame(c(a = 1, b = x), x = 2:3, x > 3), "result is empty, conditions are too restrictive")
  expect_warning(gen.list(a, a = NULL), "result is empty, variable range is NULL")
  
})

test_that("Wrong parametrizations", {
  expect_error(gen.list(x + y, y = x:2, x = 1:2), "could not evaluate variable range of 'y'")
  expect_error(gen.list(a, a = list(1,2)), "unexpected value for variable 'a', expecting a vector of atomics")
})

test_that("Basic list and vector tests", {
  expect_equal(gen.list(x, x = 1:3), lapply(1:3, identity))
  
  y <- 1
  expect_equal(gen.vector(x + y, x = 1:3), 2:4)
  expect_equal(gen.data.frame(x + y, x = 1:2, y = 1:2), data.frame(V1 = c(2, 3, 3, 4)))
  expect_equal(gen.list(c(x, y), x = 1:2, y = x:2), list(c(1, 1), c(1, 2), c(2, 2)))
  expect_equal(gen.vector(sum(c(x_1, ..., x_4) * c(1, -1)), x_ = 1:2), c(0, 1, -1, 0, 1, 2, 0, 1, -1, 0, -2, -1, 0, 1, -1, 0))
  
  expect_equal(gen.list(list(x_1, ..., x_2), x_ = 1:2), list(list(1, 1), list(2, 1), list(1, 2), list(2,2)))
  
  expect_equal(gen.list(c(x_1, x_2), x_ = 1:2, x_1 == x_3, x_2 == x_3), list(c(1, 1), c(2,2)))
  expect_equal(gen.list(c(x_1, x_2), x_ = 1:2, x_1 == x_3 && x_2 == x_3), list(c(1, 1), c(2,2)))
  expect_equal(gen.vector(x_1 + x_2, x_ = (1:3)*10, x_1 = 1:2), c(11, 12, 21, 22, 31, 32))
})

test_that("Logical tests", {
  expect_equal(gen.logical.or(x_i == x_(i+1), i = 1:2, j = i:2), quote(x_1 == x_2 | (x_1 == x_2 | x_2 == x_3)))
  expect_equal(gen.logical.and(x_i == x_j, i = 1:3, j = (i+1):3), quote(x_1 == x_2 & (x_1 == x_3 & x_2 == x_3)))
  expect_equal(gen.logical.and(x_i == x_j, i = 1:3, j = i:3, i != j), quote(x_1 == x_2 & (x_1 == x_3 & x_2 == x_3)))
  expect_equal(gen.logical.and(x_(i_1) == x_(i_2), i_ = 1:3, i_1 < i_2), quote(x_1 == x_2 & (x_1 == x_3 & x_2 == x_3)))
})

test_that("Logical list tests", {
  permutations <- gen.list(c(x_1, ..., x_4), x_ = 1:4, gen.logical.and(x_i != x_j, i = 1:4, j = (i+1):4))
  expect_equal(length(permutations), factorial(4))
  expect_equal(length(unique(permutations)), factorial(4))
  expect_equal(vapply(permutations, function(x) length(unique(x)), 0), rep(4, factorial(4)))
  
  expect_equal(gen.data.frame(c(a = x_1, b = x_2, c = x_3), x_ = 1:2, gen.logical.or(x_i != x_j, i=1:3, j=1:3)),
               data.frame(a = c(2,1,2,1,2,1), b = c(1,2,2,1,1,2), c = c(1,1,1,2,2,2)))
  
  expect_equal(gen.list(c(x_1, ..., x_4), x_ = 1:2, gen.logical.and(x_i == x_j, i = 1:4, j=(i+1):4)), list(c(1,1,1,1),c(2,2,2,2)))
  
  expect_equal(gen.list(c(x_1, ..., x_4), x_ = 0:-1, gen.logical.and(x_(i_1) == x_(i_2), i_ = 4:1, i_1 > i_2)), list(c(0,0,0,0),c(-1,-1,-1,-1)))
})


test_that("Basic data frame tests", {
  expect_equal(gen.data.frame(c(a = x, b = y), x = 1:3, y = 1:3, x < y), data.frame(a = c(1, 1, 2), b = c(2, 3, 3)))
  expect_equal(gen.data.frame(c(a = a_1 + 10, b = a_2), a_ = 1:3, a_2 != 2),
               data.frame(a = c(11, 12, 13, 11, 12, 13), b = c(1, 1, 1, 3, 3, 3)))
  
  expect_equal(gen.data.frame(c(a = x_1, b = x_2), x_ = 1:2, x_1 == x_3, x_2 == x_3), data.frame(a = c(1, 2), b = c(1, 2)))
  
  expect_equal(gen.data.frame(c(a = a, sumdiv = sum(gen.vector(x, x = 1:(a-1), a %% x == 0))), a = 2:10), 
               data.frame(a = c(2, 3, 4, 5, 6, 7, 8, 9, 10), sumdiv = c(1, 1, 3, 1, 6, 1, 7, 4, 8)))
  
  expect_equal(gen.data.frame(c(a, 1 + 2), a = 1:2), data.frame(a = c(1, 2), V2 = 3))
  expect_equal(gen.data.frame(c(x = a, b), a = 1:2, b = 1), data.frame(x = c(1, 2), b = 1))
  expect_equal(gen.data.frame(c(b = a, b), a = 1:2, b = 1), data.frame(b = c(1, 2), V2 = 1))
  expect_equal(gen.data.frame(a, a = 1:5),  data.frame(a = 1:5))
  
  expect_equal(gen.data.frame(list(a = i, b = "x_{i}"), i = 1:2, byrow = TRUE),
               structure(list(V1 = c("1", "x_1"), V1 = c("2", "x_2")), class = "data.frame", row.names = c("a",  "b")))
  
  expect_error(gen.data.frame(data.frame(a = c(1,2)), i = 1:2),
               "the inner expression was evaluated to a data frame with 2 rows, but expected exactly one row")
  
  expect_equal(gen.data.frame(c(a = c(1, 2) * i), i = 1:3),
               data.frame(a1 = c(1, 2, 3), a2 = c(2, 4, 6)))
  
  expect_error(gen.data.frame(list(a = c(1, 2) * i), i = 1:3),
               "the inner expression was evaluated to a data frame with 2 rows, but expected exactly one row")
})

test_that("Named lists/vectors/dataframes tests", {
  
  expect_equal(gen.named.list("a_{i}", i, i = 1:4), list(a_1 = 1, a_2 = 2, a_3 = 3, a_4 = 4))
  
  expect_equal(gen.data.frame(c(a_1 = x_1,..., a_3 = x_3), x_ = 1:2),
               data.frame(a_1 = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), a_2 = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), a_3 = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L)))

  expect_equal(gen.named.vector("a{10+a}", 2*a, a=1:2), c(a11 = 2, a12 = 4))
  
  expect_equal(gen.named.list("sum({a_1}, {a_2}, {a_3}, {a_4})", sum(a_1, ..., a_4), a_ = 1:2, a_1 + ... + a_4 <= 5),
               list("sum(1, 1, 1, 1)" = 4, "sum(2, 1, 1, 1)" = 5, "sum(1, 2, 1, 1)" = 5, "sum(1, 1, 2, 1)" = 5, "sum(1, 1, 1, 2)" = 5))
  
  expect_equal(gen.named.list.expr("a_{i}", a_i, i = 1:5), quote(list(a_1 = a_1, a_2 = a_2, a_3 = a_3, a_4 = a_4, a_5 = a_5)))
  
  expect_equal(gen.named.vector.expr(paste0("v", "{v_1}"), v_1, v_ = 1:2), quote(c(v1 = 1L, v2 = 2L)))

  expect_equal(gen.data.frame(gen.named.vector.expr(paste0("x", "a_{i}"), a_i, i = 1:3), a_ = 1:2),
               data.frame(xa_1 = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), xa_2 = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), xa_3 = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L)))
  
  expect_equal(gen.data.frame(gen.named.list(paste0("x", "a_{i}"), a_i, i = 1:3), a_ = 1:2),
               data.frame(xa_1 = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), xa_2 = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), xa_3 = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L)))
  
  expect_equal(gen.named.data.frame("col_{i}", 10 * i + c(a = 1, b = 2), i = 1:2),
               data.frame(a = c(11, 21), b = c(12, 22), row.names = c("col_1",  "col_2")))
  
  expect_equal(gen.named.data.frame("col_{a}", c(a, b), a = 1:2, b = 1),
               data.frame(a = c(1, 2), b = c(1, 1), row.names = c("col_1",  "col_2")))
  
  expect_equal(gen.data.frame(list(a = i, b = "x{j}"), j = 0:2, i = 1:2),
               structure(list(a = c(1L, 1L, 1L, 2L, 2L, 2L), b = c("x0", "x1", "x2", "x0", "x1", "x2")), class = "data.frame", row.names = c(NA,-6L)))
  
  expect_equal(gen.named.data.frame("{i}", list(i, n = "x{j}"), j = 0:2, i = 1:2, byrow = TRUE),
               structure(list("1" = c("1", "x0"), "1" = c("1", "x1"), "1" = c("1",  "x2"), "2" = c("2", "x0"), "2" = c("2", "x1"), "2" = c("2", "x2" )), class = "data.frame", row.names = c("i", "n")))
  
  expect_equal(gen.list(gen.named.list("res_{i}x{j}", i * j, j = i:2), i = 1:2), list(list(res_1x1 = 1, res_1x2 = 2), list(res_2x2 = 4)))

  expect_equal(gen.list(gen.named.vector("res_{i}x{j}", i * j, j = i:2), i = 1:2), list(c(res_1x1 = 1, res_1x2 = 2), c(res_2x2 = 4)))
  
  str <- "x{i}"
  expect_equal(gen.data.frame(gen.named.vector(str, i+j, i = 1:2), j = 1:2),
               structure(list(x1 = 2:3, x2 = 3:4), class = "data.frame", row.names = c(NA,  -2L)))
  
  expect_equal(gen.named.vector(str, i, i = 1:2), c(x1 = 1, x2 = 2))
  
  expect_equal(gen.data.frame(gen.named.list('a_{j}', j * i, j = 1:2), i = 1:3),
               data.frame(a_1 = 1:3, a_2 = c(2L, 4L, 6L)))

  expect_equal(gen.named.data.frame("x{n}", gen.named.data.frame(paste0(a_1, ..., a_3), sum(a_1, ..., a_3, n)[1], a_ = 1:2, byrow = TRUE), n = 1:2),
               structure(list("111" = 4:5, "211" = 5:6, "121" = 5:6, "221" = 6:7,      "112" = 5:6, "212" = 6:7, "122" = 6:7, "222" = 7:8), row.names = c("x1",  "x2"), class = "data.frame"))
})

test_that("three dots tests", {
  expect_equal(gen.data.frame(c(i_1, ..., i_3), i_ = 1:2), expand.grid(list(i_1 = 1:2, i_2 = 1:2, i_3 = 1:2), KEEP.OUT.ATTRS = FALSE))
  expect_equal(gen.vector(sum(c(i_1, ..., i_4)), i_ = 0:1), c(0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4))
  expect_equal(gen.vector(sum(i_1, ..., i_4),    i_ = 0:1), c(0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4))
  
  expect_equal(gen.list(c(0, val_1, ..., val_4, 100), val_ = 1:5, val_1 < val_2, val_2 < val_3, val_3 < val_4),
               list(c(0, 1, 2, 3, 4, 100), c(0, 1, 2, 3, 5, 100),  c(0, 1, 2, 4, 5, 100), c(0, 1, 3, 4, 5, 100), c(0, 2, 3, 4, 5, 100)))
  
  expect_equal(gen.list(c(a_3, ..., a_1, a_3 + ... + a_1, a_3 - ... - a_1), a_ = 1:5, 1 + (a_1 + ... + a_3) + 1 == 7),
               list(c(1, 1, 3, 5, -3), c(1, 2, 2, 5, -3), c(1, 3, 1, 5, -3), c(2, 1, 2, 5, -1), c(2, 2, 1, 5, -1), c(3, 1, 1, 5, 1)))
  
  expect_equal(gen.data.frame(c(a1 = a_1, a2 = a_2, a3 = a_3, a4 = a_4), a_ = 1:3, a_1 + ... + a_4 == 5),
               data.frame(a1 = c(2, 1, 1, 1), a2 = c(1, 2, 1, 1), a3 = c(1, 1, 2, 1), a4 = c(1, 1, 1, 2)))
  
  expect_equal(gen.data.frame(c(a_1 = a_1, ..., a_4 = a_4), a_ = 1), data.frame(a_1 = 1, a_2 = 1, a_3 = 1, a_4 = 1))
  
  expect_error(gen.data.frame(c(a_1 = a_2, ..., a_4 = a_4), a_ = 1:2),
               "the name range 'a_1, ..., a_4' has a different length than the expression range 'a_2, ..., a_4'")
})

test_that("nesting tests", {
  expect_equal(gen.vector(a, a = 2:100, a == sum(gen.vector(x, x = 1:(a-1), a %% x == 0))), c(6, 28))
})
  
test_that("helper function tests", {
  f <- function(x) { x %% 3 == 0 }
  expect_equal(gen.vector(x, x = 1:10, f(x)), c(3, 6, 9))
  g <- function(a, b) { 10 * a + b }
  expect_equal(gen.vector(g(a_1, a_2), a_ = 1:10, f(a_1), a_2 %% 4 == 0), c(34, 64, 94, 38, 68, 98))
  x <- 1
  y <- 1
  expect_equal(gen.data.frame(c(a = g(x, y)), x = 2:(2+f(3))), data.frame(a = c(21, 31)))
})

test_that("lambda function test", {
  expect_equal(gen.data.frame(c(num = a, sumdiv = {sum(gen.vector(x, x = 1:(a-1), a %% x == 0))}), a = 3:6),
               data.frame(num = c(3, 4, 5, 6), sumdiv = c(1, 3, 1, 6)))
})

test_that("chained start/stop tests", {
  expect_equal(gen.list(c(a, b, c), a = 1:3, b = a:3, c = a:b), 
               list(c(1, 1, 1), c(1, 2, 1), c(1, 3, 1), c(1, 2, 2), c(2, 2, 2), c(1, 3, 2), c(2, 3, 2), c(1, 3, 3), c(2, 3, 3), c(3, 3, 3)))
  expect_equal(gen.vector(100 * x + 10 * y + z, x = 1:2, y = x:2, z = y:2), c(111, 112, 122, 222))
  expect_equal(gen.data.frame(c(a = x, b = y), x = 1:2, y = x:3), data.frame(a = c(1, 1, 2, 1, 2), b = c(1, 2, 2, 3, 3)))
  x <- 1
  y <- 1 # may not influence the list comprehension
  expect_equal(gen.data.frame(c(x, y), x = 1:2, y = x:3), data.frame(x = c(1, 1, 2, 1, 2), y = c(1, 2, 2, 3, 3)))
})

test_that("seq tests", {
  expect_equal(gen.vector(x + y, x = seq(10, 30, 10), y = seq(1, 2, 0.5)), c(11.0, 21.0, 31.0, 11.5, 21.5, 31.5, 12.0, 22.0, 32.0))
  expect_equal(gen.list(c(x,y), x = seq(1, 3), y = x:2), list(c(1,1),c(1,2),c(2,2)))
  # do not substitute within seq!
  expect_error(gen.list(c(x,y), x = seq(1, 3), y = seq(x, 3)), "values must be length 3")
})

test_that("expression tests", {
  expect_equal(gen.list.expr(x, x = 1:3), quote(list(1L,2L,3L)))
  expect_equal(gen.vector.expr(a_i, i = 1:5), quote(c(a_1, a_2, a_3, a_4, a_5)))
  expect_equal(gen.list(gen.vector.expr(a_i, i = 1:3), a_ = 1:2), list(c(1, 1, 1), c(2, 1, 1), c(1, 2, 1), c(2, 2, 1), c(1, 1, 2), c(2, 1, 2), c(1, 2, 2), c(2, 2, 2)))
  expect_equal(gen.list.expr(a_(i+1), i = 1:3), quote(list(a_2, a_3, a_4)))
  expect_equal(gen.vector.expr(a_(if (i<=2) i else 10), i = 1:3), quote(c(a_1, a_2, a_10)))
  expect_equal(gen.vector.expr(a_((i)), i = 1:2), quote(c(a_(1L), a_(2L))))
  expect_equal(gen.list.expr(c(x_1, ..., x_5, a), a = 1), quote(list(c(x_1, x_2, x_3, x_4, x_5, 1))))
})


test_that("character tests", {
  expect_equal(gen.list("a{i}_{2*i}", i = 1:3), list("a1_2", "a2_4", "a3_6"))
  expect_equal(gen.vector("{if (i==1) { 'a' } else 'b'}{i}", i = 1:3), c("a1", "b2", "b3"))
  
  x <- 1
  expect_equal(gen.vector("{i}{j}, {x}", i = 1:2, j = i:2), c("11, 1", "12, 1", "22, 1"))
  expect_equal(gen.vector("{x+y}", x = 10:11, y = x:11), c("20", "21", "22"))
  
  expect_equal(gen.vector("{i+1}_{i}", i = 1:2), c("2_1", "3_2"))
  expect_equal(gen.vector("{{a}}", i = 1:2), c("{{a}}", "{{a}}"))
})

test_that("special type tests", {
  expect_equal(gen.data.frame(list(x = as.difftime("0:{i}:30")), i = 1:5), 
               structure(list(x = structure(c(1.5, 2.5, 3.5, 4.5, 5.5), class = "difftime", units = "mins")), row.names = c(NA,  -5L), class = "data.frame"))
  expect_equal(gen.data.frame(as.difftime("0:{i}:30"), i = 1:5),
               structure(list(V1 = structure(c(1.5, 2.5, 3.5, 4.5, 5.5), class = "difftime", units = "mins")), row.names = c(NA,  -5L), class = "data.frame"))
  
})

test_that("non-numeric test", {
  expect_equal(gen.vector(m, m = month.abb, substr(m, 1, 1) == "J"), c("Jan", "Jun", "Jul"))
  expect_equal(gen.list(m, m = month.abb, substr(m, 1, 1) == "J"), list("Jan", "Jun", "Jul"))
})

test_that("sorting", {
  expect_equal(gen.data.frame(c(x_1 = x_1, x_2 = x_2), x_ = 1:2, x_2 = 1:2), data.frame(x_1 = c(1L, 2L, 1L, 2L), x_2 = c(1L, 1L, 2L, 2L)))
  expect_equal(gen.data.frame(c(a = a, b = b), a = 1:2, b = 1:2), data.frame(a = c(1L, 2L, 1L, 2L), b = c(1L, 1L, 2L, 2L)))
  expect_equal(gen.data.frame(c(a = a, b = b), b = 1:2, a = 1:2), data.frame(a = c(1L, 1L, 2L, 2L), b = c(1L, 2L, 1L, 2L)))
  expect_equal(gen.data.frame(c(x_1 = x_1, x_2 = x_2, a = a), x_ = 1:2, a = 1:2, x_1 = 1:2), 
               data.frame(x_1 = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), x_2 = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), a = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L)))
})

test_that("substitutions", {
  expect_equal(gen.data.frame(c(i, j), i = 0:3, j = 2 * i), data.frame(i = c(0, 1, 2, 3), j = c(0, 2, 4, 6)))
  expect_equal(gen.data.frame(c(i, j), i = 0:3, j = 2 * i, j < 6), data.frame(i = c(0, 1, 2), j = c(0, 2, 4)))
})

test_that("matrix standard tests", {
  expect_equal(gen.matrix(gen.vector(i+j, i = 1:2), j = 1:3), matrix(c(2, 3, 3, 4, 4, 5), ncol = 2, byrow = TRUE))
  expect_equal(gen.matrix(c(1, a), a = 1:2), matrix(c(1, 1, 1, 2), ncol = 2, byrow = TRUE))
  expect_equal(gen.matrix(c(1, a = a), a = 1:2), matrix(c(1, 1, 1, 2), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("V1", "a"))))
  expect_equal(gen.named.matrix("row{a}", c(1, a = a), a = 1:2), matrix(c(1, 1, 1, 2), ncol = 2, byrow = TRUE, dimnames = list(c("row1", "row2"), c("V1", "a"))))
  
  expect_equal(gen.matrix(i+j, i=1:2, j=i:2),  matrix(c(2, 3, 4), ncol = 1))
  expect_equal(gen.matrix(c(a = 10*i+j), i=1:2, j=1:3), matrix(c(11, 21, 12, 22, 13, 23), ncol = 1, byrow = TRUE, dimnames = list(NULL, "a")))
  expect_equal(gen.named.matrix("{i}{j}", 10*i+j, i=1:2, j=1:3),
               matrix(c(11, 21, 12, 22, 13, 23), ncol = 1, byrow = TRUE, dimnames = list(c("11", "21", "12", "22", "13", "23"))))
  
  expect_equal(gen.matrix(data.frame(a = 1, b = i), i = 1:2, byrow = TRUE),
               matrix(c(1, 1, 1, 2), ncol = 2, dimnames = list(c("a",  "b"), NULL)))
  expect_equal(gen.matrix(data.frame(x+y), x = 1:2, y = 1:2),
               matrix(c(2, 3, 3, 4), ncol = 1, byrow = TRUE, dimnames = list(NULL, c("V1"))))
  expect_equal(gen.matrix(list(x+y), x = 1:2, y = 1:2), matrix(c(2, 3, 3, 4), ncol = 2, byrow = TRUE))
  
  expect_equal(gen.matrix(matrix(c(1,i), ncol=2), i = 1:2, byrow = TRUE),
               matrix(c(1, 1, 1, 2), ncol = 2, byrow = TRUE))
  
  expect_error(gen.matrix(data.frame(c(1,2)), i = 1),
               "the inner expression was evaluated to a data frame with 2 rows, but expected exactly one row")
})

test_that("matrix 2dim tests", {
  expect_equal(gen.matrix(10*i+j, i=1:2, j=1:3), matrix(gen.vector(10*i+j, i=1:2, j=1:3), ncol = 3))
  expect_equal(gen.matrix(10*i+j, i=1:2, j=1:3, byrow = TRUE), matrix(gen.vector(10*i+j, i=1:2, j=1:3), ncol = 2, byrow = TRUE))
  x <- 4
  expect_equal(gen.matrix(10*i+j, i=1:2, j=seq(1,6,x)), matrix(c(11, 15, 21, 25), ncol = 2, byrow = TRUE))
  expect_equal(gen.matrix(10*i+j, i=1:2, j=1:3, byrow = TRUE), matrix(c(11, 12, 13, 21, 22, 23), ncol = 2, byrow = FALSE))
})


test_that("tests with dplyr", {
  df <- gen.data.frame(c(a_1, ..., a_4), a_ = 1:2)
  expect_equal(dplyr::filter(df, !!gen.logical.and(a_i == a_(i+1), i = 1:3)),
               data.frame(a_1 = 1:2, a_2 = 1:2, a_3 = 1:2, a_4 = 1:2))
  expect_equal(dplyr::filter(df, !!gen.logical.or(a_i == a_(i+1) & a_(i+1) == a_(i+2), i = 1:2)),
               data.frame(a_1 = c(1L, 2L, 2L, 1L, 1L, 2L), a_2 = c(1L, 1L,  2L, 1L, 2L, 2L), 
                          a_3 = c(1L, 1L, 2L, 1L, 2L, 2L), a_4 = c(1L,  1L, 1L, 2L, 2L, 2L)))
})