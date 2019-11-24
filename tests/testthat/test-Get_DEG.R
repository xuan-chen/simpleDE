de.a = data.frame(gene = rownames(g),
                       t = rep(0,4),
                       FC = rep(1,4),
                       log_FC = c(0.1, 0.4, -0.6, 0.9),
                       p_value = c(1, 0.02, 0.5, 0.01),
                       fdr = c(1, 0.02, 0.5, 0.01),
                       row.names = rownames(g))


test_that("Get_DEG by log_FC works", {
  res = Get_DEG(de.a, "log_FC", 0.5, is.sort = T)
  res.expect = data.frame(log_FC = c(0.9, -0.6),
                                 p_value = c(0.01, 0.5),
                                 fdr = c(0.01, 0.5),
                                 row.names = c("gene4", "gene3"))

  expect_equal(res, res.expect)
})

test_that("Get_DEG by log_FC works 2", {
  res = Get_DEG(de.a, "log_FC", 0.5, is.sort = F)
  res.expect = data.frame(log_FC = c(-0.6, 0.9),
                          p_value = c(0.5, 0.01),
                          fdr = c(0.5, 0.01),
                          row.names = c("gene3", "gene4"))

  expect_equal(res, res.expect)
})

test_that("Get_DEG by p_value works", {
  res = Get_DEG(de.a, "p_value", 0.05, is.sort = T)
  res.expect = data.frame(log_FC = c(0.9, 0.4),
                          p_value = c(0.01, 0.02),
                          fdr = c(0.01, 0.02),
                          row.names = c("gene4", "gene2"))

  expect_equal(res, res.expect)
})

test_that("Get_DEG by p_value works 2", {
  res = Get_DEG(de.a, "fdr", 0.05, is.sort = F)
  res.expect = data.frame(log_FC = c(0.4, 0.9),
                          p_value = c(0.02, 0.01),
                          fdr = c(0.02, 0.01),
                          row.names = c("gene2", "gene4"))

  expect_equal(res, res.expect)
})
