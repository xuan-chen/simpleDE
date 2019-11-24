a = matrix(c(1:8,1:8), 4, 4)
rownames(a) = sapply(1:4, function(x) paste0("gene", x))
colnames(a) = sapply(1:4, function(x) paste0("sample",x))
g = data.frame(ENTREZ_ID = c(123,124,125,126))
rownames(g) = sapply(1:4, function(x) paste0("gene", x))
s = data.frame(batch = c(1,2,2,1))
rownames(s) = sapply(1:4, function(x) paste0("sample",x))
simple.a = Create_simpleDE(a, s, g)
simple.a = Normalize_Data(simple.a, "size_factor")

test_that("DEA using colnames works", {
  de.a = DEA_test(simple.a, group = "batch", verbose = T)
  de.expect = data.frame(gene = rownames(g),
                         t = rep(0,4),
                         FC = rep(1,4),
                         log_FC = rep(0, 4),
                         p_value = rep(1,4),
                         bonferroni = rep(1,4),
                         fdr = rep(1,4),
                         row.names = rownames(g))
  expect_equal(de.a, de.expect)
})

test_that("DEA using index works", {
  de.a = DEA_test(simple.a, group = c(1,2,2,1), verbose = F)
  de.expect = data.frame(gene = rownames(g),
                         t = rep(0,4),
                         FC = rep(1,4),
                         log_FC = rep(0, 4),
                         p_value = rep(1,4),
                         bonferroni = rep(1,4),
                         fdr = rep(1,4),
                         row.names = rownames(g))
  expect_equal(de.a, de.expect)
})
