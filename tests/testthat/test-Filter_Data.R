a = matrix(1:12, 4, 3)
rownames(a) = sapply(1:4, function(x) paste0("gene", x))
colnames(a) = sapply(1:3, function(x) paste0("sample",x))
g = data.frame(ENTREZ_ID = c(123,124,125,126))
rownames(g) = sapply(1:4, function(x) paste0("gene", x))
s = data.frame(batch = c(1,2,2))
rownames(s) = sapply(1:3, function(x) paste0("sample",x))
simple.a = Create_simpleDE(a, s, g)

test_that("filter Works", {
  filter.a = Filter_Data(simple.a, min_gene = 0, min_read = 0, verbose = T)
  expect_equal(simple.a, filter.a)
})

test_that("filter verbose Works", {
  filter.a = Filter_Data(simple.a, min_gene = 0, min_read = 0, verbose = T)
  expect_equal(simple.a, filter.a)
})
