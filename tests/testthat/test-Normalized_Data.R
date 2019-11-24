a = matrix(1:12, 4, 3)
rownames(a) = sapply(1:4, function(x) paste0("gene", x))
colnames(a) = sapply(1:3, function(x) paste0("sample",x))
g = data.frame(ENTREZ_ID = c(123,124,125,126))
rownames(g) = sapply(1:4, function(x) paste0("gene", x))
s = data.frame(batch = c(1,2,2))
rownames(s) = sapply(1:3, function(x) paste0("sample",x))
simple.a = Create_simpleDE(a, s, g)

test_that("size factor normalization works", {
  norm.a = Normalize_Data(simple.a, "size_factor")

  raw_data = simple.a$data$raw
  pseudo_ref = exp(rowMeans(log(raw_data + 1)))
  size_factor = apply(raw_data / pseudo_ref, 1, median)
  norm_data = raw_data / size_factor

  expect_equal(norm.a$data$normalized, norm_data)
})


test_that("simplest normalization works", {
  norm.b = Normalize_Data(simple.a, "simplest")

  raw_data = simple.a$data$raw
  norm_data = raw_data / colMeans(raw_data)

  expect_equal(norm.b$data$normalized, norm_data)
})

test_that("median normalization works", {
  norm.c = Normalize_Data(simple.a, "median")

  raw_data = simple.a$data$raw
  norm_data = raw_data / apply(raw_data, 2, median)

  expect_equal(norm.c$data$normalized, norm_data)
})

test_that("none normalization works", {
  norm.d = Normalize_Data(simple.a, "none")

  expect_equal(norm.d$data$normalized, simple.a$data$raw)
})
