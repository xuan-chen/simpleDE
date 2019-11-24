test_that("Create_from_just_mtx Works", {
  a = matrix(rpois(12, 1), 4, 3)
  rownames(a) = sapply(1:4, function(x) paste0("gene", x))
  colnames(a) = sapply(1:3, function(x) paste0("sample",x))
  simple.a = Create_simpleDE(a)

  a.expect = list(data = list(raw = a),
                  sample_info = data.frame(sample_id = colnames(a), row.names = colnames(a), stringsAsFactors = F),
                  gene_info = data.frame(gene_id = rownames(a), row.names = rownames(a), stringsAsFactors = F))

  expect_equal(simple.a, a.expect)
})

test_that("Create_from_mtx_with_metadata Works", {
  a = matrix(rpois(12, 1), 4, 3)
  rownames(a) = sapply(1:4, function(x) paste0("gene", x))
  colnames(a) = sapply(1:3, function(x) paste0("sample",x))
  g = data.frame(ENTREZ_ID = c(123,124,125,126))
  rownames(g) = sapply(1:4, function(x) paste0("gene", x))
  s = data.frame(batch = c(1,2,2))
  rownames(s) = sapply(1:3, function(x) paste0("sample",x))
  simple.a = Create_simpleDE(a, s, g)

  a.expect = list(data = list(raw = a),
                  sample_info = data.frame(sample_id = colnames(a), s, row.names = colnames(a), stringsAsFactors = F),
                  gene_info = data.frame(gene_id = rownames(a), g, row.names = rownames(a), stringsAsFactors = F))


  expect_equal(simple.a, a.expect)
})

