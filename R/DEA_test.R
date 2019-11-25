#' DEA_test
#'
#' Perform differential expression analysis using t-test
#'
#' @usage DEA_test(x, group)
#'
#' @param x a list from Normalize_data
#' @param group a colname from sample_info or a vetor of factor indexing two group for comparison
#' @param verbose Print the information of reference and process
#'
#' @return a matrix containg fold change, test statistics and p value with adjustment
#'
#' @examples
#' a = matrix(c(1:8,1:8), 4, 4)
#' rownames(a) = sapply(1:4, function(x) paste0("gene", x))
#' colnames(a) = sapply(1:4, function(x) paste0("sample",x))
#' g = data.frame(ENTREZ_ID = c(123,124,125,126))
#' rownames(g) = sapply(1:4, function(x) paste0("gene", x))
#' s = data.frame(batch = c(1,2,2,1))
#' rownames(s) = sapply(1:4, function(x) paste0("sample",x))
#' simple.a = Create_simpleDE(a, s, g)
#' simple.a = Filter_Data(simple.a, 0, 0)
#' simple.a = Normalize_Data(simple.a, "size_factor")
#'
#' # Using colname from sample_info
#' de.a = DEA_test(simple.a, group = "batch")
#'
#' # Using customized index
#' de.a = DEA_test(simple.a, group = c(1,1,2,2))
#'
#' @export
#'
DEA_test = function(x, group, verbose = T){
  dt = x$data$normalized

  if (length(group) == 1 & is.character(group)){
    index = as.factor(x$sample_info[,group])
    grp_name = group
  } else if (length(group) == nrow(x$sample_info)){
    index = as.factor(group)
    grp_name = "input_index"
  }

  ref = levels(index)[1]
  ref_index = which(index == ref)

  if (verbose) cat(paste0("----- Performing DEA with reference as ", grp_name, "=",  ref, " -----\n"))

  test_res = apply(dt, 1, function(dt_row){
    # dt_row = dt[1,]
    dt1 = dt_row[ref_index]
    dt2 = dt_row[-ref_index]
    res = t.test(dt1, dt2, "two.sided")
    res = c(res$statistic, FC = mean(dt2) / mean(dt1), log_FC = log(mean(dt2) / mean(dt1)), p_value = res$p.value)
    return(res)
  })
  test_res = t(test_res)
  test_res = data.frame(gene = rownames(test_res), data.frame(test_res))
  test_res$bonferroni = p.adjust(test_res$p_value, method = "bonferroni")
  test_res$fdr = p.adjust(test_res$p_value, method = "fdr")

  if (verbose) cat("-----  Done! ----- \n")
  return(test_res)
}
