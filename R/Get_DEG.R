#' Get_DEG
#'
#' Get differentail expression genes based on optional criteria
#'
#' @usage Get_DEG(x, criteria = c("p.value", "log_FC", "fdr"), threshold, is.sort = T)
#'
#' @param x a data.frame containing results from DEA_test
#' @param criteria criteria to filter DEG, with options of p.value, absolute value of log_FC and fdr
#' @param threshold threshold of the criteria to filter DEG
#' @param is.sort whether sorting genes based on criteria
#'
#' @return a data.frame containing interesting gene
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
#' res1 = Get_DEG(de.a, criteria = "log_FC", threshold = 0.1)
#' # Multiple filtering
#' # Get_DEG(res1, criteria = "fdr", threshold = 0.1)
#'
#' @export
#'

Get_DEG = function(x, criteria, threshold, is.sort = T){

    if (criteria == "log_FC") {
    index = abs(x["log_FC"]) > threshold
    res = x[index, c("log_FC", "p_value", "fdr")]
    if (is.sort) res = res[order(abs(res$log_FC), decreasing = T), ]
  } else if (criteria %in% c("p_value", "log_FC", "fdr")){
    index = x[criteria] < threshold
    res = x[index, c("log_FC", "p_value", "fdr")]
    if (is.sort) res = res[order(res[criteria], decreasing = F), ]
  }

  return(res)
}
