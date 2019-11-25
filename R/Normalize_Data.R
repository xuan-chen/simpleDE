#' Normalize_Data
#'
#' Normalize raw data with optional normalization techniques
#'
#' @usage Normalize_Data(x, option = c("size_factor", "simplest", "median"))
#'
#' @param x a list from Filter_Data
#' @param option option of normalization:
#'   "simplest": divided by mean reads of each cell
#'   "median": divided by median reads of each cell
#'   "size_factor": (default)divided by size factor, which is the median of the ratios of raw data and geometric mean
#'   "none": not performing normalization
#'
#' @return a list adding with a new element containing normalized data
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
#' @export
#'

Normalize_Data = function(x, option = "size_factor"){
  raw_data = x$data$raw

  if (option == "size_factor"){
    pseudo_ref = exp(rowMeans(log(raw_data + 1)))
    size_factor = apply(raw_data / pseudo_ref, 1, median)
    norm_data = raw_data / size_factor
  }else if (option == "simplest"){
    norm_data = raw_data / colMeans(raw_data)
  }else if (option == "median"){
    norm_data = raw_data / apply(raw_data, 2, median)
  }else if (option == "none"){
    norm_data = raw_data
  }

  x$data$normalized = norm_data

  return(x)
}
