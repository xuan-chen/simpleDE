#' Create_simpleDE
#'
#' Creates a list containg expression matrix, sample information and gene annotation.
#'
#' @usage Create_simpleDE(expr_mtx, sample_info = NULL, gene_info = NULL)
#'
#' @param expr_mtx expression matrix (n genes * p cells) for expression data
#' @param sample_info dataframe of p rows, storing sample information. If it is not specified, a data.frame is created with one
#'   column of colnames of expr_mtx as sample id.
#' @param gene_info dataframe of n rows, storing gene annotation information. If it is not specified, a data.frame is created with one
#'   column of rownames of expr_mtx as gene id.
#'
#' @return a list containing all data and information
#'
#' @examples
#' # Create with only an expression matrix
#' a = matrix(rpois(12, 1), 4, 3)
#' rownames(a) = sapply(1:4, function(x) paste0("gene", x))
#' colnames(a) = sapply(1:3, function(x) paste0("sample",x))
#' simple.a = Create_simpleDE(a)
#'
#' # Create with information of samples and genes
#' g = data.frame(ENTREZ_ID = c(123,124,125,126))
#' rownames(g) = sapply(1:4, function(x) paste0("gene", x))
#' s = data.frame(batch = c(1,2,2))
#' rownames(s) = sapply(1:3, function(x) paste0("sample",x))
#' simple.a = Create_simpleDE(a, s, g)
#'
#'
#' @export
#'


Create_simpleDE = function(expr_mtx, sample_info = NULL, gene_info = NULL){

  e = as.matrix(expr_mtx)

  if (is.null(sample_info)) s = data.frame(sample_id = colnames(expr_mtx), stringsAsFactors = F) else
    s = data.frame(sample_id = colnames(expr_mtx), data.frame(sample_info), stringsAsFactors = F)
  if (is.null(gene_info)) g = data.frame(gene_id = rownames(expr_mtx), stringsAsFactors = F) else
    g = data.frame(gene_id = rownames(expr_mtx), data.frame(gene_info), stringsAsFactors = F)

  colnames(e) = rownames(s) = colnames(expr_mtx)
  rownames(e) = rownames(g) = rownames(expr_mtx)

  DElist = list(data = list(raw = e),
                sample_info = s,
                gene_info = g)

  return(DElist)
}
