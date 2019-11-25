#' Filter_Data
#'
#' Filter the genes with low expression and cells with low reads
#'
#' @usage Filter_Data(x, min_gene = 5, min_read = 5, verbose = T)
#'
#' @param x a list created from Create_simpleDE
#' @param min_gene filter out samples with expressed gene less than this
#' @param min_read filter out genes with total reads less than this
#' @param verbose Print the information of filtering
#'
#' @return the origin list with some genes and cells removed
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
#'
#' @export
#'


Filter_Data = function(x, min_gene = 5, min_read = 5, verbose = T){

  mtx = x$data$raw

  s_index = colSums(mtx>0) > min_gene
  g_index = rowSums(mtx) > min_read

  s_filter = nrow(x$sample_info) - length(s_index)
  g_filter = nrow(x$gene_info) - length(g_index)

  x$data$raw = mtx[g_index, s_index]
  x$sample_info = x$sample_info[s_index, , drop = F]
  x$gene_info = x$gene_info[g_index, , drop = F]

  if (verbose){
    print("\n")
    print(paste0(s_filter, " samples are filtered."))
    print(paste0(g_filter, " genes are filtered."))
  }

  return(x)
}
