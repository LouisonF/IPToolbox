
#' @title A correlogram plotting function
#' @description This function will call the correlogram plotting function from corrplot package 
#' @details Call example : plot.correlogram("mypdfname",matric.coefficients, matrix.pvalues, 0.95, "number")
#'
#' @param pdf.name : the name of the pdf file (and by default also title of the plot)
#' @param matrix.coef : A square matrix with all coefficients from square_matrix_from_edge_list
#' @param matrix.pvalues : A square matrix with all pvalues from square_matrix_from_edge_list
#' @param level : the value of confidence (0.95 by default)
#' @param method : the corrplot method, number by default
#' @param number.size : a numeric value that will act as a multiplier on the number size
#' @param pvalues.size : a numeric value that will act as a multiplier on the pvalues size
#' @param title : the title of the plot, by default the pdf anme
#' @param mar : margin values. By default 0,0,1,0
#' @param label.size : a numeric value that will act as a multiplier on the label size
#'
#' @return return a corrplot
#' @import corrplot

plot.correlogram <- function(pdf.name, matrix.coef, matrix.pvalues, level = 0.95, method = "number", number.size = 0.35, pvalues.size = 0.3, title = pdf.name, mar = c(0,0,1,0), label.size = 0.5)
{
	pdf(file=pdf.name)
	corrplot(as.matrix(matrix.coef), p.mat = as.matrix(matrix.pvalues), sig.level = 1-level, method = method, number.cex = number.size, pch.cex = pvalues.size, title = title, , mar, tl.cex = label.size)
	dev.off()
}

#' @title A co-expression network plotting function
#' @description This function will plot a co-expression network
#' @details Call example : plot.coexpression(myedgetable, "This is the title of my co-expression network", mymatrixoffoldchange, color = "fold-change", "path to a folder", "The name of the file")
#'
#' @param edge.table : a edge table (or edge list) from the function create_correlation_table (or create_correlation_table_double_df)
#' @param directed : Takes TRUE (default) if the graph as to be directed or FALSE if not
#' @param title : the title of the graph
#' @param matrix.foldchange : default is NULL, is coloring technique is fold-change then a matrix of fold-change is needed
#' @param color : default is "fold-change", define the color technique
#' @param directory : set a path where to store the pdf
#' @param filename : default is "random_filename.pdf", set the name of the pdf file
#' @param vertex.size : default is 3.5, set the size of nodes in the co-expression network
#' @param vertex.label : default is NA, set the labels of nodes in the co-expression network
#' @param edge.arrow.size : default is NA, set the arrow size in the co-expression network

#'
#' @return return a dataframe
#' @import igraph

plot.coexpression <- function(edge.table, directed = TRUE, title, matrix.foldchange = NULL, color = "fold-change", directory, filename = "random_filename.pdf", vertex.size = 3.5, vertex.label = NA, edge.arrow.size = NA)
{
	graph <- graph_from_edgelist(as.matrix(edge.table[,1:2]), directed)
	graphCol = ifelse(as.numeric(edge.table[,3]) <=0, "yellow","blue")
if(color == "fold-change")
{
    node.color <- NULL
    dim <- dim(matrix.foldchange)[1]
    graph<-simplify(graph)
    ordered.vertices <-get.data.frame(graph, what="vertices")
    ordered.vertices <-  gsub("-", ".", ordered.vertices$name)
    for (x in ordered.vertices)
    {
      print(x)
      if(median(matrix.foldchange[c(1:dim),x]) <= 0)
      {
        node.color <- append(node.color, 0)
      }else
      {
        node.color <- append(node.color, 1)
      }
  	}

  	vertex.color <- ifelse(node.color == 1, "red", "green")

  	setwd(directory)
    
    pdf(file = filename)
    plot(graph, main = title, vertex.size=3.5, vertex.label=NA , edge.arrow.size = 0, edge.color=graphColAll, vertex.color=vertex.color )
    dev.off()
}

  	setwd(directory)
    
    pdf(file = filename)
    plot(graph, main = title, vertex.size=3.5, vertex.label=NA , edge.arrow.size = 0)
    dev.off()

}

