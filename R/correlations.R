#' @title Global correlation method
#'
#' @description This function will compute the coefficient of correlation of two variables (given by their names)
#' and according to their type.
#' This function will be automatically called by the modelise function if it detects that there is only one explanation variable.
#'
#' @details Call example : correlation(data, "group", "X.tissus")
#'
#' @param dataframe : a dataframe from list_type function.
#' @param var1 : The name of a variable, between double quotes
#' @param var2 : The name of a variable, between double quotes
#'
#' @return return the coefficient of correlation
#' @import WGCNA

correlation <- function(dataframe, var1, var2){

	type.var1 <- dataframe$type[grep(var1, colnames(dataframe$data))]
	type.var2 <- dataframe$type[grep(var2, colnames(dataframe$data))]

	X <- dataframe$data[,c(var1)]
	Y <- dataframe$data[,c(var2)]

	len.X <- length(X)
	len.Y <- length(Y)


	if(type.var1 == "Binary")
	{
		if(len.X > 10 && len.Y > 10)
		{
			result <- cor.test(X,Y, use = "na.or.complete", method = "spearman", exact=FALSE)
			print(result)
			result <- result$estimate
		}else
		{
			result <- cor(X,Y, use = "na.or.complete", method = "spearman")
			print("The length of at least one varialbe is under 10, consult spearman's table for the interpretation")
		}
	}
	else if(type.var1 == "Continuous")
	{
		if(len.X > 10 && len.Y > 10)
		{
			result1 <- cor.test(X,Y, use = "na.or.complete", method = "pearson")
			result2 <- bicorAndPvalue(X,Y, use="pairwise.complete.obs")
			print(result1)
			print(result2)
				if(result1$p.value > result2$p)
				{
					print("The method with the best pvalue is the Biweight midcorrelation")
					result <- result2$bicor
				}
				else
				{
					print("The method with the best pvalue is the Pearson correlation")
					result <- result1$estimate
				}
		}
		else
		{
			result <- cor(X,Y, use = "na.or.complete", method = "pearson")
			print("The length of at least one variable is under 10, consult pearson's table for the interpretation")

			
		}

		

	}
	else if(type.var1 == "Categorial")
	{
		if(len.X > 10 && len.Y > 10)
		{
			result <- cor.test(X,Y, use = "na.or.complete", method = "kendall", exact=FALSE)
			print(result)
			result <- result$estimate
		}else
		{
			result <- cor(X,Y, use = "na.or.complete", method = "kendall", exact=FALSE)
			print("The length of at least one variable is under 10, consult kendall's table for the interpretation")

		}
	}
	return(result)
}

#' @title A correlation statistical test taking a matrix
#'
#' @description This function will compute the pvalues of a correlation test between variables in a matrix
#' @details Call example : correlation(data, "group", "X.tissus")
#'
#' @param mat : a matrix of quantitatives values
#' @param ... : Any usual parameter for the cor.test function from R
#'
#' @return return the coefficient of correlation

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}


#' @title A correlation table function
#' @description This function will compute the coefficients of correlation and return a differential or common edge list
#' @details Call example : create_correlation_table_double_df(groupA, groupB, 0.7, 0.95, "omit", "spearman", FALSE, "common")
#'
#' @param data : a dataframe with variables in column and numeric values
#' @param data2 : a second dataframe with variables in column and numeric values
#' @param cutoff : a value of absolute difference or similarity that will choose if edges are included in the edgelist or not
#' @param level: the percentage of confidence (0.95 for 95%)
#' @param na: default is "omit", select the na action 
#' @param method: default is "pearson", the name of the correlation technique. available: pearson, spearman, kendall, bicor, hoeffding
#' @param computepvalues: default is FALSE, if you want the function to compute pvalues set TRUE. Computation will be longer.
#' @param type : default is "differential", in differential mode, edges with a abs(coef1-coef2) > cutoff will be included in the edge list. If type = "common", edges with abs(coef1-coef2) < cutoff will be included.
#'
#' @return return a edge list table
#' @import Hmisc
#' @import WGCNA

create_correlation_table_double_df <- function(data, data2 = NULL, cutoff, level = 0.95, na, method = "pearson", computepvalues = FALSE, type = "differential")
{

  variable <- colnames(data)
  df <- data.frame(Var1 = numeric(), Var2 = numeric(), coefficients = numeric(),check.names = FALSE)
  df2 <- data.frame(Var1 = numeric(), Var2 = numeric(), coefficients = numeric(),check.names = FALSE)
  #for (test in variable)
  if(type == "differential")
  {
    
 
  for(i in 1:length(variable))
  {
    test <- variable[i]
    print(i)
    
    for (j in 1:i)
    {
      var <- variable[j]
      var1 <- data[,i]
      var2 <- data[,j]
      var3 <- data2[,i]
      var4 <- data2[,j]
      if(method == "bicor")
      {
      	bicor1 <- bicorAndPvalue(var1,var2, use="pairwise.complete.obs")
      	coef1 <- bicor1$bicor
      	bicor2 <- bicorAndPvalue(var3,var4, use="pairwise.complete.obs")
      	coef2 <- bicor2$bicor
      }else if (method == "hoeffding")
        {
          hoef1<- hoeffd(var1, var2)
          coef1 <- hoef1$D
          coef1 <- as.numeric(coef1[2])

          hoef2<- hoeffd(var3, var4)
          coef2 <- hoef2$D
          coef2 <- as.numeric(coef2[2])
        }else
      	{
      		if(na == "omit")
      		{
        		coef1 <- as.numeric(cor(var1,var2, method = method, use = "na.or.complete"))
        		coef2 <- as.numeric(cor(var3,var4, method = method, use = "na.or.complete"))
      		}else
      		{
        		coef1 <- as.numeric(cor(var1,var2, method = method))
        		coef2 <- as.numeric(cor(var3,var4, method = method))
      		}
      	}
      
      
      if(is.na(coef1) || coef1 > 0.999)
        {
          next
        
        }else
        {
        	if(method == "bicor")
        	{
        	  
        	  if(computepvalues == TRUE)
        	  {
        		  pvalues1 <- bicor1$p
        		  pvalues2 <- bicor2$p

          		  if((abs(coef1-coef2) > cutoff))
          		  {
          		    df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
          		  }else
          		  {
          		    next
          		  }
        	  }else if((abs(coef1-coef2) > cutoff))

          			{
           				df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
          			}else
          			{
           				next
          			}
        	}else if(method == "hoeffding")
          {
            if(computepvalues == TRUE)
            {

                pvalues1 <- hoef1$P
                pvalues1 <- as.numeric(pvalues1[2])

                pvalues2 <- hoef2$P
                pvalues2 <- as.numeric(pvalues2[2])

                if((abs(coef1-coef2) > cutoff))
                {
                  df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
                }else
                {
                  next
                }
            }else if((abs(coef1-coef2) > cutoff))
              {
                df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
              }else
              {
                next
              }
            }else
            {

        	  if(computepvalues == TRUE)
        	    {
        	    pvalues1 <- cor.test(var1,var2, alternative = c("two.sided"), method = method, conf.level = 0.95)
        	    pvalues2 <- cor.test(var3,var4, alternative = c("two.sided"), method = method, conf.level = 0.95)
        	    if((abs(coef1-coef2) > cutoff) & (pvalues1$p.value < 1-level))
        	      {
        	        df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)), as.numeric(pvalues))
        	      }else
        	      {
        	        next
        	      }
        	  }else if((abs(coef1-coef2) > cutoff))
        	    {
        	      df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
        	    }else
        	    {
        	      next
        	    }
        	}

          
        }
    }
  }
  }else 
  {
  for(i in 1:length(variable))
  {
    test <- variable[i]
    print(i)
    
    for (j in 1:i)
    {
      var <- variable[j]
      var1 <- data[,i]
      var2 <- data[,j]
      var3 <- data2[,i]
      var4 <- data2[,j]
      if(method == "bicor")
      {
        bicor1 <- bicorAndPvalue(var1,var2, use="pairwise.complete.obs")
        coef1 <- bicor1$bicor
        bicor2 <- bicorAndPvalue(var3,var4, use="pairwise.complete.obs")
        coef2 <- bicor2$bicor
      }else if (method == "hoeffding")
        {
          hoef1<- hoeffd(var1, var2)
          coef1 <- hoef1$D
          coef1 <- as.numeric(coef1[2])

          hoef2<- hoeffd(var3, var4)
          coef2 <- hoef2$D
          coef2 <- as.numeric(coef2[2])
        }else
        {
          if(na == "omit")
          {
            coef1 <- as.numeric(cor(var1,var2, method = method, use = "na.or.complete"))
            coef2 <- as.numeric(cor(var3,var4, method = method, use = "na.or.complete"))
          }else
          {
            coef1 <- as.numeric(cor(var1,var2, method = method))
            coef2 <- as.numeric(cor(var3,var4, method = method))
          }
        }
      
      
      if(is.na(coef1) || coef1 > 0.999)
        {
          next
        
        }else
        {
          if(method == "bicor")
          {
            
            if(computepvalues == TRUE)
            {
              pvalues1 <- bicor1$p
              pvalues2 <- bicor2$p

                if((abs(coef1-coef2) < cutoff))
                {
                  df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)), as.numeric(pvalues))
                }else
                {
                  next
                }
            }else if((abs(coef1-coef2) < cutoff))

                {
                  df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
                }else
                {
                  next
                }
          }else if(method == "hoeffding")
          {
            if(computepvalues == TRUE)
            {

                pvalues1 <- hoef1$P
                pvalues1 <- as.numeric(pvalues1[2])

                pvalues2 <- hoef2$P
                pvalues2 <- as.numeric(pvalues2[2])

                if((abs(coef1-coef2) < cutoff))
                {
                  df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
                }else
                {
                  next
                }
            }else if((abs(coef1-coef2) < cutoff))
              {
                df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
              }else
              {
                next
              }
            }else
            {

            if(computepvalues == TRUE)
              {
              pvalues1 <- cor.test(var1,var2, alternative = c("two.sided"), method = method, conf.level = 0.95)
              pvalues2 <- cor.test(var3,var4, alternative = c("two.sided"), method = method, conf.level = 0.95)
              if((abs(coef1-coef2) < cutoff))
                {
                  df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
                }else
                {
                  next
                }
            }else if((abs(coef1-coef2) < cutoff))
              {
                df[nrow(df) + 1,] = c(test,var, abs(as.numeric(coef1-coef2)))
              }else
              {
                next
              }
          }
          
        }
    }
  }
}
  return(df)
}


#' @title A correlation table function
#' @description This function will compute the coefficients of correlation and the associated p-values between all variables in the dataframe
#' @details Call example : create_correlation_table(dataframe)
#'
#' @param data : a dataframe with variables in column and numeric values
#' @param cutoff : a value of absolute difference or similarity that will choose if edges are included in the edgelist or not
#' @param level: the percentage of confidence (0.95 for 95%)
#' @param na: default is "omit", select the na action 
#' @param method: default is "pearson", the name of the correlation technique. available: pearson, spearman, kendall, bicor, hoeffding
#' @param computepvalues: default is FALSE, if you want the function to compute pvalues set TRUE. Computation will be longer.
#'
#' @return return a edge list table
#' @import Hmisc
#' @import WGCNA
create_correlation_table <- function(data, cutoff, level = 0.95, na, method, computepvalues)
{

  variable <- colnames(data)
if(computepvalues == TRUE)
   {
      df <- data.frame(Var1 = numeric(), Var2 = numeric(), coefficients = numeric(), "p-values" = numeric(), check.names = FALSE)
   }else
   {
    df <- data.frame(Var1 = numeric(), Var2 = numeric(), coefficients = numeric(), check.names = FALSE)
   }
  for(i in 1:length(variable))
  {
    test <- variable[i]
    print(i)
    
    #for (var in variable)
    for (j in 1:i)
    {
      var <- variable[j]
      var1 <- data[,i]
      var2 <- data[,var]
      if(method == "bicor")
      {
      	bicor <- bicorAndPvalue(var1,var2, use="pairwise.complete.obs")
      	coef <- bicor$bicor
      }else if (method == "hoeffding")
        {
          corr.hoeffd<- hoeffd(var1, var2)
          coef1 <- corr.hoeffd$D
          coef <- as.numeric(coef1[2])

        }else
      	{
      		if(na == "omit")
      		{
        		coef <- cor(var1,var2, method = method, use = "na.or.complete")
      		}else
      		{
        		coef <- cor(var1,var2, method = method)
      		}
      	}
      
      
      if(is.na(coef) || coef > 0.999)
        {
          next
        
        }else
        {
        	if(method == "bicor")
        	{
        		pvalues <- bicor$p
        		    if((abs(coef) > cutoff) & (pvalues < 1-level) & (abs(coef) < 1))
          			{
           				df[nrow(df) + 1,] = c(test,var, as.numeric(coef), as.numeric(pvalues))
          			}else
          			{
           				next
          			}
        	}else if(method == "hoeffding")
          {
            if(computepvalues == TRUE)
            {

                pvalues <- corr.hoeffd$P
                pvalues <- as.numeric(pvalues[2])

                if((abs(coef) > cutoff))
                {
                  df[nrow(df) + 1,] = c(test,var, as.numeric(coef), as.numeric(pvalues))
                }else
                {
                  next
                }
            }else if((abs(coef) > cutoff))
              {
                df[nrow(df) + 1,] = c(test,var, as.numeric(coef))
              }else
              {
                next
              }
            }else
        	{
        		pvalues <- cor.test(var1,var2, alternative = c("two.sided"), method = method, conf.level = 0.95)
        		if((abs(coef) > cutoff) & (pvalues$p.value < 1-level) & (abs(coef) < 1))
          			{
           				df[nrow(df) + 1,] = c(test,var, as.numeric(coef), as.numeric(pvalues$p.value))
          			}else
          			{
           				next
          			}
        	}


          
        }


    }
  }
  return(df)
}


#' @title A square matrix creation function 
#' @description This function will compute a square matrix from an edgelist (from correlation tables functions)
#' @details Call example : square_matrix_from_edge_list(edge.list, "coefficients")
#'
#' @param data : a edge list dataframe from create_correlation table function
#' @param content : default is "coefficients", determine what is in the square matrix
#'
#' @return return a square matrix

square_matrix_from_edge_list <- function(data, content = "coefficients"){
  
  
  if(content == "coefficients")
  {
    content <- 3
  }else if(content == "pvalues")
  {
    content <- 4
  }else
  {
    stop("content must be coefficients or pvalues")
  }
  cols.matrix <- unique(data[,2])
  rows.matrix <- unique(data[,1])
  names <- unique(c(cols.matrix,rows.matrix))
  matrix.data <- matrix(nrow = length(names), ncol = length(names))
  rownames(matrix.data) <- names
  colnames(matrix.data) <- names
  matrix.data <- as.data.frame(matrix.data)
  

  i <- 1
  for (var in names)
  {
    print(var)
    
    for (var2 in names)
    {

      if(any(grep(var, data[i,1])) != TRUE & any(grep(var2, data[i,2])) != TRUE) next

          matrix.data[var,var2] <- as.numeric(data[i,content])
          matrix.data[var2,var] <- as.numeric(data[i,content])
          i <- i+1

    }
    
  }
  return(matrix.data)
}

