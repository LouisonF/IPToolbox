#' @title List the type of variables in a dataframe
#'
#' @description
#' This function analyse variables types available in the dataframe
#' These types are added to a list
#' The order of this list is the order of variables in the dataframe
#' 
#' @param dataframe a dataframe with raw data
#'
#' @return an object containing a list and the input dataframe
#' 
#' @import base

list_type <- function(dataframe) {
  
  
  dimdf <- dim.data.frame(dataframe)
  dimdf <- as.numeric(dimdf[2])
  dataframe <- list(data=dataframe,type=c(NULL))
  
  i <- NULL
  for (i in 1:dimdf) {
    if(is.factor(dataframe$data[,i]) == TRUE)
    {
      dataframe$type <- append(dataframe$type,"Categorial")
    }else if (is.numeric(dataframe$data[,i]) == TRUE)
    {
      if((0 %in% dataframe$data[,i] && 1 %in% dataframe$data[,i] && 2 %in% dataframe$data[,i]))
      {
        dataframe$type <- append(dataframe$type,"Categorial")
      }
      
      else if( 0 %in% dataframe$data[,i] && 1 %in% dataframe$data[,i])
      {
        dataframe$type <- append(dataframe$type,"Binary")
      }
      
      else
      {
        dataframe$type <- append(dataframe$type,"Continuous")
      }
    }
    
    else if (is.double(dataframe$data[,i]) == TRUE)
    {
      dataframe$type <- append(dataframe$type,"Continuous")
    }else if(is.character(dataframe$data[,i]) == TRUE)
    {
      dataframe$type <- append(dataframe$type,"Character")
    }
  }
  return(dataframe)
}

#' @title Random sampling in a dataframe in order to create learning and validation samples
#'
#' @description 
#'
#' @details 
#'
#' @param dataframe : a dataframe with raw data : individuals are in rows, variables in columns
#' @param reset : TRUE if we want to replace the drawn value, FALSE otherwise
#'
#' @return return the learning sample in a dataframe

learning_sample <- function(dataframe, reset){
  
  datadim <- dim(dataframe)
  datadim <- datadim[1]
  datadim <- as.integer(datadim)
  
  if(reset == TRUE){
    apprentissage <- dataframe[sample(nrow(dataframe), size = datadim, replace = TRUE),]
    print("duplicate rownames takes a decimal because 2 rows can't have the same name")
    
  }
  else if (reset == FALSE){
    apprentissage <- dataframe[sample(nrow(dataframe), size = datadim/2, replace = FALSE),]
    
  }
  else{
    print("the reset parameter have to be TRUE or FALSE")
  }
  return(apprentissage) #Return only one DF
}

#' @title Create the validation sample from data not in the learning sample
#'
#' @description 
#'
#' @details 
#'
#' @param Learning_sample : The Learning dataframe creted with the Learning_Sample function
#' @param dataframe : The dataframe used with the Learning_Sample function
#'
#' @return return the validation dataframe associated with the parameter:  learning_sample

validation_sample <- function(learning_sample, dataframe){
  
  validation <- dataframe[-c(as.numeric(row.names(learning_sample))),]
  return(validation)
}


#' @title Report creation for IPToolbox R package
#'
#' @description 
#'
#' @details Called by the model function
#'
#' @param dataframe : The dataframe used in the model the user wants to report
#' @param dependantvar : The name of the dependant variable used in the model the user wants to report.
#' @param explanationvar : The list of explanation variable used in the model the user wants to report.
#' @param directory : The working directory (where the Rdata file is located) (Currently seeking an alternative to this method)
#' @return An invitation to comile the .tex file. The autocompilation in a pdf is implemented but if the user doesn't have pdftex it will fail.
#' 
#' @import xtable

create_report <- function(dataframe, dependantvar, explanationvar, directory, reportname = NULL){
  
  setwd(directory)
  
  type.var <- dataframe$type[grep(dependantvar, colnames(dataframe$data))]
  
  if(type.var == "Binary")
  {  
    load(file = "report_binary.Rdata")
    selected.var <- as.data.frame(model$coefficient)
    selected.var <- t(selected.var)
    selected.var <- colnames(selected.var)[-1]
    if(is.null(reportname))
    {
      
      file.name.tex <- paste0("logistic-regression",".",dependantvar,paste(collapse = ".",selected.var),".tex")
      file.name.pdf <- paste0("logistic-regression",".",dependantvar,paste(collapse = ".",selected.var),".pdf")
      dir.name <- paste0("logistic-regression",".",dependantvar,paste(collapse = ".",selected.var))
      
      if(nchar(dir.name)>80)
      {
        hour <- Sys.time()
        hour <- paste0(format(hour, "%Y-%m-%d_%Hh-%M-%S_"))
        dir.name <- paste0("logistic-regression","_",dependantvar,".","numerousvar",hour)
        file.name.tex <- paste0("logistic-regression",".",dependantvar,".","numerousvar",hour,".tex")
        file.name.pdf <- paste0("logistic-regression",".",dependantvar,".","numerousvar",hour,".pdf")
      }
    }
    else
    {
      file.name.tex <- paste0(reportname,".tex")
      file.name.pdf <- paste0(reportname,".pdf")
      dir.name <- paste0(reportname)
    }
    
    if(auc(roc) != 1)
    {
      dir.create(dir.name)
      file.copy("report_binary.Rdata", dir.name)
      file.copy("IPToolbox_report_binary.Rnw", dir.name)
      setwd(dir.name)
      load(file = "report_binary.Rdata")
      dir.create("figs")
      Sweave("IPToolbox_report_binary.Rnw")
      file.rename("IPToolbox_report_binary.tex",file.name.tex)
      system(paste("pdflatex", file.name.tex))
      file.copy(file.name.pdf,directory)
      setwd("..")
      unlink(dir.name, recursive=TRUE)
    }else
    {
      print("GLM algorithm did not converge, this is probably due to a too high number of explanation variables")
    }

    
  }else if(type.var == "Continuous")
  {
    load(file = "report_continuous.Rdata")
    selected.var <- as.data.frame(model$coefficient)
    selected.var <- t(selected.var)
    selected.var <- colnames(selected.var)[-1]
    if(is.null(reportname))
    {
      
      file.name.tex <- paste0("linear-regression.",dependantvar,paste(collapse = ".",selected.var),".tex")
      file.name.pdf <- paste0("linear-regression.",dependantvar,paste(collapse = ".",selected.var),".pdf")
      dir.name <- paste0("logistic-regression.",dependantvar,paste(collapse = ".",selected.var))
      
      if(nchar(dir.name)>80)
      {
        hour <- Sys.time()
        hour <- paste0(format(hour, "%Y-%m-%d_%Hh-%M-%S_"))
        dir.name <- paste0("linear-regression",".",dependantvar,".","numerousvar",hour)
        file.name.tex <- paste0("linear-regression",".",dependantvar,".","numerousvar",hour,".tex")
        file.name.pdf <- paste0("linear-regression",".",dependantvar,".","numerousvar",hour,".pdf")
      }
    }else
      {
        file.name.tex <- paste0(reportname,".tex")
        file.name.pdf <- paste0(reportname,".pdf")
        dir.name <- paste0(reportname)
      }
    setwd(directory)
    load(file = "report_continuous.Rdata")
    dir.create(dir.name)
    file.copy("report_continuous.Rdata", dir.name)
    file.copy("IPToolbox_report_continuous.Rnw", dir.name)
    setwd(dir.name)
    
    dir.create("figs")
    Sweave("IPToolbox_report_continuous.Rnw")
    file.rename("IPToolbox_report_continuous.tex",file.name.tex)
    system(paste("pdflatex", file.name.tex))
    file.copy(file.name.pdf,directory)
    setwd("..")
    unlink(dir.name, recursive=TRUE)
  }else if (type.var == "Categorial")
  {
    
    load(file = "report_binary.Rdata")
    selected.var <- as.data.frame(summary(model)$coefficient)
    selected.var <- colnames(selected.var)[-1]
    if(is.null(reportname))
    {
      
      file.name.tex <- paste0("logistic-regression",".",dependantvar,paste(collapse = ".",selected.var),".tex")
      file.name.pdf <- paste0("logistic-regression",".",dependantvar,paste(collapse = ".",selected.var),".pdf")
      dir.name <- paste0("logistic-regression",".",dependantvar,paste(collapse = ".",selected.var))
      
      if(nchar(dir.name)>80)
      {
        hour <- Sys.time()
        hour <- paste0(format(hour, "%Y-%m-%d_%Hh-%M-%S_"))
        dir.name <- paste0("logistic-regression","_",dependantvar,".","numerousvar",hour)
        file.name.tex <- paste0("logistic-regression",".",dependantvar,".","numerousvar",hour,".tex")
        file.name.pdf <- paste0("logistic-regression",".",dependantvar,".","numerousvar",hour,".pdf")
      }
    }else
    {
      file.name.tex <- paste0(reportname,".tex")
      file.name.pdf <- paste0(reportname,".pdf")
      dir.name <- paste0(reportname)
    }
    if(auc(roc) != 1)
    {
      dir.create(dir.name)
      file.copy("report_binary.Rdata", dir.name)
      file.copy("IPToolbox_report_binary.Rnw", dir.name)
      setwd(dir.name)
      load(file = "report_binary.Rdata")
      dir.create("figs")
      Sweave("IPToolbox_report_binary.Rnw")
      file.rename("IPToolbox_report_binary.tex",file.name.tex)
      system(paste("pdflatex", file.name.tex))
      file.copy(file.name.pdf,directory)
      setwd("..")
      unlink(dir.name, recursive=TRUE)
    }else
    {
      print("GLM algorithm did not converge, this is probably due to a too high number of explanation variables")
    }
    
  }
  else
  {
    print("The dependant variable is neither binary or continuous")
  }
  
  return("If tex compilation fail, install pdflatex")
}

#' @title HUB and Bottlenecks computation function
#'
#' @description 
#'
#' @details 
#'
#' @param graph : A igraph object 
#' @param filename.hub : the name of hubs files whithout extension
#' @param filename.bottlenecks : the name of bottlenecks files whitout extention
#' @param top : default = true, if top is true compute the top 100 of hubs and bottlenecks (if there is more than 100 hubs and/or 100 bottlenecks)
#' @return write tsv files with hubs and bottlenecks 
#' 
#' @import igraph


hub_bottlenecks <- function(graph, filename.hub, filename.bottlenecks, top = TRUE)
{
  hub <- hub.score(graph)
  hub <- as.data.frame(sort(hub$vector, decreasing = T))
  colnames(hub)[1] <- "values"
  hub$nodes <- rownames(hub)
  hub <- hub[,c(2,1)]

  if(top == TRUE & dim(hub)[1] > 100)
  {
    hub_100 <- as.data.frame(hub[c(1:100),])
    hub_100$nodes <- rownames(hub_100)
    filename.hub_100 <-  paste0(filename.hub,"_top100.tsv")
    write.table(hub_100, file=filename.hub_100, sep='\t', row.names = FALSE)
  }

  filename <- paste0(filename.hub,".tsv")
  write.table(hub, file=filename, sep='\t', row.names= FALSE)

  bottlenecks <- betweenness(graph)
  bottlenecks <- as.data.frame(sort(bottlenecks, decreasing = T))
  colnames(bottlenecks)[1] <- "values"
  bottlenecks$nodes <- rownames(bottlenecks)
  bottlenecks <- bottlenecks[,c(2,1)]

  if(top == TRUE & dim(bottlenecks)[1] > 100)
  {
    bottlenecks_100 <- as.data.frame(bottlenecks[c(1:100),])
    bottlenecks_100$nodes <- rownames(bottlenecks_100)
    filename.bottlenecks_100 <-  paste0(filename.bottlenecks,"_top100.tsv")
    write.table(bottlenecks_100, file=filename.bottlenecks_100, sep='\t', row.names = FALSE)
  }
  filename <- paste0(filename.bottlenecks,".tsv")
  write.table(bottlenecks, file=filename, sep='\t', row.names= FALSE)
}

#' @title Number of ROC curves for a repsonse vector
#' @description 
#'
#' @details 
#'
#' @param response: a vector containg each possible response
#'
#' @return return the number of ROC curves (possible unique association in response)

Number.ROC <- function(response){

n <- 0
len <- length(response)
while(len != 1)
{

  len <- len-1
  n <- len+n
}
return(n)
}

#' @title Recode categorial variable
#' @description Recode a numeric categorial variable with strings from a response vector
#'
#' @details 
#'
#' @param data: a dataframe containing variables to recode
#' @param col.number: the numeric position of the column in the datafrale
#' @param response: a vector containg each possible response
#'
#' @return return the number of ROC curves (possible unique association in response)

Recode.response <- function(data, col.number, response)
{
x <- 0
for (i in 0:length(response)) 
{
  for (j in 1:dim(data)[1])
  {
    if(is.na(as.numeric(data[j,col.number]))) next
    if(as.numeric(data[j,col.number]) == x)
    {
      data[j,col.number] <- response[i]
    }
    print(x)
    print(data[j,col.number])

  }
  
  x <- x+1  
}
return(data)
}