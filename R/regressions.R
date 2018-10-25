#' @title Global modelisation method 
#'
#' @description 
#'
#' @details Call example : model(data, "group", "random_test", 0.95, vecteur_explanationvar).
#' mode take "everyone" by default. everyone mean that the predict will be made on every available individual.
#' if mode = random_reset, a learning sample with reset will me made and the predict data will be the validation sample.
#' if mode = random, a learning sample will me made by randomly sampling patients in the dataframe. Predict data will be the validation sample.
#' if mode = a vector with patients id's for the learning sample, these patients will be in the learning sample and the predict data will be the validation sample.
#'
#' @param dataframe : a dataframe from list_type function.
#' @param dependantvar : the name between quotes of the dependant variable.
#' @param mode : the sample technique to use.
#' @param level : the level of confidence for the prediction ex: 0.95 (equal alpha risk 5 percent).
#' @param explanationvar : a vector with the names of columns refering to explanation variables.
#'
#' @return return the model, objects with data for the report creation.
#' @return if the dependant variable is binary, then also return roc object
#' 
#' @import pROC
#' @import ggplot2
#'
model <- function(dataframe, dependantvar, mode = "everyone", level = 0.95, explanationvar, switch, response = NULL)
{
if(length(dependantvar) >1)
{
  i <- 1
  dependantvar.word <- paste0("^",var,"$", sep="")
  type.var[i] <- dataframe$type[grep(dependantvar.word, colnames(dataframe$data))]
  i <- i+1
  for(var in dependantvar)
  {
    dependantvar.word <- paste0("^",var,"$", sep="")
    type.var[i] <- dataframe$type[grep(dependantvar.word, colnames(dataframe$data))]
    if(type.var[i] != type.var[i-1])
    {
      stop("Dependant variables types are not equals")
    }
  }
  type.var <- type.var[i]
}else
{
  dependantvar.word <- paste0("^",dependantvar,"$", sep="")
  type.var <- dataframe$type[grep(dependantvar.word, colnames(dataframe$data))]
}

	if(switch == TRUE)
	{
		print("There is only one explanation variable, the correlation function is called")
		print("The result of the function model is going to be a coefficient of correlation")

		result.predict <- correlation(dataframe, dependantvar, explanationvar)
	}
	else
	{

		if(mode == "everyone")
		{
		  dataframe <- data
			data.model <- dataframe$data
			data.predict <- dataframe$data[,c(dependantvar,explanationvar)]
		}else if(mode == "random_reset")
		{
			data.model <- Learning_Sample(dataframe$data, TRUE)
			data.predict <- Validation_Sample(data.model,dataframe$data)
		}else if(mode == "random")
		{
			data.model <- Learning_Sample(dataframe$data, FALSE)
			data.predict <- Validation_Sample(data.model,dataframe$data)
		}else if (is.vector(mode))
		{
			data.model <- dataframe$data[c(mode),]
			data.predict <- Validation_Sample(data.model,dataframe$data)
		}else
		{
			print("The mode parameter is wrong, please check the man page")
		}


		if(is.null(response) == FALSE)
		{
		  if(length(response) > 2)
		  {
		    model <- model.multinomial.logistic(data.model,data.predict, dependantvar,level, explanationvar, switch, response)
		    
		  }else if ((type.var == "Binary" || type.var == "Categorial"))
		    {
		      model <- model.logistic(data.model,data.predict, dependantvar,level, explanationvar, switch, response)
		    }

		}
		else if (type.var == "Continuous" && is.null(response) == TRUE)
		{
			model <- model.linear(data.model,data.predict, dependantvar, level, explanationvar, switch)
		}	
	
	}
return(model)
}

#' @title Automatized model selection
#'
#' @description 
#'
#' @details Call example : model_selection(data, "group", "random_test", 0.95, vecteur_explanationvar, FALSE).
#' mode take "everyone" by default. everyone mean that the predict will be made on every available individual
#' if mode = random_reset, a learning sample with reset will me made and the predict data will be the validation sample.
#' if mode = random, a learning sample will me made by randomly sampling patients in the dataframe. Predict data will be the validation sample.
#' if mode = a vector with patients id's for the learning sample, these patients will be in the learning sample and the predict data will be the validation sample.
#'
#' @param dataframe : a dataframe from list_type function.
#' @param dependantvar : the name between quotes of the dependant variable.
#' @param mode : the sample technique to use.
#' @param level : the level of confidence for the prediction ex: 0.95 (equal alpha risk 5 percent).
#' @param explanationvar : a vector with the names of columns refering to explanation variables.
#'
#' @return return the optimal model (selection based on pvalues) 
#' 
#' @import pROC
#' @import ggplot2
#' @import nnet


model_selection <- function(dataframe, dependantvar, mode = "everyone", level = 0.95, explanationvar, switch, response)
{


	count <- 0
	response <- response
	roc.curve <- NULL
	frmla.model <- as.formula(paste(dependantvar, paste("~"), paste(explanationvar, collapse = "+"),sep =""))
	type.var <- dataframe$type[grep(dependantvar, colnames(dataframe$data))]

	model <- model(dataframe, dependantvar, mode, level, explanationvar, switch, response)

	pvalues <- summary(model[[1]])[["coefficients"]][,4]
	pvalues <- pvalues[-1]
	to.remove <- which.max(pvalues)

		while(pvalues[to.remove] > 1-level)
		{
		  if(as.numeric(length(explanationvar) <= 1)){break}
		  
			model <- model(dataframe, dependantvar, mode, level, explanationvar, switch, response)
			pvalues <- summary(model[[1]])[["coefficients"]][,4]
			pvalues <- pvalues[-1]
			to.remove <- as.numeric(which.max(pvalues))
			explanationvar <- explanationvar[-to.remove]
			count <- count+1
			print(count)
			print(pvalues[to.remove])
			
		}
	if(type.var == "Binary" || type.var == "Categorial")
	{
	  load("report_binary.Rdata")
	  frmla.no.selection <- frmla.model
	  list_object <- c("model","roc", "matrice_roc", "pval", "dataplot","coefficients.pvalues.table" ,"coefficients.pvalues","specificity.sensitivity","model.accuracy.percentage", "model.accuracy", "frmla.no.selection")
	  
	  save(list = list_object,file="report_binary.Rdata")
	}else
	{
	  load("report_continuous.Rdata")
	  frmla.no.selection <- frmla.model
	  list_object <- c("model","dataplot","coefficients.pvalues.table" ,"coefficients.pvalues","frmla.no.selection", "pval", "pdelta", "square_value","plot_pred", "plot_coef", "plot_top")
	  save(list = list_object,file="report_continuous.Rdata")
	}

	return(model)


}




#' @title Logistic regression function
#'
#' @description 
#'
#' @details Call example : model.logistic(data, "group", 0.95, vecteur_explanationvar).
#'
#' @param dataframe : a dataframe from list_type function.
#' @param dependantvar : the name between quotes of the dependant variable : value needs to be binary or categorial
#' @param mode : the sample technique to use.
#' @param level : the level of confidence for the prediction ex: 0.95 (equal alpha risk 5 percent).
#' @param explanationvar : a vector with the names of columns refering to explanation variables.
#'
#' @return return the model, objects with data for the report creation.
#' @return if the dependant variable is binary, then also return roc object
#' 
#' @import pROC
#' @import ggplot2
#'



model.logistic <- function(data.model,data.predict, dependantvar, level = 0.95, explanationvar, switch, response){

			frmla <- as.formula(paste(dependantvar, paste("~"), paste(explanationvar, collapse = "+"),sep =""))
			model <- glm(formula = frmla, family = "binomial", data = data.model, na.action = na.exclude)

			data.predict.noNA <- na.omit(data.predict)
			validation.pred.values <- data.predict.noNA[,c(dependantvar)]

			result.predict <- predict.glm(model, data.predict, level=0.95, na.action = na.exclude)

			roc <- roc(validation.pred.values,result.predict)
	    	auc <- auc(roc)
	    	auc <- formatC(as.numeric(auc), digits = 4)
	    	matrice_roc <- coords(roc,x = "best", ret = c("fp","tp","fn","tn", "1-specificity", "1-sensitivity", "sensitivity", "specificity", "threshold"))
	    	
	    	#####Data collection
	    	status2 <- paste0("status-",response[2])
	    	status1 <- paste0("status-",response[1])
	    	model.accuracy <- data.frame(no = numeric(),  yes= numeric(), check.names = FALSE)
	      	colnames(model.accuracy)[1] <- status1
	      	colnames(model.accuracy)[2] <- status2
	    	list.accuracy.indicators <- matrice_roc
	    	model.accuracy[nrow(model.accuracy)+1,] <- format(as.numeric(c(list.accuracy.indicators[4],list.accuracy.indicators[3])), digits = 4)
	    	model.accuracy[nrow(model.accuracy)+1,] <- format(as.numeric(c(list.accuracy.indicators[1],list.accuracy.indicators[2])), digits = 4)
	    	rownames(model.accuracy) <- c(paste0("predicted-",response[1]), paste0("predicted-",response[2]))
	    	#
	    	#####Percentage version of this table
	    	model.accuracy.percentage <- data.frame(no = numeric(), yes = numeric(), check.names = FALSE)
	    	colnames(model.accuracy.percentage)[1] <- status1
	    	colnames(model.accuracy.percentage)[2] <- status2
	    	total.individuals <- list.accuracy.indicators[1] + list.accuracy.indicators[2] + list.accuracy.indicators[3] + list.accuracy.indicators[4]
	    	model.accuracy.percentage[nrow(model.accuracy.percentage)+1,] <- format(as.numeric(c(((list.accuracy.indicators[4]/total.individuals)*100),((list.accuracy.indicators[3]/total.individuals)*100))), nsmall = 2, digits = 3)
	    	model.accuracy.percentage[nrow(model.accuracy.percentage)+1,] <- format(as.numeric(c(((list.accuracy.indicators[1]/total.individuals)*100),((list.accuracy.indicators[2]/total.individuals)*100))), nsmall = 2, digits = 3)
	    	rownames(model.accuracy.percentage) <- c(paste0("predicted-",response[1]), paste0("predicted-",response[2]))
	    	#

	    	#####Specificity
	    	specificity.sensitivity <- data.frame(specificity= numeric(), sensitivity = numeric(), "1-specificity" = numeric(), "1-sensitivity" = numeric(), check.names = FALSE)
	    	specificity.sensitivity[nrow(specificity.sensitivity)+1,] <- format(as.numeric(c(list.accuracy.indicators[8],list.accuracy.indicators[7],list.accuracy.indicators[5],list.accuracy.indicators[6])), digits = 4, nsmall = 4)
	    	#
	    	#####P-values
	    	coefficients.pvalues <- as.data.frame(summary(model)$coefficients[,c(1,4)], check.names = FALSE)
	    	colnames(coefficients.pvalues)[1] <- "coefficients"
	    	colnames(coefficients.pvalues)[2] <- "p.values"
	    	coefficients.pvalues <- coefficients.pvalues[-1,]
	    	
	    	coefficients.pvalues.table <- as.data.frame(summary(model)$coefficients[,c(1,4)], check.names = FALSE)
	    	colnames(coefficients.pvalues.table)[1] <- "coefficients"
	    	colnames(coefficients.pvalues.table)[2] <- "p.values"
	    	coefficients.pvalues.table <- coefficients.pvalues.table[-1,]
	    	coefficients.pvalues.table$p.values <- format(coefficients.pvalues.table$p.values, digits = 2, nsmall = 2 )
	    	coefficients.pvalues.table$coefficients <- format(coefficients.pvalues.table$coefficients, digits = 2, nsmall = 2)
	    	
	    	coefficients.pvalues$p.values <- -log10(as.numeric(coefficients.pvalues$p.values))
	    	coefficients.pvalues$p.values <- format(as.numeric(coefficients.pvalues$p.values), digits = 2, nsmall = 2) 
	    	coefficients.pvalues$p.values <- as.numeric(coefficients.pvalues$p.values)
	    	coefficients.pvalues$coefficients <- format(as.numeric(coefficients.pvalues$coefficients), digits = 2, nsmall = 2)
	    	coefficients.pvalues$coefficients <- as.numeric(coefficients.pvalues$coefficients)
	    	coefficients.pvalues <- as.data.frame(coefficients.pvalues)
	    	
	    	abs.coef <- abs(coefficients.pvalues$coefficients)
	    	abs.coef.max <- as.numeric(which.max(abs.coef))
	    	
	    	max.log10 <- as.numeric(which.max(coefficients.pvalues$p.values))
	    	
	    	if(abs.coef[abs.coef.max] <= 10)
	    	{

	    	pval <- ggplot(coefficients.pvalues, aes(x=coefficients.pvalues$coefficients, y=coefficients.pvalues$p.values, label = rownames(coefficients.pvalues))) + geom_jitter() + geom_text(vjust = -.5) + ylab("-log(p.values)") + geom_hline(aes(yintercept=1.30), colour = "red") + geom_text(aes(0,1.30,label = "p.value = 0.05", vjust = -1, hjust = -.1, colour = "red")) + theme(legend.position="none") + scale_x_continuous(name="coefficients values", limits=c(-10, 10)) + scale_y_continuous(name="-log10(p.values)", limits=c(0, coefficients.pvalues$p.values[max.log10]+1))

	    	}else if(abs.coef[abs.coef.max] <= 20)
	    	{

	    	pval <- ggplot(coefficients.pvalues, aes(x=coefficients.pvalues$coefficients, y=coefficients.pvalues$p.values, label = rownames(coefficients.pvalues))) + geom_jitter() + geom_text(vjust = -.5) + ylab("-log(p.values)") + geom_hline(aes(yintercept=1.30), colour = "red") + geom_text(aes(0,1.30,label = "p.value = 0.05", vjust = -1, hjust = -.1, colour = "red")) + theme(legend.position="none") + scale_x_continuous(name="coefficients values", limits=c(-20, 20)) + scale_y_continuous(name="-log10(p.values)", limits=c(0, coefficients.pvalues$p.values[max.log10]+1))

	    	}else if(abs.coef[abs.coef.max] <= 40)
	    	{

	    	pval <- ggplot(coefficients.pvalues, aes(x=coefficients.pvalues$coefficients, y=coefficients.pvalues$p.values, label = rownames(coefficients.pvalues))) + geom_jitter() + geom_text(vjust = -.5) + ylab("-log(p.values)") + geom_hline(aes(yintercept=1.30), colour = "red") + geom_text(aes(0,1.30,label = "p.value = 0.05", vjust = -1, hjust = -.1, colour = "red")) + theme(legend.position="none") + scale_x_continuous(name="coefficients values", limits=c(-40, 40)) + scale_y_continuous(name="-log10(p.values)", limits=c(0, coefficients.pvalues$p.values[max.log10]+1))

	    	}else
	    	{

	    	pval <- ggplot(coefficients.pvalues, aes(x=coefficients.pvalues$coefficients, y=coefficients.pvalues$p.values, label = rownames(coefficients.pvalues))) + geom_jitter() + geom_text(vjust = -.5) + ylab("-log(p.values)") + geom_hline(aes(yintercept=1.30), colour = "red") + geom_text(aes(0,1.30,label = "p.value = 0.05", vjust = -1, hjust = -.1, colour = "red")) + theme(legend.position="none") + scale_x_continuous(name="coefficients values", limits=c(-abs.coef[abs.coef.max], abs.coef[abs.coef.max])) + scale_y_continuous(name="-log10(p.values)", limits=c(0, coefficients.pvalues$p.values[max.log10]+1))

	    	}	    	
	    	#

	    	#####Table prediction delta
	    	threshold <- list.accuracy.indicators[9]
	    	datamodel <- as.data.frame(model$y)
	    	datapredict <- NULL
	    	datapredict$predicted <- ifelse(result.predict>threshold,1,0)
	    	datapredict <- as.data.frame(datapredict)
	    	colnames(datamodel)[1] <- "status"
	    	colnames(datapredict)[1] <- "predicted"
	    	
	    	dataplot <- cbind(datamodel,datapredict)
	    	if(dim(model$data)[1] != (dim(dataplot)[1]))
	    	{
	    	  dataplot$individuals <- rownames(model[["model"]])
	    	}else
	    	{
	    	  dataplot$individuals <- model$data[,1]
	    	}
	    	dataplot$status <- ifelse(dataplot$status == 0, response[1], response[2])
	    	dataplot$predicted <- ifelse(dataplot$predicted == 0, response[1], response[2])
	    	dataplot <- dataplot[,c(3,1,2)]
	    	colnames(dataplot)[1] <- "ID"
	    	#

	    	frmla.no.selection <- NULL
	    	list_object <- c("model","roc", "matrice_roc", "pval", "dataplot","coefficients.pvalues.table" ,"coefficients.pvalues","specificity.sensitivity","model.accuracy.percentage", "model.accuracy", "frmla.no.selection")
	    	save(list = list_object,file="report_binary.Rdata")

	    	return(list(model,result.predict, matrice_roc,roc))
}

#' @title Multinomial Logistic regression function
#'
#' @description 
#'
#' @details Call example : model.logistic(data, "group", 0.95, vecteur_explanationvar).
#'
#' @param dataframe : a dataframe from list_type function.
#' @param dependantvar : the name between quotes of the dependant variable : value needs to be binary or categorial
#' @param mode : the sample technique to use.
#' @param level : the level of confidence for the prediction ex: 0.95 (equal alpha risk 5 percent).
#' @param explanationvar : a vector with the names of columns refering to explanation variables.
#'
#' @return return the model, objects with data for the report creation.
#' @return if the dependant variable is binary, then also return roc object
#' 
#' @import pROC
#' @import ggplot2
#' @import ordinal
#'
model.multinomial.logistic <- function(data.model,data.predict, dependantvar, level = 0.95, explanationvar, switch, response){
  
  frmla <- as.formula(paste(dependantvar, paste("~"), paste(explanationvar, collapse = "+"),sep =""))

  
  model <- clm(frmla, data = data.model, na.action = na.omit)
  
  nbre.roc <- Number.ROC(response)

  data.predict.noNA <- na.omit(data.predict)
  validation.pred.values <- data.predict.noNA[,c(dependantvar)]
  result.predict <- predict(model,data.predict.noNA )
  
  roc <- multiclass.roc(validation.pred.values,result.predict$fit)
  rocs <- roc[['rocs']]

  for (i in 1:nbre.roc) 
  {
  	  assign(paste("roc.curve", i, sep = ""), plot.roc(rocs[[i]]))
  	  assign(paste("matrice_roc", i, sep = ""), coords(rocs[[i]],x = "best", ret = c("fp","tp","fn","tn", "1-specificity", "1-sensitivity", "sensitivity", "specificity", "threshold"))) 
  }
 
  #####Data collection
  summary <- summary(model)
  pvalues.table <- as.data.frame(summary$coefficients[,4])
  coefficients.table <- as.data.frame(summary$coefficients[,1])
  #

  #####P-values
  coefficients.pvalues.table <- cbind(pvalues.table,coefficients.table)
  colnames(coefficients.pvalues.table)[1] <- "p.values"
  colnames(coefficients.pvalues.table)[2] <- "coefficients"
  coefficients.pvalues.table <- coefficients.pvalues.table[-c(1:length(response)-1),]
  coefficients.pvalues <- coefficients.pvalues.table
  coefficients.pvalues.table <- coefficients.pvalues.table[order(coefficients.pvalues.table$p.values , decreasing = FALSE),]
  coefficients.pvalues.table$p.values <- format(coefficients.pvalues.table$p.values, digits = 2, nsmall = 2 )
  coefficients.pvalues.table$coefficients <- format(coefficients.pvalues.table$coefficients, digits = 2, nsmall = 2)

  coefficients.pvalues$p.values <- -log10(as.numeric(coefficients.pvalues$p.values))
  coefficients.pvalues$p.values <- format(as.numeric(coefficients.pvalues$p.values), digits = 2, nsmall = 2) 
  coefficients.pvalues$p.values <- as.numeric(coefficients.pvalues$p.values)
  coefficients.pvalues$coefficients <- format(as.numeric(coefficients.pvalues$coefficients), digits = 2, nsmall = 2)
  coefficients.pvalues$coefficients <- as.numeric(coefficients.pvalues$coefficients)
  coefficients.pvalues$p.values <- ifelse(coefficients.pvalues$p.values == "Inf",0,coefficients.pvalues$p.values)
  coefficients.pvalues <- as.data.frame(coefficients.pvalues)

  abs.coef <- abs(coefficients.pvalues$coefficients)
  abs.coef.max <- as.numeric(which.max(abs.coef))
  
  max.log10 <- as.numeric(which.max(coefficients.pvalues$p.values))
  

  if(abs.coef[abs.coef.max] <= 10)
  {
    
    pval <- ggplot(coefficients.pvalues, aes(x=coefficients.pvalues$coefficients, y=coefficients.pvalues$p.values, label = rownames(coefficients.pvalues))) + geom_jitter() + geom_text(vjust = -.5) + ylab("-log(p.values)") + geom_hline(aes(yintercept=1.30), colour = "red") + geom_text(aes(0,1.30,label = "p.value = 0.05", vjust = -1, hjust = -.1, colour = "red")) + theme(legend.position="none") + scale_x_continuous(name="coefficients values", limits=c(-10, 10)) + scale_y_continuous(name="-log10(p.values)", limits=c(0, coefficients.pvalues$p.values[max.log10]+1))
    
  }else if(abs.coef[abs.coef.max] <= 20)
  {
    
    pval <- ggplot(coefficients.pvalues, aes(x=coefficients.pvalues$coefficients, y=coefficients.pvalues$p.values, label = rownames(coefficients.pvalues))) + geom_jitter() + geom_text(vjust = -.5) + ylab("-log(p.values)") + geom_hline(aes(yintercept=1.30), colour = "red") + geom_text(aes(0,1.30,label = "p.value = 0.05", vjust = -1, hjust = -.1, colour = "red")) + theme(legend.position="none") + scale_x_continuous(name="coefficients values", limits=c(-20, 20)) + scale_y_continuous(name="-log10(p.values)", limits=c(0, coefficients.pvalues$p.values[max.log10]+1))
    
  }else if(abs.coef[abs.coef.max] <= 40)
  {
    
    pval <- ggplot(coefficients.pvalues, aes(x=coefficients.pvalues$coefficients, y=coefficients.pvalues$p.values, label = rownames(coefficients.pvalues))) + geom_jitter() + geom_text(vjust = -.5) + ylab("-log(p.values)") + geom_hline(aes(yintercept=1.30), colour = "red") + geom_text(aes(0,1.30,label = "p.value = 0.05", vjust = -1, hjust = -.1, colour = "red")) + theme(legend.position="none") + scale_x_continuous(name="coefficients values", limits=c(-40, 40)) + scale_y_continuous(name="-log10(p.values)", limits=c(0, coefficients.pvalues$p.values[max.log10]+1))
    
  }else
  {
    
    pval <- ggplot(coefficients.pvalues, aes(x=coefficients.pvalues$coefficients, y=coefficients.pvalues$p.values, label = rownames(coefficients.pvalues))) + geom_jitter() + geom_text(vjust = -.5) + ylab("-log(p.values)") + geom_hline(aes(yintercept=1.30), colour = "red") + geom_text(aes(0,1.30,label = "p.value = 0.05", vjust = -1, hjust = -.1, colour = "red")) + theme(legend.position="none") + scale_x_continuous(name="coefficients values", limits=c(-abs.coef[abs.coef.max], abs.coef[abs.coef.max])) + scale_y_continuous(name="-log10(p.values)", limits=c(0, coefficients.pvalues$p.values[max.log10]+1))
    
  }	    	
  #
  result.predict.class <- predict(model, na.omit(data.predict), type = "class")
  result.predict.class <- result.predict.class$fit

  #####Table prediction delta
  datamodel <- as.data.frame(model$y)
  datapredict <- NULL
  datapredict$predicted <- result.predict.class
  datapredict <- as.data.frame(datapredict)
  colnames(datamodel)[1] <- "provided"
  colnames(datapredict)[1] <- "predicted"
  
  
  dataplot <- cbind(datamodel,datapredict)
  
  dataplot$individuals <- rownames(model[["model"]])

###automatisation

Recode.response(dataplot,1,response)
Recode.response(dataplot,2,response)

####PARTIE A AUTOMATISER ##CAT3
  #dataplot$provided <- ifelse(dataplot$provided == 0, response[1], ifelse(dataplot$provided == 1, response[2], ifelse(dataplot$provided == 2, response[3], ifelse(dataplot$provided == 3, response[4], ifelse(dataplot$provided == 4, response[5], dataplot$provided)))))

  #dataplot$predicted <- ifelse(dataplot$predicted == 0, response[1], ifelse(dataplot$predicted == 1, response[2], ifelse(dataplot$predicted == 2, response[3], ifelse(dataplot$predicted == 3, response[4], ifelse(dataplot$predicted == 4, response[5], dataplot$predicted)))))
####### ##CAT2
  #dataplot$provided <- ifelse(dataplot$provided == 0, response[1], ifelse(dataplot$provided == 1, response[2], ifelse(dataplot$provided == 2, response[3], dataplot$provided)))
  #dataplot$predicted <- ifelse(dataplot$predicted == 0, response[1], ifelse(dataplot$predicted == 1, response[2], ifelse(dataplot$predicted == 2, response[3], dataplot$predicted)))
########
  dataplot <- dataplot[,c(3,1,2)]
  colnames(dataplot)[1] <- "ID"                        
  ##
  

  model.accuracy <- data.frame(no = NA,  yes= NA, check.names = FALSE)
  
  
  for (i in 1:length(response))
  {
    colname <- paste0("predicted-", response[i])
    model.accuracy[colname] <- 0
    model.accuracy[nrow(model.accuracy)+1,] <- 0
    
  }
  model.accuracy <- as.data.frame(model.accuracy[-1,-c(1:2)])
  for (i in 1:length(response))
  {
    rownames(model.accuracy)[i] <- paste0("provided-", response[i])
  }
  
  

    
    for (j in 1:length(dataplot$provided))
    {
      if(dataplot$provided[j] == dataplot$predicted[j])
      {
        print(dataplot$provided[j])
        print(dataplot$predicted[j])
        col.number <- as.numeric(grep(dataplot$predicted[j], colnames(model.accuracy)))
        row.number <- as.numeric(grep(dataplot$provided[j], rownames(model.accuracy)))
        model.accuracy[row.number, col.number] <- model.accuracy[row.number, col.number] +1
      }else
      {
        print(j)
        col.number <- as.numeric(grep(dataplot$predicted[j], colnames(model.accuracy)))
        row.number <- as.numeric(grep(dataplot$provided[j], rownames(model.accuracy)))
        model.accuracy[row.number, col.number] <- model.accuracy[row.number, col.number] +1
      }
    }
n.tot <- length(dataplot$ID)

model.accuracy.percentage <- model.accuracy/n.tot*100
model.accuracy.percentage <- format(model.accuracy.percentage, digits = 1, nsmall = 1 )
  ##
  frmla.no.selection <- NULL
  list_object <- c("model","roc","coefficients.pvalues", "coefficients.pvalues.table", "frmla.no.selection","pval", "dataplot","model.accuracy", "model.accuracy.percentage")
  pdf("rocs.pdf")
  for (i in 1:nbre.roc) 
  {
  	  list_object <- append(list_object, paste0("roc.curve",i))
  	  list_object <- append(list_object, paste0("matrice_roc",i))
  	  plot.name <- noquote(paste0("roc.curve",i))
	  plot(get(plot.name))
  }
  dev.off()
  save(list = list_object,file="report_binary.Rdata")
}

#' @title linear modelisation method
#'
#' @description 
#'
#' @details Call example : model.linear(data, data.predict, "group", 0.95, vecteur_explanationvar, FALSE).
#'
#' @param dataframe : a dataframe from list_type function.
#' @param data.predict : a dataframe  with data to predict from learning.sample
#' @param dependantvar : the name between quotes of the dependant variable : values needs to be continuous
#' @param level : the level of confidence for the prediction ex: 0.95 (equal alpha risk 5 percent).
#' @param explanationvar : a vector with the names of columns refering to explanation variables.
#' @param switch : If TRUE and dependantvar
#'
#' @return return the model, objects with data for the report creation.
#' @return if the dependant variable is binary, then also return roc object
#' 
#' @import pROC
#' @import ggplot2
#'



model.linear <- function(data.model,data.predict, dependantvar, level = 0.95, explanationvar, switch)
{


			frmla <- as.formula(paste(dependantvar, paste("~"), paste(explanationvar, collapse = " + "),sep =""))
			model <- lm(formula = frmla, data = data.model, na.action = na.exclude)

			  data.predict.noNA <- na.omit(data.predict)
	    	validation.pred.values <- data.predict.noNA[,c(dependantvar)]

	    	result.predict <- predict.lm(model, data.predict, level=level, na.action = na.exclude)

	    	coefficients.pvalues <- as.data.frame(summary(model)$coefficients[,c(1,4)], check.nresames = FALSE)
	    	colnames(coefficients.pvalues)[1] <- "coefficients"
	    	colnames(coefficients.pvalues)[2] <- "p.values"
	    	coefficients.pvalues <- coefficients.pvalues[-1,]
	    	
	    	coefficients.pvalues.table <- as.data.frame(summary(model)$coefficients[,c(1,4)], check.names = FALSE)
	    	colnames(coefficients.pvalues.table)[1] <- "coefficients"
	    	colnames(coefficients.pvalues.table)[2] <- "p.values"
	    	coefficients.pvalues.table <- coefficients.pvalues.table[-1,]
	    	coefficients.pvalues.table$p.values <- format(coefficients.pvalues.table$p.values, digits = 3, nsmall = 3 )
	    	coefficients.pvalues.table$coefficients <- format(coefficients.pvalues.table$coefficients, digits = 2, nsmall = 2)
	    	
		    coefficients.pvalues$p.values <- -log10(as.numeric(coefficients.pvalues$p.values))
	    	coefficients.pvalues$p.values <- format(as.numeric(coefficients.pvalues$p.values), digits = 4, nsmall = 4) 
	    	coefficients.pvalues$p.values <- as.numeric(coefficients.pvalues$p.values)
	    	coefficients.pvalues$coefficients <- format(as.numeric(coefficients.pvalues$coefficients), digits = 4, nsmall = 4)
	    	coefficients.pvalues$coefficients <- as.numeric(coefficients.pvalues$coefficients)
	    	coefficients.pvalues <- as.data.frame(coefficients.pvalues)
	    	
	    	abs.coef <- abs(coefficients.pvalues$coefficients)
	    	abs.coef.max <- as.numeric(which.max(abs.coef))
	    	
	    	max.log10 <- as.numeric(which.max(coefficients.pvalues$p.values))
	    	
	    	if(is.data.frame(coefficients.pvalues) == FALSE) 
	    	{
	    		coefficients.pvalues <- as.data.frame(coefficients.pvalues)
	    	}

	    	##############################
	    	#table differences de prediction
	    	datamodel <- model$model[dependantvar]
	    	datamodel <- as.data.frame(datamodel, check.names = FALSE)
	    	datapredict <- model[["fitted.values"]]
	    	datapredict <- as.data.frame(datapredict, check.names = FALSE)
	    	colnames(datamodel)[1] <- "provided"
	    	colnames(datapredict)[1] <- "predicted"
	    	
	    	dataplot <- cbind(datamodel,datapredict)
	    	dataplot$individuals <- rownames(model[["model"]])
	    	dataplot <- dataplot[,c(3,1,2)]
	    	colnames(dataplot)[1] <- "ID"
	    	dataplot$delta <- abs(dataplot$predicted-dataplot$provided)
	    	dataplot$delta <- round(dataplot$delta,4)
	    	dataplot <- dataplot[order(dataplot$delta, decreasing = TRUE),]
	    	dataplot$ID <- factor(dataplot$ID, levels = dataplot$ID[order(dataplot$delta)]) #peut etre rev

	    	###############################
	    	#Sum(delta)² 
	    	square_value <- sqrt(sum(dataplot$delta^2))
	    	###############################
	    	#plot delta
	    	
	    	pdelta <- ggplot(dataplot, aes(x=dataplot$delta, y=dataplot$ID)) + geom_jitter() + ylab("abs(predicted-provided)") + theme( axis.text.y = element_text(color="#993333", size=2)) + scale_x_continuous(name="abs(predicted-provided)", limits=c(0, max(dataplot$delta)+20)) + ylab("Sample ID")
	    	
	    	plot(pdelta)
	    	
	    	##
	    	################################
	    	#plot pred
			for (i in 1:dim(dataplot)[1]) {
			  
			if(any(grep("A",dataplot[i,1])))
			{
			  dataplot[i,5] <-  "red"
			}else if(any(grep("B",dataplot[i,1])))
			{
			  dataplot[i,5] <-  "green"
			}else
			{
			  dataplot[i,5] <-  "blue"
			}
			
			}

			plot_pred <- ggplot(dataplot, aes(x=dataplot$provided, y=dataplot$predicted, label = rownames(dataplot), colour = dataplot[,5])) + geom_point() + scale_colour_identity() + geom_text(vjust = -.5) + ylab("predicted values")  + theme(legend.position="none", plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) + scale_x_continuous(name="provided values", limits=c(min(dataplot$provided)-1, max(abs(dataplot$provided) +1))) + scale_y_continuous(name="predicted values", limits=c(min(dataplot$predicted)-1, max(abs(dataplot$predicted) +1)))
	    	

	    	pval <- ggplot(coefficients.pvalues, aes(x=coefficients.pvalues$coefficients, y=coefficients.pvalues$p.values, label = rownames(coefficients.pvalues))) + geom_jitter() + geom_text(vjust = -.5) + ylab("-log(p.values)") + geom_hline(aes(yintercept=1.30), colour = "red") + geom_text(aes(0,1.30,label = "p.value = 0.05", vjust = -1, hjust = -.1, colour = "red")) + theme(legend.position="none", plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) + scale_x_continuous(name="coefficients values", limits=c(-abs.coef[abs.coef.max]-1, abs.coef[abs.coef.max]+1)) + scale_y_continuous(name="-log10(p.values)", limits=c(0, coefficients.pvalues$p.values[max.log10]+1))

	    	##

	    	################################
	    	#Top_coef

	    	coefficients <- as.data.frame(summary(model)$coefficients[,1])
	    	intercept.position <- as.numeric(grep("Intercept", rownames(coefficients)))
	    	coefficients <- t(coefficients)
	    	coefficients <- as.data.frame(coefficients[,-intercept.position])
	    	coefficients$genes <- rownames(coefficients)

	    	coefficients <- as.data.frame(coefficients[order(abs(coefficients[,1]), decreasing = TRUE),])
	    	top5_coef <- as.data.frame(coefficients[c(1:5),])

	    	plot_coef <- ggplot(coefficients, aes(x=coefficients$genes, y=coefficients[,1])) + ylab("coefficients values")  + theme(legend.position="none", axis.text.x = element_text(face="bold", color="black", size=10, angle=45, hjust = 1), axis.text.y = element_text(face="bold", color="black", size=10)) + scale_x_discrete(name="genes", breaks=coefficients$genes, labels=coefficients$genes) + scale_y_continuous(name="coefficients values", limits=c(-max(abs(coefficients[,1]) +1), max(abs(coefficients[,1]) +1))) + geom_bar(stat = "identity")

	    	plot_top <- ggplot(top5_coef, aes(x=top5_coef$genes, y=top5_coef[,1])) + ylab("coefficients values")  + theme(legend.position="none", axis.text.x = element_text(face="bold", color="black", size=10, angle=45, hjust = 1), axis.text.y = element_text(face="bold", color="black", size=10)) + scale_x_discrete(name="genes", breaks=top5_coef$genes, labels=top5_coef$genes) + scale_y_continuous(name="coefficients values", limits=c(-max(abs(top5_coef[,1]) +1), max(abs(top5_coef[,1]) +1))) + geom_bar(stat = "identity")


		   	frmla.no.selection <- NULL
	    	list_object <- c("model", "dataplot", "pval", "coefficients.pvalues.table", "coefficients.pvalues", "frmla.no.selection", "pdelta", "square_value", "plot_pred", "plot_coef", "plot_top")
	    	save(list = list_object,file="report_continuous.Rdata")
	    	return(list(model,pval,dataplot,coefficients.pvalues.table,coefficients.pvalues, square_value, plot_pred, plot_coef, plot_top))
}
