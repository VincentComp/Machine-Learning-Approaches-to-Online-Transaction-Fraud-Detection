# Install packages
#install.packages('dplyr')
#install.packages('forcats')
#install.packages('ggcorrplot')
#install.packages('ggplot2')
#install.packages('patchwork')
#install.packages('tidyr')

# Load libraries
library(dplyr)
library(forcats)
library(ggcorrplot)
library(ggplot2)
library(patchwork)
library(tidyr)

# Load the data
dataset = read.csv("./A1_data.csv")


#====================Distinguish Attributes====================
str(dataset)
summary(dataset)

labels = dataset[2]
features = dataset[-2]

table(sapply(features, class)) #distribution of type of data

#split column into labels, numerical, categorical
labelCol = names(labels)
numericalCols  <- names(features)[sapply(features, is.numeric)]            
categoricalCols <- names(features)[!sapply(features, is.numeric)]
stopifnot("The cols does not match after split" = setequal(c(numericalCols,categoricalCols),names(features))) # Check cols match after split

#Print the result
print(paste("Number of numericial feature =",length(numericalCols)))
print("Names of numerical feature:")
print(numericalCols)

print(paste("Number of categorical feature =",length(categoricalCols)))
print("Names of categorical feature:")
print(categoricalCols)


#====================Identification and Handling of Missing Values====================
#--------------------Identification --------------------
#Remove column with 100 % missing
missRatio <- colMeans(is.na(dataset))
allMissColName <- names(missRatio[missRatio == 1])
datasetCleanMiss <- dataset[, missRatio < 1, drop = FALSE] #keep dim

#Print the result
print("Table of missRatio (before removal)")
print(missRatio)
print(paste("Number of columns with 100% missing data =",length(allMissColName)))


#Update the columnsName
labelCol <- labelCol[!labelCol %in% allMissColName]
numericalCols <- numericalCols[!numericalCols %in% allMissColName] #remove discarded-col name for numerical
categoricalCols <- categoricalCols[!categoricalCols %in% allMissColName] #remove discarded-col name for categorical
missRatio <- missRatio[!names(missRatio) %in% allMissColName]
stopifnot("The cols does not match after remove" = setequal(c(labelCol,numericalCols,categoricalCols),names(datasetCleanMiss))) # Check cols match after remove

#Print the result after clean (for checking)
missRatio <- colMeans(is.na(datasetCleanMiss))
print("Table of missRatio (after removal)")
print(missRatio)

#--------------------Handling --------------------
#Imputate missing with constant  
datasetCleanMiss <- datasetCleanMiss %>% #pipe operator
  mutate(
    #Numerical:: median imputation
    across(
      all_of(numericalCols),
      ~ replace_na(.x, median(.x, na.rm = TRUE)) #lambda for imputate NA to Mean
    ),
    
    #Categorical:: "miss" (p.s. convert logic to string first -> avoid type mismatch)
    across(
      all_of(categoricalCols),
      ~ replace_na(if (is.logical(.x)) as.character(.x) else .x, "miss")
    )
  )

#Remove constant columns
nDistinctVec <- datasetCleanMiss %>% 
  summarise(across(everything(), ~ n_distinct(.))) %>%
  unlist(use.names = TRUE)
constantColName <- names(nDistinctVec)[nDistinctVec == 1] #get the column  name

datasetCleanMiss <- datasetCleanMiss %>%
  select(where(~ n_distinct(.) > 1))  #Drop constant volumn

print(paste("Number of Constant Column ", length(constantColName)))
print(paste("Name of Constant Column ", constantColName))
labelCol <- labelCol[!labelCol %in% constantColName] #update the column name list
numericalCols <- numericalCols[!numericalCols %in% constantColName]
categoricalCols <- categoricalCols[!categoricalCols %in% constantColName]



#=====================Univariate Analysis==========================
#------Plot the histogram of numerical features in 4*4 layout------
plots <- list() #list of plot
for (col in numericalCols) {
  p <- ggplot(datasetCleanMiss, aes(x = .data[[col]])) +
    geom_histogram(bins = 20, na.rm = TRUE) +
    theme_minimal() +
    labs(
      title = paste0("Histogram of ", col),
      x = col,
      y = "Count"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 8),
      axis.text  = element_text(size = 7)
    )
  
  plots[[length(plots) + 1]] <- p
}

group_size <- 16
num_groups <- ceiling(length(plots) / group_size)

for (i in seq_len(num_groups)) {
  start_idx <- (i - 1) * group_size + 1
  end_idx   <- min(i * group_size, length(plots))
  subset_plots <- plots[start_idx:end_idx]
  
  #if less than 16 plot -> add empty plot to keep the shape 
  if (length(subset_plots) < group_size) {
    n_missing   <- group_size - length(subset_plots)
    blank_plots <- replicate(n_missing, patchwork::plot_spacer(), simplify = FALSE)
    subset_plots <- c(subset_plots, blank_plots)
  }
  
  combined_plot <- wrap_plots(subset_plots, ncol = 4, nrow = 4)
  print(combined_plot)
}


#------Plot the box-and-whisker of numerical features in 4*4 layout------
plots <- list()

for (col in numericalCols) {
  
  p <- ggplot(datasetCleanMiss, aes(x = .data[[col]])) +
    geom_boxplot(na.rm = TRUE) +
    theme_minimal() +
    labs(
      title = paste0("Boxplot of ", col),
      x = col,
      y = NULL
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 8),
      axis.text  = element_text(size = 7)
    )
  
  plots[[length(plots) + 1]] <- p
}

group_size <- 16
num_groups <- ceiling(length(plots) / group_size)

for (i in seq_len(num_groups)) {
  
  start_idx <- (i - 1) * group_size + 1
  end_idx   <- min(i * group_size, length(plots))
  
  subset_plots <- plots[start_idx:end_idx]
  
  #if less than 16 plot -> add empty plot to keep the shape 
  if (length(subset_plots) < group_size) {
    n_missing   <- group_size - length(subset_plots)
    blank_plots <- replicate(n_missing, patchwork::plot_spacer(), simplify = FALSE)
    subset_plots <- c(subset_plots, blank_plots)
  }
  
  combined_plot <- wrap_plots(subset_plots, ncol = 4, nrow = 4)
  
  print(combined_plot)
}

#------Plot the bar chart of labels and categorical features in 4*4 layout------
plots  <- list()
top_n  <- 15
catCols_all <- c("isFraud", categoricalCols)

for (col in catCols_all) {
  
  dfPlot <- datasetCleanMiss %>%
    dplyr::transmute(val = .data[[col]]) %>%
    dplyr::mutate(val = as.factor(val))
  
  #number of remaining categories after top_n
  n_total  <- dplyr::n_distinct(dfPlot$val, na.rm = TRUE)
  n_remain <- max(n_total - top_n, 0)
  
  dfPlot <- dfPlot %>%
    dplyr::mutate(
      val = fct_lump_n(val, n = top_n,
                       other_level = paste0(n_remain, "-others"))
    )
  
  p <- ggplot(dfPlot, aes(x = fct_infreq(val))) +
    geom_bar(na.rm = TRUE) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste0("Bar Chart of ", col, " (Top ", top_n, " + Others)"),
      x = col,
      y = "Count"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 8),
      axis.text  = element_text(size = 7)
    )
  
  plots[[length(plots) + 1]] <- p
}

group_size <- 16
num_groups <- ceiling(length(plots) / group_size)

for (i in seq_len(num_groups)) {
  
  start_idx <- (i - 1) * group_size + 1
  end_idx   <- min(i * group_size, length(plots))
  
  subset_plots <- plots[start_idx:end_idx]
  
  #if less than 16 plot -> add empty plot to keep the shape 
  if (length(subset_plots) < group_size) {
    n_missing   <- group_size - length(subset_plots)
    blank_plots <- replicate(n_missing, patchwork::plot_spacer(), simplify = FALSE)
    subset_plots <- c(subset_plots, blank_plots)
  }
  
  combined_plot <- wrap_plots(subset_plots, ncol = 4, nrow = 4)
  print(combined_plot)
}




#===============Outlier Detection and Management===============
capRateMin <- 0.001   #If capRate < 0.1% then skip winsoring：
iqrEps <- 0           #If IQR = 0, then skip



#-----------Identify the outlier----------------
#Calculate the outerlier rate
outlierStats <- lapply(numericalCols, function(col){
  x <- datasetCleanMiss[[col]]
  
  medianVal <- median(x, na.rm = TRUE)
  iqrVal <- IQR(x, na.rm = TRUE)
  
  lowerBound <- medianVal - 3 * iqrVal / (2 * 0.6745)
  upperBound <- medianVal + 3 * iqrVal / (2 * 0.6745)
  
  outlierMask <- is.finite(x) & (x < lowerBound | x > upperBound)
  outlierRate <- mean(outlierMask, na.rm = TRUE)
  
  data.frame(
    feature = col,
    outlierRate = outlierRate
  )
})
outlierStats <- dplyr::bind_rows(outlierStats)
print(outlierStats)

#Calculate the average outerlier rate
avgOutlierRate <- mean(outlierStats$outlierRate, na.rm = TRUE)
print(paste("Average outlier rate =", round(avgOutlierRate, 4)))



#-----------Handle the outlier----------------
datasetCleanOutlier <- datasetCleanMiss %>%
  mutate(
    across(
      all_of(numericalCols),
      ~ {
        x <- .x

        medianVal <- median(x, na.rm = TRUE)
        iqrVal <- IQR(x, na.rm = TRUE)

        #Protection 1: IQR too small => skip
        if (is.na(iqrVal) || iqrVal <= iqrEps) return(x)
        lowerBound <- medianVal - 3 * iqrVal / (2 * 0.6745)
        upperBound <- medianVal + 3 * iqrVal / (2 * 0.6745)

        #Protection 2: Too few values would be capped => skip
        capMask <- is.finite(x) & (x < lowerBound | x > upperBound)
        capRate <- mean(capMask, na.rm = TRUE)
        if (is.na(capRate) || capRate < capRateMin) return(x)

        #Winsorize
        x <- pmin(pmax(x, lowerBound), upperBound)
        x #return x
      }
    )
  )







#==========================Bivariate Analysis==========================
#------Bivariate hist on numerical features------
plots <- list()

for (col in numericalCols) {
  
  p <- ggplot(
    datasetCleanOutlier,
    aes(x = .data[[col]], fill = as.factor(isFraud))
  ) +
    
    geom_histogram(
      bins = 30,
      alpha = 0.8,
      color = "black",
      na.rm = TRUE
    ) +
    
    facet_wrap(~ isFraud, ncol = 1, scales = "free_y") +   # ⭐ KEY LINE
    
    scale_fill_manual(values = c("0" = "#F8766D",
                                 "1" = "#00BFC4")) +
    
    theme_minimal() +
    
    labs(
      title = paste0("Histogram of ", col, " by Label"),
      x = col,
      y = "Count",
      fill = "Label"
    ) +
    
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 8),
      axis.text  = element_text(size = 7),
      strip.text = element_text(size = 9, face = "bold")
    )
  
  plots[[length(plots)+1]] <- p
}


#4x4 layout 
group_size <- 16
num_groups <- ceiling(length(plots)/group_size)

for(i in seq_len(num_groups)){
  
  start_idx <- (i-1)*group_size + 1
  end_idx   <- min(i*group_size, length(plots))
  
  subset_plots <- plots[start_idx:end_idx]
  
  if(length(subset_plots) < group_size){
    n_missing <- group_size - length(subset_plots)
    subset_plots <- c(
      subset_plots,
      replicate(n_missing, patchwork::plot_spacer(), simplify = FALSE)
    )
  }
  
  combined_plot <- wrap_plots(subset_plots, ncol=4, nrow=4)
  print(combined_plot)
}





#------Bivariate box on numerical features------
plots <- list()

for (col in numericalCols) {
  
  p <- ggplot(datasetCleanOutlier,
              aes(x = as.factor(isFraud),
                  y = .data[[col]],
                  fill = as.factor(isFraud))) +
    
    geom_boxplot(na.rm = TRUE, alpha = 0.8) +
    
    scale_fill_manual(values = c("#F8766D","#00BFC4")) +
    
    theme_minimal() +
    labs(
      title = paste0("Boxplot of ", col, " by Label"),
      x = "Label",
      y = col,
      fill = "Label"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 8),
      axis.text  = element_text(size = 7)
    )
  
  plots[[length(plots)+1]] <- p
}


#4x4 layout
group_size <- 16
num_groups <- ceiling(length(plots)/group_size)

for(i in seq_len(num_groups)){
  
  start_idx <- (i-1)*group_size + 1
  end_idx   <- min(i*group_size, length(plots))
  
  subset_plots <- plots[start_idx:end_idx]
  
  if(length(subset_plots) < group_size){
    n_missing <- group_size - length(subset_plots)
    subset_plots <- c(
      subset_plots,
      replicate(n_missing, patchwork::plot_spacer(), simplify = FALSE)
    )
  }
  
  combined_plot <- wrap_plots(subset_plots, ncol=4, nrow=4)
  print(combined_plot)
}

#------Bivariate Bar on categorical features------
plots <- list()
top_n <- 5

for (col in categoricalCols) {
  
  dfPlot <- datasetCleanOutlier %>%
    dplyr::select(isFraud, all_of(col)) %>%
    dplyr::mutate(value = as.factor(.data[[col]]))
  
  #Keep top 5 + others
  n_total  <- dplyr::n_distinct(dfPlot$value)
  n_remain <- max(n_total - top_n, 0)
  
  dfPlot <- dfPlot %>%
    dplyr::mutate(
      value = forcats::fct_lump_n(
        value,
        n = top_n,
        other_level = paste0(n_remain, "-others")
      )
    )
  
  #Calculate proportion for each label
  dfProp <- dfPlot %>%
    dplyr::group_by(isFraud, value) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(isFraud) %>%
    dplyr::mutate(prop = n / sum(n))
  
  #Plot
  p <- ggplot(dfProp,
              aes(x = value,
                  y = prop,
                  fill = factor(isFraud))) +
    
    geom_col(position = position_dodge(width = 0.8)) +
    
    scale_fill_manual(
      values = c("0" = "#4CAF50",   # Non-fraud
                 "1" = "#E53935"),  # Fraud
      name = "isFraud"
    ) +
    
    coord_flip() +
    
    labs(
      title = paste0("Distribution of ", col, " by isFraud"),
      x = col,
      y = "Proportion within Label"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 8),
      axis.text  = element_text(size = 7)
    )
  
  plots[[length(plots) + 1]] <- p
}

group_size <- 16
num_groups <- ceiling(length(plots) / group_size)

for (i in seq_len(num_groups)) {
  
  start_idx <- (i - 1) * group_size + 1
  end_idx   <- min(i * group_size, length(plots))
  
  subset_plots <- plots[start_idx:end_idx]
  
  if (length(subset_plots) < group_size) {
    n_missing <- group_size - length(subset_plots)
    blank_plots <- replicate(
      n_missing,
      patchwork::plot_spacer(),
      simplify = FALSE
    )
    subset_plots <- c(subset_plots, blank_plots)
  }
  
  combined_plot <- wrap_plots(subset_plots, ncol = 4, nrow = 4)
  print(combined_plot)
}

#==================Multi-variate Analysis==========================
datasetCleanOutlier$isFraud <- as.numeric(as.character(datasetCleanOutlier$isFraud)) #convert fraud back to interger
corrMat<-cor(datasetCleanOutlier[c(labelCol,numericalCols)])
ggcorrplot(
  corrMat,
  tl.cex = 8,
  title = "Correlation Matrix of Numerical Features and Fraud Label",
  ggtheme = theme_minimal()
) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )


#==================Feature Engineering==================
#-------------Feature Selection (Remove low corr item)------------
fraudCorr <- corrMat["isFraud", ]
fraudCorr <- fraudCorr[names(fraudCorr) != "isFraud"]
corrThreshold <- 0.10

keepFeatures <- names(fraudCorr[abs(fraudCorr) >= corrThreshold])
dropFeatures <- names(fraudCorr[abs(fraudCorr) < corrThreshold])


print("Dropped features:")
print(dropFeatures)
datasetCorrFiltered <- datasetCleanOutlier %>% dplyr::select(-all_of(dropFeatures))
numericalCols <- numericalCols[!numericalCols %in% dropFeatures] #remove discarded-col name for numerical

#-----------Binning the categorical feature-------------------
top_n <- 20

datasetBinned <- datasetCorrFiltered %>%
  mutate(
    across(
      all_of(categoricalCols),
      ~ {
        x <- as.factor(.x)
        
        #number of categories
        n_total  <- n_distinct(x, na.rm = TRUE)
        n_remain <- max(n_total - top_n, 0)
        
        #keep Top N + others
        forcats::fct_lump_n(
          x,
          n = top_n,
          other_level = paste0(n_remain, "-others")
        )
      }
    )
  )

#--------Generating new features------------
#Id Col we have
idCols <- c(
  "id_01","id_12","id_15","id_16","id_17",
  "id_23","id_27","id_28","id_29","id_30",
  "id_31","id_33","id_34","id_35","id_36",
  "id_37","id_38"
)


#Update dataset
finalDateset <- datasetBinned %>%
  mutate(
    HourOfDate = TranDTHour %% 24, #Data of Hour
    EmailMatch = ifelse( #P-R match
      P_emaildomain == R_emaildomain,
      1, 0
    )%>% factor(levels = c(0, 1), labels = c("Not Match", "Match")), #convert it to fractor
    IDMissingCount = rowSums(select(., all_of(idCols)) == "miss"),
    IDMissingRatio = IDMissingCount / length(idCols)
  )




















#-------------------------------------------------------------------
#=======================Assignment 2================================
#-------------------------------------------------------------------

# install.packages("caret")
# install.packages("randomForest")
# install.packages("precrec")
# install.packages("smotefamily")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("viridis")

library(caret)
library(randomForest)
library(precrec)
library(smotefamily)
library(rpart)
library(rpart.plot)
library(viridis)




#==================Preparation==================
#---------------Setting the seed + encoding the label as factor---------------
set.seed(7410)
modelDataset <- finalDateset
modelDataset$isFraud <- factor(
  ifelse(modelDataset$isFraud == 1, "fraud", "nonFraud"),
  levels = c("nonFraud", "fraud")
)

#--------------------Data Partitioning--------------------
trainIndex <- createDataPartition(modelDataset$isFraud, p = 0.8, list = FALSE) #partition with 80:20
trainData <- modelDataset[trainIndex, , drop = FALSE]
testData <- modelDataset[-trainIndex, , drop = FALSE]

#====================Rebalancing the Train Data====================
smoteInput <- trainData #Create new copy again
featureColumns <- setdiff(names(smoteInput), "isFraud") #Get the feature columns name list
factorColumns <- featureColumns[sapply(smoteInput[, featureColumns, drop = FALSE], is.factor)] #get factor column name list 
factorLevelsList <- list() #change to dict

for (colName in factorColumns) {
  factorLevelsList[[colName]] <- levels(smoteInput[[colName]]) # {feature name: {lv1: A, lv2: B}}
  smoteInput[[colName]] <- as.numeric(smoteInput[[colName]]) # convert to interger
}

#Do the SMOTE
smoteResult <- SMOTE(
  X = smoteInput[, featureColumns, drop = FALSE],
  target = smoteInput$isFraud,
  K = 5
)

#Postprocess after SMOTE
trainSmote <- smoteResult$data
colnames(trainSmote)[ncol(trainSmote)] <- "isFraud" #change the label name back to isFraud bc SMOTE will change the label to "target"/"class"

trainSmote$isFraud <- factor(#encode the 0/1 back to the class name
  trainSmote$isFraud,
  levels = c("nonFraud", "fraud")
)


for (colName in factorColumns) {
  tempValue <- round(trainSmote[[colName]]) #round the float back to integer
  
  tempValue[tempValue < 1] <- 1 #cap as min
  tempValue[tempValue > length(factorLevelsList[[colName]])] <- length(factorLevelsList[[colName]]) #cap as max
  
  trainSmote[[colName]] <- factor(#assign the synthetic factor back to the categorical feature
    factorLevelsList[[colName]][tempValue],
    levels = factorLevelsList[[colName]]
  )
}


#====================Model 1 Decision Tree ====================
#Train the model
decisionTreeModel <- rpart(
  isFraud ~ .,
  data = trainSmote,
  method = "class",
)


#Prune the tree
bestCp <- decisionTreeModel$cptable[which.min(decisionTreeModel$cptable[,"xerror"]), "CP"]
decisionTreePruned <- prune(decisionTreeModel, cp = bestCp)

#-------------------Plot the Tree-----------------------
#Encode the DevInfo as D1,D2,D3,...
originalLevels <- levels(trainSmote$DevInfo)
short_codes <- paste0("D", seq_along(originalLevels))
splitFunDevAlias <- function(x, labs, digits, varlen, faclen) {
  currentLabels <- as.character(labs)
  for(i in 1:length(originalLevels)) {
    pattern <- originalLevels[i]
    if(!is.na(pattern) && nchar(pattern) > 0) 
      currentLabels <- gsub(pattern, short_codes[i], currentLabels, fixed = TRUE)
  }
  return(currentLabels)
}

#plot the graph
rpart.plot(
  decisionTreePruned,
  split.fun = splitFunDevAlias,
  type = 4,
  extra = 101,
  fallen.leaves = TRUE,
  varlen = 0, 
  faclen = 0,
  cex = 0.5,
  main = "Decision Tree Result"
)

#---------------------Do Prediction + evaluation [Train]-----------------------
#Predict
decisionTreeTrainProb <- predict(decisionTreePruned, newdata = trainSmote, type = "prob")[, "fraud"]
decisionTreeTrainPred <- factor(
  ifelse(decisionTreeTrainProb >= 0.5, "fraud", "nonFraud"),
  levels = levels(trainSmote$isFraud)
)

#confusion matrix
decisionTreeTrainCm <- confusionMatrix(
  data = decisionTreeTrainPred,
  reference = trainSmote$isFraud,
  positive = "fraud"
)

#Calculate area under ROC and Area under PRC
decisionTreeTrainActualNumeric <- ifelse(trainSmote$isFraud == "fraud", 1, 0)
decisionTreeTrainEvalObj <- evalmod(
  scores = decisionTreeTrainProb,
  labels = decisionTreeTrainActualNumeric,
  mode = "rocprc"
)

decisionTreeTrainAucTable <- as.data.frame(auc(decisionTreeTrainEvalObj))
decisionTreeTrainAuroc <- decisionTreeTrainAucTable$aucs[decisionTreeTrainAucTable$curvetypes == "ROC"]
decisionTreeTrainAuprc <- decisionTreeTrainAucTable$aucs[decisionTreeTrainAucTable$curvetypes == "PRC"]


#Save the metric result in a dataframe
decisionTreeTrainMetrics <- data.frame(
  dataset = "Train",
  model = "Decision Tree",
  accuracy = as.numeric(decisionTreeTrainCm$overall["Accuracy"]),
  recall = as.numeric(decisionTreeTrainCm$byClass["Recall"]),
  precision = as.numeric(decisionTreeTrainCm$byClass["Precision"]),
  f1Score = as.numeric(decisionTreeTrainCm$byClass["F1"]),
  auroc = as.numeric(decisionTreeTrainAuroc),
  auprc = as.numeric(decisionTreeTrainAuprc)
)


print("Confusion matrix[DT][Train]")
print(decisionTreeTrainCm$table)

print("Metrics[DT][Train]")
print(decisionTreeTrainMetrics)

#---------------------Do Prediction + evaluation [Test]-----------------------
#Predict
decisionTreeProb <- predict(decisionTreePruned, newdata = testData, type = "prob")[, "fraud"]
decisionTreePred <- factor(
  ifelse(decisionTreeProb >= 0.5, "fraud", "nonFraud"),
  levels = levels(testData$isFraud)
)

#confusion matrix
decisionTreeCm <- confusionMatrix(
  data = decisionTreePred,
  reference = testData$isFraud,
  positive = "fraud"
)

#Calculate area under ROC and Area under PRC
decisionTreeActualNumeric <- ifelse(testData$isFraud == "fraud", 1, 0)
decisionTreeEvalObj <- evalmod(
  scores = decisionTreeProb,
  labels = decisionTreeActualNumeric,
  mode = "rocprc"
)

decisionTreeAucTable <- as.data.frame(auc(decisionTreeEvalObj))
decisionTreeAuroc <- decisionTreeAucTable$aucs[decisionTreeAucTable$curvetypes == "ROC"]
decisionTreeAuprc <- decisionTreeAucTable$aucs[decisionTreeAucTable$curvetypes == "PRC"]


#Save the metric result in a dataframe
decisionTreeMetrics <- data.frame(
  dataset = "Test",
  model = "Decision Tree",
  accuracy = as.numeric(decisionTreeCm$overall["Accuracy"]),
  recall = as.numeric(decisionTreeCm$byClass["Recall"]),
  precision = as.numeric(decisionTreeCm$byClass["Precision"]),
  f1Score = as.numeric(decisionTreeCm$byClass["F1"]),
  auroc = as.numeric(decisionTreeAuroc),
  auprc = as.numeric(decisionTreeAuprc)
)


print("Confusion matrix[DT][Test]")
print(decisionTreeCm$table)

print("Metrics[DT][Test]")
print(decisionTreeMetrics)


#------------------------Find the importance------------------------
decisionTreeImportance <- data.frame(
  featureName = names(decisionTreePruned$variable.importance),
  importance = as.numeric(decisionTreePruned$variable.importance),
  row.names = NULL
) %>% arrange(desc(importance)) #sort from large to small

print("Decision Tree feature importance:")
print(decisionTreeImportance)

#--------------Plot the graphs-------------------------
#Plot the Importance
ggplot(decisionTreeImportance, aes(x = fct_reorder(featureName, importance), y = importance)) +
  geom_segment(aes(xend = fct_reorder(featureName, importance), yend = 0), 
               color = "lightgray", linetype = "dotted") +
  geom_point(size = 2) + #Use dot
  coord_flip() +  #Horizontally print
  theme_minimal() + #white background
  labs(
    title = "Feature Importance of Decision Tree (Gini-based)",
    x = "Features",
    y = "Importance Score"  
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 9)
  )

#Plot the confusion matrix[Train]
decisionTreeTrainCmDf <- as.data.frame(decisionTreeTrainCm$table)
ggplot(decisionTreeTrainCmDf, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#FFEDCE", high = "#FF8383") +
  theme_minimal() +
  labs(
    title = "Confusion Matrix of Decision Tree [Train]",
    x = "Actual",
    y = "Predicted"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the confusion matrix[Test]
decisionTreeCmDf <- as.data.frame(decisionTreeCm$table)
ggplot(decisionTreeCmDf, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#FFEDCE", high = "#FF8383") +
  theme_minimal() +
  labs(
    title = "Confusion Matrix of Decision Tree [Test]",
    x = "Actual",
    y = "Predicted"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the ROC[Train]
autoplot(decisionTreeTrainEvalObj, curvetype = "ROC") +
  theme_minimal() +
  labs(title = "ROC Curve of Decision Tree [Train]") +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the PRC[Train]
autoplot(decisionTreeTrainEvalObj, curvetype = "PRC") +
  theme_minimal() +
  labs(title = "PRC Curve of Decision Tree [Train]") +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the ROC[Test]
autoplot(decisionTreeEvalObj, curvetype = "ROC") +
  theme_minimal() +
  labs(title = "ROC Curve of Decision Tree [Test]") +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the PRC[Test]
autoplot(decisionTreeEvalObj, curvetype = "PRC") +
  theme_minimal() +
  labs(title = "PRC Curve of Decision Tree [Test]") +
  theme(plot.title = element_text(face = "bold", size = 14))


#==================== Model 2: Random Forest ====================
#Train the model
randomForestModel <- randomForest(
  isFraud ~ .,
  data = trainSmote,
  importance = TRUE,
  ntree = 100,
)

#---------------------Do Prediction + evaluation [Train]-----------------------
#Predict
randomForestTrainProb <- predict(randomForestModel, newdata = trainSmote, type = "prob")[, "fraud"]
randomForestTrainPred <- factor(
  ifelse(randomForestTrainProb >= 0.5, "fraud", "nonFraud"),
  levels = levels(trainSmote$isFraud)
)

#Confusion Matrix
randomForestTrainCm <- confusionMatrix(
  data = randomForestTrainPred,
  reference = trainSmote$isFraud,
  positive = "fraud"
)

#Calculate area under ROC and Area under PRC
randomForestTrainActualNumeric <- ifelse(trainSmote$isFraud == "fraud", 1, 0)
randomForestTrainEvalObj <- evalmod(
  scores = randomForestTrainProb,
  labels = randomForestTrainActualNumeric,
  mode = "rocprc"
)

randomForestTrainAucTable <- as.data.frame(auc(randomForestTrainEvalObj))
randomForestTrainAuroc <- randomForestTrainAucTable$aucs[randomForestTrainAucTable$curvetypes == "ROC"]
randomForestTrainAuprc <- randomForestTrainAucTable$aucs[randomForestTrainAucTable$curvetypes == "PRC"]

#Save the result in the evaluation metric
randomForestTrainMetrics <- data.frame(
  dataset = "Train",
  model = "Random Forest",
  accuracy = as.numeric(randomForestTrainCm$overall["Accuracy"]),
  recall = as.numeric(randomForestTrainCm$byClass["Recall"]),
  precision = as.numeric(randomForestTrainCm$byClass["Precision"]),
  f1Score = as.numeric(randomForestTrainCm$byClass["F1"]),
  auroc = as.numeric(randomForestTrainAuroc),
  auprc = as.numeric(randomForestTrainAuprc)
)

print("Random Forest confusion matrix[Train]:")
print(randomForestTrainCm$table)

print("Random Forest metrics[Train]:")
print(randomForestTrainMetrics)

#Predict
randomForestProb <- predict(randomForestModel, newdata = testData, type = "prob")[, "fraud"]
randomForestPred <- factor(
  ifelse(randomForestProb >= 0.5, "fraud", "nonFraud"),
  levels = levels(testData$isFraud)
)

#Confusion Matrix
randomForestCm <- confusionMatrix(
  data = randomForestPred,
  reference = testData$isFraud,
  positive = "fraud"
)

#Calculate area under ROC and Area under PRC
randomForestActualNumeric <- ifelse(testData$isFraud == "fraud", 1, 0)
randomForestEvalObj <- evalmod(
  scores = randomForestProb,
  labels = randomForestActualNumeric,
  mode = "rocprc"
)

randomForestAucTable <- as.data.frame(auc(randomForestEvalObj))
randomForestAuroc <- randomForestAucTable$aucs[randomForestAucTable$curvetypes == "ROC"]
randomForestAuprc <- randomForestAucTable$aucs[randomForestAucTable$curvetypes == "PRC"]


#Save the result in the evaluation metric
randomForestMetrics <- data.frame(
  dataset = "Test",
  model = "Random Forest",
  accuracy = as.numeric(randomForestCm$overall["Accuracy"]),
  recall = as.numeric(randomForestCm$byClass["Recall"]),
  precision = as.numeric(randomForestCm$byClass["Precision"]),
  f1Score = as.numeric(randomForestCm$byClass["F1"]),
  auroc = as.numeric(randomForestAuroc),
  auprc = as.numeric(randomForestAuprc)
)

print("Random Forest confusion matrix[Test]:")
print(randomForestCm$table)

print("Random Forest metrics[Test]:")
print(randomForestMetrics)

#----------------Find the importance----------------
randomForestImportance <- data.frame(
  featureName = rownames(importance(randomForestModel)),
  importance(randomForestModel),
  row.names = NULL
) %>% arrange(desc(MeanDecreaseGini))

print("Random Forest feature importance:")
print(randomForestImportance)

varImpPlot(randomForestModel, main = "Feature Importance of Random Forest (Mean Decrease Gini)")

#Plot the confusion matrix[Train]
randomForestTrainCmDf <- as.data.frame(randomForestTrainCm$table)
ggplot(randomForestTrainCmDf, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#FFEDCE", high = "#FF8383") +
  theme_minimal() +
  labs(
    title = "Confusion Matrix of Random Forest [Train]",
    x = "Actual",
    y = "Predicted"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the confusion matrix[Test]
randomForestCmDf <- as.data.frame(randomForestCm$table)
ggplot(randomForestCmDf, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#FFEDCE", high = "#FF8383") +
  theme_minimal() +
  labs(
    title = "Confusion Matrix of Random Forest [Test]",
    x = "Actual",
    y = "Predicted"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the ROC[Train]
autoplot(randomForestTrainEvalObj, curvetype = "ROC") +
  theme_minimal() +
  labs(title = "ROC Curve of Random Forest [Train]") +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the PRC[Train]
autoplot(randomForestTrainEvalObj, curvetype = "PRC") +
  theme_minimal() +
  labs(title = "PRC Curve of Random Forest [Train]") +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the ROC[Test]
autoplot(randomForestEvalObj, curvetype = "ROC") +
  theme_minimal() +
  labs(title = "ROC Curve of Random Forest [Test]") +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the PRC[Test]
autoplot(randomForestEvalObj, curvetype = "PRC") +
  theme_minimal() +
  labs(title = "PRC Curve of Random Forest [Test]") +
  theme(plot.title = element_text(face = "bold", size = 14))

#Plot the Importance by ggplot
ggplot(randomForestImportance[1:min(20, nrow(randomForestImportance)), ], aes(x = fct_reorder(featureName, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_segment(aes(xend = fct_reorder(featureName, MeanDecreaseGini), yend = 0), 
               color = "lightgray", linetype = "dotted") +
  geom_point(size = 2) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Feature Importance of Random Forest (Mean Decrease Gini)",
    x = "Features",
    y = "Mean Decrease Gini"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 9)
  )

#==================== Model Comparison ====================

modelComparison <- bind_rows(
  decisionTreeTrainMetrics,
  decisionTreeMetrics,
  randomForestTrainMetrics,
  randomForestMetrics
)

print("Model comparison:")
print(modelComparison)