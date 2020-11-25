#' @title Missing Value Imputation by Seasonal Moving Window Algorithm (SMWA)

#' 

#' @description an algorithm that enables maximum utilization of the available big datasets for imputation. 
#' This is a Seasonal and Trend decomposition using Loess (STL) based Seasonal Moving Window Algorithm, which is capable of handling patterns with trend as well as cyclic characteristics. 
#' \itemize{
#'    \item{"head" - a finite set of data before the missing interval}
#'
#'    \item{"tail" - a finite set of data after the missing interval}
#'    }
#' The combination of head, missing data, and tail forms the window. 
#' This window then slides through the past data and 
#' the best matching window with the minimum root mean square error (RMSE)
#' is imputed in the missing data.
#' @author Sowmya Chandrasekaran
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object in which missing values shall be replaced 
#' @param motifSize integer value that specifies the size of dataset for "head" and "tail"
#' @param minGap integer value that specifies the minimum length of missing gap to be imputed
#' @param observationWindow integer value that specifies the maximum length of past window
#' @param option accepts string head, tail, both.
#'The user can choose if they want to frame the window only with "head" or "tail" or using "both." 
#'Recommended is to use "both" for better performance

#' @examples
#' #Example 1:# Prerequisite: Create Time series with missing values
#' x <- ts(c(1,2,3,4,1,2,3,4,1,2,3,4,NA,NA,3,4,1,2,3,4),frequency = 4)
#' result <- na.smwa(x)
#' accuracy(result,x)

#' #Example 2: # Prerequisite: Make NAs in AirPassengers dataset
#' data <- AirPassengers
#' data[60:68]<-NA
#' imputed_airpass1<-na.smwa(data)
#' accuracy(imputed_airpass1,AirPassengers)
#' imputed_airpass2<-na.smwa(data,motifSize=2,observationWindow = 60)
#' accuracy(imputed_airpass2,AirPassengers)

#' @references Sowmya Chandrasekaran, Martin Zaefferer, Steffen Moritz, Jörg Stork, Martina Friese, Andreas Fischbach, Thomas Bartz-Beielstein (2016). "Data Preprocessing: A New Algorithm for Univariate Imputation Designed Specifically for Industrial Needs". 
#' @export

na.smwa<- function(x, motifSize = 1, minGap = 1, observationWindow = 10,option = "both") {
  
  
  
  window_size <- motifSize
  
  w <- observationWindow
  
  data <- x
  
  ##
  ## 1. Input Check and Transformation
  ##
  
  
  # 1.1 Check if NAs are present
  if (!anyNA(data)) {
    return(data)
  }
  
  # 1.2 special handling data types
  if (any(class(data) == "tbl")) {
    data <- as.vector(as.data.frame(data)[, 1])
  }
  
  # 1.3 Checks and corrections for wrong data dimension
  
  # Check if input dimensionality is not as expected
  if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
    stop("Wrong input type for parameter x")
  }
  
  # Altering multivariate objects with 1 column (which are essentially
  # univariate) to be dim = NULL
  if (!is.null(dim(data)[2])) {
    data <- data[, 1]
  }
  
  # 1.4 Check if input is numeric
  if (!is.numeric(data)) {
    stop("Input x is not numeric")
  }
  
  # 1.5 Check if motifSize is an integer
  if(window_size%%1!=0){
    stop("motifSize is not an integer")
  }
  
  # 1.6 Check if minGap is an integer
  if(minGap%%1!=0){
    stop("minGap is not an integer")
  }
  
  # 1.7 Check if observationWindow is an integer
  if(observationWindow%%1!=0){
    stop("observationWindow is not an integer")
  }
  
  # 1.8 Check if option value is as expected
  list_option <- list("head","tail","both")
  check_value <- match(option,list_option,nomatch = 0)
  if(check_value==0){
        stop("Wrong parameter 'option' given. Value must be either 'head' or 'tail' or 'both'.")
      }
  ##
  ## End Input Check
  ##
  ## Imputation Code
  # Identify the missing data index
  
  missindx <- is.na(data)  
  
  actual_missing<-which(is.na(data))
  
  
  # Perform linear interpolation as pre step for stl(), since it can't be done with NAs
  
  data.interp = na_interpolation(data)

  
  ## Time series decomposition
  
  data.stl <- stl(data.interp,robust=TRUE, s.window = 11)

  # Get the seasonal, trend and irregular component
  
  data.seasonality <- data.stl$time.series[ ,1]
  
  data.trend <- data.stl$time.series[ ,2]
  
  data.irregular <- data.stl$time.series[ ,3]
  
  # Remove trend component and pass remaining two as input to SWMA
  
  data.saisonality.irregular <- data.seasonality + data.irregular

  
  #Imputaion for trend component, perform linear interpolation for stl_trend and bring missing values back for trend
  
  data.trend[missindx] <- NA
  
  data.trend <- na_interpolation(data.trend)

  imputeddata <- data.saisonality.irregular
  
  acc1 <- acc2 <- s_e <- seq(1:w)
  
  temp1 <- temp2 <- 0
  
  intern <- 1
  
  big_size <- minGap

  big_win <- NULL

  #Identify the indices with missing intervals >= minGap

  for( i in 1:(length(actual_missing) - 1)){
    
    if(actual_missing[i] + 1 == actual_missing[i+1])
      
    {
      
      temp1 <- temp1 + 1
      
    }
    
    else
      
    {
      
      temp1 <- temp2 <- 0
      
    }
    
    if ((temp1 > big_size) && (temp2 == 0))
      
    {
      
      big_win[intern] <- (actual_missing[i] - big_size)
      
      temp2 <- 1
      
      intern <- intern+1
      
    }
    
  }

  
  if(big_size==1){
    
    big_win <- actual_missing
    
  }

  for (inter in 1:length(big_win)){ 

    #init varibales with big_win at position inter
    
    indexstart <- indexend <- iq <- big_win[inter]
    
    
    while(is.na(data[indexend])) # Identify actual missing gap
      
    {
      indexend <- indexend + 1
    }
    
    indexgap <- indexend - indexstart
    

    #Formulate head + missinggap + tail as window
    
    #Find the best fitting past window with least RMSE for head  and tail
    
    for (temp_iq in 1:w){
      
      tryCatch(
        
        if((indexstart - temp_iq - (2 * window_size) - indexgap) >= 1)
          
        {
          
          # try catch to ensure if past window of specified length 'w' exist before NA
          
          acc1[temp_iq] <- sqrt(sum((imputeddata[(indexstart - window_size) : (indexstart-1)] - imputeddata[(indexstart - temp_iq - (2*window_size) - indexgap+1) : (indexstart - window_size - temp_iq - indexgap)])^2) / window_size)
          
          acc2[temp_iq] <- sqrt(sum((imputeddata[(indexend) : (indexend+window_size-1)] - imputeddata[(indexstart-temp_iq - window_size + 1) : (indexstart-temp_iq)])^2) / window_size)
          
        },
        
        error = function(e) { warning(" Check  Initial NA point has past window  >= 'w'")}
        
      ) 
      
    }
    
    if (option == "head")
      
    { acc <- acc1 }
    
    else if (option == "tail")
      
    { acc <- acc2 }
    
    else if (option == "both")
      
    { acc <- acc1 + acc2 }

    
    # Best fitting window
    
    bestindx <- s_e[which(acc %in% sort(acc)[1])]
    
    imputeddata[iq : (iq + indexgap-1)] <- imputeddata[(iq - bestindx[1] - indexgap - window_size + 1) : (iq - bestindx[1] - window_size)]

  }

  #Combine components back
  
  data <- imputeddata + data.trend

  return(data)
  
}
