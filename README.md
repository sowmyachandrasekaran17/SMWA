# SMWA: Seasonal Moving Window Algorithm
Missing Value Imputation by Seasonal Moving Window Algorithm (SMWA)

## Abstract
Data pre-processing is a key research topic in data mining because it plays a
crucial role in improving the accuracy of any data mining algorithm. In most
real world cases, a significant amount of the recorded data is found missing
due to most diverse errors. This loss of data is nearly always unavoidable.
Recovery of missing data plays a vital role in avoiding inaccurate data
mining decisions. Most multivariate imputation methods are not compatible
to univariate datasets and the traditional univariate imputation techniques
become highly biased as the missing data gap increases. With the current
technological advancements abundant data is being captured every second.
Hence, we intend to develop a new algorithm that enables maximum
utilization of the available big datasets for imputation. In this paper, we
present a Seasonal and Trend decomposition using Loess (STL) based
Seasonal Moving Window Algorithm, which is capable of handling patterns
with trend as well as cyclic characteristics. We show that the algorithm is
highly suitable for pre-processing of large datasets.


## Pre-requisites
Kindly install the dependent R packages: stats, forecast,imputeTS. 

## General Info
SMWA is an algorithm that enables maximum utilization of the available big datasets for imputation. This is a Seasonal and Trend decomposition using Loess (STL) based Seasonal Moving Window Algorithm, which is capable of handling patterns with trend as well as cyclic characteristics. 

"head" - a finite set of data before the missing interval

"tail" - a finite set of data after the missing interval

The combination of head, missing data, and tail forms the window. This window then slides through the past data and the best matching window with the minimum root mean square error (RMSE) is imputed in the missing data.

## Usage
na.smwa(x, motifSize = 1, minGap = 1, observationWindow = 10, option = "both")

Arguments
x-	Numeric Vector (vector) or Time Series (ts) object in which missing values shall be replaced

motifSize	-integer value that specifies the size of dataset for "head" and "tail"

minGap	-integer value that specifies the minimum length of missing gap to be imputed

observationWindow	-integer value that specifies the maximum length of past window

option	-accepts string head, tail, both. The user can choose if they want to frame the window only with "head" or "tail" or using "both." Recommended is to use "both" for better performance
## Example
´´´R
library(imputeTS)
library(forecast)
#Example 1:# Prerequisite: Create Time series with missing values
x <- ts(c(1,2,3,4,1,2,3,4,1,2,3,4,NA,NA,3,4,1,2,3,4),frequency = 4)
result <- na.smwa(x)
accuracy(result,x)
#Example 2: # Prerequisite: Make NAs in AirPassengers dataset
data <- AirPassengers
data[60:68]<-NA
imputed_airpass1<-na.smwa(data)
accuracy(imputed_airpass1,AirPassengers)
imputed_airpass2<-na.smwa(data,motifSize=2,observationWindow = 60)
accuracy(imputed_airpass2,AirPassengers)
´´´
## References
Sowmya Chandrasekaran, Martin Zaefferer, Steffen Moritz, Jörg Stork, Martina Friese, Andreas Fischbach, Thomas Bartz-Beielstein (2016). "Data Preprocessing: A New Algorithm for Univariate Imputation Designed Specifically for Industrial Needs".

