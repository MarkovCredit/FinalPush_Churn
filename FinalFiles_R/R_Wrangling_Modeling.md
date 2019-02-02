
# Churn / Customer Retention Modeling in R
From a strategic perspective, we would like to identify customers who are more likely to churn earlier in the process so
we can have a meaningful interaction with the subscriber to help them achieve their goals.

From a modeling perspective, we would like to gain insight from our data into behavior, variables and patterns that indicate higher probabilty of churn.Then, we'd like to predict churn and in turn, feed it back into the business to drive results. 


```R
#Load in libraries
library(dplyr)
library(binr)
library(OneR)
library(ggplot2)
library(GGally)
library(car)
library(reshape)
library(devtools)
library(openxlsx)
library(zip)
library(RColorBrewer)
library(caret)
library(e1071)
library(pROC)
library(plotly)
library(crosstalk)
library(randomForest)
library(rpart)
library(adabag)
# R.Version()
```

    Loading required package: foreach
    Loading required package: doParallel
    Loading required package: iterators
    Loading required package: parallel
    


```R
# load in the data. The data was pre-cleaned with a mixture of python and R after querying the source database.
model_data <- read.csv('df_combined_012619.csv')
```


```R
#It looks like there are duplicates in here since there are 9,562 levels of what should be a unique columns
#with 9,925 observations. Going to remove them later with duplicated function. Here we can see that the the long data 
#has been converted to a tidy format (57,000 rows containing multiple months per customer > one row per client)
str(model_data)
```

    'data.frame':	9925 obs. of  61 variables:
     $ X                              : int  1 2 3 4 5 6 7 8 9 10 ...
     $ pid                            : Factor w/ 9562 levels "10Agu9063","10Ala9061",..: 2539 9236 1027 1309 4303 5176 2753 5283 5308 3337 ...
     $ birth.year                     : Factor w/ 7430 levels "1918-01-15","1918-01-24",..: 2828 1379 1151 3493 4423 7082 2433 2889 103 1699 ...
     $ first.payment                  : Factor w/ 288 levels "2018-01-17","2018-01-24",..: 231 112 NA 253 133 227 122 153 152 68 ...
     $ last.payment                   : Factor w/ 3015 levels "2018-01-17 00:00:00",..: 1803 759 243 2026 2136 1457 2587 1796 999 2420 ...
     $ transaction.count              : int  4 8 NA 3 8 4 8 6 7 11 ...
     $ total.spend                    : num  596 392 NA 447 792 ...
     $ processor                      : Factor w/ 2 levels "Square","Stripe": 2 2 2 2 2 2 2 2 2 2 ...
     $ customer.status                : Factor w/ 2 levels "Existing","New": 2 1 2 1 1 2 2 1 2 1 ...
     $ clinic.id                      : int  24 9 17 19 33 40 25 40 40 27 ...
     $ member.code                    : Factor w/ 4 levels "BWM","BWM29",..: 4 1 1 4 3 1 1 1 3 3 ...
     $ member.status                  : Factor w/ 2 levels "Active","Cancel": 1 1 2 1 1 1 1 1 1 1 ...
     $ membership                     : Factor w/ 3 levels "BWM","WPM","WUM": 3 1 1 3 2 1 1 1 2 2 ...
     $ num_visits_2                   : int  0 0 0 0 0 0 0 0 0 0 ...
     $ num_visits_3                   : int  0 2 0 0 0 0 0 0 0 0 ...
     $ num_visits_4                   : int  0 1 0 0 0 0 0 0 0 9 ...
     $ num_visits_5                   : int  0 0 4 0 0 0 0 0 0 11 ...
     $ num_visits_6                   : int  0 1 3 0 0 0 3 0 0 10 ...
     $ num_visits_7                   : int  0 0 5 0 8 0 4 2 4 4 ...
     $ num_visits_8                   : int  0 0 0 0 0 0 5 2 15 1 ...
     $ num_visits_9                   : int  0 0 0 0 0 0 0 10 10 2 ...
     $ num_visits_10                  : int  3 0 0 0 0 1 2 1 9 1 ...
     $ num_visits_11                  : int  16 0 0 1 0 10 0 0 5 3 ...
     $ num_visits_12                  : int  18 0 0 18 0 17 0 5 1 2 ...
     $ weightchange_2                 : num  0 0 0 0 0 0 0 0 0 0 ...
     $ weightchange_3                 : num  0 0 0 0 0 0 0 0 0 0 ...
     $ weightchange_4                 : num  0 0 0 0 0 0 0 0 0 -7.4 ...
     $ weightchange_5                 : num  0 0 -0.4 0 0 0 0 0 0 -4.7 ...
     $ weightchange_6                 : num  0 0 -0.4 0 0 0 -1.2 0 0 -5.4 ...
     $ weightchange_7                 : num  0 0 -0.3 0 -2 0 -0.9 -2.3 -1.2 -1 ...
     $ weightchange_8                 : num  0 0 0 0 0 0 1.7 -1.1 -5 0 ...
     $ weightchange_9                 : num  0 0 0 0 0 0 0 -2.7 -1.7 -0.7 ...
     $ weightchange_10                : num  -3.8 0 0 0 0 0 0.7 0 -1.4 0 ...
     $ weightchange_11                : num  -10.2 0 0 0 0 -15.1 0 0 -0.1 0.7 ...
     $ weightchange_12                : num  -6.7 0 0 -10.2 0 -5.4 0 -4.5 0 0 ...
     $ monthly_initial_visit_weight_2 : num  0 0 0 0 0 0 0 0 0 0 ...
     $ monthly_initial_visit_weight_3 : num  0 197 0 0 0 ...
     $ monthly_initial_visit_weight_4 : num  0 197 0 0 0 ...
     $ monthly_initial_visit_weight_5 : num  0 0 161 0 0 ...
     $ monthly_initial_visit_weight_6 : num  0 191 163 0 0 ...
     $ monthly_initial_visit_weight_7 : num  0 0 163 0 169 ...
     $ monthly_initial_visit_weight_8 : num  0 0 0 0 0 ...
     $ monthly_initial_visit_weight_9 : num  0 0 0 0 0 ...
     $ monthly_initial_visit_weight_10: num  250 0 0 0 0 ...
     $ monthly_initial_visit_weight_11: num  244 0 0 201 0 ...
     $ monthly_initial_visit_weight_12: num  234 0 0 201 0 ...
     $ months_0orno_visits            : int  8 8 8 9 10 8 7 6 5 2 ...
     $ first_month_0_visits           : int  1 1 1 1 1 1 1 1 1 1 ...
     $ consec_0_visits                : num  8 6 5 9 5 8 4 5 5 2 ...
     $ consec_0_visits_v2             : int  0 1 1 0 1 0 0 0 0 0 ...
     $ first_month_visited            : int  9 2 4 10 6 9 5 6 6 3 ...
     $ firstpay_index                 : int  9 5 4 10 5 9 5 6 6 3 ...
     $ churn_v2                       : int  0 1 1 0 1 0 0 0 0 0 ...
     $ avg_visits                     : num  3.364 0.364 1.091 1.727 0.727 ...
     $ first_month_weightlost         : num  1 NA 1 2 1 2 1 1 1 1 ...
     $ first_month_weightlost_10      : num  2 NA NA 2 NA 2 NA NA NA NA ...
     $ first_month_weightgain         : num  NA NA NA NA NA NA 3 NA NA NA ...
     $ first_month_weightgain_10      : num  NA NA NA NA NA NA NA NA NA NA ...
     $ avg_weight_change              : num  -1.882 0 -0.1 -0.927 -0.182 ...
     $ stdev_visits                   : num  6.816 0.674 1.921 5.405 2.412 ...
     $ stdev_weight_change            : num  3.527 0 0.173 3.075 0.603 ...
    


```R
#Create a new data frame to hold the data we want
model_data_discrete <- model_data %>% select (pid,avg_weight_change,avg_visits, stdev_weight_change,stdev_visits,
                                              consec_0_visits_v2,first_month_weightlost, first_month_weightgain, membership, processor,
                                              total.spend,birth.year,churn_v2)
```


```R
#remove duplicates
model_data_discrete <- model_data_discrete[!duplicated(model_data_discrete$pid),]
```


```R
#confirm the dups are removed
print(paste("There are",length(unique(model_data_discrete$pid)),"customer IDs and the number of rows = ",dim(model_data_discrete)[1]))
```

    [1] "There are 9550 customer IDs and the number of rows =  9550"
    


```R
#read in new data with gender added
gender_data <- read.csv('datawithgenderbothpids.csv', stringsAsFactors = FALSE)
head(gender_data)
```


<table>
<thead><tr><th scope=col>X.1</th><th scope=col>X</th><th scope=col>pid</th><th scope=col>birth.year</th><th scope=col>first.payment</th><th scope=col>last.payment</th><th scope=col>transaction.count</th><th scope=col>total.spend</th><th scope=col>processor</th><th scope=col>customer.status</th><th scope=col>...</th><th scope=col>first_month_0_visits</th><th scope=col>consec_0_visits</th><th scope=col>consec_0_visits_v2</th><th scope=col>first_month_visited</th><th scope=col>firstpay_index</th><th scope=col>churn_v2</th><th scope=col>visitsuntil_weightlost</th><th scope=col>visitsuntil_weightlost_factor</th><th scope=col>pid2</th><th scope=col>gender</th></tr></thead>
<tbody>
	<tr><td>1                  </td><td>1                  </td><td>24Oes9064          </td><td>1962-04-26         </td><td>2018-10-25         </td><td>2018-12-25 18:13:00</td><td> 4                 </td><td>596                </td><td>Stripe             </td><td>New                </td><td>...                </td><td>1                  </td><td>8                  </td><td>0                  </td><td> 9                 </td><td> 9                 </td><td>NoChurn            </td><td> 1                 </td><td>1                  </td><td>24OesM4471         </td><td>F                  </td></tr>
	<tr><td>2                  </td><td>2                  </td><td>9Coy9025           </td><td>1954-06-14         </td><td>2018-06-01         </td><td>2018-12-01 18:39:00</td><td> 8                 </td><td>392                </td><td>Stripe             </td><td>Existing           </td><td>...                </td><td>1                  </td><td>6                  </td><td>1                  </td><td> 2                 </td><td> 5                 </td><td>Churn              </td><td>-1                 </td><td>Didnt Lose Weight  </td><td>9CoyA5182          </td><td>F                  </td></tr>
	<tr><td>3                  </td><td>3                  </td><td>17Abo9059          </td><td>1952-12-07         </td><td>NA                 </td><td>2018-06-18 16:41:00</td><td>NA                 </td><td> NA                </td><td>Stripe             </td><td>New                </td><td>...                </td><td>1                  </td><td>5                  </td><td>1                  </td><td> 4                 </td><td> 4                 </td><td>Churn              </td><td> 1                 </td><td>1                  </td><td>17AboS9791         </td><td>F                  </td></tr>
	<tr><td>4                  </td><td>4                  </td><td>19Bar1443          </td><td>1965-06-07         </td><td>2018-11-28         </td><td>2018-12-28 17:03:00</td><td> 3                 </td><td>447                </td><td>Stripe             </td><td>Existing           </td><td>...                </td><td>1                  </td><td>9                  </td><td>0                  </td><td>10                 </td><td>10                 </td><td>NoChurn            </td><td> 2                 </td><td>2 to 3             </td><td>19BarM4357         </td><td>F                  </td></tr>
	<tr><td>5                  </td><td>5                  </td><td>33Fai3547          </td><td>1969-12-09         </td><td>2018-06-29         </td><td>2018-12-29 20:04:00</td><td> 8                 </td><td>792                </td><td>Stripe             </td><td>Existing           </td><td>...                </td><td>1                  </td><td>5                  </td><td>1                  </td><td> 6                 </td><td> 5                 </td><td>Churn              </td><td> 1                 </td><td>1                  </td><td>33FaiK4793         </td><td>F                  </td></tr>
	<tr><td>6                  </td><td>6                  </td><td>40Gra9064          </td><td>1993-10-31         </td><td>2018-10-19         </td><td>2018-12-19 19:28:00</td><td> 4                 </td><td>247                </td><td>Stripe             </td><td>New                </td><td>...                </td><td>1                  </td><td>8                  </td><td>0                  </td><td> 9                 </td><td> 9                 </td><td>NoChurn            </td><td> 2                 </td><td>2 to 3             </td><td>40GraK4623         </td><td>F                  </td></tr>
</tbody>
</table>




```R
#joining in gender data and corrected primary key
model_data_discrete <- left_join(model_data_discrete, gender_data[,c('pid','pid2','gender','visitsuntil_weightlost_factor')], by = 'pid')
```

    Warning message:
    "Column `pid` joining factor and character vector, coercing into character vector"


```R
# str(model_data_discrete$gender)
# model_data_discrete$gender <- tolower(model_data_discrete$gender)
# model_data_discrete$gender <- ifelse(model_data_discrete$gender == 'm','Male','Female')
model_data_discrete$gender <- as.factor(model_data_discrete$gender)
model_data_discrete$churn_v2 <- as.factor(ifelse(model_data_discrete$churn_v2 == 0, 'No Churn', 'Churn'))
```


```R
#In case there were duplicates again with the new data join, remove them
model_data_discrete <- model_data_discrete[!is.na(model_data_discrete$gender),]
```


```R
#Again need to confirm the # of rows and unique customer IDs match up
print(paste("There are",length(unique(model_data_discrete$pid)),"customer IDs and the number of rows = ",dim(model_data_discrete)[1]))
```

    [1] "There are 9550 customer IDs and the number of rows =  9550"
    

## Feature Engineering: how can we transform our data to extract more meaningful variables?


```R
colnames(model_data)
```


<ol class=list-inline>
	<li>'X'</li>
	<li>'pid'</li>
	<li>'birth.year'</li>
	<li>'first.payment'</li>
	<li>'last.payment'</li>
	<li>'transaction.count'</li>
	<li>'total.spend'</li>
	<li>'processor'</li>
	<li>'customer.status'</li>
	<li>'clinic.id'</li>
	<li>'member.code'</li>
	<li>'member.status'</li>
	<li>'membership'</li>
	<li>'num_visits_2'</li>
	<li>'num_visits_3'</li>
	<li>'num_visits_4'</li>
	<li>'num_visits_5'</li>
	<li>'num_visits_6'</li>
	<li>'num_visits_7'</li>
	<li>'num_visits_8'</li>
	<li>'num_visits_9'</li>
	<li>'num_visits_10'</li>
	<li>'num_visits_11'</li>
	<li>'num_visits_12'</li>
	<li>'weightchange_2'</li>
	<li>'weightchange_3'</li>
	<li>'weightchange_4'</li>
	<li>'weightchange_5'</li>
	<li>'weightchange_6'</li>
	<li>'weightchange_7'</li>
	<li>'weightchange_8'</li>
	<li>'weightchange_9'</li>
	<li>'weightchange_10'</li>
	<li>'weightchange_11'</li>
	<li>'weightchange_12'</li>
	<li>'monthly_initial_visit_weight_2'</li>
	<li>'monthly_initial_visit_weight_3'</li>
	<li>'monthly_initial_visit_weight_4'</li>
	<li>'monthly_initial_visit_weight_5'</li>
	<li>'monthly_initial_visit_weight_6'</li>
	<li>'monthly_initial_visit_weight_7'</li>
	<li>'monthly_initial_visit_weight_8'</li>
	<li>'monthly_initial_visit_weight_9'</li>
	<li>'monthly_initial_visit_weight_10'</li>
	<li>'monthly_initial_visit_weight_11'</li>
	<li>'monthly_initial_visit_weight_12'</li>
	<li>'months_0orno_visits'</li>
	<li>'first_month_0_visits'</li>
	<li>'consec_0_visits'</li>
	<li>'consec_0_visits_v2'</li>
	<li>'first_month_visited'</li>
	<li>'firstpay_index'</li>
	<li>'churn_v2'</li>
	<li>'avg_visits'</li>
	<li>'first_month_weightlost'</li>
	<li>'first_month_weightlost_10'</li>
	<li>'first_month_weightgain'</li>
	<li>'first_month_weightgain_10'</li>
	<li>'avg_weight_change'</li>
	<li>'stdev_visits'</li>
	<li>'stdev_weight_change'</li>
</ol>




```R
#Create a generation variable

#Let's feature engineer other features with apply which lets us iterate over rows pretty easily

model_data$avg_visits <- apply(select(model_data,starts_with("num_visits")),
                               1,function(x) mean(x,na.rm = TRUE))

model_data$first_month_weightlost <- apply(select(model_data,starts_with("weightchange")),
                                            1,function(x) which(x < 0)[1]) - model_data$first_month_visited + 1

model_data$first_month_weightlost_10 <- apply(select(model_data,starts_with("weightchange")),
                                               1,function(x) which(x <= -10)[1]) - model_data$first_month_visited + 1


model_data$first_month_weightgain <- apply(select(model_data,starts_with("weightchange")),
                                            1,function(x) which(x > 1)[1]) - model_data$first_month_visited + 1


model_data$first_month_weightgain_10 <- apply(select(model_data,starts_with("weightchange")),
                                               1,function(x) which(x >= 10)[1]) - model_data$first_month_visited + 1

model_data$avg_weight_change <- apply(select(model_data,starts_with("weightchange")),
                                       1,function(x) mean(x,na.rm = TRUE))

model_data$stdev_visits <- apply(select(model_data,starts_with("num_visits")),
                                  1,function(x) sd(x,na.rm = TRUE))

model_data$stdev_weight_change <- apply(select(model_data,starts_with("weightchange")),
                                         1,function(x) sd(x,na.rm = TRUE))
```


```R
model_data_discrete$generation <- as.factor(ifelse(is.na(model_data_discrete$birthyear_factor), "Gen X",ifelse(
  model_data_discrete$birthyear_factor <= 1945, "Silent",ifelse(
  model_data_discrete$birthyear_factor <= 1964, "Baby Boomers",
  ifelse(    model_data_discrete$birthyear_factor <= 1979, "Gen X",
             ifelse(
               model_data_discrete$birthyear_factor <= 1985, "Xennials",
               ifelse(
                 model_data_discrete$birthyear_factor <= 1994, "Millennials",
                 ifelse(
                   model_data_discrete$birthyear_factor <= 2012, "iGen","Other"))))))))
```


```R
#Bin the data into 5 equally distributed groups. Chose not to use Weight-of-Evidence binning since
#equal counts are easier for visualizations and avoid the problem of class imbalance; since this is a small data set
#I am more concerned with class imbalance than WoE. Merge the binned groups with the original data
test <- as.data.frame(apply(select(model_data_discrete,avg_weight_change,avg_visits,stdev_weight_change,
                                   stdev_visits,first_month_weightlost,first_month_weightgain,total.spend,
                                   ),2,function(x) bin(x,nbins =5, method = "content")))


model_data_discrete <- cbind(model_data_discrete, test)

```


```R
#Adjust the target variable to be a factor and change the levels so that the visualizations arent screwed up 
#(for some reason they were shown in the incorrect order)
model_data_discrete$churn_v2 <- as.factor(model_data_discrete$churn_v2)
                            
levels(model_data_discrete$churn_v2) <- c("Churn","NoChurn")
```


```R
#Append binned to the end of the names to prevent duplicate names for columns 
names(test) <- paste(names(test),"binned")
```


```R
#Change some of the levels since they are confusing
levels(model_data_discrete$`first_month_weightgain binned`) <- c("0","1 to 3","4 to 10")

levels(model_data_discrete$`first_month_weightlost binned`) <- c("0","1","2 to 10")
levels(model_data_discrete$`avg_weight_change binned`) <- c("0 to 25","-0.6 to -0.1","-1.12 to -.6","-1.85 to -1.12",
                                                            "-14.4 to -1.85")
levels(model_data_discrete$`avg_visits binned`) <- c("0 to 1.18","1.18 to 2.27","2.27 to 3.73","3.73 to 5.65",
                                                            "5.65 to 31")
```


```R
str(model_data_discrete)
```

    'data.frame':	9550 obs. of  27 variables:
     $ pid                          : chr  "24Oes9064" "9Coy9025" "17Abo9059" "19Bar1443" ...
     $ avg_weight_change            : num  -1.882 0 -0.1 -0.927 -0.182 ...
     $ avg_visits                   : num  3.364 0.364 1.091 1.727 0.727 ...
     $ stdev_weight_change          : num  3.527 0 0.173 3.075 0.603 ...
     $ stdev_visits                 : num  6.816 0.674 1.921 5.405 2.412 ...
     $ consec_0_visits_v2           : int  0 1 1 0 1 0 0 0 0 0 ...
     $ first_month_weightlost       : num  0 0 0 1 0 1 0 0 0 0 ...
     $ first_month_weightgain       : num  0 0 0 0 0 0 2 0 0 0 ...
     $ membership                   : Factor w/ 3 levels "BWM","WPM","WUM": 3 1 1 3 2 1 1 1 2 2 ...
     $ processor                    : Factor w/ 2 levels "Square","Stripe": 2 2 2 2 2 2 2 2 2 2 ...
     $ total.spend                  : num  596 392 0 447 792 ...
     $ birth.year                   : Factor w/ 7430 levels "1918-01-15","1918-01-24",..: 2828 1379 1151 3493 4423 7082 2433 2889 103 1699 ...
     $ churn_v2                     : Factor w/ 2 levels "No Churn","Churn": 1 2 2 1 2 1 1 1 1 1 ...
     $ birthyear_factor             : num  1962 1954 1952 1965 1969 ...
     $ generation                   : Factor w/ 6 levels "Baby Boomers",..: 1 1 1 2 2 4 1 1 5 1 ...
     $ avg_weight_change binned     : Factor w/ 5 levels "0 to 25","-0.6 to -0.1",..: 5 1 2 3 2 5 1 3 3 4 ...
     $ avg_visits binned            : Factor w/ 5 levels "0 to 1.18","1.18 to 2.27",..: 3 1 1 2 1 3 2 2 4 4 ...
     $ stdev_weight_change binned   : Factor w/ 5 levels "(-0.0861,1.29]",..: 4 1 1 4 1 5 1 2 2 4 ...
     $ stdev_visits binned          : Factor w/ 5 levels "(-0.0177,1.69]",..: 5 1 2 4 2 4 2 3 4 3 ...
     $ first_month_weightlost binned: Factor w/ 3 levels "0","1","2 to 10": 1 1 1 2 1 2 1 1 1 1 ...
     $ first_month_weightgain binned: Factor w/ 3 levels "0","1 to 3","4 to 10": 1 1 1 1 1 1 2 1 1 1 ...
     $ total.spend binned           : Factor w/ 5 levels "(-4.52,319]",..: 4 3 1 3 4 1 3 1 4 5 ...
     $ predicted_glm_NoChurn        : num  0.871 0.685 0.482 0.813 0.524 ...
     $ predicted_glm_Churn          : num  0.129 0.315 0.518 0.187 0.476 ...
     $ pid2                         : chr  "24OesM4471" "9CoyA5182" "17AboS9791" "19BarM4357" ...
     $ gender                       : Factor w/ 2 levels "Female","Male": 1 1 1 1 1 1 1 1 1 1 ...
     $ visitsuntil_weightlost_factor: chr  "1" "Didnt Lose Weight" "1" "2 to 3" ...
    

## Plotting 


```R
# Basic plot showing churn vs non-churn in our data
g <- ggplot(model_data_discrete,aes(churn_v2))
g + geom_bar(aes(y = (..count..)/sum(..count..)))+
scale_y_continuous(labels = scales::percent)+
# ylab("Percentage of All Subscribers")+
labs(x = 'Churn',y = 'Percentage of All Subscribers',title = 'Churn vs No Churn')


```




![png](output_22_1.png)



```R
#Let's look at average weight change per month as a function of average visits per month: we expect a high correlation
g2 <- ggplot(model_data_discrete, aes(x = avg_visits, y = avg_weight_change))+
  geom_point(aes(colour = churn_v2))+
  geom_smooth(method = 'loess', formula = y ~ x)+
  facet_wrap(~generation)+
    labs(x = 'Avg Visits per Month (2018)',y = 'Avg Weight Change per Month',title = 'Weight Change as function of Visits - Generation')
  

g2
# ggsave('WtChange_Visits_Generation.png', plot = g2)
```




![png](output_23_1.png)


### Box Plots of various factors with Churn as the fill


```R
#Now lets look at membership type avg weight change and churn
boxplot_1 <- qplot(x = as.factor(model_data_discrete$churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                 fill = membership, main = "Membership and Weight Change - Churn",xlab = 'No Churn vs Churn', ylim = c(-10,10)) +
 scale_fill_discrete(name="Membership Type")
boxplot_1

ggsave('Membership and weight change.png',boxplot_1)
```

    Warning message:
    "Removed 7 rows containing non-finite values (stat_boxplot)."



    Saving 6.67 x 6.67 in image
    Warning message:
    "Removed 7 rows containing non-finite values (stat_boxplot)."


![png](output_25_3.png)



```R
# #Now lets look at first month sub lost weight vs avg weight change and churn

# boxplot_2 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
#                    fill = `first_month_weightlost binned`, main = "First Month Lost Weight vs Avg Total Weight Change - Churn",
#                   xlab = 'No Churn vs Churn', ylim = c(-10,10))

# boxplot_2

```


```R
# #Now lets look at first month sub gained weight vs avg weight change and churn

# boxplot_3 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
#                    fill = `first_month_weightgain binned`, main = "First Month Gained Weight and Avg Total Weight Change - Churn",
#                   xlab = 'No Churn vs Churn', ylim = c(-10,10))

# boxplot_3
```


```R
#Now lets look at variability in visits per month vs avg weight change and churn


boxplot_4 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `stdev_visits binned`, main = "Variabiltiy in Visits and Avg Total Weight Change - Churn",xlab = 'No Churn vs Churn')+ scale_fill_discrete(name="Visit Variability")
boxplot_4
```




![png](output_28_1.png)



```R
#Now lets look at generation vs avg weight change and churn

boxplot_5 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = generation, main = "Generation and Avg Total Weight Change - Churn", xlab = 'No Churn vs Churn')+ 
scale_fill_discrete(name="Gen")
boxplot_5
```




![png](output_29_1.png)



```R
#Need to re-level churn variable since it was throwing off the visualizations
model_data_discrete$churn_v2 <- factor(model_data_discrete$churn_v2,levels(model_data_discrete$churn_v2)[c(2,1)])
```

### Bar Charts of various factors with Churn as the fill


```R
bar_avg_weight_change <- ggplot(data=model_data_discrete, aes(x=`avg_weight_change binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position = 'dodge')+
  labs(x = 'Avg Weight Change Binned (lbs) 2018',title = 'Avg Weight Loss by Churn')+ scale_fill_discrete(name="Churn")
                         
bar_avg_weight_change
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_32_2.png)



```R
bar_avg_visits <- ggplot(data=model_data_discrete, aes(x=`avg_visits binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position = 'dodge')+
  labs(x = 'Avg Visits per Month (2018)',title = 'Avg Visits Per Month by Churn')+scale_fill_discrete(name="Churn")

bar_avg_visits

# bar_avg_visits
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_33_2.png)



```R
bar_std_weight_change <- ggplot(data=model_data_discrete, aes(x=`stdev_weight_change binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position = 'dodge')+
  labs(x = 'Std Dev of Weight Change (lbs) in 2018',title = 'Variabilty in Weight Loss v Churn')+ 
scale_fill_discrete(name="Churn")

bar_std_weight_change
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_34_2.png)



```R
#First month weight lost and gained binned looks off; lets visualize the raw against Churn
#Let's look at average weight change per month as a function of average visits per month: we expect a high correlation
g_fm <- ggplot(data=model_data_discrete, aes(x=model_data_discrete$visitsuntil_weightlost_factor, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position = 'dodge')+
  labs(x = 'Avg Weight Change Binned (lbs) 2018',title = 'First Month Sub Lost Weight v Churn')+ scale_fill_discrete(name="Churn")
                         

  

g_fm
# ggsave('WtChange_Visits_Generation.png', plot = g2)
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_35_2.png)



```R
bar_processor <- ggplot(data=model_data_discrete, aes(x=model_data_discrete$processor, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position = 'dodge')+
  labs(title = 'Processor v Churn', x = "Processor")+ 
scale_fill_discrete(name="Churn")

bar_processor
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_36_2.png)



```R
bar_spend_binned <- ggplot(data=model_data_discrete, aes(x=model_data_discrete$`total.spend binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position = 'dodge')+
  labs(title = 'Avg Spend Binned v Churn', x = "Avg Total Spend")+
scale_fill_discrete(name="Churn")

bar_spend_binned
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_37_2.png)



```R
# Need to reorder the levels here so that the graph displays correctly. This is an ordered factor so that order matters.
model_data_discrete$`total.spend binned` <- factor(model_data_discrete$`total.spend binned`, levels = c("0-319","319-594","594-894","894-1,190","1,190 - 4,520"))

```


```R

bar_gen <- ggplot(data=model_data_discrete, aes(x=generation, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position = 'dodge')+
  labs(x = 'Generation',title = 'Generation v Churn')+
scale_fill_discrete(name="Churn")

bar_gen
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_39_2.png)


Lets explore plotly in R (the plots crashed my browser so commenting them out for now)


```R
# p <- plot_ly(
#   model_data_discrete, x = ~avg_visits, y = ~avg_weight_change,
#   color = ~predicted_glm_Churn, size = ~predicted_glm_Churn
# )
# p
```


```R
# p <- plot_ly(
#   model_data_discrete, x = ~avg_visits, y = ~avg_weight_change,
#   color = ~predicted_glm_Churn, size = ~predicted_glm_Churn
# )
# p
```


```R

# p_3d <- plot_ly(model_data_discrete, x = ~avg_visits, y = ~avg_weight_change, z = ~predicted_glm_Churn, color = ~churn_v2, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'Avg Monthly Visits'),
#                      yaxis = list(title = 'Avg Monhtly Weight Change'),
#                      zaxis = list(title = 'Predicted Churn Probability')))
# p_3d

```

## Prep data for modeling: check collinearities, set seed, split and train/fit


```R
ggpairs(model_data_discrete[,2:8], columnLabels = c("Avg Wt Chg","Avg Visits","Std Dev Wt Ch ","Std Dev Visits","Consec 0 Visits",
                                                   "1st Mth Wt Loss","1st Mth Wt Gain"), title = 'Corr Plot with Distributions')


my_colors <- brewer.pal(nlevels(as.factor(model_data_discrete$churn_v2)), "Set2")
```



    Warning message in eval(expr, envir, enclos):
    "restarting interrupted promise evaluation"Warning message in eval(expr, envir, enclos):
    "internal error -3 in R_decompress1"


    Error in eval(expr, envir, enclos): lazy-load database 'C:/Users/markl/Anaconda3/Lib/R/library/RColorBrewer/R/RColorBrewer.rdb' is corrupt
    Traceback:
    



![png](output_45_3.png)



```R
# Split the data into training and test sets; setting our seed to 123 for reproducible results
```


```R
set.seed(123)
train_index <- caret::createDataPartition(model_data_discrete$churn_v2,p = 0.7, list = FALSE)
train_set <- model_data_discrete[train_index,]
test_set <- model_data_discrete[-train_index,]
```


```R
#Using Caret train function, we can pre-process using centering and scaling
```


```R
colnames(train_set)
```


<ol class=list-inline>
	<li>'pid'</li>
	<li>'avg_weight_change'</li>
	<li>'avg_visits'</li>
	<li>'stdev_weight_change'</li>
	<li>'stdev_visits'</li>
	<li>'consec_0_visits_v2'</li>
	<li>'first_month_weightlost'</li>
	<li>'first_month_weightgain'</li>
	<li>'membership'</li>
	<li>'processor'</li>
	<li>'total.spend'</li>
	<li>'birth.year'</li>
	<li>'churn_v2'</li>
	<li>'birthyear_factor'</li>
	<li>'generation'</li>
	<li>'avg_weight_change binned'</li>
	<li>'avg_visits binned'</li>
	<li>'stdev_weight_change binned'</li>
	<li>'stdev_visits binned'</li>
	<li>'first_month_weightlost binned'</li>
	<li>'first_month_weightgain binned'</li>
	<li>'total.spend binned'</li>
	<li>'predicted_glm_NoChurn'</li>
	<li>'predicted_glm_Churn'</li>
	<li>'pid2'</li>
	<li>'gender'</li>
	<li>'visitsuntil_weightlost_factor'</li>
</ol>



## Fit a Logistic Regression Model with Caret


```R
model_glm <- caret::train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` + gender + visitsuntil_weightlost_factor +
                              `first_month_weightgain binned` + `total.spend binned`+ membership +  generation
                          ,preProc = c("center","scale"),
             data = train_set, method = "glm", family = "binomial")
 
 
```


```R
model_glm
```


    Generalized Linear Model 
    
    6686 samples
       8 predictor
       2 classes: 'No Churn', 'Churn' 
    
    Pre-processing: centered (26), scaled (26) 
    Resampling: Bootstrapped (25 reps) 
    Summary of sample sizes: 6686, 6686, 6686, 6686, 6686, 6686, ... 
    Resampling results:
    
      Accuracy   Kappa    
      0.7927289  0.2701546
    



```R
#Calling summary on our model to view the coefficients: the coefficients are in relation to the reference level:
#e.g. avg weight change binned reference level is 
summary(model_glm)
```


    
    Call:
    NULL
    
    Deviance Residuals: 
        Min       1Q   Median       3Q      Max  
    -1.9588  -0.7109  -0.3985  -0.1643   3.0003  
    
    Coefficients:
                                                     Estimate Std. Error z value
    (Intercept)                                      -1.67441    0.04423 -37.858
    `\\`avg_weight_change binned\\`-0.6 to -0.1`      0.08635    0.04819   1.792
    `\\`avg_weight_change binned\\`-1.12 to -.6`      0.06738    0.05194   1.297
    `\\`avg_weight_change binned\\`-1.85 to -1.12`    0.01602    0.05565   0.288
    `\\`avg_weight_change binned\\`-14.4 to -1.85`   -0.02644    0.06356  -0.416
    `\\`avg_visits binned\\`1.18 to 2.27`            -0.27676    0.03863  -7.164
    `\\`avg_visits binned\\`2.27 to 3.73`            -0.54787    0.04612 -11.879
    `\\`avg_visits binned\\`3.73 to 5.65`            -0.83180    0.05417 -15.355
    `\\`avg_visits binned\\`5.65 to 31`              -1.36805    0.07972 -17.161
    genderMale                                        0.01748    0.03289   0.532
    `visitsuntil_weightlost_factor2 to 3`            -0.15838    0.03588  -4.414
    `visitsuntil_weightlost_factor4 to 5`            -0.11267    0.03850  -2.927
    `visitsuntil_weightlost_factor5+`                -0.01581    0.03352  -0.472
    `visitsuntil_weightlost_factorDidnt Lose Weight` -0.00227    0.03813  -0.060
    `\\`first_month_weightgain binned\\`1 to 3`      -0.13312    0.03995  -3.332
    `\\`first_month_weightgain binned\\`4 to 10`     -0.31443    0.04869  -6.458
    `\\`total.spend binned\\`319-594`                -0.21810    0.04155  -5.249
    `\\`total.spend binned\\`594-894`                -0.03492    0.04807  -0.726
    `\\`total.spend binned\\`894-1,190`               0.31905    0.04470   7.138
    `\\`total.spend binned\\`1,190 - 4,520`           0.32306    0.05634   5.734
    membershipWPM                                     0.02890    0.05133   0.563
    membershipWUM                                    -0.15685    0.05927  -2.646
    `generationGen X`                                 0.09675    0.03628   2.667
    generationiGen                                    0.08709    0.02950   2.953
    generationMillennials                             0.17554    0.03127   5.614
    generationSilent                                 -0.01170    0.03553  -0.329
    generationXennials                                0.02264    0.03325   0.681
                                                     Pr(>|z|)    
    (Intercept)                                       < 2e-16 ***
    `\\`avg_weight_change binned\\`-0.6 to -0.1`     0.073134 .  
    `\\`avg_weight_change binned\\`-1.12 to -.6`     0.194560    
    `\\`avg_weight_change binned\\`-1.85 to -1.12`   0.773437    
    `\\`avg_weight_change binned\\`-14.4 to -1.85`   0.677397    
    `\\`avg_visits binned\\`1.18 to 2.27`            7.81e-13 ***
    `\\`avg_visits binned\\`2.27 to 3.73`             < 2e-16 ***
    `\\`avg_visits binned\\`3.73 to 5.65`             < 2e-16 ***
    `\\`avg_visits binned\\`5.65 to 31`               < 2e-16 ***
    genderMale                                       0.595014    
    `visitsuntil_weightlost_factor2 to 3`            1.01e-05 ***
    `visitsuntil_weightlost_factor4 to 5`            0.003426 ** 
    `visitsuntil_weightlost_factor5+`                0.637101    
    `visitsuntil_weightlost_factorDidnt Lose Weight` 0.952523    
    `\\`first_month_weightgain binned\\`1 to 3`      0.000863 ***
    `\\`first_month_weightgain binned\\`4 to 10`     1.06e-10 ***
    `\\`total.spend binned\\`319-594`                1.53e-07 ***
    `\\`total.spend binned\\`594-894`                0.467576    
    `\\`total.spend binned\\`894-1,190`              9.50e-13 ***
    `\\`total.spend binned\\`1,190 - 4,520`          9.78e-09 ***
    membershipWPM                                    0.573401    
    membershipWUM                                    0.008134 ** 
    `generationGen X`                                0.007651 ** 
    generationiGen                                   0.003151 ** 
    generationMillennials                            1.97e-08 ***
    generationSilent                                 0.741863    
    generationXennials                               0.496006    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    (Dispersion parameter for binomial family taken to be 1)
    
        Null deviance: 7113.7  on 6685  degrees of freedom
    Residual deviance: 5768.8  on 6659  degrees of freedom
    AIC: 5822.8
    
    Number of Fisher Scoring iterations: 6
    


## Explaining the coefficients:
We exponentiate the coefficients to provide us with an interpretable output. 

Each of the coefficient values can be interpreted as follows: 
For each 1 unit change in the variable, the odds of Churn increase (decrease) by the coefficient amount. But, since we binned all of our variables, they are actually in reference to the baseline category. Notice how for generation, Babyboomers isnt listed? well, thats because they are the reference category. So, Gen X has an increased odds ratio of 1.10x, all else equal, of being churn in this model. 



```R
levels(model_data_discrete$generation)
```


<ol class=list-inline>
	<li>'Baby Boomers'</li>
	<li>'Gen X'</li>
	<li>'iGen'</li>
	<li>'Millennials'</li>
	<li>'Silent'</li>
	<li>'Xennials'</li>
</ol>




```R
 exp(coef(model_glm$finalModel))

```


<dl class=dl-horizontal>
	<dt>(Intercept)</dt>
		<dd>0.187419507472878</dd>
	<dt>`\`avg_weight_change binned\`-0.6 to -0.1`</dt>
		<dd>1.09018994631605</dd>
	<dt>`\`avg_weight_change binned\`-1.12 to -.6`</dt>
		<dd>1.06970132276319</dd>
	<dt>`\`avg_weight_change binned\`-1.85 to -1.12`</dt>
		<dd>1.01614962733761</dd>
	<dt>`\`avg_weight_change binned\`-14.4 to -1.85`</dt>
		<dd>0.973904028672536</dd>
	<dt>`\`avg_visits binned\`1.18 to 2.27`</dt>
		<dd>0.758233666918215</dd>
	<dt>`\`avg_visits binned\`2.27 to 3.73`</dt>
		<dd>0.578178725862532</dd>
	<dt>`\`avg_visits binned\`3.73 to 5.65`</dt>
		<dd>0.435266673054713</dd>
	<dt>`\`avg_visits binned\`5.65 to 31`</dt>
		<dd>0.25460377187374</dd>
	<dt>genderMale</dt>
		<dd>1.01763683046844</dd>
	<dt>`visitsuntil_weightlost_factor2 to 3`</dt>
		<dd>0.853520876308615</dd>
	<dt>`visitsuntil_weightlost_factor4 to 5`</dt>
		<dd>0.893447392338229</dd>
	<dt>`visitsuntil_weightlost_factor5+`</dt>
		<dd>0.984313414719902</dd>
	<dt>`visitsuntil_weightlost_factorDidnt Lose Weight`</dt>
		<dd>0.997732108402759</dd>
	<dt>`\`first_month_weightgain binned\`1 to 3`</dt>
		<dd>0.875359674163057</dd>
	<dt>`\`first_month_weightgain binned\`4 to 10`</dt>
		<dd>0.730204641336815</dd>
	<dt>`\`total.spend binned\`319-594`</dt>
		<dd>0.804048133041916</dd>
	<dt>`\`total.spend binned\`594-894`</dt>
		<dd>0.965679612889217</dd>
	<dt>`\`total.spend binned\`894-1,190`</dt>
		<dd>1.37581876095858</dd>
	<dt>`\`total.spend binned\`1,190 - 4,520`</dt>
		<dd>1.38134969390992</dd>
	<dt>membershipWPM</dt>
		<dd>1.02932389044586</dd>
	<dt>membershipWUM</dt>
		<dd>0.854828119632264</dd>
	<dt>`generationGen X`</dt>
		<dd>1.10158930815332</dd>
	<dt>generationiGen</dt>
		<dd>1.09099315502219</dd>
	<dt>generationMillennials</dt>
		<dd>1.19188562365364</dd>
	<dt>generationSilent</dt>
		<dd>0.988364915371729</dd>
	<dt>generationXennials</dt>
		<dd>1.02289529319978</dd>
</dl>




```R
predictions_glm_1 <- predict(model_glm,test_set, type = "prob")
# colnames(predictions_glm_1) <- c('NoChurn','Churn')
```

## Fit a KNN model with caret


```R
model_knn <- train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` + gender + visitsuntil_weightlost_factor + membership + generation
                   , data = train_set, method = "knn",
                   preProc = c("center","scale"),
                   tuneGrid = data.frame(.k = 1:20),
                   trControl = trainControl(method = "cv"))
predictions_knn <- predict(model_knn,test_set, type = "prob")
```


```R
# Need this to draw the ROC curve later
result.roc.knn <- roc(test_set$churn_v2, predictions_knn$Churn) 
```


```R
plot(model_knn,main = 'KNN model - Accuracy by Neighbor # ')
```




![png](output_61_1.png)


## Random Forest
In random forests, the model uses "bootstrapping" which is multiple random sampling; then, using a random subset of predictors
at each stage, fit a tree to each sample which creates the forest. Then, combine all the individual trees to improve predictions.




```R
train_set_rf <- train_set %>% select(churn_v2,`avg_weight_change binned`, `avg_visits binned`, membership, generation, `first_month_weightlost binned`)
test_set_rf <- test_set %>% select(churn_v2,`avg_weight_change binned`, `avg_visits binned`, membership, generation, `first_month_weightlost binned`)
rfmodel <- randomForest(churn_v2 ~ ., data = train_set_rf, ntree = 500, mtry =4, nodesize = 5,
                              importance =TRUE)
```


```R
rfmodel
```


    
    Call:
     randomForest(formula = churn_v2 ~ ., data = train_set_rf, ntree = 500,      mtry = 4, nodesize = 5, importance = TRUE) 
                   Type of random forest: classification
                         Number of trees: 500
    No. of variables tried at each split: 4
    
            OOB estimate of  error rate: 24.23%
    Confusion matrix:
             No Churn Churn class.error
    No Churn     4758   428  0.08252989
    Churn        1191   306  0.79559118



```R
varImpPlot(rfmodel, type = 1, main = "Variable Importance Plot Random Forest")
```


![png](output_65_0.png)



```R
head(rf_pred[,2])
rf_pred_raw <- predict(rfmodel, test_set_rf_)
```


<dl class=dl-horizontal>
	<dt>2</dt>
		<dd>0.79</dd>
	<dt>4</dt>
		<dd>0.094</dd>
	<dt>10</dt>
		<dd>0</dd>
	<dt>16</dt>
		<dd>0</dd>
	<dt>17</dt>
		<dd>0.198</dd>
	<dt>18</dt>
		<dd>0</dd>
</dl>




    Error in as.data.frame(newdata): object 'test_set_rf_' not found
    Traceback:
    

    1. predict(rfmodel, test_set_rf_)

    2. predict.randomForest(rfmodel, test_set_rf_)

    3. as.data.frame(newdata)



```R
confusionMatrix(rf_pred_raw, test_set_rf$churn_v2, positive = "Churn")
```


    Confusion Matrix and Statistics
    
              Reference
    Prediction No Churn Churn
      No Churn     2053   498
      Churn         170   143
                                              
                   Accuracy : 0.7668          
                     95% CI : (0.7508, 0.7821)
        No Information Rate : 0.7762          
        P-Value [Acc > NIR] : 0.8908          
                                              
                      Kappa : 0.1793          
     Mcnemar's Test P-Value : <2e-16          
                                              
                Sensitivity : 0.22309         
                Specificity : 0.92353         
             Pos Pred Value : 0.45687         
             Neg Pred Value : 0.80478         
                 Prevalence : 0.22381         
             Detection Rate : 0.04993         
       Detection Prevalence : 0.10929         
          Balanced Accuracy : 0.57331         
                                              
           'Positive' Class : Churn           
                                              


## Evaluating Class Probabilities with ROC Curves and AUC (Area under the Curve)
The Receiver Operating Characteristic (ROC) curve is a tool used to assess how well a model can discriminate between two classes at different thresholds. It graphs the sensitivity % (true positive rate) vs specificity % (1-false positive rate). 
True positive rate is ratio of correctly classifying churners divided by all actual churn; False positive rate is the ratio of falsely predicting churn divided by all those who werent churn.

The AUC, which is the area under the ROC curve, has an important statistical property: the AUC of a classifier is equivalent to the probability that the classifier will rank a randomly chosen positive instance higher than a randomly chosen negative instance.



```R
plot.roc(result.roc_glm1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="light blue", print.thres=TRUE, main = 'ROC Curve Logistic Regression Model')

```


![png](output_69_0.png)



```R
plot_pred_type_distribution <- function(dfs, threshold) {
  v <- rep(NA, nrow(dfs))
  v <- ifelse(dfs$Churn >= threshold & dfs$churn_v2 == "Churn", "TP", v)
  v <- ifelse(dfs$Churn >= threshold & dfs$churn_v2 == "No Churn", "FP", v)
  v <- ifelse(dfs$Churn < threshold & dfs$churn_v2 == "Churn", "FN", v)
  v <- ifelse(dfs$Churn < threshold & dfs$churn_v2 == "No Churn", "TN", v)
  
  dfs$pred_type <- v
  
  ggplot(data=dfs, aes(x=churn_v2, y=Churn)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Visualizing the Model: Threshold at %.2f", threshold))
}
```


```R
predictions_full_set <- predict(model_glm,model_data_discrete,type = "prob")

dataforplot_glm1 <- cbind(predictions_full_set,model_data_discrete)

```

## Visualizing the ROC curve: tradeoff between True & False Negative and True & False Positives


```R
plot_pred_type_distribution(dataforplot_glm1,0.22)
```




![png](output_73_1.png)



```R
#ROC for KNN
plot.roc(result.roc.knn, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="light blue", print.thres=TRUE, main = 'ROC Curve KNN Model')


```


![png](output_74_0.png)



```R
#ROC for random forest
rf_pred <- predict(rfmodel, test_set, type = "prob")
```


```R
result.roc.rf <- roc(test_set$churn_v2, rf_pred[,2])
```


```R
plot.roc(result.roc.rf, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="light blue", print.thres=TRUE, main = 'ROC Curve Random Forest Model')
```


![png](output_77_0.png)



```R
#Lets take a look at all three models together

gg_first_3_mods_ROC <- ggroc(list(glm1 = result.roc_glm1, knn = result.roc.knn,rf = result.roc.rf))
gg_first_3_mods_ROC + ggtitle('Logistic Regression, KNN and R.F. - ROC Curves')
```




![png](output_78_1.png)



```R
predictions_glm_all_data <- predict(model_glm, model_data_discrete, type = "prob")
```


```R
dim(predictions_glm_all_data)
```


<ol class=list-inline>
	<li>9550</li>
	<li>2</li>
</ol>




```R
head(predictions_glm_all_data)

```


<table>
<thead><tr><th scope=col>No Churn</th><th scope=col>Churn</th></tr></thead>
<tbody>
	<tr><td>0.8753424</td><td>0.1246576</td></tr>
	<tr><td>0.6697341</td><td>0.3302659</td></tr>
	<tr><td>0.4821546</td><td>0.5178454</td></tr>
	<tr><td>0.8470631</td><td>0.1529369</td></tr>
	<tr><td>0.4394196</td><td>0.5605804</td></tr>
	<tr><td>0.7647015</td><td>0.2352985</td></tr>
</tbody>
</table>




```R
colnames(predictions_glm_all_data) <- c("GlmPredictedNoChurn","GlmPredictedChurn")

```


```R
model_data_discrete$predicted_glm_NoChurn <- predictions_glm_all_data$GlmPredictedNoChurn
model_data_discrete$predicted_glm_Churn <- predictions_glm_all_data$GlmPredictedChurn
```


```R
write.csv(model_data_discrete, 'model_data_final.csv')
```
