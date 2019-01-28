
###Churn/ Customer Retention Modeling


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
# R.Version()
```

    Type 'citation("pROC")' for a citation.
    
    Attaching package: 'pROC'
    
    The following objects are masked from 'package:stats':
    
        cov, smooth, var
    
    


```R
model_data <- read.csv('df_combined_012619.csv')
```


```R
model_data$avg_visits <- apply(select(model_data,starts_with("num_visits")),
                               1,function(x) mean(x,na.rm = TRUE))

model_data$first_month_weightlost <- apply(select(model_data,starts_with("weightchange")),
                                            1,function(x) which(x < 0)[1]) - model_data$first_month_visited

model_data$first_month_weightlost_10 <- apply(select(model_data,starts_with("weightchange")),
                                               1,function(x) which(x <= -10)[1]) - model_data$first_month_visited


model_data$first_month_weightgain <- apply(select(model_data,starts_with("weightchange")),
                                            1,function(x) which(x > 1)[1]) - model_data$first_month_visited


model_data$first_month_weightgain_10 <- apply(select(model_data,starts_with("weightchange")),
                                               1,function(x) which(x >= 10)[1]) - model_data$first_month_visited

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
#Discretize the data into bins
model_data_discrete <- model_data %>% select (pid,avg_weight_change,avg_visits, stdev_weight_change,stdev_visits,
                                              consec_0_visits_v2,first_month_weightlost, first_month_weightgain, membership, processor,
                                              total.spend,birth.year,churn_v2)

model_data_discrete$birthyear_factor <- as.integer(substr(model_data_discrete$birth.year,1,4))


#Filling in NAs with 0s
model_data_discrete[is.na(model_data_discrete)] <- 0 
model_data_discrete$churn_v2 <- as.factor(ifelse(model_data_discrete$churn_v2 == 0, 'No Churn', 'Churn'))

summary(model_data_discrete)
```

    Warning message in `[<-.factor`(`*tmp*`, thisvar, value = 0):
    "invalid factor level, NA generated"Warning message in `[<-.factor`(`*tmp*`, thisvar, value = 0):
    "invalid factor level, NA generated"


            pid       avg_weight_change    avg_visits       stdev_weight_change
     19Tor9012:   8   Min.   :-14.3545   Min.   : 0.09091   Min.   : 0.000     
     48Tay9061:   6   1st Qu.: -1.6364   1st Qu.: 1.45454   1st Qu.: 1.490     
     10Cas9040:   4   Median : -0.8455   Median : 3.00000   Median : 2.390     
     10Dur9054:   4   Mean   : -1.0620   Mean   : 3.69204   Mean   : 2.590     
     10Mar8984:   4   3rd Qu.: -0.2455   3rd Qu.: 5.09091   3rd Qu.: 3.377     
     10Pac9062:   4   Max.   : 25.3091   Max.   :31.00000   Max.   :86.087     
     (Other)  :9895                                                            
      stdev_visits    consec_0_visits_v2 first_month_weightlost
     Min.   : 0.000   Min.   :0.0000     Min.   : 0.0000       
     1st Qu.: 2.014   1st Qu.:0.0000     1st Qu.: 0.0000       
     Median : 3.710   Median :0.0000     Median : 0.0000       
     Mean   : 3.982   Mean   :0.1886     Mean   : 0.5813       
     3rd Qu.: 5.658   3rd Qu.:0.0000     3rd Qu.: 1.0000       
     Max.   :17.677   Max.   :1.0000     Max.   :10.0000       
                                                               
     first_month_weightgain membership  processor     total.spend    
     Min.   : 0.000         BWM:2462   Square:3763   Min.   :   0.0  
     1st Qu.: 0.000         WPM:3654   Stripe:6162   1st Qu.: 343.0  
     Median : 0.000         WUM:3809                 Median : 745.0  
     Mean   : 1.463                                  Mean   : 783.1  
     3rd Qu.: 3.000                                  3rd Qu.:1089.0  
     Max.   :10.000                                  Max.   :4516.0  
                                                                     
          birth.year       churn_v2    birthyear_factor        generation  
     1960-05-05:   6   Churn   :2218   Min.   :   0     Baby Boomers:4224  
     1966-12-10:   6   No Churn:7707   1st Qu.:1957     Gen X       :3699  
     1969-02-17:   6                   Median :1965     iGen        : 313  
     1970-03-22:   6                   Mean   :1966     Millennials : 602  
     1971-09-24:   6                   3rd Qu.:1974     Silent      : 410  
     (Other)   :9891                   Max.   :2007     Xennials    : 673  
     NA's      :   4                                    NA's        :   4  



```R
#Merge the binned groups with the original data
test <- as.data.frame(apply(model_data_discrete[2:8],2,function(x) bin(x,nbins =5, method = "content")))
names(test) <- paste(names(test),"binned")
model_data_discrete <- cbind(model_data_discrete, test)
model_data_discrete$churn_v2 <- as.factor(model_data_discrete$churn_v2)
                            
levels(model_data_discrete$churn_v2) <- c("Churn","NoChurn")
```


```R
head(model_data_discrete)
```


<table>
<thead><tr><th scope=col>model_data$birth.year</th><th scope=col>pid</th><th scope=col>avg_weight_change</th><th scope=col>avg_visits</th><th scope=col>stdev_weight_change</th><th scope=col>stdev_visits</th><th scope=col>consec_0_visits_v2</th><th scope=col>first_month_weightlost</th><th scope=col>first_month_weightgain</th><th scope=col>membership</th><th scope=col>...</th><th scope=col>birthyear_factor</th><th scope=col>generation</th><th scope=col>avg_weight_change binned</th><th scope=col>avg_visits binned</th><th scope=col>stdev_weight_change binned</th><th scope=col>stdev_visits binned</th><th scope=col>first_month_weightlost binned</th><th scope=col>first_month_weightgain binned</th><th scope=col>total.spend binned</th><th scope=col>model_data</th></tr></thead>
<tbody>
	<tr><td>1962-04-26    </td><td>24Oes9064     </td><td>-1.8818182    </td><td>3.3636364     </td><td>3.5272704     </td><td>6.8157571     </td><td>0             </td><td>0             </td><td>0             </td><td>WUM           </td><td>...           </td><td>1962          </td><td>Baby Boomers  </td><td>-14.4 to -1.85</td><td>2.27 to 3.73  </td><td>(2.74,3.65]   </td><td>(6.11,17.7]   </td><td>0             </td><td>0             </td><td>(594,894]     </td><td>2828          </td></tr>
	<tr><td>1954-06-14    </td><td>9Coy9025      </td><td> 0.0000000    </td><td>0.3636364     </td><td>0.0000000     </td><td>0.6741999     </td><td>1             </td><td>0             </td><td>0             </td><td>BWM           </td><td>...           </td><td>1954          </td><td>Baby Boomers  </td><td>0 to 25       </td><td>0 to 1.18     </td><td>(-0.0861,1.29]</td><td>(-0.0177,1.69]</td><td>0             </td><td>0             </td><td>(319,594]     </td><td>1379          </td></tr>
	<tr><td>1952-12-07    </td><td>17Abo9059     </td><td>-0.1000000    </td><td>1.0909091     </td><td>0.1732051     </td><td>1.9211739     </td><td>1             </td><td>0             </td><td>0             </td><td>BWM           </td><td>...           </td><td>1952          </td><td>Baby Boomers  </td><td>-0.6 to -0.1  </td><td>0 to 1.18     </td><td>(-0.0861,1.29]</td><td>(1.69,3.01]   </td><td>0             </td><td>0             </td><td>(-4.52,319]   </td><td>1151          </td></tr>
	<tr><td>1965-06-07    </td><td>19Bar1443     </td><td>-0.9272727    </td><td>1.7272727     </td><td>3.0754157     </td><td>5.4053845     </td><td>0             </td><td>1             </td><td>0             </td><td>WUM           </td><td>...           </td><td>1965          </td><td>Gen X         </td><td>-1.12 to -.6  </td><td>1.18 to 2.27  </td><td>(2.74,3.65]   </td><td>(4.43,6.11]   </td><td>1             </td><td>0             </td><td>(319,594]     </td><td>3493          </td></tr>
	<tr><td>1969-12-09    </td><td>33Fai3547     </td><td>-0.1818182    </td><td>0.7272727     </td><td>0.6030227     </td><td>2.4120908     </td><td>1             </td><td>0             </td><td>0             </td><td>WPM           </td><td>...           </td><td>1969          </td><td>Gen X         </td><td>-0.6 to -0.1  </td><td>0 to 1.18     </td><td>(-0.0861,1.29]</td><td>(1.69,3.01]   </td><td>0             </td><td>0             </td><td>(594,894]     </td><td>4423          </td></tr>
	<tr><td>1993-10-31    </td><td>40Gra9064     </td><td>-1.8636364    </td><td>2.5454545     </td><td>4.6793745     </td><td>5.6455936     </td><td>0             </td><td>1             </td><td>0             </td><td>BWM           </td><td>...           </td><td>1993          </td><td>Millennials   </td><td>-14.4 to -1.85</td><td>2.27 to 3.73  </td><td>(3.65,86.2]   </td><td>(4.43,6.11]   </td><td>1             </td><td>0             </td><td>(-4.52,319]   </td><td>7082          </td></tr>
</tbody>
</table>




```R
str(model_data_discrete)
```

    'data.frame':	9925 obs. of  22 variables:
     $ pid                          : Factor w/ 9562 levels "10Agu9063","10Ala9061",..: 2539 9236 1027 1309 4303 5176 2753 5283 5308 3337 ...
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
    


```R
test <- as.data.frame(apply(select(model_data_discrete,avg_weight_change,avg_visits,stdev_weight_change,
                                   stdev_visits,first_month_weightlost,first_month_weightgain,total.spend,
                                   ),2,function(x) bin(x,nbins =5, method = "content")))

names(test) <- paste(names(test),"binned")
model_data_discrete <- cbind(model_data_discrete, test)
```


```R
levels(model_data_discrete$`first_month_weightgain binned`) <- c("0","1 to 3","4 to 10")

levels(model_data_discrete$`first_month_weightlost binned`) <- c("0","1","2 to 10")
levels(model_data_discrete$`avg_weight_change binned`) <- c("0 to 25","-0.6 to -0.1","-1.12 to -.6","-1.85 to -1.12",
                                                            "-14.4 to -1.85")
levels(model_data_discrete$`avg_visits binned`) <- c("0 to 1.18","1.18 to 2.27","2.27 to 3.73","3.73 to 5.65",
                                                            "5.65 to 31")
```


```R
# Basic plot showing churn vs non-churn in our data
g <- ggplot(model_data_discrete,aes(churn_v2))
g + geom_bar(aes(y = (..count..)/sum(..count..)))+
scale_y_continuous(labels = scales::percent)+
ylab("Percentage of All Subscribers")


```




![png](output_11_1.png)



```R
g2 <- ggplot(model_data_discrete, aes(x = avg_visits, y = avg_weight_change))+
  geom_point(aes(colour = churn_v2))+
  geom_smooth(method = 'loess', formula = y ~ x)+
  facet_wrap(~`first_month_weightgain binned`)
  

g2
```




![png](output_12_1.png)



```R
g3 <- ggplot(model_data_discrete, aes(x = avg_visits, y = avg_weight_change))+
  geom_point()+
  facet_wrap(~churn_v2)
g3
```




![png](output_13_1.png)



```R

ggpairs(model_data_discrete[,2:8])
ggcorr(model_data_discrete[2:8], method = c("everything", "pearson")) 

my_colors <- brewer.pal(nlevels(as.factor(model_data_discrete$churn_v2)), "Set2")
```






![png](output_14_2.png)


    Warning message in eval(expr, envir, enclos):
    "restarting interrupted promise evaluation"Warning message in eval(expr, envir, enclos):
    "internal error -3 in R_decompress1"


    Error in eval(expr, envir, enclos): lazy-load database 'C:/Users/markl/Anaconda3/Lib/R/library/RColorBrewer/R/RColorBrewer.rdb' is corrupt
    Traceback:
    



![png](output_14_5.png)



```R
boxplot_1 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data,geom="boxplot", 
                 fill = membership, main = "Membership and Weight Change - Churn")
boxplot_1
```




![png](output_15_1.png)



```R

boxplot_2 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `first_month_weightlost binned`, main = "First Month Lost Weight vs Avg Total Weight Change - Churn")

boxplot_2
```




![png](output_16_1.png)



```R
boxplot_3 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `first_month_weightgain binned`, main = "First Month Gained Weight and Avg Total Weight Change - Churn")

boxplot_3
```




![png](output_17_1.png)



```R






boxplot_4 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `stdev_visits binned`, main = "Variabiltiy in Visits and Avg Total Weight Change - Churn")
boxplot_4
```




![png](output_18_1.png)



```R
model_data_discrete$churn_v2 <- factor(model_data_discrete$churn_v2,levels(model_data_discrete$churn_v2)[c(2,1)])
```


```R
bar_avg_weight_change <- ggplot(data=model_data_discrete, aes(x=`avg_weight_change binned`, fill = churn_v2)) +
  geom_bar() +
  geom_label(stat='count', aes(label=..count..), position = 'dodge')+
  labs(title = 'Weight Loss v Churn')

bar_avg_weight_change
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_20_2.png)



```R
bar_avg_visits <- ggplot(data=model_data_discrete, aes(x=`avg_visits binned`, fill = churn_v2)) +
  geom_bar() +
  geom_label(stat='count', aes(label=..count..), position = 'dodge')+
  labs(title = 'Avg Visits Per Month v Churn')

bar_avg_visits

# bar_avg_visits
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_21_2.png)



```R
bar_std_weight_change <- ggplot(data=model_data_discrete, aes(x=`stdev_weight_change binned`, fill = churn_v2)) +
  geom_bar() +
  geom_label(stat='count', aes(label=..count..), position = 'dodge')+
  labs(title = 'Variabilty in Weight Loss v Churn')

bar_std_weight_change
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_22_2.png)



```R
bar_first_month_gain <- ggplot(data=model_data_discrete, aes(x=`first_month_weightgain binned`, fill = churn_v2)) +
  geom_bar() +
  geom_label(stat='count', aes(label=..count..), position = 'dodge')+
  labs(title = 'First Month Weight Gain v Churn', x = "First month (or NA) of weight Gain")

bar_first_month_gain
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_23_2.png)



```R
bar_first_month_lost <- ggplot(data=model_data_discrete, aes(x=`first_month_weightlost binned`, fill = churn_v2)) +
  geom_bar() +
  geom_label(stat='count', aes(label=..count..), position = 'dodge')+
  labs(title = 'First Month Weight Lost v Churn', x = "First month (or NA) of Weight Loss")

bar_first_month_lost
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_24_2.png)



```R
bar_gen <- ggplot(data=model_data_discrete, aes(x=generation, fill = churn_v2)) +
  geom_bar() +
  geom_label(stat='count', aes(label=..count..), position = 'dodge')+
  labs(title = 'Generation v Churn')

bar_gen
```

    Warning message:
    "Width not defined. Set with `position_dodge(width = ?)`"




![png](output_25_2.png)



```R

```


```R
ggpairs(model_data_discrete[,2:8], columnLabels = c("Avg Wt Chg","Avg Visits","Std Dev Wt Ch ","Std Dev Visits","Consec 0 Visits",
                                                   "1st Mth Wt Loss","1st Mth Wt Gain"))


my_colors <- brewer.pal(nlevels(as.factor(model_data_discrete$churn_v2)), "Set2")
```



    Warning message in eval(expr, envir, enclos):
    "restarting interrupted promise evaluation"Warning message in eval(expr, envir, enclos):
    "internal error -3 in R_decompress1"


    Error in eval(expr, envir, enclos): lazy-load database 'C:/Users/markl/Anaconda3/Lib/R/library/RColorBrewer/R/RColorBrewer.rdb' is corrupt
    Traceback:
    



![png](output_27_3.png)



```R
set.seed(123)
train_index <- caret::createDataPartition(model_data_discrete$churn_v2,p = 0.7, list = FALSE)
train_set <- model_data_discrete[train_index,]
test_set <- model_data_discrete[-train_index,]
```


```R
model_glm <- caret::train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` +`stdev_weight_change binned`+ generation +
                       `stdev_visits binned` + `first_month_weightlost binned`,preProc = c("center","scale"),
             data = train_set, method = "glm", family = "binomial")
 
 
```


```R
model_glm
```


    Generalized Linear Model 
    
    6948 samples
       6 predictor
       2 classes: 'No Churn', 'Churn' 
    
    Pre-processing: centered (23), scaled (23) 
    Resampling: Bootstrapped (25 reps) 
    Summary of sample sizes: 6948, 6948, 6948, 6948, 6948, 6948, ... 
    Resampling results:
    
      Accuracy   Kappa    
      0.7764597  0.1556623
    



```R
model_glm_2 <- caret::train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` + generation
                               , data = train_set, method = "glm", family = "binomial")
```


```R
model_glm_2
```


    Generalized Linear Model 
    
    6948 samples
       3 predictor
       2 classes: 'No Churn', 'Churn' 
    
    No pre-processing
    Resampling: Bootstrapped (25 reps) 
    Summary of sample sizes: 6948, 6948, 6948, 6948, 6948, 6948, ... 
    Resampling results:
    
      Accuracy   Kappa    
      0.7734036  0.1143439
    



```R
predictions_glm_2 <- predict(model_glm_2,test_set, type = "prob")
colnames(predictions_glm_2) <- c('NoChurn','Churn')

```

## Evaluating Class Probabilities with ROC Curves and AUC (Area under the Curve)
The Receiver Operating Characteristic (ROC) curve is used to assess the accuracy of a continuous measurement for predicting a binary outcome. What is the tradeoff between sensitivity (predicting churn events correctly, aka true positive) versus specificity (non churn samples are predicted as non churn) and at what cut-off level are the two optimized?
ROC is about 'model discrimination' and telling two classes apart. 



```R
#Lets' look at the AUC for our Logistic Regression model version 2 (just has 2 predictors: average weight change and average visits)
# The 0.22 is the cutoff since that is the rate in the population of churn. We can adjust this level but will leave alone for this
#exercise. 
result.roc <- roc(test_set$churn_v2, predictions_glm_2$Churn) # Draw ROC curve.
 plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")
```


![png](output_35_0.png)



```R
#Running another logistic regression model but we are adding another variable 'first month weight gain binned' 
model_glm_4 <- caret::train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` +  `first_month_weightgain binned`
                             , data = train_set, method = "glm", family = "binomial")
 predictions_glm_4 <- predict(model_glm_4,test_set, type = "prob")

colnames(predictions_glm_4) <- c('NoChurn','Churn')
result.roc_glm4 <- roc(test_set$churn_v2, predictions_glm_4$Churn) # Draw ROC curve.
 plot(result.roc_glm4, print.thres="best", print.thres.best.method="closest.topleft")
```


![png](output_36_0.png)



```R
model_knn <- train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` + generation
                   , data = train_set, method = "knn",
                   preProc = c("center","scale"),
                   tuneGrid = data.frame(.k = 1:20),
                   trControl = trainControl(method = "cv"))
predictions_knn <- predict(model_knn,test_set, type = "prob")
```


```R
#
result.roc.knn <- roc(test_set$churn_v2, predictions_knn$Churn) # Draw ROC curve.
plot(result.roc.knn, print.thres="best", print.thres.best.method="closest.topleft")

```


![png](output_38_0.png)



```R

```
