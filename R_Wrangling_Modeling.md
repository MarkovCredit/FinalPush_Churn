

```R
# remove.packages('openxlsx')
# install.packages('openxlsx')
install.packages('pROC')


```

    Warning message:
    "unable to access index for repository http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/3.5:
      cannot open URL 'http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/3.5/PACKAGES'"

    package 'pROC' successfully unpacked and MD5 sums checked
    
    The downloaded binary packages are in
    	C:\Users\markl\AppData\Local\Temp\RtmpQNNtZC\downloaded_packages
    


```R
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
#Discretize the data into bins
model_data_discrete <- model_data %>% select (pid,avg_weight_change,avg_visits, stdev_weight_change,stdev_visits,
                                              consec_0_visits_v2,first_month_weightlost, first_month_weightgain,
                                              churn_v2)

#Filling in NAs with 0s
model_data_discrete[is.na(model_data_discrete)] <- 0 
```


```R
test <- as.data.frame(apply(model_data_discrete[2:8],2,function(x) bin(x,nbins =5, method = "content")))

names(test) <- paste(names(test),"binned")
model_data_discrete <- cbind(model_data_discrete, test)
model_data_discrete$churn_v2 <- as.factor(model_data_discrete$churn_v2)

```


```R
g <- ggplot(model_data_discrete,aes(churn_v2))
g + geom_bar()


```




![png](output_6_1.png)



```R
g2 <- ggplot(model_data_discrete, aes(x = avg_visits, y = avg_weight_change))+
  geom_point()+
  facet_wrap(~churn_v2)

g2
```




![png](output_7_1.png)



```R
g3 <- ggplot(model_data_discrete, aes(x = avg_visits, y = avg_weight_change))+
  geom_point()+
  facet_wrap(~churn_v2)
g3
```




![png](output_8_1.png)



```R

ggpairs(model_data_discrete[,2:8])
ggcorr(model_data_discrete[2:8], method = c("everything", "pearson")) 

my_colors <- brewer.pal(nlevels(as.factor(model_data_discrete$churn_v2)), "Set2")
```


    Error in ggpairs(model_data_discrete[, 2:8]): could not find function "ggpairs"
    Traceback:
    



```R
boxplot_1 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data,geom="boxplot", 
                 fill = membership, main = "Membership and Weight Change - Churn")
boxplot_1
```




![png](output_10_1.png)



```R

boxplot_2 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `first_month_weightlost binned`, main = "First Month Lost Weight vs Avg Total Weight Change - Churn")

boxplot_2
```




![png](output_11_1.png)



```R
boxplot_3 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `first_month_weightgain binned`, main = "First Month Gained Weight and Avg Total Weight Change - Churn")

boxplot_3
```




![png](output_12_1.png)



```R






boxplot_4 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `stdev_visits binned`, main = "Variabiltiy in Visits and Avg Total Weight Change - Churn")
boxplot_4
```




![png](output_13_1.png)



```R
bar_avg_weight_change <- ggplot(data=model_data_discrete, aes(x=`avg_weight_change binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'Weight Loss v Churn')

bar_avg_weight_change
```




![png](output_14_1.png)



```R
bar_avg_visits <- ggplot(data=model_data_discrete, aes(x=`avg_visits binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'Avg Visits Per Month v Churn')

bar_avg_visits
```




![png](output_15_1.png)



```R
bar_std_weight_change <- ggplot(data=model_data_discrete, aes(x=`stdev_weight_change binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'Variabilty in Weight Loss v Churn')

bar_std_weight_change
```




![png](output_16_1.png)



```R
bar_first_month_gain <- ggplot(data=model_data_discrete, aes(x=`first_month_weightgain binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'First Month Weight Gain v Churn')

bar_first_month_gain
```




![png](output_17_1.png)



```R
bar_first_month_lost <- ggplot(data=model_data_discrete, aes(x=`first_month_weightlost binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'First Month Weight Lost v Churn')

bar_first_month_lost
```




![png](output_18_1.png)



```R
ggpairs(model_data_discrete[,2:8])
ggcorr(model_data_discrete[2:8], method = c("everything", "pearson")) 

my_colors <- brewer.pal(nlevels(as.factor(model_data_discrete$churn_v2)), "Set2")
```






![png](output_19_2.png)



    Error in brewer.pal(nlevels(as.factor(model_data_discrete$churn_v2)), : could not find function "brewer.pal"
    Traceback:
    



![png](output_19_4.png)



```R
set.seed(123)
train_index <- caret::createDataPartition(model_data_discrete$churn_v2,p = 0.7, list = FALSE)
train_set <- model_data_discrete[train_index,]
test_set <- model_data_discrete[-train_index,]
```


```R
model_glm <- caret::train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` +`stdev_weight_change binned` +
                       `stdev_visits binned` + `first_month_weightlost binned`,
             data = train_set, method = "glm", family = "binomial")
 
 
```


```R
model_glm
```


    Generalized Linear Model 
    
    6948 samples
       5 predictor
       2 classes: '0', '1' 
    
    No pre-processing
    Resampling: Bootstrapped (25 reps) 
    Summary of sample sizes: 6948, 6948, 6948, 6948, 6948, 6948, ... 
    Resampling results:
    
      Accuracy   Kappa   
      0.7755605  0.136912
    



```R
model_glm_2 <- caret::train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` 
                               , data = train_set, method = "glm", family = "binomial")
```


```R
model_glm_2
```


    Generalized Linear Model 
    
    6948 samples
       2 predictor
       2 classes: '0', '1' 
    
    No pre-processing
    Resampling: Bootstrapped (25 reps) 
    Summary of sample sizes: 6948, 6948, 6948, 6948, 6948, 6948, ... 
    Resampling results:
    
      Accuracy   Kappa    
      0.7727962  0.1249013
    



```R
predictions_glm_2 <- predict(model_glm_2,test_set, type = "prob")
colnames(predictions_glm_2) <- c('NoChurn','Churn')

```


```R
#Lets' look at the AUC for our Logistic Regression model version 2 (just has 2 predictors: average weight change and average visits)

result.roc <- roc(test_set$churn_v2, predictions_glm_2$Churn) # Draw ROC curve.
 plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")
```


![png](output_26_0.png)



```R
model_knn <- train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` 
                   , data = train_set, method = "knn",
                   preProc = c("center","scale"),
                   tuneGrid = data.frame(.k = 1:20),
                   trControl = trainControl(method = "cv"))
predictions_knn <- predict(model_knn,test_set, type = "prob")
```


```R
#The AUC here is 70.9%
result.roc.knn <- roc(test_set$churn_v2, predictions_knn$`1`) # Draw ROC curve.
plot(result.roc.knn, print.thres="best", print.thres.best.method="closest.topleft")
```


![png](output_28_0.png)

