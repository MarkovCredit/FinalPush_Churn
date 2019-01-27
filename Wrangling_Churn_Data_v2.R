
library(GGally)
library(dplyr)
library(RColorBrewer)
library(car)
library(ggplot2)
library(OneR)
library(markdown)
library(digest)
library(reshape2)

# install.packages('reshape2')
#load in the cleaneddata
model_data <- read.csv('df_combined_012619.csv')

#create additional variabes

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





#Discretize the data into bins
model_data_discrete <- model_data %>% select (pid,avg_weight_change,avg_visits, stdev_weight_change,stdev_visits,
                                              consec_0_visits_v2,first_month_weightlost, first_month_weightgain,
                                              churn_v2)

#Filling in NAs with 0s
model_data_discrete[is.na(model_data_discrete)] <- 0 



#merging the binned columns with the original data
test <- as.data.frame(apply(model_data_discrete[2:8],2,function(x) bin(x,nbins =5, method = "content")))

names(test) <- paste(names(test),"binned")
model_data_discrete <- cbind(model_data_discrete, test)
model_data_discrete$churn_v2 <- as.factor(model_data_discrete$churn_v2)



#visualize Churn by variables
g <- ggplot(model_data_discrete,aes(churn_v2))
g + geom_bar()

g2 <- ggplot(model_data_discrete, aes(x = avg_visits, y = avg_weight_change))+
  geom_point()+
  facet_wrap(~churn_v2)

g3 <- ggplot(model_data_discrete, aes(x = avg_visits, y = avg_weight_change))+
  geom_point()+
  facet_wrap(~churn_v2)

  
ggpairs(model_data_discrete[,2:8])
ggcorr(model_data_discrete[2:8], method = c("everything", "pearson")) 

my_colors <- brewer.pal(nlevels(as.factor(model_data_discrete$churn_v2)), "Set2")





#boxplot: plotting churn with membership and avg weight change
boxplot_1 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data,geom="boxplot", 
                 fill = membership, main = "Membership and Weight Change - Churn")
boxplot_1

boxplot_2 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `first_month_weightlost binned`, main = "First Month Lost Weight vs Avg Total Weight Change - Churn")

boxplot_2

boxplot_3 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `first_month_weightgain binned`, main = "First Month Gained Weight and Avg Total Weight Change - Churn")

boxplot_3


boxplot_4 <- qplot(x = as.factor(churn_v2), y = avg_weight_change, data = model_data_discrete,geom="boxplot", 
                   fill = `stdev_visits binned`, main = "Variabiltiy in Visits and Avg Total Weight Change - Churn")
boxplot_4



bar_avg_weight_change <- ggplot(data=model_data_discrete, aes(x=`avg_weight_change binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'Weight Loss v Churn')

bar_avg_weight_change

bar_avg_visits <- ggplot(data=model_data_discrete, aes(x=`avg_visits binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'Visits v Churn')

bar_avg_visits

bar_std_weight_change <- ggplot(data=model_data_discrete, aes(x=`stdev_weight_change binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'Variabilty in Weight Loss v Churn')

bar_std_weight_change

bar_first_month_lost <- ggplot(data=model_data_discrete, aes(x=`first_month_weightlost binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'First Month Weight Loss v Churn')


bar_first_month_gain <- ggplot(data=model_data_discrete, aes(x=`first_month_weightgain binned`, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(title = 'First Month Weight Gain v Churn')

bar_first_month_gain

#i dont think variabiltiy in weight  change or visits are that predictive since they just mirror 
#actual weight  change and visits counts 



#Scatterplot matrices are a great way to roughly determine if you have a linear correlation between multiple variables.
scatterplotMatrix(~avg_weight_change+avg_visits+stdev_weight_change+stdev_visits|churn_v2, data=model_data_discrete , reg.line="" , smoother="", col=my_colors , smoother.args=list(col="grey") , cex=1.5 , pch=c(15,16,17) , 
                  main="Scatter plot Matrices - variable vs variable (1 = Churn)")



#Logisitic Regression
set.seed(123)
train_index <- caret::createDataPartition(model_data_discrete$churn_v2,p = 0.7, list = FALSE)
train_set <- model_data_discrete[train_index,]
test_set <- model_data_discrete[-train_index,]






 model_glm <- caret::train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` +`stdev_weight_change binned` +
                       `stdev_visits binned` + `first_month_weightlost binned`,
             data = train_set, method = "glm", family = "binomial")
 
 model_glm_2 <- caret::train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` 
                               , data = train_set, method = "glm", family = "binomial")
 
#take a look at the coefficient weights
 exp(coef(model_glm$finalModel))
 exp(coef(model_glm_2$finalModel))
 
 summary(model_glm)
 model_glm_2$results
 
#lets run some predictions using the first and second logistic regression models
 
 predictions_glm <- predict(model,test_set, type = "prob")
 head(predictions_glm)
 predictions_glm_2 <- predict(model_glm_2,test_set, type = "prob")
 colnames(predictions_glm_2) <- c('NoChurn','Churn')
 head(predictions_glm_2)
 
 predictions_glm_all_data <- predict(model_glm_2, model_data_discrete, type = "prob")
 colnames(predictions_glm_all_data) <- c('NoChurn','Churn')
 
 write.csv(cbind(model_data_discrete,predictions_glm_all_data), "model_predictions.csv")
 
 
 
 #ROC Curve for models
 
 #logistic regression
 
 
 
 
 
 
 
 quartz(title = 'avg weight change vs churn')
#KNN
model_knn <- train(churn_v2 ~ `avg_weight_change binned`+ `avg_visits binned` 
                   , data = train_set, method = "knn",
                   preProc = c("center","scale"),
                   tuneGrid = data.frame(.k = 1:20),
                   trControl = trainControl(method = "cv"))
predictions_knn <- predict(model_knn,test_set)
model_knn$modelInfo
model_knn$results

#Multi-Class classificiations


