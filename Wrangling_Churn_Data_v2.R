library(GGally)
library(RColorBrewer)
library(car)
library(ggplot2)
library(OneR)
library(markdown)

install.packages('OneR')
#load in the cleaneddata
model_data <- read.csv('df_combined_012619.csv')
View(model_data)

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
model_data_discrete <- cbind(model_data_discrete, test)




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
                   fill = , main = "Membership and Weight Change - Churn")



#Scatterplot matrices are a great way to roughly determine if you have a linear correlation between multiple variables.
scatterplotMatrix(~avg_weight_change+avg_visits+stdev_weight_change+stdev_visits|churn_v2, data=model_data_discrete , reg.line="" , smoother="", col=my_colors , smoother.args=list(col="grey") , cex=1.5 , pch=c(15,16,17) , 
                  main="Scatter plot Matrices - variable vs variable (1 = Churn)")



#Logisitic Regression

model <- glm(churn_v2 ~ avg_weight_change, avg_visits, stdev_weight_change,stdev_visits,consec_0_visits_v2,
             data = model_data_discrete, family = "gaussian")

#Random Forest



#Multi-Class classificiations


