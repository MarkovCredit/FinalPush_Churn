#load some libs
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(Hmisc)
library(ranger)
library(reshape2)
library(languageR)
library(data.table)
#read in the data
setwd('C:/Users/markl/Desktop/FinalPush_Churn')
df <- read.csv('patient_bigdata_00.csv', stringsAsFactors = FALSE)
df_subscribers <- readxl::read_xlsx('subscribers data set clean.xlsx',col_names = TRUE)

print(paste("There are",length(unique(df_subscribers$PID)),"unqiue subscribers in the sub set"))

View(df_subscribers)



#summary of the DF
summary(df)
str(df)



#make a copy in case something happens
df_2 <- df

#Use HMisc library to describe each column (doesnt work well with text/chr/factor)
describe(df_2)

#Drop some columns that arent necessary
df_2$FIRST_NAME <- NULL
df_2$LAST_NAME <- NULL


#need to create a clean copy of the dataset: remove bad columns, reformat and clean
df_clean <- subset(df, select = -c(INDEX,FIRST_NAME,LAST_NAME,PATIENT_ID,length_Fn))

describe(df_clean)

unique_subscribers <- unique(df_clean$PID)
print(paste("There are",length(unique_subscribers),"unique subscribers"))



#Create some new columns
df_clean$FirstVisitDate <- as.Date(df_clean$MONTHLY_INITIAL_VISIT_DATE,'%m/%d/%y')
df_clean$LastVisitDate <- as.Date(df_clean$MONTHLY_END_VISIT_DATE,'%m/%d/%y')


#drop unnecessary columns
df_clean$MONTHLY_INITIAL_VISIT_DATE <- NULL
df_clean$MONTHLY_END_VISIT_DATE <- NULL

#Feature engineering/creation
df_clean$Membership <- as.factor(ifelse(df_clean$MEMBER %like% 'BWM','BWM', df_clean$MEMBER))
df_clean$DaysBetw <- as.double(df_clean$LastVisitDate - df_clean$FirstVisitDate)
df_clean$Month <- month(df_clean$LastVisitDate)
df_clean$Year <- year(df_clean$LastVisitDate)
df_clean$MEMBER <- NULL
df_clean$OnSessionPlan <- as.factor(ifelse(!is.na(df_clean$MONTHLY_INITIAL_SESSION) | !is.na(df_clean$MONTHLY_END_SESSION),
                                    1,0))

df_clean$WeightChange <- as.double(df_clean$POUNDS_LOST) * -1                                   


df_clean$MONTHLY_END_VISIT_COMMENT <- as.factor(df_clean)


df_clean$WeightChangePerVisit <- df_clean$WeightChange / df_clean$NUM_VISITS

summary(df_clean)

attach(df_clean)
df_clean_v2 <- df_clean %>%
  select(-c(MONTHLY_END_SESSION,MONTHLY_INITIAL_SESSION))
summary(df_clean_v2)

#drop the 447 NAs since they seem to be throughout multiple columns

df_clean_tidy <- drop_na(df_clean_v2)
df_clean_tidy$Year <- year(df_clean_tidy$FirstVisitDate)
missings <- sum(is.na(df_clean_tidy))
print(paste("There are",missings,"missing data points after tidying up a bit"))


# more clean up; i dont like the column names in different casing
colnames(df_clean_tidy) <- tolower(colnames(df_clean_tidy))
colnames(df_subscribers) <- tolower(colnames(df_subscribers))

View(df_clean_tidy)
#Spread the data to a tidy format
df_spread_numerics <- dcast(df_clean_tidy, pid + membership  ~ month , 
                     value.var = c("num_visits","weightchangepervisit","monthly_initial_visit_weight"),
                     fun.aggregate = sum)
df_spread_numerics[duplicated(df_spread_numerics),]
 
View(df_spread_numerics)
summary(df_spread_numerics)
                                   # , "weightchange","weightchangepervisit",
                                   # "monthly_intial_visit_weight"))


##join the wide data set with the subscriber data set

df_combined <- left_join(df_subscribers,df_spread_numerics,by = "pid")

View(df_combined)

#Create the churn variable

df_combined$churn <- as.integer(ifelse(df_combined$`member status`%like% 'Cancel',1,0))

mean(df_combined$churn)



#There arent enough churns based on cancel only; 
#first creating a variable to count the number of NA or 0 visits per months and taking the max between the two. 
#0s and NULLs are independent


df_combined$months_0orno_visits <- apply(select(df_combined,starts_with("num_visits")),1, function(x) max(length(x[x==0]),
                                                                                                          length(x[x==0])))
                                      #+ length(x[x==0]))

#Now looking for the first months that 0 visits occurred
df_combined$first_month_0visits <- apply(select(df_combined,starts_with("num_visits"))



