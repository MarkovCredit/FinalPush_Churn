library(XML)
library(Hmisc)
library(dplyr)
library(data.table)

data <- read.csv('df_combined_012619.csv',stringsAsFactors = FALSE)
data_RevisedPID <- read.csv('df_revised_PID.csv',stringsAsFactors = FALSE)
data_with_MF <- read.csv('subscribers data set clean.csv',stringsAsFactors = FALSE)
data_long <- read.csv('subscriber_weightlossdata.csv',stringsAsFactors = FALSE)
$pid
names(data_with_MF)[names(data_with_MF)=="pid"] <- "pid2"
data_long_2 <- select(data_long,PID,GENDER,PATIENT_SINCE_DATE,TOTAL_SPENT,MONTHLY_INITIAL_VISIT_DATE,
                      POUNDS_LOST, NUM_VISITS)
colnames(data_long_2) <- tolower(colnames(data_long_2))


names(data_long_2)[names(data_long_2)=="pid"] <- "pid2"

data_long_2_bothPIDs <- left_join(data_long_2,data_RevisedPID[,c('pid','pid2')], by = 'pid2')

data_long_2_bothPIDs$num_visits <- as.integer(data_long_2_bothPIDs$num_visits)
data_long_2_bothPIDs$patient_since_date <- as.Date(data_long_2_bothPIDs$patient_since_date,'%m/%d/%Y')
data_long_2_bothPIDs$monthly_initial_visit_date <- as.Date(data_long_2_bothPIDs$monthly_initial_visit_date,'%m/%d/%Y')
data_long_2_bothPIDs$monthsonbook <- round(as.numeric(data_long_2_bothPIDs$monthly_initial_visit_date - 
                                                  data_long_2_bothPIDs$patient_since_date)/30,0)

hist(data_long_2_bothPIDs$monthsonbook)
data_long_2_bothPIDs$pounds_lost <- as.integer(data_long_2_bothPIDs$pounds_lost)
data_spread <- data.table::dcast(setDT(data_long_2_bothPIDs),pid ~ monthsonbook, 
                                 value.var = c('pounds_lost','total_spent','num_visits'))

View(data_with_MF$Gender)


describe(data_with_MF$Gender)

colnames(data_with_MF) <- tolower(colnames(data_with_MF))

data_with_gender <- left_join(data_with_MF,data_RevisedPID[,c('pid','pid2')], by = "pid2")

data_with_gender_startdate <- dplyr::filter(data_with_MF, Gender == 'F')


data_with_gender_2 <- data_with_gender[!duplicated(data_with_gender$pid2),]

write.csv(data_with_gender_2,'data_with_gender_2.csv')





View(data)

data$visitsuntil_weightlost <- apply(select(data,starts_with("weightchange")),
                                     1,function(x) which(x < 0 )[1]) - data$first_month_visited + 1
data$visitsuntil_weightlost[is.na(data$visitsuntil_weightlost)] <- -1
summary(data$visitsuntil_weightlost)
data$visitsuntil_weightlost_factor <- ifelse(data$visitsuntil_weightlost <0, 'Didnt Lose Weight',
                                             ifelse(data$visitsuntil_weightlost <= 1,'1',
                                                    ifelse(data$visitsuntil_weightlost <= 3,'2 to 3',
                                                    ifelse(data$visitsuntil_weightlost <= 5,'4 to 5',
                                             '5+'))))
data$churn_v2 <- ifelse(data$churn_v2 == 0,'NoChurn','Churn')
data$churn_v2  <- as.factor(data$churn_v2)

data$churn_v2 <- relevel(data$churn_v2,"NoChurn","Churn")
data_2 <- left_join(data,data_with_gender_2[,c('pid2','pid','gender','patient.since')], by ='pid')
data_2$gender <- as.factor(data_2$gender)
bar_avg_weight_change_3 <- ggplot(data=data_2, 
                                aes(x=visitsuntil_weightlost_factor, fill = churn_v2)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position = 'dodge',size = 3,
            check_overlap = TRUE)+
  labs(x = 'Mth Weight Lost',title = 'First Month Weight Lost v Churn')+
  facet_wrap(~gender)+
  theme(text = element_text(size=8))

bar_avg_weight_change_3
  
data_2$gender <- tolower(data_2$gender)
data_2$gender <- as.factor(data_2$gender)

data_2$gender <- ifelse(data_2$gender %like% 'm','Male','Female')
data_2$gender <- as.factor(data_2$gender)
summary(data_2$gender)



write.csv(data_2,'datawithgenderbothpids.csv')




