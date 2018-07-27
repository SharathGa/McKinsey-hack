# auth- @Sharath_Gangalapadu

library(mosaic)
install.packages('dplyr')
library( dplyr )
library( purrr)
library( gdata)
library(readr)

#installing Amelia package to visualise the missing values . 
install.packages('Amelia')
library(Amelia)

#loading the data into the dframe .
dft <- read_csv('/Users/sharathg/Desktop/mckinsey hackathon /train_ZoGVYWq.csv' )
glimpse( train)

head( dft)
names ( dft )

#finding the unique values in the sourcing channel 
unique( dft$sourcing_channel)
unique( dft['sourcing_channel'])

#names of the columns in df 
n <- names( dft )
n
#unique values in the df 
unique_values <- map ( n , ~ unique(dft[.]))
unique_values

#factor varibles in the dataframe .
factors <- c( 'residence_area_type','renewal','sourcing_channel')
dft[factors]
#conversion to factors 
   dft[factors] <- map( dft[factors], as.factor)
glimpse( dft)

#Converting the age of customers from days to years . 
dft <- dft%>%mutate(age_in_years = round((age_in_days /365.25),3))  %>%select( - age_in_days)

head ( dft$age_in_years ,10 ) 

# The names of the columns are recoded so that they do not cause errors later in the analysis 

dft<- dft %>% rename(  Count_326_months_late = 'Count_3-6_months_late',
                      Count_6212_months_late= 'Count_6-12_months_late' )  

#counting the number of missing values by column 

missval <- map(dft , function(x) sum(is.na(x)))
missval

#total obseravtions in df 
nrow( dft)
dft <- dfd 
head( dft)
names( dfd)

## filling up the NA's in count_326_months , count_6212_months , count_more_than_12_months'

dft$Count_6212_months_late[is.na(dft$Count_6212_months_late)] <- median(dft$Count_6212_months_late,na.rm = T) 

dft$Count_more_than_12_months_late[is.na(dft$Count_more_than_12_months_late)] <- median(dft$Count_more_than_12_months_late,na.rm = T)

dft$Count_326_months_late[is.na(dft$Count_326_months_late)] <- median(dft$Count_326_months_late,na.rm = T) 

#### there are 2974 missing values in underwriting score out of which 200 have not been renewed .So the imputing values here will be 
### be based accordingly . 
#median of the ids that renewed and those did not . 
dft%>%filter( renewal == 0) %>%summarize( score = median(application_underwriting_score,na.rm = TRUE ))      
dft%>%filter( renewal == 1) %>%summarize( score = median(application_underwriting_score,na.rm = TRUE ))      

##accordingly the values have been imputed to the missing values , so that they correspond with not renewed and renewed . 

nrow(dft) 
dfp <- dft %>% filter( is.na( application_underwriting_score)) %>% mutate (application_underwriting_score= ifelse(renewal==1,99.2,99.0))
#dft$application_underwriting_score[is.na(dft$application_underwriting_score)] <-ifelse(renewal==1,99.2,99.0) 

dfh <- dft[!is.na(dft$application_underwriting_score),]
nrow( dfh) +nrow( dfp)
dft <- bind_rows( dfh ,dfp  )
nrow(dft)
#checking the missing values in the train dataset 

valCheck <- map(dft , function(x) sum(is.na(x)))
valCheck                                              

#Clean dataset ready for analysis . 
# We have a perfect train with proper treatment that need to be stored .
write_csv( dft , "/Users/sharathg/Desktop/mckinsey hackathon /train.csv")
  
 



glimpse( dft )
### BASIC EDA OF THE DATASET TO FIND SOME PATTERNS . 
#summary of policies that were renewed and non renewed .
#we can observe an imbalance here in the data set . 
?glm
table( dft$renewal)
n <- names ( dft ) 
n

#Incentive model EDA . 
n
dfi <- dft %>% select ( id ,Income ,sourcing_channel, premium , residence_area_type, renewal) %>% 
               group_by(residence_area_type,sourcing_channel)%>%mutate( prcnt_income = (premium/Income)*100 )%>% 
     summarise( mean = mean(premium), median =median(premium),pi = mean(prcnt_income),pim=median(prcnt_income),avg_i = mean(Income),
                     yearly_premium = mean(premium)*12)
              

dfi 
library ( dplyr )
#calculating the percent of income , the monthly premium is . 
yt <- dft %>% select( age_in_years, Income , premium, renewal) %>% mutate( prcnt_income = (premium/Income)*100 ) 



#Summary of premium to percent income . 
yt%>%group_by(renewal)%>% summarise(max_pr_inc =max( prcnt_income), 
                          min_pr_inc= min(prcnt_income) ,median =median(prcnt_income), mean = mean(prcnt_income))


senior_citizens <- dft %>% filter ( age_in_years >=80 ) %>%summarise( total = n())
senior_citizens

#filtering for the data under 90 ratings that have been insured in the process . 
dft %>% filter ( application_underwriting_score <= 90 ) %>% summarise( total = n( ))




# There are no ids under 90 that have been insured , but we can try for finding the NA's that have been issued the insurance 
n                   
# the df that has NA's 
NA_df <- dft %>% filter ( is.na(application_underwriting_score)  & renewal ==1 ) 

head(NA_df)
setequal( NA_df$id, NA_df_renew$id)
#replacing the NA's with median application scores that were renewed . 
NA_df$application_underwriting_score 
nrow( NA_df_renew)
NA_df_renew <- NA_df %>% filter( renewal == 1 )

NA_df$application_underwriting_score <- median (dft$application_underwriting_score, na.rm =  T )
head( NA_df)
nrow( dft)
#deleting certain rows from the main dft so that these could be replaced by the inputed values . 
nrow( NA_df)

sum( is.na( dft$application_underwriting_score))
nrow(as.data.frame( NA_df$id)) 
NA_df$id 
dfg <- dft[-c(), ]
names(dfg)
nrow(dfg)
nrow(dft)
length( unique( NA_df$id))
nrow ( dft) - nrow( dfg)
nrow( dft) - 2774
nrow(dfg)+nrow(NA_df)  
  
nrow(dft)





NA_df_renew <- NA_df %>% filter( renewal == 1 ) %>% summarise( total = n())
# Out of the 2994 missing values of application_underwriting score , 2774 have renewed meaning they had beed issued 
# insurance and hence they are having a score greater than 90 . 

# percentage of missing values that can be afforded is : 
q <- (291 / nrow( dft )) * 100
q
# 0.36 %  is a very small value and hence we can directly discard these values and perform artificial imputation on the 
# data that is in application_underwritingscore  ( just replacing the missing values with the mean of the distribution .
. 
NAS_holders <- NA_df %>% filter ( renewal == 1 )

?unique()
l<- mean( unique(dft$application_underwriting_score , na.rm = T ))
l





##################### Without any change to data , the imputation that has been skipped lets apply
###################### logistic regression to the problem . 
glimpse( dft)
model_dft <- dft %>% select(-id )
names(model_dft)
model <- glm( formula , family = binomial( link = 'logit'),data = model_dft )
summary( model)
names(dfg)
Xvar <- c("perc_premium_paid_by_cash_credit","Income","Count_326_months_late" , 
"Count_6212_months_late", "Count_more_than_12_months_late","application_underwriting_score" ,"no_of_premiums_paid",
"sourcing_channel", "residence_area_type" ,"premium", "age_in_years" )

formula<- as.formula(paste('renewal',"~",paste(Xvar,collapse="+"))) 
formula
anova( model , test = "Chisq")

test <- read_csv("/Users/sharathg/Desktop/mckinsey hackathon /test_66516Ee.csv")
factorsT <- c( 'residence_area_type','sourcing_channel')
test[factorsT]
test <-test%>%mutate(age_in_years = round((age_in_days /365.25),3))  %>%select( - age_in_days)
test<- test %>% rename(  Count_326_months_late = 'Count_3-6_months_late',
                      Count_6212_months_late= 'Count_6-12_months_late' )  
#test <- na.omit( test)
#sum(is.na( sample_solution))
#conversion to factors 
dim(test)
klo<-
  test[factorsT] <- map( test[factorsT], as.factor)
renewal <- predict(model ,newdata = test[, Xvar ], type ='response' ) 
sum(is.na( renewal))
unique(test$sourcing_channel)
head( test )
sample_solution <- cbind( test , renewal)
head(sample_solution)
glimpse( sample_solution)
nrow( test )
sample_solution1<- sample_solution %>% mutate( incentives = 0.06* (premium )*12 )  %>%
                     select( id , renewal , incentives)
names( sample_solution )


sample_solution2<- sample_solution %>% mutate( incentives = 0.06* (premium ) )  %>%
                     select( id , renewal , incentives)
write_csv(sample_solution, "/Users/sharathg/Desktop/mckinsey hackathon /soln1.csv")
write_csv(sample_solution2, "/Users/sharathg/Desktop/mckinsey hackathon /soln2.csv")

#### cleaning the testing file . 

r <- map(test, function(x) sum(is.na(x)))
r
test$Count_326_months_late[is.na(test$Count_326_months_late)] <- median(test$Count_326_months_late,na.rm = T) 

sum(is.na( test$Count_326_months_late))

test$Count_6212_months_late[is.na(test$Count_6212_months_late)] <- median(test$Count_6212_months_late,na.rm = T) 

test$Count_more_than_12_months_late[is.na(test$Count_more_than_12_months_late)] <- median(test$Count_more_than_12_months_late,na.rm = T)

test$application_underwriting_score[is.na(test$application_underwriting_score)] <- median(test$application_underwriting_score,na.rm = T) 

w<- map(test, function(x) sum(is.na(x)))
w

write_csv(test ,  "/Users/sharathg/Desktop/mckinsey hackathon /test.csv")

getwd()

######## Analysis 3 , new model for solution 3 : 
model_dft <- dft %>% select(-id )
names(model_dft)
sum(is.na(dft))

names(dfg)
Xvar <- c("perc_premium_paid_by_cash_credit","Income","Count_326_months_late" , 
"Count_6212_months_late", "Count_more_than_12_months_late","application_underwriting_score" ,"no_of_premiums_paid",
"sourcing_channel", "residence_area_type" ,"premium", "age_in_years" )
formula<- as.formula(paste('renewal',"~",paste(Xvar,collapse="+"))) 
formula

model3 <- glm( formula , family = binomial( link = 'logit'),data = model_dft )

summary( model)
anova(model3 , test = "Chisq")
xVarMod <- c("perc_premium_paid_by_cash_credit",'Count_326_months_late','Count_6212_months_late',
             'Count_more_than_12_months_late','application_underwriting_score','no_of_premiums_paid',
             'sourcing_channel','age_in_years','Income')

formula3i<- as.formula(paste('renewal',"~",paste(xVarMod,collapse="+")))

model3i<-glm( formula3i , family = binomial( link = 'logit'),data = model_dft )
summary(model3i)
anova( model3i , test = "Chisq")
renewal <- predict(model ,newdata = test[, Xvar ], type ='response' ) 
fitted_values4<- predict(model3 , newdata =test[,Xvar], type= 'response' )
fitted_value3i <- predict ( model3i, newdata =test[,xVarMod], type= 'response' )
length(fitted_value3i) 
nrow( test)
test <- read_csv('/Users/sharathg/Desktop/mckinsey hackathon /test.csv')
dft <- read_csv('/Users/sharathg/Desktop/mckinsey hackathon /train.csv')
soln3 <- cbind( test, fitted_value3i)
soln4<- cbind( test, fitted_values4)
##incentive  
unique( dft$residence_area_type)
names (dft)
s
head(soln3)
effort =  10 *(1- exp(incentives/400)) ,  deltaP = 20*(1-exp(-effort/5)),
renewal = deltaP*(fitted_value3i) + fitted_value3i

soln3 <- soln3 %>% mutate(  incentives = 0.06*(premium ) ,renewal = fitted_value3i  )   %>%select(id,renewal,incentives)
  
soln4<- soln3 %>% mutate( renewal = fitted_value3i)   %>% select(id,renewal,incentives)                                  
                              
write_csv(soln4, '/Users/sharathg/Desktop/mckinsey hackathon /soln4.csv')  
glimpse(soln3)
unique( soln3$renewal) 
write_csv(soln3, '/Users/sharathg/Desktop/mckinsey hackathon /soln3.csv')                                                 
                                   

