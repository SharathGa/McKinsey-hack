 # @Sharath_Gangalapadu

#packages to be used in the analyis . 

library( dplyr )
library( purrr)
library( gdata)
library(readr)
library(ggplot2)
install.packages('e1071')
library(e1071)
library(mosaic)

### Notes : The data has been cleaned prior to the analysis with imputation for missing values . 

#Setting up the working environment . 

setwd('/Users/sharathg/Desktop/mckinsey hackathon ')

#loading the training  and testing dataset  

df <- read_csv('train.csv')

test<- read_csv('test.csv')

#taking a look of the dataset 

glimpse( df)

glimpse(test)

#counting the number of missing values by column 
 
map( df , function(x) sum(is.na(x)))

#names of the columns in the dataframe .  

names( df )

#creating a factor vector , to be used later to convert the columns to factors( categorical variables )

factors <- c( 'residence_area_type','renewal','sourcing_channel')

#conversion to factors 

df[factors] <- map( df[factors], as.factor)

glimpse(df)

# removing the unused dataframes to free up the space . 

rm(dft)
rm(model_dft)


# plotting the boxplots for varibles to find the outliers . 

inco <- ggplot( df ,aes ( x = 1 , y = Income))+ geom_boxplot()+ scale_x_log10() + scale_y_log10()
inco

# number of possible outliers . 
nrow(df %>% filter ( Income > 1000000 )) 

range ( df$Income)


#summary statistics of the data . 

qw <- df %>% select ( id ,Income ,sourcing_channel, premium , residence_area_type, renewal) %>% 
  group_by(residence_area_type,sourcing_channel)%>%mutate( prcnt_income = (premium/Income)*100 )%>% 
  summarise( mean = mean(premium), median =median(premium),pi = mean(prcnt_income),pim=median(prcnt_income),avg_i = mean(Income),
             yearly_premium = mean(premium)*12, max= max(Income), min = min( Income))
qw

#percentage of id with income greater than 1 mil are : 
nrow(df)
(nrow(df %>% filter ( Income > 1000000 )) / nrow(df )) *100 

#this is very less ie 0.62 % , hence I decide to consider them as outliers and delete them from the data frame( 498 values) . 

# exploring other variables  
## age 

boxplot( df$age_in_years)

nrow(df %>% filter ( age_in_years > 96.5))

#premium 

boxplot( df$premium)

nrow(df %>% filter ( premium > 40000))

## defining the new data frame , after filtering for ages greater than 96.5 and income greater than 1 mil . 

df <- df %>% filter( Income < 1000000,age_in_years < 96.5) 
df

#saving the data into the csv file . 

write_csv(df , "trainMod.csv")

glimpse(df)



##########################################
###### Building the Svm models for the data . 

#Varibles to be included in the model .

Xvar <- c("perc_premium_paid_by_cash_credit","Income","Count_326_months_late" , 
          "Count_6212_months_late", "Count_more_than_12_months_late","application_underwriting_score" ,"no_of_premiums_paid",
          "sourcing_channel", "residence_area_type" ,"premium", "age_in_years" )


#formula of the svm regression .

formula5 <- as.formula(paste( 'renewal', "~", paste(Xvar, collapse = "+ ")))
formula5

# training the model . 

model5 <- svm( formula5, df, probability = TRUE  ) 

#predicitng the results 

predict(model5 , test , probability= T )


#######################
###### Logistic REgression on df . 

formula5 

## training a logistic model 
model5<-glm( formula5 , family = binomial( link = 'logit'),data = df )

#summary of the model . 

summary(model5)

#performing anova on model 

anova( model5, test = "Chisq")

#predicting the values on test data  . 

fitvalue5 <- predict(model5 ,newdata = test[, Xvar ], type ='response' )

# binding it to the test data and selecting the required columns . 

soln5 <- cbind(test ,fitvalue5)

## incentive formula 

soln5 <- soln5 %>% mutate (    incentives = case_when (
  residence_area_type=="Rural" & sourcing_channel=='A'~ 0.03 *( premium),
  residence_area_type=="Rural" & sourcing_channel=='B'~ 0.03 *( premium),
  residence_area_type=="Rural" & sourcing_channel=='C'~ 0.03 *( premium),
  residence_area_type=="Rural" & sourcing_channel=='D'~ 0.04 *( premium),
  residence_area_type=="Rural" & sourcing_channel=='E'~ 0.05 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='A'~ 0.03 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='B'~ 0.03 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='C'~ 0.03 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='D'~ 0.04 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='E'~ 0.05 *( premium)
) )


## mutating for columns wtihout the acutal increase del
soln5 <- soln5 %>% mutate( effort = 10*( 1- exp(-incentives/400)) , deltaP= 0.2*(1-exp(-effort/5)) ) %>% select(id, fitvalue5,incentives)
soln5
#writing the values into the solution file for submission . 

write_csv(soln5,'soln5.csv')

max(soln5$renewal)
min(soln5$renewal)

### Creating a new incentive model for the problem . 

# this time a flat 3% commision will be granted as incentives to the agents for their service 

soln6 <- cbind( test, fitvalue5)

soln6 <- soln6%>% mutate( incentives = 0.06 * premium,  renewal =  fitvalue5 ) %>% select( id , renewal, incentives)

write_csv(soln6 , 'soln6.csv')

######################
############### Applying cross validation on training and finding the AUC score . 

install.packages('ROCR')
library( ROCR )

############Final solution with some changes in the incentives for the agents . 

soln7<- cbind(test ,fitvalue5)

## incentive formula 

soln11 <- soln7 %>% mutate (    incentives = case_when (
  residence_area_type=="Rural" & sourcing_channel=='A'~ 0.03 *( premium),
  residence_area_type=="Rural" & sourcing_channel=='B'~ 0.03 *( premium),
  residence_area_type=="Rural" & sourcing_channel=='C'~ 0.03 *( premium),
  residence_area_type=="Rural" & sourcing_channel=='D'~ 0.03 *( premium),
  residence_area_type=="Rural" & sourcing_channel=='E'~ 0.08 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='A'~ 0.03 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='B'~ 0.03 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='C'~ 0.03 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='D'~ 0.07 *( premium),
  residence_area_type=="Urban" & sourcing_channel=='E'~ 0.08 *( premium)
) )


## mutating for columns wtihout the acutal increase del
soln11<- soln7 %>% mutate( effort = 10*( 1- exp(-incentives/400)) , deltaP= 0.2*(1-exp(-effort/5)) ) %>% select(id, fitvalue5,incentives)
soln7
#writing the values into the solution file for submission . 

write_csv(soln11,'soln11.csv')




