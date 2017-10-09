OnlineNewsPopularity <- read.csv("C:/Users/Akshat/Desktop/Machine Learning/OnlineNewsPopularity.csv", header = T, na.strings=c("NA","NaN", " "))
summary(OnlineNewsPopularity)

str(OnlineNewsPopularity)

#1)Pre-Processing
#drop url as we dont need a string while we plot a model
drops <- c("url")
OnlineNewsPopularity=OnlineNewsPopularity[ , !(names(OnlineNewsPopularity) %in% drops)]


#Conversion of factor variables
#All the variables are integers and some of the variables containing '1' & '0' have to converted into factor variables
summary(OnlineNewsPopularity)
#data_channel_is_lifestyle	 data_channel_is_entertainment	 data_channel_is_bus	 data_channel_is_socmed	 data_channel_is_tech	 data_channel_is_world
#weekday_is_monday	 weekday_is_tuesday	 weekday_is_wednesday	 weekday_is_thursday	 weekday_is_friday	 weekday_is_saturday	 weekday_is_sunday	 is_weekend

OnlineNewsPopularity$data_channel_is_lifestyle = factor(OnlineNewsPopularity$data_channel_is_lifestyle)
OnlineNewsPopularity$data_channel_is_entertainment = factor(OnlineNewsPopularity$data_channel_is_entertainment)
OnlineNewsPopularity$data_channel_is_bus = factor(OnlineNewsPopularity$data_channel_is_bus)
OnlineNewsPopularity$data_channel_is_socmed = factor(OnlineNewsPopularity$data_channel_is_socmed)
OnlineNewsPopularity$data_channel_is_tech = factor(OnlineNewsPopularity$data_channel_is_tech)
OnlineNewsPopularity$data_channel_is_world = factor(OnlineNewsPopularity$data_channel_is_world)
OnlineNewsPopularity$weekday_is_monday = factor(OnlineNewsPopularity$weekday_is_monday)
OnlineNewsPopularity$weekday_is_tuesday = factor(OnlineNewsPopularity$weekday_is_tuesday)
OnlineNewsPopularity$weekday_is_wednesday = factor(OnlineNewsPopularity$weekday_is_wednesday)
OnlineNewsPopularity$weekday_is_thursday = factor(OnlineNewsPopularity$weekday_is_thursday)
OnlineNewsPopularity$weekday_is_friday = factor(OnlineNewsPopularity$weekday_is_friday)
OnlineNewsPopularity$weekday_is_saturday = factor(OnlineNewsPopularity$weekday_is_saturday)
OnlineNewsPopularity$weekday_is_sunday = factor(OnlineNewsPopularity$weekday_is_sunday)
OnlineNewsPopularity$is_weekend = factor(OnlineNewsPopularity$is_weekend)
str(OnlineNewsPopularity)
head(OnlineNewsPopularity)
summary(OnlineNewsPopularity)

#Plotting a model with all the variables
model1 =lm(shares ~ ., data =OnlineNewsPopularity )
summary(model1)
#The R-squared value i.e 0.02355 is very small using 59 attributes

set.seed(174007425)
#taking 75% for random sampling
idxTrain <- sample(nrow(OnlineNewsPopularity),as.integer(nrow(OnlineNewsPopularity)*0.75))
train.OnlineNewsPopularity = OnlineNewsPopularity[idxTrain,]
test.OnlineNewsPopularity= OnlineNewsPopularity[-idxTrain,]

model1_train = lm(shares ~ ., data =train.OnlineNewsPopularity )
pred1 = predict(model1_train,test.OnlineNewsPopularity[,-60])
accuracy1 = sum(diag(table(test.OnlineNewsPopularity$shares,pred1)))/nrow(test.OnlineNewsPopularity)
accuracy1

#The accuracy is 0

#plotting a model with the significant attributes where p-value is <0.05
# We consider timedelta, n_token_content, n_tokens_titlenum_self_hrefs,average_token_length ,data_channel_is_lifestyle ,num_hrefs,data_channel_is_entertainment ,data_channel_is_bus,
#kw_max_min,kw_min_max,kw_min_avg,kw_max_avg,kw_avg_avg,self_reference_min_shares,global_subjectivity

model2 = lm(shares ~ timedelta + n_tokens_content + n_tokens_title + num_self_hrefs+ average_token_length +data_channel_is_lifestyle +num_hrefs +   data_channel_is_entertainment +data_channel_is_bus+kw_max_min+kw_min_max+ kw_min_avg + kw_max_avg +kw_avg_avg + self_reference_min_shares +global_subjectivity , data = OnlineNewsPopularity )
summary(model2)

#the R-squared value is 0.02147 which has been acheieved by 16 attributes
#Thus there is not much of a significant difference in the valur of R-squared , and thus model2 fits the data better because of the less no of attributes that we have used for the model

set.seed(174007425)
idxTrain <- sample(nrow(OnlineNewsPopularity),as.integer(nrow(OnlineNewsPopularity)*0.75))
train.OnlineNewsPopularity = OnlineNewsPopularity[idxTrain,]
test.OnlineNewsPopularity= OnlineNewsPopularity[-idxTrain,]

model2_train = lm(shares ~ timedelta + n_tokens_content + n_tokens_title + num_self_hrefs+ average_token_length +data_channel_is_lifestyle +num_hrefs +   data_channel_is_entertainment +data_channel_is_bus+kw_max_min+kw_min_max+ kw_min_avg + kw_max_avg +kw_avg_avg + self_reference_min_shares +global_subjectivity , data = train.OnlineNewsPopularity )


pred = predict(model2,test.OnlineNewsPopularity[,-60])

accuracy2 = sum(diag(table(test.OnlineNewsPopularity$shares,pred)))/nrow(test.OnlineNewsPopularity)
accuracy2

#The accuracy is equal to 0.000201796

#the model with 16 attributes is a better model because the R-squared value is almost the same but by using just 16 attributes to predict the dependent variable and also there is a very slight increase in accuracy.