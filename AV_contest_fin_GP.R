install.packages('randomForest')
install.packages('caret')
install.packages("vars") #If not already installed
install.packages("astsa") #If not already installed
install.packages('sqldf')
library(vars)
library(astsa)
library('randomForest')
library('caret')
library('sqldf')
library('forecast')
install.packages('forecast')

train_data=read.csv('C:\\Users\\Rajesh\\Documents\\AV_contest\\train_GzS76OK\\train.csv')
test_data=read.csv('C:\\Users\\Rajesh\\Documents\\AV_contest\\test_QoiMO9B.csv')

final_predicted_data=data.frame()

for(i in 1:length(center_id$center_id))
{
  cid<-center_id$center_id[i]
  print(cid)
  temp_cid=train_data[train_data$center_id==cid,]
  temp_meal_id=dplyr::distinct(temp_cid,temp_cid$meal_id)
  #print(nrow(temp_meal_id))
  for(x in 1:nrow(temp_meal_id))
  {
    #train modudle
    print('inside meal ID loop')
    #print(temp_meal_id$`temp_cid$meal_id`[x])
    temp_train_data=train_data[train_data$center_id==cid&train_data$meal_id==temp_meal_id$`temp_cid$meal_id`[x], ]
    #print('For cid and mealid:%d,%d',cid,temp_meal_id$`temp_cid$meal_id`[x])
    #temp_train_data_fil<-temp_train_data[,c('checkout_price','base_price','emailer_for_promotion','thomepage_featured','num_orders')]
    #temp_train_data$homepage_featured
    
    temp_train_data_fil=sqldf("select checkout_price,base_price,emailer_for_promotion,homepage_featured,num_orders from temp_train_data")
    print(head(temp_train_data_fil))
    model=randomForest(temp_train_data_fil$num_orders~.,data = temp_train_data_fil,ntree=300)
    #fit_control=trainControl(method = 'repeatedcv',number=2,repeats=2,search = 'grid')
    #model<-train(num_orders~.,data = df_1,methods="gbm",trControl = fit_control,verbose = FALSE)
    print(summary(model))
    #print(head(temp_train_data))
    
    
    temp_test_data=test_data[test_data$center_id==cid&test_data$meal_id==temp_meal_id$`temp_cid$meal_id`[x],]
    temp_test_data_fil=sqldf("select checkout_price,base_price,emailer_for_promotion,homepage_featured from temp_test_data")
    predicted_value=predict(model,temp_test_data_fil)
    temp_test_data_fin=cbind(temp_test_data,predicted_value)
    final_predicted_data=rbind(final_predicted_data,temp_test_data_fin)
  }
  #print(sqldf("select count(*) from train_data where center_id='%s'",10))
  
}