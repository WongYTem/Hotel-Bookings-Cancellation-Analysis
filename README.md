# hotel-booking-demand
this dataset is from https://www.kaggle.com/jessemostipak/hotel-booking-demand/code
## Neural Network model to predict weather a booking will be cancelled
The Neural Network model from [NN Model for booking cancellation using Keras in Python](https://github.com/WongYTem/hotel-booking-demand/blob/main/NN%20Model%20for%20booking%20cancellation%20using%20Keras%20in%20Python.py) for predicting cancellation has almost 100% accuracy (>0.999). 

![image](https://user-images.githubusercontent.com/97471111/149686770-2e470cb5-9d47-4bc6-8274-3be0dd346003.png)

##  Cancellation status 
 
```
# distribution of cancel
canrate_pie<-df %>% 
  group_by(is_canceled) %>%
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(canrate_pie, aes(x = "", y = perc, fill = is_canceled)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_bw()
```
![image](https://user-images.githubusercontent.com/97471111/149841280-149a826e-a978-4359-b1b1-e6b69eae0d99.png)

There 40% of the bookings are cancelled in the dataset, and high cancellation rate could mean low turnover rate. Therefore, we need to find out the variables that is likely cause cancellation from customers.

###   The number of previous cancellations are positively related to the cancellation rate
```
canrate_prev<-df %>% 
  group_by(previous_cancellations) %>%
  summarize(bookings=n(),can_rate=sum(is_canceled==1)/n())

ggplot(canrate_prev, aes(x=previous_cancellations, y=can_rate, group=1)) +
  geom_point() +
  geom_line()+
  geom_smooth()+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  theme(legend.key = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(colour = NULL)))+
  labs(title = "Cancellation Status by the # of previous bookings that are canceled",
       x = "Previous bookings canceled",
       y = "Cancellation rate")
```
![image](https://user-images.githubusercontent.com/97471111/149841712-ecddb59d-b6da-4368-8e0c-ae29c177a9f5.png)
In this line graph, there is a positive correlation between cancellation rate and the # of previous cancellation
![image](https://user-images.githubusercontent.com/97471111/150013585-554dc80c-4bfa-4e71-b379-8c0a9214d449.png)


###  April to October are peak seasons for cancellations
```
canrate_month<-df %>% 
  group_by(arrival_date_month) %>%
  summarize(bookings=n(),can_rate=sum(is_canceled==1)/n())%>%
  arrange(match(arrival_date_month, month.name))

canrate_month$arrival_date_month = factor(canrate_month$arrival_date_month, levels = month.name)

ggplot(canrate_month, aes(x=arrival_date_month, y=can_rate, group=1)) +
  geom_point() +
  geom_line()+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  theme(legend.key = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(colour = NULL)))+
  labs(title = "Cancellation rate by Month",
       x = "Month",
       y = "Cancellation rate")
```
![image](https://user-images.githubusercontent.com/97471111/149841809-32a33cd9-24f4-47f9-99fd-86b664275479.png)

## Accuracy of prediction models on cancellation
 Decision tree with pruning: 0.9345
 Random forest: 0.9421
 Gradient boosting: 0.7796
 
 Random Forest has the highest accuracy, so I will interpret the prediction results of RF 
 '''
 rf_tree <- randomForest(is_canceled~.-country-agent-company, data = train, ntree = 150)
pred_rf <- predict(rf_tree, test)
confusionMatrix(data = as.factor(pred_rf), 
                reference = as.factor(test$is_canceled))
importance(rf_tree)
varImpPlot(rf_tree)
 '''
 ![image](https://user-images.githubusercontent.com/97471111/150013951-438245e8-4dff-4858-8d86-11c02b1f2c58.png)

By plotting the variance importance of the RF model, we can see the mean decrease in Gini coefficient of the variables, and the 3 most important variables that affect the cancelltion rate is reservation staus date, deposit type and the lead time.

###   Reservation status date on cancellation prediction
'''
canrate_res<-df %>% 
  group_by(reservation_status_date) %>%
  summarize(bookings=n(), can_rate=sum(is_canceled==1)/n())%>%
  arrange(-can_rate,-bookings)
'''
![image](https://user-images.githubusercontent.com/97471111/150014161-5a585a29-6c90-4497-bd4d-7cb6629276ac.png)

For the reservation status date, we group the bookings by date and sort them by cancellation rate. We found that there are many dates that have 100% cancellation, which indicates these cancellations does not happen on individual customers. 

![image](https://user-images.githubusercontent.com/97471111/150014248-a99c7f23-84fe-48ff-80ea-9f0674904cb5.png)
And by running an logistic regression on the cancellation rate grouped by dates, we can see market segment in groups and complementary is most statically significant and have the highest estimate


###   Deposit type and lead time on cancellation prediction
'''
#dt for deposit & lead time
df$is_leadtime1week <-factor(ifelse(df$lead_time<=10,0, 1))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
# get training and testing data
train <- df[train_ind, ]
test <- df[-train_ind, ]
train <- train  %>% dplyr::select(-reservation_status)
test <- test  %>% dplyr::select(-reservation_status)

dt_dele <- rpart(is_canceled~ deposit_type+is_leadtime1week, data = train, control = list(cp = 0))
rpart.plot(dt_dele)
'''
![image](https://user-images.githubusercontent.com/97471111/150015840-fa4b1c62-0da9-4ca8-9a7c-7adb6946609d.png)


As for the other 2 important variable in RF, we run a decision tree of them for the cancellation. The result shows When deposit is refundable or there is no deposit required, the cancellation prediction is cancelled. But when deposit is not refundable, the prediction is not cancelled

![image](https://user-images.githubusercontent.com/97471111/150015900-d1245d0d-5102-4c8c-a08b-a20e3ff170a8.png)

A unit increase in lead time(a day) will increase the log odds of cancellation by 0.005854, while holding other variables unchanged
⇒ lower the lead time, lower chances of cancellation
