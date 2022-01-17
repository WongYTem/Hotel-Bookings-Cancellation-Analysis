# hotel-booking-demand
this dataset is from https://www.kaggle.com/jessemostipak/hotel-booking-demand/code

The NN model from "NN Model for booking cancellation using Keras in Python.Py" for predicting cancellation is sucessful with almost 100% accuracy

![image](https://user-images.githubusercontent.com/97471111/149686770-2e470cb5-9d47-4bc6-8274-3be0dd346003.png)

##  Cancellation problem statment 
###    Almost 40% Bookings cancelled 
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
 
 Random Forest has the highest accuracy, so will interpret the prediction results of RF 
 '''
 
 '''

the most important variables are reservation_status_date, deposit_type and lead_time

