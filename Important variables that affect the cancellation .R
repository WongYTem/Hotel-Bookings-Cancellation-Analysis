#########################################################
# hotel: problem statement
#########################################################
library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application
library(rpart.plot)  # for plotting decision trees

library(nnet)
library(caret)       # meta engine for decision tree application

library(ipred)       # bagging model
library(randomForest) # random forest
library(gbm)         # gradient boost
library(Metrics)     # for performance metrics

# let's read the data
df <- read.csv('hotel_bookings.csv')
df[sapply(df, is.character)] <-
  lapply(df[sapply(df, is.character)], as.factor)

df$reservation_status_date<-as.Date(df$reservation_status_date)
#replace null values
summary(df)
n <- length(df$children)
for (i in 1:n) {
  if (is.na(df$children[i]))
    df$children[i] <- df$babies[i]
}
# let's check distribution of cancel
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
# cancellation by previous cancellation
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
#by month
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
  labs(title = "Cancellation Status by Month",
       x = "Month",
       y = "Cancellation rate")

# let's do prediction. let's first split the data
smp_size <- floor(0.8 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
# get training and testing data
train <- df[train_ind, ]
test <- df[-train_ind, ]
train <- train  %>% dplyr::select(-reservation_status)
test <- test  %>% dplyr::select(-reservation_status)
#reservation status is dropped because its not a variable affecting the cancellation
#instead, cancellation is affecting reservation status
###########
# decision tree with prunning
###########
dt <- rpart(is_canceled~., data = train, control = list(cp = 0))
pdt <- prune(dt, cp = dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"])
pdt$variable.importance
# get predicted values
predict_pdt <- predict(pdt, test, type = 'class')
confusionMatrix(data = as.factor(predict_pdt), 
                reference = as.factor(test$is_canceled))
###########
##gradient boost
###########
set.seed(123)
train$is_canceled<- as.character(train$is_canceled)
dt_gb <- gbm(is_canceled~.-reservation_status_date, data = train, distribution = 'bernoulli')
# make predictions and evaluate performance based on testing data
predict_gb <- predict(dt_gb, test)
predict_gbclass <- ifelse(predict_gb > 0.5, 1, 0)
confusionMatrix(data = as.factor(predict_gbclass), 
                reference = as.factor(test$is_canceled))
summary.gbm(dt_gb)
###########
## random forest
###########
#agent, country, company is dropped bc they rf package only allows variables with no more than 53 class
rf_tree <- randomForest(is_canceled~.-country-agent-company, data = train, ntree = 150)
pred_rf <- predict(rf_tree, test)
confusionMatrix(data = as.factor(pred_rf), 
                reference = as.factor(test$is_canceled))
importance(rf_tree)
varImpPlot(rf_tree)
###########
##random forest gives the highest accuracy! So will interpret the prediction results of RF 
# the most important variables are reservation_status_date, deposit_type and lead_time
###########
##reservation status date
canrate_res<-df %>% 
  group_by(reservation_status_date) %>%
  summarize(bookings=n(), can_rate=sum(is_canceled==1)/n())%>%
  arrange(-can_rate,-bookings)
#why can_rate of some dates are 100%
canrate_res<-df %>% 
  group_by(reservation_status_date) %>%
  summarize(bookings=n(),market_seg=market_segment, can_rate=sum(is_canceled==1)/n())%>%
  arrange(-can_rate,-bookings)
lm_res<- glm(can_rate ~ .-reservation_status_date-bookings, data=canrate_res)
summary(lm_res)
#groups is possibly the most important factor for 100% cancel rate for some date
ggplot(data = canrate_res,
       aes(
         x = market_seg,
         y = can_rate)) +
  geom_bar(stat="identity", position = position_dodge())  +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Cancellations by Market Segment",
       x = "Market segment",
       y = "Cancellation rate") +
  theme_bw()

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
# logistic regression for lead time
lm_le<- glm(is_canceled ~ lead_time, family = 'binomial', data=train)
summary(lm_le)

