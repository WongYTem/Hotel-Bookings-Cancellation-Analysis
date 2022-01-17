#########################################################
# hotel: problem statement
#########################################################
library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application
library(rpart.plot)  # for plotting decision trees

# From now on, we will explore titanic data
# let's read the data
df <- read.csv('hotel_bookings.csv')
df$is_canceled<-factor(df$is_canceled)
df$reservation_status_date<-as.Date(df$reservation_status_date)
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

# cancellation by previous not cancelled
canrate_prevnot<-df %>% 
  group_by(previous_bookings_not_canceled) %>%
  summarize(bookings=n(),can_rate=sum(is_canceled==1)/n())

ggplot(canrate_prevnot, aes(x=previous_bookings_not_canceled, y=can_rate, group=1)) +
  geom_point() +
  geom_line()+
  geom_smooth()+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  theme(legend.key = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(colour = NULL)))+
  labs(title = "Cancellation Status by Previous bookings not canceled",
       x = "Previous bookings that are not canceled",
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

#by date
canrate_date<-df %>% 
  group_by(arrival_date_day_of_month) %>%
  summarize(bookings=n(),can_rate=sum(is_canceled==1)/n())

ggplot(canrate_date, aes(x=arrival_date_day_of_month, y=can_rate, group=1)) +
  geom_point() +
  geom_line()+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  theme(legend.key = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(colour = NULL)))+
  labs(title = "Cancellation Status by Day of the Month",
       x = "Day of the month",
       y = "Cancellation rate")
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

lm_le<- glm(is_canceled ~ lead_time, family = 'binomial', data=train)
summary(lm_le)

