library(tidyverse)
setwd("C:/Users/Stefa/Documents/Uni/Projektassistenz/Paper II SHORT TERM REVERSAL/")

# Load news_events 
news_events_no_dupl <- read_csv("./Data/news_event_df_no_dupl_minimal.csv")
  
# Load z_scores 
z_scores <- read_csv('./Data/z_score_event_df.csv')
z_scores_merged <- read_csv('./Data/z_score_event_df_sentiment.csv')

alldates <- as_tibble(sort(unique(z_scores$Date)))
alldates <- cbind(alldates,1:nrow(alldates))
names(alldates) <- c("Date","Date_index")

temp <- z_scores %>%
        left_join(alldates, by="Date") %>% 
        rename(z_score_tp1 = z_score)  %>% 
        select(Ticker, Date_index, z_score_tp1)


dataset <- z_scores %>% 
           left_join(alldates, by="Date")            %>% 
           mutate(Date_index_merge = Date_index + 1) %>% 
           rename(return_mc_to_mo_tp1 = return_mc_to_mo, return_mo_to_mc_tp1 = return_mo_to_mc) %>%
           left_join(temp, by=c("Date_index_merge"="Date_index", "Ticker")) %>%
           left_join((news_events_no_dupl %>% 
                        left_join(alldates, by="Date") %>% 
                        select(-c("Date","prev_close_date"))), 
                     by=c("Date_index_merge"="Date_index", "Ticker")) %>% 
           select(-c("Date_index","Date_index_merge"))



mod1 = lm(z_score_tp1 ~ z_score,data=dataset)
mod2 = lm(z_score_tp1 ~ z_score + I(z_score^2),data=dataset)
modTest = lm( return_mc_to_mo_tp1 ~ sentiment, data=dataset)

mod1a = lm(z_score_tp1 ~ z_score, data=(dataset %>% filter(is.na(sentiment))))
mod1b = lm(z_score_tp1 ~ z_score, data=(dataset %>% filter(!is.na(sentiment))))

mod2a = lm(z_score_tp1 ~ z_score  + I(z_score^2),data=(dataset %>% filter(is.na(sentiment))))
mod2b = lm(z_score_tp1 ~ z_score  + I(z_score^2),data=(dataset %>% filter(!is.na(sentiment))))

sCrit <- 0.45
zCrit <- 2
regset <- dataset %>% filter(!is.na(sentiment) & !is.na(z_score)) %>%
  mutate(si = if_else(sentiment>=sCrit,1,if_else(sentiment<=-sCrit,-1,0))) %>%
  mutate(strngSent = abs(si)) %>% 
  mutate(zi = if_else(z_score>=zCrit,1,if_else(z_score<=-zCrit,-1,0)))
modB0 <- lm(z_score_tp1 ~ z_score * sentiment,data=regset)
modB0a <- lm(z_score_tp1 ~ z_score * I(abs(sentiment)),data=regset)
modB0b <- lm(z_score_tp1 ~ z_score * sentiment + z_score * I(abs(sentiment)),data=regset)
modB1 <- lm(z_score_tp1 ~ z_score * si,data=regset)
modB2 <- lm(z_score_tp1 ~ zi * si,data=regset)
modB3 <- lm(z_score_tp1 ~ zi * strngSent,data=regset)
modB3 <- lm(z_score_tp1 ~ zi*si + zi * strngSent,data=regset)

print(summary(modB3))


#mod1a = lm(return_mo_to_mc  ~ return_mo_to_mc_tm1 ,data=dataset)
#mod1b = lm(return_mo_to_mc  ~ z_score_tm1,data=dataset)
#mod2 = lm(z_score ~ z_score_tm1,data=(dataset %>% filter(is.na(sentiment))))
#mod2a = lm(return_mo_to_mc  ~ return_mo_to_mc_tm1 ,data=(dataset %>% filter(is.na(sentiment))))


