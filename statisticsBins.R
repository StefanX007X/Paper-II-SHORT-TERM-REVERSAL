library(tidyverse)
#require(PMCMR)
require(PMCMRplus)
#library(Hmisc)

setwd("C:/Users/Stefa/Documents/Uni/Projektassistenz/Paper II SHORT TERM REVERSAL/")

# Load news_events 
news_events_no_dupl <- read_csv("./Data/news_event_df_no_dupl.csv")

news_events_daytime <- read_csv("./Data/news_event_df_daytime_minimal.csv")

# Load z_scores 
z_scores <- read_csv(unz("./Data/z_score_event_df.zip", 'z_score_event_df.csv'))
#z_scores_old <- read_csv(unz("z_scores.old.zip",'z_scores.csv'))

#z_scores_merged <- read_csv(unz("z_scores_merged.zip","z_scores_merged.csv"))

alldates <- as_tibble(sort(unique(z_scores$Date)))
alldates <- cbind(alldates,1:nrow(alldates))
names(alldates) <- c("Date","Date_index")

temp <- z_scores %>%
  left_join(alldates, by="Date") %>% rename(z_score_tp1 = z_score) %>% select(Ticker, Date_index, z_score_tp1)


dataset <- z_scores %>% left_join(alldates, by="Date") %>% mutate(Date_index_merge = Date_index + 1) %>% rename(return_mc_to_mo_tp1 = return_mc_to_mo, return_mo_to_mc_tp1 = return_mo_to_mc) %>%
  left_join(temp,by=c("Date_index_merge"="Date_index","Ticker")) %>%
  left_join((news_events_no_dupl %>% left_join(alldates,by="Date") %>% select(-c("Date","prev_close_date"))),by=c("Date_index_merge"="Date_index","Ticker")) %>% select(-c("Date_index","Date_index_merge")) %>%
  left_join(news_events_daytime %>% rename(sentimentDaytime = Sentiment), by=c("Ticker","Date"))



sentBins <- c(-Inf,-0.45,0.45,Inf)
zBins <- c(-Inf,-2.0,0.0,2.0,Inf)

dataset2 <- dataset %>% mutate(sent = if_else(!is.na(Sentiment), Sentiment,0)) %>%
  mutate(siWithNews = as.numeric(cut(Sentiment,breaks=sentBins))) %>%
  mutate(si = as.numeric(cut(sent,breaks=sentBins))) %>%
  mutate(zi = as.numeric(cut(z_score,breaks=zBins)))
  

##
# negative z, news versus no news
testset <- dataset2 %>% filter(z_score<=0.0) %>% mutate(withSent = !is.na(Sentiment))
dim(testset)
sum(testset$withSent)
print(t.test(testset$z_score_tp1[testset$withSent],testset$z_score_tp1[!testset$withSent]))
print(wilcox.test(testset$z_score_tp1[testset$withSent],testset$z_score_tp1[!testset$withSent],paired=FALSE))

testset <- dataset2 %>% filter(z_score<=0.0) %>% mutate(withSent = !si==2)
dim(testset)
sum(testset$withSent)
print(t.test(testset$z_score_tp1[testset$withSent],testset$z_score_tp1[!testset$withSent]))
print(wilcox.test(testset$z_score_tp1[testset$withSent],testset$z_score_tp1[!testset$withSent],paired=FALSE))

# positive z, news versus no news
testset <- dataset2 %>% filter(z_score>0.0) %>% mutate(withSent = !is.na(sentiment))
dim(testset)
sum(testset$withSent)
print(t.test(testset$z_score_tp1[testset$withSent],testset$z_score_tp1[!testset$withSent]))
print(wilcox.test(testset$z_score_tp1[testset$withSent],testset$z_score_tp1[!testset$withSent],paired=FALSE))

testset <- dataset2 %>% filter(z_score>0.0) %>% mutate(withSent = !si==2)
dim(testset)
sum(testset$withSent)
print(t.test(testset$z_score_tp1[testset$withSent],testset$z_score_tp1[!testset$withSent]))
print(wilcox.test(testset$z_score_tp1[testset$withSent],testset$z_score_tp1[!testset$withSent],paired=FALSE))


testset <- dataset2 %>% filter(z_score<=0.0) %>% mutate(withSent = !is.na(sentiment))
dim(testset)
sum(testset$withSent)
print(t.test(testset$z_score_tp1[testset$si==1],testset$z_score_tp1[testset$si==3]))
print(t.test(testset$z_score_tp1[testset$si==1],testset$z_score_tp1[is.na(testset$sentiment)]))
print(t.test(testset$z_score_tp1[testset$si==3],testset$z_score_tp1[is.na(testset$sentiment)]))

print(t.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[testset$si==3]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[is.na(testset$sentiment)]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[testset$si==2]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==3],testset$return_mo_to_mc_tp1[is.na(testset$sentiment)]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==3],testset$return_mo_to_mc_tp1[testset$si==2]))

print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[testset$si==3],paired=FALSE))
print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[is.na(testset$sentiment)],paired=FALSE))
print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[testset$si==2],paired=FALSE))
print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==3],testset$return_mo_to_mc_tp1[is.na(testset$sentiment)],paired=FALSE))
print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==3],testset$return_mo_to_mc_tp1[testset$si==2],paired=FALSE))

print(t.test(testset$return_mo_to_mc_tp1[is.na(testset$sentiment)]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==2]))

print(kruskal.test(return_mo_to_mc_tp1~si,data=testset))
#print(posthoc.kruskal.dunn.test(testset$return_mo_to_mc_tp1,testset$si,p.adjust.method = "none"))
print(kwAllPairsDunnTest(return_mo_to_mc_tp1 ~si, data = testset,p.adjust.method="none"))
fit = aov(return_mo_to_mc_tp1 ~si,data=testset)
res <- tukeyTest(fit)
summary(res)
summaryGroup(res)


## remove observations that have neutral sentiment -> si==2 means "no-news"
testset <- testset %>% filter(!(si ==2 & !is.na(sentiment))) 
print(kruskal.test(return_mo_to_mc_tp1~si,data=testset))
#print(posthoc.kruskal.dunn.test(testset$return_mo_to_mc_tp1,testset$si,p.adjust.method = "none"))
print(kwAllPairsDunnTest(return_mo_to_mc_tp1 ~si, data = testset,p.adjust.method="none"))
fit = aov(return_mo_to_mc_tp1 ~si,data=testset)
res <- tukeyTest(fit)
summary(res)
summaryGroup(res)

######
## positive z_score

testset <- dataset2 %>% filter(z_score>0.0) %>% mutate(withSent = !is.na(sentiment))
dim(testset)
sum(testset$withSent)
print(t.test(testset$z_score_tp1[testset$si==1],testset$z_score_tp1[testset$si==3]))
print(t.test(testset$z_score_tp1[testset$si==1],testset$z_score_tp1[is.na(testset$sentiment)]))
print(t.test(testset$z_score_tp1[testset$si==3],testset$z_score_tp1[is.na(testset$sentiment)]))

print(t.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[testset$si==3]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[is.na(testset$sentiment)]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[testset$si==2]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==3],testset$return_mo_to_mc_tp1[is.na(testset$sentiment)]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==3],testset$return_mo_to_mc_tp1[testset$si==2]))

print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[testset$si==3],paired=FALSE))
print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[is.na(testset$sentiment)],paired=FALSE))
print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==1],testset$return_mo_to_mc_tp1[testset$si==2],paired=FALSE))
print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==3],testset$return_mo_to_mc_tp1[is.na(testset$sentiment)],paired=FALSE))
print(wilcox.test(testset$return_mo_to_mc_tp1[testset$si==3],testset$return_mo_to_mc_tp1[testset$si==2],paired=FALSE))


print(t.test(testset$return_mo_to_mc_tp1[is.na(testset$sentiment)]))
print(t.test(testset$return_mo_to_mc_tp1[testset$si==2]))

print(kruskal.test(return_mo_to_mc_tp1~si,data=testset))
#print(posthoc.kruskal.dunn.test(testset$return_mo_to_mc_tp1,testset$si,p.adjust.method = "none"))
print(kwAllPairsDunnTest(return_mo_to_mc_tp1 ~si, data = testset,p.adjust.method="none"))
fit = aov(return_mo_to_mc_tp1 ~si,data=testset)
res <- tukeyTest(fit)
summary(res)
summaryGroup(res)


## remove observations that have neutral sentiment -> si==2 means "no-news"
testset <- testset %>% filter(!(si ==2 & !is.na(sentiment))) 
print(kruskal.test(return_mo_to_mc_tp1~si,data=testset))
#print(posthoc.kruskal.dunn.test(testset$return_mo_to_mc_tp1,testset$si,p.adjust.method = "none"))
print(kwAllPairsDunnTest(return_mo_to_mc_tp1 ~si, data = testset,p.adjust.method="none"))

fit = aov(return_mo_to_mc_tp1 ~si,data=testset)
res <- tukeyTest(fit)
summary(res)
summaryGroup(res)
