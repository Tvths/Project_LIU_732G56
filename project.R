### Läsa in data set.
data = read.csv("goalkeeper.csv", sep=";")



data2 = read.csv("C:/Users/timce/OneDrive/Dokument/Rcode_2024_2025/732G56/datamaterial/utespelare.csv", sep=";")
print(length(table(data2$position)))
only_GK <- data2[data2$position == "GK",]
only_Tony_Yeboah <- only_GK[only_GK$name == "Tony Yeboah",]
only_GK$average.rating <- as.numeric(gsub(",",".",only_GK$average.rating))
test_cor <- only_GK %>% select(average.rating,han)
cor(test_cor, use = "complete.obs")

only_GK$ref
### Libraries used in project
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("nnet")
library(nnet)
install.packages("caret")
library(caret)
install.packages("readr")
library(readr)
install.packages("stringr")
library(stringr)
install.packages("kableExtra")
library(kableExtra)
install.packages("lme4")
library(lme4)
install.packages("lmerTest")
library(lmerTest)
install.packages("glmmTMB")
library(glmmTMB) 



### Convert variable result to status(win, lose, draw)
data <- data %>%
  mutate(status = ifelse(as.numeric(sub("-.*", "", c(data$result))) > as.numeric(sub(".*-", "", c(data$result))), 
                         "win", 
                         ifelse(as.numeric(sub("-.*", "", c(data$result))) < as.numeric(sub(".*-", "", c(data$result))), 
                                "lose", 
                                "draw")))


### Visualize data
# Gruppindela och sammanfatta per målvakt
top_goalkeepers <- data %>%
  group_by(name) %>%
  summarise(
    avg_rating = mean(as.numeric(average.rating), na.rm = TRUE),
    clean_sheets = sum(clean.sheets, na.rm = TRUE),
    club = first(club)
  ) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n=10)

print(top_goalkeepers)

# Visualisering
# top 10 målvakter
ggplot(top_goalkeepers, aes(x = avg_rating, y = reorder(name, avg_rating), fill = factor(clean_sheets))) +
  geom_col() +
  labs(
    title = "Topp 10 målvakter: Betyg och hållna nollor",
    x = "Genomsnittligt betyg",
    y = "Målvakt",
    fill = "Clean Sheets"
  ) +
  theme_minimal()

###
top_goalkeepers$clean_sheets

### Gruppindela och sammafatta per FC
# Top FC som har störst andel räddningarnar
top_save <- data %>%
  group_by(club) %>%
  summarise(
    avg_save = mean(as.numeric(save.),na.rm=TRUE)
  ) %>%
  arrange(desc(avg_save))
# visualisering
ggplot(top_save, aes(x = avg_save, y = reorder(club, avg_save))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Genomsnittlig räddningsprocent per klubb",
    x = "Räddningsprocent (%)",
    y = "Klubb"
  ) +
  theme_minimal()



### Summera antal straffar och räddningar per målvakt
#
penalty_stats <- data %>%
  group_by(name) %>%
  summarise(
    faced = sum(as.numeric(data$penalties.faced),na.rm = TRUE),
    saved = sum(as.numeric(data$penalties.saved),na.rm = TRUE)
  ) %>%
  mutate(
    save_rate = ifelse(faced > 0, (saved / faced) * 100, 0)
  ) %>%
  arrange(desc(save_rate)) %>%
  slice_head(n=10)

### visualisering
ggplot(penalty_stats, aes(x = save_rate, y = reorder(name, save_rate))) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Straffräddningsprocent per målvakt",
    x = "Räddningsprocent (%)",
    y = "Målvakt"
  ) +
  theme_minimal()





### 
data$season <- as.character(str_extract(data$matchday, "S\\d+"))
data$season <- as.integer(str_remove(data$season, "S"))

summary_data <- data %>%
  group_by(season) %>%
  summarise(
    Amount_goalkeeper = n_distinct(name),
    Amount_team = n_distinct(club)
  )

### visualisering
ggplot(summary_data, aes(x = season)) +
  geom_line(aes(y = Amount_goalkeeper, color = "Antal målvakter")) +
  geom_point(aes(y = Amount_goalkeeper, color = "Antal målvakter")) +
  geom_line(aes(y = Amount_team, color = "Antal lag")) +
  geom_point(aes(y = Amount_team, color = "Antal lag")) +
  labs(
    title = "Antal målvakter och antal lag per säsong",
    x = "Säsong", y = "Antal", color = NULL
  ) +
  scale_x_continuous(breaks = summary_data$season) +
  theme_minimal()



### inför modellanpassning
data_test <- table(data$name)
sort(data_test)

test <- split(data, data$name)
topgoalkeeper_data <- test[["Scott Sterling"]]
table(topgoalkeeper_data$status)
class(train_data$average.rating)

### modellanpassning  






# Gör modelldata (du kan välja fler prediktorer!)
model_data <- data %>%
  select(status, minutes.played, total.saves, average.rating) %>%
  na.omit()

# Omvandla match_outcome till faktor
model_data$status <- as.factor(model_data$status)
model_data$average.rating <- as.numeric(gsub(",",".",model_data$average.rating))





### test variable
data$average.rating <- as.numeric(gsub(",",".",data$average.rating))
data$save. <- as.numeric(gsub(",",".",data$save.))
data$xg.prevented <- as.numeric(gsub(",",".",data$xg.prevented))
data$xsave. <- as.numeric(gsub(",",".",data$xsave.))

###
data$minutes.played <- as.numeric(data$minutes.played)
data$penalties.saved <- as.numeric(data$penalties.saved)
table(data$apps)
class(data$xg.prevented)

### corelation matrix
numeric_vars <- data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_vars,use="complete.obs")
kable(cor_matrix, caption = "Correlation Matrix of Numeric Variables") %>%
  kable_styling()






### visualisering
ggplot(data) + aes(x=minutes.played,y=average.rating) + geom_point() +   scale_y_continuous(breaks = seq(5, 11, 1))
ggplot(data) + aes(x=penalties.faced,y=average.rating) + geom_point() +  scale_y_continuous(breaks = seq(0, 10, 1))
ggplot(data) + aes(x=gid,y=average.rating) + geom_point() +  scale_y_continuous(breaks = seq(0, 10, 1))
ggplot(data) + aes(x=penalties.saved,y=average.rating) + geom_point() +  scale_y_continuous(breaks = seq(0, 10, 1))
ggplot(data) + aes(x=xg.prevented,y=average.rating) + geom_point() +  scale_y_continuous(breaks = seq(0, 10, 1)) 
ggplot(data) + aes(x=player.of.the.match,y=average.rating) + geom_point() +  scale_y_continuous(breaks = seq(0, 10, 1))
ggplot(data) + aes(x=apps,y=average.rating) + geom_point() +  scale_y_continuous(breaks = seq(0, 10, 1))
ggplot(data) + aes(x=opponent,y=average.rating) + geom_violin()
ggplot(data) + aes(x=name,y=average.rating) + geom_violin()
ggplot(data) + aes(x=club,y=average.rating) + geom_violin()




### anpassning modell med lmer()
data <- na.omit(data)
modell_full <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name)+(1|opponent)+(1|club),data = data,REML = TRUE)
modell_1 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+(1|name)+(1|opponent)+(1|club),data = data,REML = FALSE)
modell_2 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+(1|name)+(1|opponent)+(1|club),data = data,REML = FALSE)
modell_3 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+(1|name)+(1|opponent)+(1|club),data = data,REML = FALSE)
modell_4 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+(1|name)+(1|opponent)+(1|club),data = data,REML = FALSE)
modell_5 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+(1|name)+(1|opponent)+(1|club),data = data,REML = FALSE)
modell_6 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+(1|name)+(1|opponent)+(1|club),data = data,REML = FALSE)
modell_7 <- lmer(average.rating ~ total.saves+save.+xg.prevented+(1|name)+(1|opponent)+(1|club),data = data,REML = FALSE)
modell_8 <- lmer(average.rating ~ total.saves+save.+(1|name)+(1|opponent)+(1|club),data = data,REML = FALSE)
modell_9 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name),data = data,REML = FALSE)
modell_10 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|opponent),data = data,REML = FALSE)
modell_11 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|club),data = data,REML = FALSE)
modell_12 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name)+(1|opponent),data = data,REML = FALSE)
modell_13 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name)+(1|club),data = data,REML = FALSE)
modell_14 <- lmer(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|club)+(1|opponent),data = data,REML = FALSE)
anova(modell_1,modell_2,modell_3,modell_4,modell_5,modell_6,modell_7,modell_8,modell_9,modell_10,modell_11,modell_12,modell_13,modell_14,modell_full)


### anpassning modell med glmTMB()
modell_full <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name)+(1|opponent)+(1|club),data = dataValid,family = gaussian)
modell_1 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+(1|name)+(1|opponent)+(1|club),data = dataValid,family = gaussian)
modell_2 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+(1|name)+(1|opponent)+(1|club),data = dataValid,family = gaussian)
modell_3 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+(1|name)+(1|opponent)+(1|club),data = dataValid,family = gaussian)
modell_4 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+(1|name)+(1|opponent)+(1|club),data = dataValid,family = gaussian)
modell_5 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+(1|name)+(1|opponent)+(1|club),data = dataValid,family = gaussian)
modell_6 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+(1|name)+(1|opponent)+(1|club),data = dataValid,family = gaussian)
modell_7 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+(1|name)+(1|opponent)+(1|club),data = dataValid,family = gaussian)
modell_8 <- glmmTMB(average.rating ~ total.saves+save.+(1|name)+(1|opponent)+(1|club),data = dataValid,family = gaussian)
modell_9 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name),data = dataValid,family = gaussian)
modell_10 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|opponent),data = dataValid,family = gaussian)
modell_11 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|club),data = dataValid,family = gaussian)
modell_12 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name)+(1|opponent),data = dataValid,family = gaussian)
modell_13 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name)+(1|club),data = dataValid,family = gaussian)
modell_14 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|club)+(1|opponent),data = dataValid,family = gaussian)
anova(modell_1,modell_2,modell_3,modell_4,modell_5,modell_6,modell_7,modell_8,modell_9,modell_10,modell_11,modell_12,modell_13,modell_14,modell_full)


summary(modell_13)
AIC(modell_13)




modell_numeric <- lm(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.,data=data)
summary(modell_numeric)
AIC(modell_numeric)
###



### extern validering

# Antalet observationer totalt
n <- nrow(data)

# Antalet som tilldelas till träningsmängden utifrån en andel på 2/3
nTrain <- n*(2/3) 

# Sätter ett seed för reproducerbarhet
set.seed(355)

# Index (utvalda observationer) till träningsmängden
indexTrain <- sample(x = n, size = nTrain, replace = FALSE)

# Plockar ut utvalda observationer från materialet med positiv 
# indexering (lägger till) för träning och negativ indexering 
# (tar bort) för validering
dataTrain <- data[indexTrain,]
dataValid <- data[-indexTrain,]


n2 <- nrow(dataValid)
nvalid <- n2*(1/2)
set.seed(355)
indexvalid <- sample(x=n2,size = nvalid,replace = FALSE)
valid1 <- dataValid[indexvalid,]
valid2 <- dataValid[-indexvalid,]

### train data
modell_13_train <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name)+(1|club),data = dataTrain,family = gaussian)
summary(modell_13_train)
AIC(modell_13_train)


### validering data 
### valid1
modell_13_validering1 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name)+(1|club),data = valid1,family = gaussian)
summary(modell_13_validering1)
AIC(modell_13_validering1)

### Valid2
modell_13_validering2 <- glmmTMB(average.rating ~ total.saves+save.+xg.prevented+player.of.the.match+clean.sheets+conceded+saves.parried+saves.held+saves.tipped+penalties.saved+xsave.+(1|name)+(1|club),data = valid2,family = gaussian)
summary(modell_13_validering2)
AIC(modell_13_validering2)



anova(modell_13_train,modell_13_validering)


install.packages("performance")
library(performance)

r2(modell_13_train)
r2(modell_13_validering1)
r2(modell_13_validering2)



# Prediktera nya anpassade värden på Y för valideringsmängden
predict1 <- predict(object = modell_1, newdata = dataValid)
predict2 <- predict(object = modell_2, newdata = dataValid)
predict3 <- predict(object = modell_3, newdata = dataValid)
predict4 <- predict(object = modell_4, newdata = dataValid)
predict5 <- predict(object = modell_5, newdata = dataValid)
predict6 <- predict(object = modell_6, newdata = dataValid)
predict7 <- predict(object = modell_7, newdata = dataValid)
predict8 <- predict(object = modell_8, newdata = dataValid)
predict9 <- predict(object = modell_9, newdata = dataValid)
predict10 <- predict(object = modell_10, newdata = dataValid)
predict11 <- predict(object = modell_11, newdata = dataValid)
predict12 <- predict(object = modell_12, newdata = dataValid)
predict13 <- predict(object = modell_13, newdata = dataValid)
predict14 <- predict(object = modell_14, newdata = dataValid)

### MSE
RMSE <- sqrt(mean((modell_1$response - predict1)^2))

library(ModelMetrics)

