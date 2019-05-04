team19 <- read_excel("D:/Google Drive/Math/Regression Methods/Project/NBA/Team per game stats 14-19.xlsx",sheet=6)
team18$`3PM2PM` <- team18$`3PM` + team18$`2PM`
team18$`3PA2PA` <- team18$`3PA` + team18$`2PA`

team13 <- read_excel("D:/Google Drive/Math/Regression Methods/Project/NBA/Team per game stats 14-19.xlsx",sheet=1)

ggplot(team18) +
  geom_point(aes(x=WIN, y=FTM), size = 4, color="red")

temp$`+/-` <- read.delim("clipboard",header  = FALSE)
team18[,24] <- temp
colnames(team18)[24] <- "+/-"

team13[,26] <- rep(13,30)
colnames(team13)[26] <- "YEAR"
team13$FGM <- team13$`2PM` + team13$`3PM`
team13$FGA <- team13$`2PA` + team13$`3PA`
team13$`eFG%` <- (1.5*team13$`3PM` + team13$`2PM`)/team13$`FGA`
team13 <- team13[,c(1:4,26:28,5:25)]

team13$`2PM` <- team13$FGM - team13$`3PM`
team13$`2PA` <- team13$FGA - team13$`3PA`
team13$`2P%` <- team13$`2PM`/team13$`2PA`
team13 <- team13[,c(1:7,23:25,8:22)]
team13[,7] <- (1.5*team13$`3PM` + team13$`2PM`)/team13$FGA

# import old data
ind <- match(team13$TEAM, TeamName$Full)
team13$TEAM <- TeamName[ind,1]

team18aaa <- team18
team17aaa <- team17
team16aaa <- team16
team15aaa <- team15

team15[,1] <- team15aaa[,1]
team17aaa <- team17aaa[,-c(2:24,26)]

def13 <- read_excel("D:/Google Drive/Math/Regression Methods/Project/NBA/defense.xlsx",sheet=1)
colnames(def13)[1] <- "TEAM"
ind <- match(def13$TEAM, TeamName$Full)
def13$TEAM <- TeamName[ind,1]

ind <- match(team13$TEAM, def13$TEAM)
team13[,34] <- def13[ind,16]
#----------------------------------------------------------------------------------------------------------#
ind <- match(team19$TEAM,def19$TEAM)
team19[,29:32] <-def19[ind, c(8,13:15)]

sal <- read_excel("D:/Google Drive/Math/Regression Methods/Project/NBA/Opp Stats 14-18.xlsx",sheet=6)

ind <- match(sal$TEAM, TeamName$City)
sal$TEAM <- TeamName[ind,1]
ind <- match(team19$TEAM, sal$TEAM)
team19[,33] <- sal[ind,2]

null <- lm(WIN~1, data=team16_18a)

full <- lm(WIN~FGA+`eFG%` +`3PA`+`2PA`+ FTA+`FT%`+ OREB + DREB + AST+TOV
           +STL+BLK+PACE+YEAR+`DREB%`+`3PA2PA`+ FB+OPAINT+ defRK + YEAR*`eFG%` + YEAR*PACE, data = team16_18a)

stepwise <-step(full,scope=formula(full),direction="backward")
reg_t <- lm(WIN ~ 0+ salary+defRK + `eFG%` + OREB + YEAR + TOV + `FT%`  + `eFG%`:YEAR, data = team16_18a)
summary(reg_t)

subsets <- regsubsets(WIN ~`eFG%`+ YEAR +PACE+DREB + STL +OREB +TOV+defRK+`FT%`+YEAR*`eFG%`,
                      intercept =F,data = team16_18mat)
plot(subsets)
##-----------------------------------------------------------------------------------------------------------
reg_def1 <-lm( WIN ~0+`eFG%`+ YEAR +DREB + STL +OREB +TOV+defRK+`FT%`+YEAR*`eFG%`, data = team16_18a)
reg_def1b <-lm( WIN ~0+`eFG%`+ YEAR +PACE+DREB + STL +OREB +TOV+defRK+YEAR*`eFG%`, data = team16_18a)

reg_def1c <-lm( WIN ~0+`eFG%`+ YEAR +OREB +TOV+defRK+SALARY+`FT%`+YEAR*`eFG%`, data = team16_18a)


coe1c <- reg_def1c$coefficients

predict19 <- coe1c[1]*team19$`eFG%`+coe1c[2]*team19$YEAR + coe1c[3]*team19$OREB +coe1c[4]*team19$TOV +
  coe1c[5]*team19$defRK+coe1c[6]*team19$SALARY+coe1c[7]*team19$`FT%`+coe1c[8]*team19$YEAR*team19$`eFG%`

predict18c <- coe1c[1]*team18$`eFG%`+coe1c[2]*team18$YEAR + coe1c[3]*team18$OREB +coe1c[4]*team18$TOV +
  coe1c[5]*team18$defRK+coe1c[6]*team18$SALARY+coe1c[7]*team18$`FT%`+coe1c[8]*team18$YEAR*team18$`eFG%`

predict17c <- coe1c[1]*team17$`eFG%`+coe1c[2]*team17$YEAR + coe1c[3]*team17$OREB +coe1c[4]*team17$TOV +
  coe1c[5]*team17$defRK+coe1c[6]*team17$SALARY+coe1c[7]*team17$`FT%`+coe1c[8]*team17$YEAR*team17$`eFG%`

predict16c <- coe1c[1]*team16$`eFG%`+coe1c[2]*team16$YEAR + coe1c[3]*team16$OREB +coe1c[4]*team16$TOV +
  coe1c[5]*team16$defRK+coe1c[6]*team16$SALARY+coe1c[7]*team16$`FT%`+coe1c[8]*team16$YEAR*team16$`eFG%`
#-----------------------------------------------
predict18 <- coe1a[1]*team18$`eFG%`+coe1a[2]*team18$YEAR + coe1a[3]*team18$PACE +coe1a[4]*team18$DREB +
  coe1a[5]*team18$STL + coe1a[6]*team18$OREB + coe1a[7]*team18$TOV+coe1a[8]*team18$defRK

predict17 <- coe1a[1]*team17$`eFG%`+coe1a[2]*team17$YEAR + coe1a[3]*team17$PACE +coe1a[4]*team17$DREB +
  coe1a[5]*team17$STL + coe1a[6]*team17$OREB + coe1a[7]*team17$TOV+coe1a[8]*team17$defRK

predict16 <- coe1a[1]*team16$`eFG%`+coe1a[2]*team16$YEAR + coe1a[3]*team16$PACE +coe1a[4]*team16$DREB +
  coe1a[5]*team16$STL + coe1a[6]*team16$OREB + coe1a[7]*team16$TOV+coe1a[8]*team16$defRK


predict19 <- coe1a[1]+ coe1a[2]*team19$`eFG%`+coe1a[3]*team19$YEAR + coe1a[3]*team19$PACE +coe1a[5]*team19$DREB +
  coe1a[6]*team19$STL + coe1a[7]*team19$OREB + coe1a[8]*team19$TOV+
  coe1a[9]*team19$defRK+coe1a[10]*team19$SALARY

predict16c <- round(predict16c)
team16$predict1c <- predict16c

team19$res1a <- team19$predict1a - team19$WIN
qqnorm(team19$res1a)
ggplot(team18) + geom_point(aes(x=predict1a,y=res1a))
sum(team18$res1a)

reg<-lm(WIN ~`eFG%`+ YEAR+PACE+DREB+STL+OREB+TOV+`2PA`, data=team16_18a)
###------------------------------------------------------------------------------------------------------------


reg<-lm(WIN ~`eFG%`+ YEAR+PACE+DREB+STL+OREB+TOV+`2PA`, data=team16_18a)
summary(reg)

coe <- reg$coefficients
predict19 <- coe[1]*team19$`eFG%` + coe[2]*19 + coe[3]*team19$PACE + coe[4]*team19$DREB +
  coe[5]*team19$STL + coe[6]*team19$OREB +coe[7]*team19$TOV + coe[8]*team19$`2PA`

predict19 <- round(predict19)
team19$predict <- predict19
#----------------------------------------------------------------------------------------------------------#
predict15 <- coe[1]*team15$`eFG%` + coe[2]*team15$YEAR + coe[3]*team15$PACE + coe[4]*team15$DREB +
  coe[5]*team15$STL + coe[6]*team15$OREB +coe[7]*team15$TOV + coe[8]*team15$`2PA`
predict15 <- round(predict15)
team15$predict <- predict15
#----------------------------------------------------------------------------------------------------------#
predict14 <- coe[1]*team14$`eFG%` + coe[2]*team14$YEAR + coe[3]*team14$PACE + coe[4]*team14$DREB +
  coe[5]*team14$STL + coe[6]*team14$OREB +coe[7]*team14$TOV + coe[8]*team14$`2PA`
predict14 <- round(predict14)
team14$predict <- predict14
#----------------------------------------------------------------------------------------------------------#
predict13 <- coe[1]*team13$`eFG%` + coe[2]*team13$YEAR + coe[3]*team13$PACE + coe[4]*team13$DREB +
  coe[5]*team13$STL + coe[6]*team13$OREB +coe[7]*team13$TOV + coe[8]*team13$`2PA`
predict13 <- round(predict13)
team13$predict <- predict13

team16$TEAM <- factor(team16$TEAM, levels = unique(team16$TEAM)) ## Make team name as factor

new<- predict(reg_def1c, interval="prediction")
new_df <- cbind(team19,new)

ggplot(team19) +
  geom_point(aes(x= TEAM, y=predict.or19), size = 3) +
  geom_text(aes(x= TEAM, y=predict.or19, label =predict.or19),vjust =2)+
  geom_label(aes(x =TEAM, y=WIN,label=WIN), color="red", alpha=0.7) +
  xlab("Season 2019") +
  ylab("Wins Estimation") +
  theme(title = element_text(face="bold",size = 18),
        axis.text.y = element_text(face = "bold", size = 12))

pred <- predict(reg_aaa1, team19a, interval = "prediction")
conf <- predict(reg_aaa1, team19a, interval = "confidence")



ggplot(team19a,aes(x= TEAM, y=predict19_5b)) +
  geom_point(size = 2) +
  geom_text(aes(label =predict19_5b),vjust =2)+
  geom_label(aes(x =TEAM, y=WIN,label=WIN), color="red", alpha=0.7) +
  geom_line(aes(y=lwr), color ="red", linetype = "dashed") +
  geom_line(aes(y=upr), color ="red", ) +
  xlab("Season 2019") +
  ylab("Wins Prediction") +
  theme(title = element_text(face="bold",size = 18),
        axis.text.y = element_text(face = "bold", size = 12))

  


####
reg1<-lm(WIN ~ `eFG%`+ YEAR+PACE+DREB+STL+OREB+TOV+SALARY+`FT%`, data=team16_18a)

predict18 <- coe1[1] + coe1[2]*team18$`eFG%` + coe1[3]*team18$YEAR + coe1[4]*team18$PACE + coe1[5]*team18$DREB +
  coe1[6]*team18$STL + coe1[7]*team18$OREB +coe1[8]*team18$TOV + coe1[9]*team18$SALARY + coe1[10]*team18$`FT%`
predict18 <- round(predict18)


pp <- ggplot(team13_18, aes(x = `3PA`, y = WIN)) +
  geom_point(aes(size = `3PA2PA`, color = `3P%`),alpha = 0.7) +
  scale_size(range=c(2,18)) +
  scale_colour_gradient(low = "red", high = "green") +
  geom_text(aes(label=TEAM),hjust=0,vjust=0, size = 5) +
  xlab("Average 3-point attempted per game") +
  ylab("Wins") +
  labs(title = 'Season: {2000+floor(frame_time)}', x = '3-point attempted/Game', y = 'Wins') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22)) +
  transition_time(YEAR) +
  ease_aes('linear')

animate(pp, nframe = 72, fps = 4)
animate(pp, nframe = 6, fps = 1)

options(gganimate.dev_args = list(width = 1600, height = 800))


ggplot(data = league) +
  geom_point(aes(x = Season, y = `3PA`, size = PTS, color=Pace)) +
  scale_size(range=c(2,14)) +
  scale_colour_gradient(low = "pink", high = "red") +
  guides(fill = guide_colourbar(barwidth = 8)) +
  xlab("Season") +
  ylab("Average 3-point attempted per game") +
  theme(title = element_text(face="bold",size = 18),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12))



ggplot(data = team16_18, aes(x = `3PA`, y = WIN)) +
  geom_point(aes(size = `3PA2PA`, color = `3P%`)) +
  facet_wrap(~YEAR) +
  scale_size(range=c(2,14)) +
  scale_colour_gradient(low = "red", high = "green") +
  geom_text(data = subset(team16_18,WIN >50),aes(label=TEAM),hjust=0,vjust=-1, size = 5, angle = 45) +
  xlab("Average 3-point attempted per game") +
  ylab("Wins") +
  theme(title = element_text(face="bold",size = 16),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12))
  

   facet_wrap(~contiental)

anim_save("pp1.gif", animation = last_animation(),
          path = "D:/Google Drive/Math/Regression Methods/Project/NBA/Plots")
#----------------------------------------------------------------------------------------------------------

null <- lm(WIN ~ 0, data = team16_18a)
full <- lm(WIN~0+FGA+`eFG%` +`3PA`+`2PA`+ FTA+`FT%`+ OREB + DREB + AST+TOV
           +STL+BLK+PACE+YEAR+`DREB%`+`3PA2PA`+ FB+OPAINT+ defRK + YEAR*`eFG%`+
           YEAR*`2PA`+ YEAR*`3PA`, data = team16_18a)
stepwise <-step(full, direction = "backward")

reg <- lm(WIN ~ 0 +`eFG%`+OREB+DREB + TOV + STL + PACE+YEAR+defRK + YEAR*`eFG%`, data=team16_18a)
summary(reg)


subsets <- regsubsets(WIN ~ `eFG%`+ OREB + DREB + TOV + STL + PACE + YEAR+defRK+YEAR*`eFG%`, data=team16_18a)
plot(subsets)
plot (subsets, scale="Cp")

coe <- reg$coefficients 
predict16 <- coe[1]+coe[2]*team16$`eFG%`+ coe[3]*team16$OREB + coe[4]*team16$DREB+
  coe[5]*team16$TOV + coe[6]*team16$STL+coe[7]*team16$PACE+coe[8]*team16$YEAR+coe[9]*team16$defRK+
  coe[10]*team16$YEAR*team16$`eFG%`

predict16 <- round(predict16)
team16$predict <- predict16


  reg <- lm(WIN ~ `eFG%` + FTA + `FT%` + OREB + DREB + TOV + STL  PACE + YEAR + FB + defRK + `eFG%`:YEAR, data = team16_18a)
summary(reg)


reg.or <- lm(WIN ~ 0+`eFG%` + `3PA` + `2PA` + `FT%` + OREB + DREB + TOV + STL + 
               YEAR + defRK + `eFG%`:YEAR, data=team16_18a)
summary(reg.or)

coe.or <- reg.or$coefficients 

predict.or19 <- coe.or[1]*team19$`eFG%` + coe.or[2]*team19$`3PA` + coe.or[3]*team19$`2PA`+
  coe.or[4]*team19$`FT%` + coe.or[5]*team19$OREB + coe.or[6]*team19$DREB+
  coe.or[7]*team19$TOV + coe.or[8]*team19$STL + coe.or[9]*team19$YEAR+
  coe.or[10]*team19$defRK + coe.or[11]*team19$`eFG%`*team19$YEAR
predict.or19 <- round(predict.or19)
team19$predict.or19 <- predict.or19










