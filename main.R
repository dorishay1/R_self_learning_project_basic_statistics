#Question 1#
 
sweet <- c("Twist","Egozi","Kitkat","Timeout","Ears_of_aman")

salty <- c("Layes","Bamba","Doritos","Bisli")

rattles <- c("Blue","Pink","Green")

#options for mishloah manot per category= OFM2#

sweet_OFM2 <- choose(length(sweet),2)
salty_OFM2 <- choose(length(salty),2)
rattles_OFM2 <- choose(length(rattles),1)
#number of options for a traditional mishloah manot=TMMO#

TMMO <- (sweet_OFM2*salty_OFM2*rattles_OFM2)

#question 2#

library(tidyverse)
data <- read.csv("Star.csv")

str(data)
 
 #mode for catgorial variables#

mo.sex <- which.max(table(data$sex))

mo.classk <- which.max(table(data$classk))

tsc <- table(data$sex,data$classk) #table sex classk

prop.table.tsc <- prop.table(tsc)

tb <- sum(tsc[1,]) #totalboys

p.tsc <- prop.table(tsc)
p.boy <- sum(p.tsc[1,])

p.tsci <- c(p.tsc[1,])

for(i in 1:3){
  p.tsci[i] <- c(p.tsci[i])/p.boy
}
#P(classk|boy)12

#question 3#

mean.math <- mean(data$tmathssk)

median.math <- median(data$tmathssk)

Mo.math <- which.max(table(data$tmathssk))

#precentace of mode#

precentage.diff.mode <- (1-(Mo.math/NROW(data$tmathssk)))

#AD#

mathrow <- data$tmathssk
for(i in 1:NROW(data$tmathssk)){
  mathrow[i] <- abs(mathrow[i]-median.math)
}

#AD#

AD.math <- sum(mathrow)/NROW(data$tmathssk)

#SD#

sd.math<- sd(data$tmathssk)

#Range#

t.range.math<- range(data$tmathssk)
range.math <- t.range.math[2]-t.range.math[1]

#IQR#

IQR.math <-IQR(data$tmathssk)

#question 4 - trim mean for math and read tasks#

trim.mean.math <- mean(data$tmathssk,trim = 0.1)
trim.mean.read <- mean(data$treadssk,trim = 0.1)

#question 5#
  #scales for read and math#
  
  data$scale.math <- scale(data$tmathssk)
  data$scale.read <- scale(data$treadssk)

  #quantile#

  data$quantile.math <- percent_rank(data$tmathssk)
  data$quantile.read <- percent_rank(data$treadssk)

#question 6 - finished#
 
#cor plot - cor between math and read score#
  
library(ggplot2)  
  cor.math.read.plot <- ggplot(data, aes(x= tmathssk, y=treadssk, color=sex, shape=classk)) + 
    geom_point(size=5, alpha=0.6) + geom_abline()

    #mean score for each obs.#
      data$mean.score <- (data$tmathssk+data$treadssk)/2

#class type and grades plot #
classtype.score.plot <- ggplot(data,mapping = aes(x= mean.score, fill = classk)) +
  geom_density(alpha=.6)

#question 7#

data2 <- data

data3 <- group_by(data2,race)

race.vs.read <- summarise(data3,mean(treadssk))

race.vs.math <- summarise(data3,mean(tmathssk))
  #Black gets lower grade than white in both tests#


#question 8#
#magic trick success for 1000 rep#

iftach_guess <- sum(sample(1:13,2))

number_of_guesses <- 1000

magic_trick <- rep(0,number_of_guesses)
for (i in 1:number_of_guesses) {
  dad_guess <- sum(sample(1:13,2))
  if (length(intersect(iftach_guess,dad_guess))){magic_trick[i] <- 100} else{magic_trick[i] <- -20}
  
}

mean_magic_trick <- mean(magic_trick)

var_magic_trick <- var(magic_trick)

data.magic.trick <- data.frame(magic_trick)

#magic trick success for 1000000 rep#  

number_of_guesses_2 <- 1000000

magic_trick_2 <- rep(0,number_of_guesses_2)
for (i in 1:number_of_guesses_2) {
  dad_guess <- sum(sample(1:13,2))
  if (length(intersect(iftach_guess,dad_guess))){magic_trick_2[i] <- 100} else{magic_trick_2[i] <- -20}
  
}

mean_magic_trick_2 <- mean(magic_trick_2)

var_magic_trick_2 <- var(magic_trick_2)

data.magic.trick.2 <- data.frame(magic_trick_2)

#mean and var of magic trick is more accurate when the number of guesses is higher#


#question 9#

lambada <- 1/10

r.exp30 <- rexp(n = 30,rate = lambada)

mean.exp30<-mean(r.exp30)

game <- rep(0,1000)
for(i in 1:1000){
  game[i] <- mean(rexp(n = 30,rate = lambada))
}

data.game <- data.frame(game)

library(ggplot2)
mean.game.plot <- ggplot(data = data.game,mapping = aes(data.game[,1]))+
  geom_density()+
  xlim(0,30)
mean.game <- mean(game)

#question10#
data$ntile.totexp <- ntile(data$totexpk,8)
