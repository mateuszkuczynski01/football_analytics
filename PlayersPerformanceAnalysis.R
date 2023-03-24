#Data from fbref.com
#Link: https://fbref.com/en/

# This code allows you to perform a performance analysis
# of all players from a certain league using data from 
# fbref.com
# It works for all leagues except TOP5 European leagues

setwd("D:\\R-Soccer\\Data")
library(dplyr)

players <- read.csv("Ekstraklasa.csv")
head(players)
str(players)

#Cleaning Data
#Remove the 1st column and last column

players <- players[,-1]
players <- players[,-23]
head(players)

#Fix the names of columns
colnames(players) <- c("player.name","nation","position","team",
                       "age","born","MP","starts","min","90s",
                       "Gs","As","G-PK","PK","PKatt","YCs","RCs",
                       "Gs/90","As/90","Gs+As/90","Gs-PK/90",
                       "Gs+As-PK/90")

head(players)

#Clean player names
players$player.name
players$player.name <- substr(players$player.name,1,nchar(players$player.name)-9)
head(players)

#Clean nations

players$nation <- substr(players$nation,4,6)
head(players)

#Clean positions
nchar(players[3,3])

clean.posiitons <- function(pos){
  if (nchar(pos)>2){
    return(substr(pos,1,2))
  }else{
    return(pos)
  }
}

players$position <- sapply(players$position,clean.posiitons)
head(players)

#Creating substitutions column
players$subs <- players$MP - players$starts

players <- cbind(players[1:8], players[23], players[9:22])
head(players)

#Let's see and plot players contribution
arrange(cbind(players[1],players[23]), desc(players$`Gs+As-PK/90`))

library(ggplot2)
library(ggthemes)
library(plotly)

cont <- ggplot(players, 
               aes(x=`Gs+As-PK/90`,y=min,color=position))

cont <- cont + geom_point(data = subset(players,`Gs+As-PK/90`>0.2 & min>500),
                  size=3.5) + 
  geom_text(aes(label=player.name), color="gray20", size=4,
                                  data = subset(players,`Gs+As-PK/90`>0.4
                                                &min>500),
                                  check_overlap = TRUE) + theme_bw()
cont


#Create a function that will plot contribution of players of a 
#particular team

show.team <- function(club=""){
 temp <- players %>% filter(team==club)
 plot <- ggplot(temp, 
                aes(x=`Gs+As-PK/90`,y=min,color=position))
 
 plot <- plot + geom_point(data = subset(temp,`Gs+As-PK/90`>0.2 & min>500),
                           size=4.5) + 
   geom_text(aes(label=player.name), color="gray20", size=5,
             data = subset(temp,`Gs+As-PK/90`>0.2
                           &min>500),
             check_overlap = TRUE) + theme_bw()
 plot
 
  }

#Now we can plot player contribution for any team
show.team("Pogoñ Szczecin")

#Let's create a player value column
install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)

values <- get_player_market_values(country_name = "Poland",
                                   start_year = 2021)

str(values)
values <- cbind(values[7:8], values[13:14],values[17:18])
head(values)
