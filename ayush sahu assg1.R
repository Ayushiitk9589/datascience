##########ques 1. part a
library(tidyverse)
library(rvest)
library(stringr)

html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
Company_names <- html %>% html_elements(".company-ellipses") %>%  html_text2() 
Company_names

table <- html %>% html_elements("#stock-list-table") %>% html_table()

CMP <- html %>% html_table()
CMP[[1]][3]

price_change <- html %>% html_table()
price_change[[1]][4]

market_cap <- html %>% html_table()
market_cap[[1]][5]

high <- html %>% html_table()
high[[1]][6]

low <- html %>% html_table()
low[[1]][7]

roe <- html %>% html_table()
roe[[1]][8]

pe <- html %>% html_table()
pe[[1]][9]

pbv <- html %>% html_table()
pbv[[1]][10]

ebitda <- html %>% html_table()
ebitda[[1]][11]


fivesales <- html %>% html_table()
fivesales [[1]][12]

fiveprofit <- html %>% html_table()
fiveprofit [[1]][13]

table

########################################### ques 1b
html <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/telecom/telecommunication-service-provider/bharti-airtel/company-info")
alltables <- html %>% html_table(fill= TRUE)

table1<- alltables[[1]]
table1

##########ques c(1)
tennis <- function(p){
  
  win <- rbinom(n=5,size=1,prob=p)
  if ((win[1] == win[2]) && (win[2]== win[3]) ){
    print("3")
  }
  else if (win[1]+win[2]+win[3]+win[4]==1 || win[1]+win[2]+win[3]+win[4]== 3) {
    print("4")
  }
  else {
    print("5")
  }
}
tennis(1)
################# part c(2)
matches <-  vector(length = 1000)
for(i in 1:1000)
{
  matches[i] <- as.numeric(tennis(0.70))
}
ans <- mean(matches)
ans

############### part d

MontyHall <- function() {
  prize <- sample(1:3,1)
  choice <- sample(1:3,1)
  monty <- sample(c(1:3)[-c(choice, prize)], 1)
  switch <- c(1:3)[-c(choice, monty)]
  if(prize == switch){
    return("1")
  }
  else {
    return("0")
  }
}
MontyHall()

#################### ques parte

html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
ranking <- html_elements(html, ".countdown-index-resposive")
ranking <- html_text(ranking) 
ranking <- str_remove_all(ranking,"[#]")
ranking

name_of_movie <- html_elements(html, ".article_movie_title a")
name_of_movie <- html_text(name_of_movie)
name_of_movie

tomato_score <- html %>% html_elements(".tMeterScore") %>% html_text() 
tomato_score

year <- html %>% html_elements(".subtle.start-year") %>% html_text() %>% str_remove_all("[()]") %>% as.numeric()
year
