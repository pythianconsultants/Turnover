library(tidyverse)
library(lubridate)
library(plotly)

votes <- read.csv("Data/votes.csv")

glimpse(votes)

votes$voteDate <- gsub("CET","",votes$voteDate)
votes$voteDate <- gsub("CEST","",votes$voteDate)
votes$voteDate <- gsub("  "," ",votes$voteDate)
votes$voteDate <- parse_date_time(votes$voteDate,"%a %b %d %H:%M:%S %Y")
votes$Month <- lubridate::month(votes$voteDate,label=TRUE)

ggplot(votes, aes(x=vote,fill=Month))+geom_bar()
votes$vote <- as.factor(votes$vote)
ggplotly(ggplot(votes, aes(x=Month,fill=vote))+geom_bar())

#Let's have a unique way of identifying employees
#We will create a unique id by combining the variables 'employee' and 'companyAlias'
#companyAlias is quite unweildy; let's shorten it
#Have a look at the LETTERS() function; Type ?LETTERS on the console

#How many unique companies?
length(unique(votes$companyAlias))

#So there are 37 unique companies; LETTERS will give us 26 and then we'll add 11 more
levels(votes$companyAlias) <- c(LETTERS,paste0(LETTERS[1],LETTERS[1:11]))

#Now combine this with the variable 'employee' which is a numeric value
votes$EmpID <- paste0(votes$employee,votes$companyAlias)

#Which company has the most number of employees casting votes?
comp <- votes%>%group_by(companyAlias)%>%summarize(Count=n())%>%arrange(desc(Count))

#At this juncture, would it be correct to say that employees of companies
#that don't have too many votes, are more likely to churn?

#Let's also look at people who have voted more than once
emp <- votes%>%group_by(EmpID,companyAlias)%>%summarize(Count=n())%>%filter(Count>1)%>%
  arrange(desc(Count))
