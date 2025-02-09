library("micromapST")
library("ggplot2")
library("tidyverse")
A <- read.csv("Drug_Overdose.csv")
head(A, 10)
df <- data.frame(A)
head(df, 10)


type = c("maptail","id","bar","bar","arrow")
lab1 = c(NA, NA, '2017 Death Rate', '2021 Death Rate','Death rate trend from 2017 to 2021')
lab2 = c(NA, NA, NA, NA,NA)
lab3 = c(NA, NA, '(Deaths per 100,000 people)', '(Deaths per 100,000 people)','(Deaths per 100,000 people)')
col1 <- c('Location', 'Location', 'X2017__Drug.Overdose.Deaths', 
          'X2021__Drug.Overdose.Deaths','X2017__Drug.Overdose.Deaths')
col2 <- c(NA,NA,NA,NA,'X2021__Drug.Overdose.Deaths')
#refVals = c(NA,NA,NA,NA,0)

panelDesc <- data.frame(type,lab1,lab2,lab3,col1,col2)
t(panelDesc)
pdf(file="STAT515_HW22.pdf",width=10,height=10)
micromapST ( A,
             panelDesc,
             rowNamesCol = 'Location',
             rowNames = 'full',
             sortVar = 'X2021__Drug.Overdose.Deaths',
             ascend = FALSE,
             title = c("Drug Overdose Deaths trend from 2017 to 2021"))
dev.off()

B <- A [(A$X2021__Drug.Overdose.Deaths - A$X2017__Drug.Overdose.Deaths) < 0,]
str(B)

colnames(B)[2]=2017
colnames(B)[6]=2021
B

data_long <- B %>%
  pivot_longer(
    cols = c(`2017`,`2021`),
    names_to = "Year",
    values_to = "Deaths"
  )

ggplot(data_long, aes(x=Year,y=Deaths, fill=Year)) + 
  geom_bar(stat='identity', color='black')+
  geom_text(aes(label = Deaths), vjust = -0.5) +
  facet_wrap(Location~.) + theme(legend.position = 'None') + 
  scale_fill_manual(values=c('cyan','orange')) +
  labs(x = "Year",
       y = "Deaths",
       title = "States with Decreasing Death Trend", size = 2)


C <- A [(A$X2021__Drug.Overdose.Deaths - A$X2017__Drug.Overdose.Deaths) > 25,]
str(C)

colnames(C)[2]=2017
colnames(C)[6]=2021
C


data_long_2 <- C %>%
  pivot_longer(
    cols = c(`2017`,`2021`),
    names_to = "Year",
    values_to = "Deaths"
  )
ggplot(data_long_2, aes(x=Year,y=Deaths, fill=Year)) + 
  geom_bar(stat='identity', color='black')+
  geom_text(aes(label = Deaths), vjust = -0.5) +
  facet_wrap(Location~.) + theme(legend.position = 'None') + 
    scale_fill_manual(values=c('cyan','orange')) +
  labs(x = "Year",
       y = "Deaths",
       title = "States with High Increasing Death Trend")

avg <- df %>%
  pivot_longer(cols = -Location, names_to = "year", values_to = "deaths")

D <- avg %>%
  group_by(year) %>%
  summarize(averagedeaths = mean(deaths))

ggplot(D, aes(x=year, y=averagedeaths)) +
  geom_line(aes(group = 1), color="blue") +
  geom_point(color="red") +
  labs(title="National Average Trend of Drug Overdose",
       x="Year",
       y="Average Deaths") +
  theme_minimal()
