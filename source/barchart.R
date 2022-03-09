#install.packages("dplyr")
library(dplyr)
library(stringr)

export <- read.csv("../data/export.csv")

New_data <- export %>%
  select(City) %>%
  mutate(Region = str_replace(City, '.+-(.+)','\\1' )) %>%
  arrange(desc(-1)) %>%
  count(Region) %>%
  mutate(rank = min_rank(-n))%>%
  arrange(desc(n))
#-------------------------------------------------------------------------------

t2 <- New_data %>%
  group_by(Region)%>%
  slice(1:10) %>%
  filter(rank < 11) 
#-------------------------------------------------------------------------------
#install.packages("ggplot2")
library(ggplot2)
#install.packages("plotly")
library(plotly)
#-------------------------------------------------------------------------------
barchart <- ggplot(t2, aes(x=Region, y=n)) + geom_bar(stat="identity", width = 0.7) + 
  labs(x="location", y="Frequency") + ggtitle("Top Ten Death Region Oversea")

#View(barchart)
#print(barchart)


