library("dplyr")
library("stringr")
library(ggplot2)

#Which overseas city in the rank of top 50 has the lowest death rate for US citizens? 
lowest <- data %>%
  select(City) %>%
  mutate(Region = str_replace(City, '.+-(.+)','\\1' )) %>%
  arrange(desc(-1)) %>%
  count(Region) %>%
  mutate(rank = min_rank(-n))%>%
  arrange(desc(n))%>%
  filter(rank == 100)
print(lowest$Region)
  
# Which overseas  city in the ranl of top 50 has the highest death rate for US citizens?
highest <- data %>%
  select(City) %>%
  mutate(Region = str_replace(City, '.+-(.+)','\\1' )) %>%
  arrange(desc(-1)) %>%
  count(Region) %>%
  mutate(rank = min_rank(-n))%>%
  arrange(desc(n)) %>%
  filter(rank == 2)
#What is the main cause of death for US citizens?
main <- data %>%
  select(Cause.of.Death) %>%
  arrange(desc(-1)) %>%
  count(Cause.of.Death) %>%
  mutate(rank = min_rank(-n))%>%
  arrange(desc(n)) 

  
#Which city is worth the risk of travel?
#Which city has improved the most (2008-2018)? 
t1 <- data %>%
  select(City, Date) %>%
  mutate(Region = str_replace(City, '.+-(.+)','\\1' )) %>%
  mutate(Year =str_sub(Date, start= -4)) %>%
  select(Region, Year)



#select(Region, Year) 

t2 <- data %>%
  select(City, Date) %>%
  mutate(Region = str_replace(City, '.+-(.+)','\\1' )) %>%
  arrange(desc(-1)) %>%
  mutate(Year =str_sub(Date, start= -4)) %>%
  count(Region, Year) %>%
  mutate(rank = min_rank(-n))%>%
  arrange(desc(n)) %>%
  select(Region, Year, n) 
  

y2008 <- t2 %>%
  filter(Year == "2008")%>%
  rename("2008" = "n") %>%
  select(Region,"2008")




temp <- data %>%
  select(City,Date) %>%
  mutate(Year =str_sub(Date, start= -4)) %>%
  filter(Year >"2007")%>%
  mutate(Region = str_replace(City, '.+-(.+)','\\1' )) %>%
  arrange(desc(-1)) %>%
  count(Region) %>%
  mutate(rank = min_rank(-n))%>%
  arrange(desc(n)) %>%
  select(Region, n)

com <- left_join(y2008, temp) %>%
mutate(rate = ((com$n/com$`2008`)^1/10 - 1)*100)
mutate(rank = min_rank(-n))%>%
  arrange(desc(rate)) 



  


#Which one has gotten worse?
  