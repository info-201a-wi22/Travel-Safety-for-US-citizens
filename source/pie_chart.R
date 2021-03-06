Travel_Safety_For_Cit <- read.csv("https://raw.githubusercontent.com/info-201a-wi22/Travel-Safety-for-US-citizens/main/data/export.csv")
City_numberdths <- Travel_Safety_For_Cit %>% 
  count(City) %>%
  mutate(rank = min_rank(-n)) %>%
  arrange(desc(n))
City_numberdths
City_numberdths <- data.frame(City_numberdths)

#------------------------------------------------------------------------------
library(plotrix)
slices <- c(169, 151, 94, 94, 70, 63, 62, 53, 49, 45, 45)
lbls <- c("Tijuana","Port Au Prince", "Baghdad","Ciudad Jaurez","Chihuahua" , "San Jose-Costa Rica", "Norte Tiajuana", "Santa Domingo", 
          "Cancun", "Nuevo Laerdo")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls, "%", sep = "")
pie(slices, labels = lbls, col=rainbow(length(lbls)),
    main="Percentage by number of Deaths in Each Region")
