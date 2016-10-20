#***Separate chair and affiliation***#

library(readxl)
library(stringr)

#Load data
sim.data <-read_excel("1997-2015 Informs Data.xlsx")

chairs <- sim.data[,4]

#Location of "(" in string
loc <- str_locate(chairs, "\\(")

#Chair name from beginning of string to "("
chair_name <- substr(chairs,1,loc-2)

#Chair affiliation between "()"
chair_affiliation <- gsub(".*\\((.*)\\).*", "\\1",chairs)

#combine columns
chairs_clean <- cbind(chair_name, chair_affiliation)

#export table to csv
write.csv(chairs_clean, file = "chairs_clean.csv")
