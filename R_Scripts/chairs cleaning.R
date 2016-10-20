#***Separate chair and affiliation***#

library(readxl)
library(stringr)

#Load data
sim.data <-read_excel("1997-2015 Informs Data.xlsx")

chairs <- sim.data[,4]

#creat paper ids
paper.id <- c(1:nrow(sim.data))

#Location of "(" in string
loc <- str_locate(chairs, "\\(")

#Chair name from beginning of string to "("
chair.name <- substr(chairs,1,loc-2)

#Chair affiliation between "()"
chair.affiliation <- gsub(".*\\((.*)\\).*", "\\1",chairs)

#combine columns
chairs_clean <- cbind(paper.id, chair.name, chair.affiliation)
chairs_clean <- data.frame(chairs_clean)

#export table to csv
write.csv(chairs_clean, file = "chairs_clean.csv", row.names=FALSE)
