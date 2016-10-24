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

#Create IDs for unique chair + affiliation combinations
chair.unique <- paste(chair.name,chair.affiliation)
chairs_clean <- transform(chairs_clean, 
                          chair.id=as.numeric(factor(chair.unique)))

#Remove id from NA chairs
chairs_clean[,4] <- gsub("1052","NA",chairs_clean[,4])

#Remove duplicates from chairs table
chairs_table <- chairs_clean[!duplicated(chairs_clean[,2:4]),2:4]

#export table to csv
write.csv(chairs_clean, file = "chairs_clean.csv", row.names=FALSE)

#export chairs table
write.csv(chairs_table, 
          file = "chairs_table.csv", row.names = FALSE)

#export chairs and papers table
write.csv(chairs_clean[-(2:3)],file = "chairs_papers.csv", row.names = FALSE)

