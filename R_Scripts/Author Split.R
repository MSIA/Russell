library(xlsx)
library(stringr)
library(plyr)

simuldata <- read.xlsx(file="1997-2015 Informs Data (Updated).xlsx", sheetIndex=1,stringsAsFactors = FALSE)
rm(list=ls()[!ls() %in% c('simuldata')])
output <- matrix(ncol = 3)
simuldata$Authors <- gsub("\\[ Full.*|\\[ Abstract.*","",simuldata$Authors)
simuldata$Authors <- gsub("\\),","\\)",simuldata$Authors)

for (i in c(1:804, 
            806:1502,
            1504:1623,
            1625:2379,
            2381:5165,
            5167:5474,
            5476:5846,
            5848:6697,
            6699:nrow(simuldata))){ # Exclude records with unpleasant nested parentheses

    test <- simuldata[i,]
    test$Authors <- gsub("\\[","\\(",test$Authors)
    test$Authors <- gsub("\\]","\\)",test$Authors)
    
    repeat{
      test2 <- gsub("[0-9A-Za-z]","",test[7])
      test2 <- gsub("\\.|\\,|\\-|í|á|ú|ã|\\&|\\/|!","",test2)
      test2 <- gsub(" ", "", test2)
      
      if(test$Authors=="NA")
        {test$Authors <- ""} else if (grepl("\\(\\(",test2)){
        test[7] <- gsub("\\) \\)", "\\)\\)",test[7])
        temp2 <- test[7]
        temp3 <- regexpr("\\))",temp2)
        temp4 <- substring(temp2,1,temp3)
        temp5 <- str_extract(temp4,".*\\(")
        temp6 <- gsub("\\(", "\\\\(", temp5)
        temp7 <- gsub(temp6,"(",temp4)
        temp8 <- gsub("\\(", "\\\\(", temp7)
        temp8 <- gsub("\\)", "\\\\)", temp8)
        test[7] <- gsub(temp8,"",test[7])
      } else {
      temp_author <- str_match(test$Authors, ".*?, and.*\\(")
      if (is.na(temp_author) || (grepl("\\(",temp_author) && !(substring(temp_author,1,5)==", and"))){
        temp_author <- str_match(test$Authors, ".*?,")
      }
      if (is.na(temp_author) || (grepl("\\(",temp_author) && !(substring(temp_author,1,5)==", and"))){
        temp_author <- str_match(test$Authors, ".*?\\(")
      }
      temp_author <- temp_author[!is.na(temp_author)]
      temp_author <- trimws(gsub("\\(","",temp_author))
      temp_aff <- str_match(test$Authors,".*?\\(.*?\\)")
      temp_aff <- temp_aff[!is.na(temp_aff)]
      temp_aff <- paste("(",strsplit(temp_aff,"\\(")[[1]][[2]],sep="")
    
      # Write out author, affiliation, and paper name to new record
      temp_record <- c(i,temp_author,trimws(gsub("\\(|\\)","",temp_aff)))
      output <- rbind(output,temp_record)
    
      # Remove added author and affiliation (if necessary)
      test$Authors <- trimws(gsub(temp_author,"",test$Authors))
      if (substr(test$Authors,1,1) == "("){
      test$Authors <- trimws(sub(trimws(temp_aff),"",test$Authors,fixed=TRUE))
      }
      }
    if (test$Authors == ""){break}
      }
}

colnames(output) <- c("PaperID", "Author", "Affiliation") # Name the columns of the output table
output <- output[2:nrow(output),] # Remove the first row of NAs that was created with the initial instanciation of the output matrix

# Removing semicolons
output <- gsub("; ","",output)

# Removing blank rows
for (i in 1:nrow(output)){
  if (output[i,2] == ""){
    output <- output[-i,]
  }
  if (i >= nrow(output)){break}
}

# Splitting out records with "and" in them
for (i in 1:nrow(output)){
  if ((grepl(" and Jr.",output[i,2]))){
    output <- rbind(output,c(output[i,1],gsub(" and ", ", ", output[i,2]),output[i,3]))
    output[i,2] <- gsub(" and Jr.", "", output[i,2])
  }
  if (grepl(" and ",output[i,2])){
    output <- rbind(output,c(output[i,1],gsub(" and ", "", str_extract(output[i,2]," and .*")),output[i,3]))
    output[i,2] <- gsub(" and .*", "", output[i,2])
  }
  if (i >= nrow(output)){break}
}

# Re-joining suffixes separated by commas
for (i in 1:nrow(output)){
  if (output[i,2] %in% c("Jr.", "Jr.,","III","II")){
    output[i-1,2] <- paste(output[i-1,2],output[i,2], sep=" ")
    output <- output[-i,]
  } else if (output[i,2] %in% c("and Jr.")){
    output <- rbind(output,c(output[i,1],paste(output[i-1,2]," Jr.",sep=""),output[i,3]))
    output <- output[-i,]
  } # special case of "and Jr."
  if (i >= nrow(output)){break}
}

# Removing commas at the ends of records and "ands" at the beginnings
for (i in 1:nrow(output)){
  output[i,2] <- gsub(",$|^and","",output[i,2])
}

# Manual additions
# 805
output <- rbind(output,c(805,"Colin R. Mason","CORDA Ltd."))
output <- rbind(output,c(805,"James Moffat","CORDA Ltd."))

# 1503
output <- rbind(output,c(1503,"Lawrence I. Goldman","Decisioneering, Inc."))
output <- rbind(output,c(1503,"Ethan Evans-Hilton","Decisioneering, Inc."))
output <- rbind(output,c(1503,"Hilary Emmett","Decisioneering (UK) Ltd."))

# 1624
output <- rbind(output,c(1624,"Hongwei Ding","INRIA-Lorraine (The French National Institute for Research in Computer Science & Control)"))
output <- rbind(output,c(1624,"Lyès Benyoucef","INRIA-Lorraine (The French National Institute for Research in Computer Science & Control)"))
output <- rbind(output,c(1624,"Xiaolan Xie","INRIA-Lorraine (The French National Institute for Research in Computer Science & Control)"))

# 2380
output <- rbind(output,c(2380,"Samuel Frimpong Boateng","Global Investments and Corporate Solutions (UK) Limited"))

# 5166
output <- rbind(output,c(5166,"Atanu Mukherjee","M.N. Dastur & Company (P) Ltd."))
output <- rbind(output,c(5166,"Arindam Som","M.N. Dastur & Company (P) Ltd."))
output <- rbind(output,c(5166,"Arnab Adak","M.N. Dastur & Company (P) Ltd."))
output <- rbind(output,c(5166,"Prateek Raj","M.N. Dastur & Company (P) Ltd."))
output <- rbind(output,c(5166,"Swarnendu Kirtania","M.N. Dastur & Company (P) Ltd."))

# 5475
output <- rbind(output,c(5475,"José Arnaldo B. Montevechi","Federal University of Itajubá (UNIFEI)"))
output <- rbind(output,c(5475,"J. Daniel Friend","Federal University of Itajubá (UNIFEI)"))

# 5847
output <- rbind(output,c(5847,"Atanu Mukherjee","M.N. Dastur & Company (P) Ltd"))
output <- rbind(output,c(5847,"Arindam Som","M.N. Dastur & Company (P) Ltd"))
output <- rbind(output,c(5847,"Arnab Adak","M.N. Dastur & Company (P) Ltd"))

# 6698
output <- rbind(output,c(6698,"Jean Wery","FORAC Research Consortium, Universite Laval"))
output <- rbind(output,c(6698,"Philippe Marier","FORAC Research Consortium, Universite Laval"))
output <- rbind(output,c(6698,"Jonathan Gaudreault","FORAC Research Consortium, Universite Laval"))
output <- rbind(output,c(6698,"Corinne Chabot","Centre de Recherche Industrielle du Québec (CRIQ)"))
output <- rbind(output,c(6698,"André Thomas","Centre de Recherche en Automatique de Nancy (CRAN) Université de Lorraine"))

# Remove whitespace
for (i in 1:ncol(output)){
  output[i] <- trimws(output[i])
}

output2 <- data.frame(output,stringsAsFactors = FALSE)
output2$Author <- trimws(output2$Author)
output2$Affiliation <- trimws(output2$Affiliation)
output2$Author <- gsub("[ ]{2,}", " ", output2$Author)
output2$Affiliation <- gsub("[ ]{2,}", " ", output2$Affiliation)
output2$Author <- gsub("(.{3,}) [A-Z]{1}\\.? ", "\\1 ",output2$Author)
output2$PaperID <- as.integer(output2$PaperID)
output2 <- output2[order(output2$PaperID),]

# Manual cleanup of affiliations and names

output2$Affiliation <- gsub("Institue|Institutre","Institute", output2$Affiliation)
output2$Affiliation <- gsub("Ubiversity|Unversity|Univesity|Unviesity|Universtiy|Universtity|Univeristy",
                            "University", output2$Affiliation)
output2$Affiliation <- gsub("^The ","", output2$Affiliation)
output2$Affiliation <- gsub("^Department of.*, |^Dept of.*, ","", output2$Affiliation)
output2$Affiliation <- gsub("([A-Za-z])University","\\1 University", output2$Affiliation)
output2$Affiliation <- gsub(" of([A-Z])"," of \\1", output2$Affiliation)
output2$Affiliation <- gsub(", College of.*|, Department.*","", output2$Affiliation)
output2$Author <- gsub("’","'", output2$Author)


output2$Affiliation <- gsub(".* Northwestern University", "Northwestern University", output2$Affiliation)
output2$Affiliation <- gsub("Georgia Tech|.*Georgia Ins.*", "Georgia Institute of Technology", output2$Affiliation)
output2$Affiliation[!(grepl("Lawrence",output2$Affiliation))] <- 
  gsub("(.*Berkeley)", "University of California at Berkeley",
       output2$Affiliation[!(grepl("Lawrence",output2$Affiliation))])
output2$Affiliation <- gsub("NC State University|North Carolina StateUniversity|NC State Univ|NCSU", 
                            "North Carolina State University",output2$Affiliation)
output2$Affiliation <- gsub(".*Rostock", "University of Rostock", output2$Affiliation)
output2$Affiliation <- gsub(".*Arizona State.*|^ASU$", "Arizona State University", output2$Affiliation)
output2$Affiliation <- gsub(".*Delft.*", "Delft University of Technology", output2$Affiliation)
output2$Affiliation <- gsub("PurdueUniversity|Purdue University School of Industrial Engineering",
                            "Purdue University", output2$Affiliation)
output2$Affiliation <- gsub("^Stanford$", "Stanford University", output2$Affiliation)
output2$Affiliation <- gsub(".*University of Maryland.*", "University of Maryland", output2$Affiliation)
output2$Affiliation <- gsub(".*Averill.*", "Averill M. Law & Associates", output2$Affiliation)
output2$Affiliation <- gsub(".*Wolverine.*", "Wolverine Software Corporation", output2$Affiliation)
output2$Affiliation <- gsub(".*Tata Institute.*", "Tata Institute of Fundamental Research", output2$Affiliation)
output2$Affiliation <- gsub(".*Naval Post.*", "Naval Postgraduate School", output2$Affiliation)
output2$Affiliation <- gsub(".*Université de Montréal.*|.*Universite de Montreal.*|UniversitÃ© de MontrÃ©al|Universit� de Montréal",
                            "University of Montreal", output2$Affiliation)
output2$Affiliation <- gsub(".*Cornell University.*", "Cornell University", output2$Affiliation)
output2$Affiliation <- gsub(".*Ann Arbor.*", "University of Michigan", output2$Affiliation)
output2$Affiliation <- gsub("Virginia Tech.", "Virginia Tech", output2$Affiliation)
output2$Affiliation <- gsub(".*central Florida.*|.*Central Florida.*", "University of Central Florida", output2$Affiliation)
output2$Affiliation <- gsub(".*Brunel University.*", "Brunel University", output2$Affiliation)
output2$Affiliation <- gsub(".*Ross School of Business.*", "University of Michigan", output2$Affiliation)


# Invidiual Authors/Affiliations
output2$Author <- gsub("Barry Lee Nelson", "Barry Nelson", output2$Author)
output2$Affiliation <- gsub("Department of Industrial Engineering and Management Sciences", "Northwestern University", output2$Affiliation)
output2$Author <- gsub("WIlson", "Wilson", output2$Author)
output2$Author[!(grepl("Paul Goldsman",output2$Author))] <- 
  gsub(".*Goldsman.*", "David Goldsman",
       output2$Author[!(grepl("Paul Goldsman",output2$Author))])
output2$Author <- gsub(".*Uhrmacher.*", "Adelinde Uhrmacher", output2$Author)
output2$Author <- gsub(".*Averill.*", "Averill Law", output2$Author)
output2$Author <- gsub(".*Schruben.*", "Lee Schruben", output2$Author)

counts <- count(output2[,-1])
counts <- counts[order(-counts$freq),]
rownames(counts) <- NULL

temp <- unique(output2[,c(1,3)])
counts2 <- count(output2[,"Author"])
counts2 <- counts2[order(-counts2$freq),]
counts3 <- count(temp[,"Affiliation"])
counts3 <- counts3[order(-counts3$freq),]
rownames(counts2) <- NULL
rownames(counts3) <- NULL
write.csv(counts3, file="Affiliations.csv")
write.csv(counts2[1:10,], file="Authors.csv")

rownames(output2) <- NULL # Re-number each record  

# write.xlsx(output2,file="Authors, Papers, and Affiliations.xlsx")

# Make authors/papers table by extracting unique values

authorsaffiliations <- as.matrix(unique(output2[c("Author","Affiliation")]))
AuthorAffiliationID <- seq(1:nrow(authorsaffiliations))
authorsaffiliations <- cbind(AuthorAffiliationID,authorsaffiliations)
rownames(authorsaffiliations) <- NULL # Re-number each record  
authorsaffiliations2 <- data.frame(authorsaffiliations,stringsAsFactors = FALSE)

output3 <- merge(output2,authorsaffiliations2)
output3 <- output3[c("PaperID","AuthorAffiliationID")]

write.xlsx(output3,file="Papers and Author-Affiliations.xlsx")

# Make authors table by extracting unique values

authors <- as.matrix(unique(authorsaffiliations2["Author"]))
AuthorID <- seq(1:nrow(authors))
authors <- cbind(AuthorID,authors)
rownames(authors) <- NULL # Re-number each record  
authors2 <- data.frame(authors,stringsAsFactors = FALSE)

write.xlsx(authors2,file="Authors.xlsx")

# Make affiliations table by extracting unique values

affiliations <- as.matrix(unique(authorsaffiliations2["Affiliation"]))
AffiliationID <- seq(1:nrow(affiliations))
affiliations <- cbind(AffiliationID,affiliations)
rownames(affiliations) <- NULL # Re-number each record  
affiliations2 <- data.frame(affiliations,stringsAsFactors = FALSE)

write.xlsx(affiliations,file="Affiliations.xlsx")

authorsaffiliations3 <- merge(authorsaffiliations2,authors2)
authorsaffiliations3 <- merge(authorsaffiliations3,affiliations2)
authorsaffiliations3 <- authorsaffiliations3[,3:5]

write.xlsx(authorsaffiliations3,file="Author-Affiliations.xlsx")

unnormalized <- merge(x = simuldata, y = output3)
unnormalized2 <- merge(x = unnormalized, y = authorsaffiliations3)
unnormalized3 <- merge(x = unnormalized2, y = authors2)
unnormalized4 <- merge(x = unnormalized3, y = affiliations2)
unnormalized4 <- unnormalized4[,-c(1:3)]


write.xlsx(unnormalized4,file="Denormalized Papers Data.xlsx")

simuldata2 <- data.frame(simuldata)


# Writing to a database
# 
# drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, host="gpdb", user="abhattacharya", 
#                   dbname="scratch", pass="",port=5432)
#  
# dbWriteTable(con, value = simuldata2, name = "Papers", append = TRUE)
# dbWriteTable(con, value = output2, name = "Authors_Papers", append = TRUE)
# dbWriteTable(con, value = authorsaffiliations3, name = "Authors_Affiliations", append = TRUE)
# dbWriteTable(con, value = authors2, name = "Authors", append = TRUE)
# dbWriteTable(con, value = affiliations2, name = "Affiliations", append = TRUE)
