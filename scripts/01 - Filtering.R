####################################################################
# This script is for Filtering the pdf files extracted for         #
# text analysis                                                    #                                                         
#                                                                  #                                                  
# Author: Mathias Bellat                                           #
# Affiliation : Tübingen University                                #
# Creation date : 12/02/2024                                       #
# E-mail: mathias.bellat@uni-tuebingen.de                          #
####################################################################

# 00 Preparation ###############################################################

# 0.1 Prepare folder ===========================================================

# Folder check
getwd()

# Set folder
setwd()

# Clean up workspace
rm(list = ls(all.names = TRUE))

# 0.2 Load packages ============================================================
install.packages("pacman")
library(pacman) #Easier way of loading packages
pacman::p_load(dplyr, tm, tidyr, tibble, stringr, readr) # Specify required packages and download it if needed

# 0.3 Session info =============================================================

sessionInfo()

# 01 Convert PDF ###############################################################
# 01.1 Select the path and the files ===========================================
path  <- "./Data/pdf_full" #Select the location of PDFs files
files <- list.files(path = "./Data/pdf_full/", pattern = "pdf")  #Make a vector of PDFs in the folder files not included

xpdf <- "C:/Program Files/xpdf-tools-win-4.05/bin64/pdftotext.exe"  #Path for XPDF tool accessible at (https://www.xpdfreader.com/download.html)


# 01.2 Transform and export in the same folder as the pdf ======================
for (i in 1:length(files)){
  pdf <- file.path(path, files[i])
  system(paste("\"",xpdf,"\" \"", pdf, "\"", sep =""), wait = FALSE)
  }

rm(list=ls())

# 02 Prepare the txt files #####################################################
# 02.1 Import the data under Corpus file =======================================
corpus <- VCorpus(DirSource(directory = "./Data/txt_full/",
                            pattern = ".txt"))  #Corpus from the package files not included

# 02.2 Clean the elements ======================================================
df <- corpus

# Function to remove element and replace it by blank space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Remove some characters
df <- tm_map(df, toSpace, "/")
df <- tm_map(df, toSpace, "@")
df <- tm_map(df, toSpace, "\\|")

# Put all tall letters to smaller
df <- tm_map(df, content_transformer(tolower)) 

# Remove numbers
df <- tm_map(df, removeNumbers)
df <- tm_map(df, removePunctuation)
df <- tm_map(df, stripWhitespace)

# 02.3 Put into data frame and export ==========================================

# Create a data frame with eh text and the ID of text(file name)
data <- data.frame(id=sapply(df, meta, "id"),
                     text=unlist(lapply(sapply(df, '[', "content"),paste,collapse="\n")),
                     stringsAsFactors=FALSE) 

# Remove page number
data$text <- gsub("[\r\n]", "", data$text)

# Check the first element
data$text[1]
write.table(data, "./Export/pre_process/export_data.txt", sep = ";", row.names = FALSE , col.names = FALSE)

# 02.4 Remove reference part ===================================================

# For separating the different part of the file and removing the "reference part"
remove_references <- function(x){
  z <- as.data.frame(x[,1]) 
  z$text <-x[,2]
  z$check <- grepl("introduction", z$text)    # Look for introduction word
  
  z$number <- ifelse(z$check == TRUE, z$number <- 3 , z$number <- 0)
  
  y <- z
  y$check <- grepl("abstract", y$text)    # Look for abstract word
  y$number <- ifelse(y$check == TRUE, y$number <- 2, y$number <- 0 )
  z$number <- ifelse(z$number == 3, z$number <- 3, z$number <- y$number)
  
  y$check <- grepl("doi", y$text)     # Look for doi mention
  y$number <- ifelse(y$check == TRUE, y$number <- 1, y$number <- 0 )
  z$number <- ifelse(z$number > 0, z$number <- z$number, z$number <- y$number)
  
  # Separate
  z$intro <- ifelse(z$number == 3, z$intro <- regmatches(z$text, gregexpr("(?<=introduction).*?(?=references)", z$text, perl=TRUE)), z$intro <- NA)
  z$abs <- ifelse(z$number >= 2, z$abs <- regmatches(z$text, gregexpr("(?<=abstract).*?(?=references)", z$text, perl=TRUE)), z$abs <- NA)
  z$doi <- ifelse(z$number >= 1, z$doi <- regmatches(z$text, gregexpr("(?<=doi).*?(?=references)", z$text, perl=TRUE)), z$doi <- NA)
 return(z)
}

clean <-  remove_references(data)

# Remove the ".pdf" old information to remove any issues with further analysis
clean[,1]<- gsub(".pdf", "", clean[,1]) 
colnames(clean) <- c("Id", "text", "check", "number", "intro",  "abs", "doi")

# 02.5 Export the elements =====================================================
save(list = c("clean", "corpus"), file = "./Export/pre_process/Pre-process.RData")

rm(list=ls())

# 03 Combine the different element #############################################
load("./Export/pre_process/Pre-process.RData")

intro <- subset(clean, clean$number == 3)
abs <- subset(clean, clean$number == 2)
doi <- subset(clean, clean$number == 1)

# Text can be seen as full but only filled with "character(0)" string

# 03.1 Clean the Introduction selection ========================================
intro$char <- intro$intro == "character(0)"  
intro_final <- subset(intro, char == FALSE)

intro <- subset(intro, char == TRUE)
intro$char <- intro$abs == "character(0)"
abs_final <- subset(intro, char == FALSE)
abs_final$number <- 2

intro <- subset(intro, char == TRUE)
intro$char <- intro$doi == "character(0)"
doi_final <- subset(intro, char == FALSE)
doi_final$number <- 1

no_resume <- subset(intro, char == TRUE)
no_resume$number <- 0

# 03.2 Clean the abstract selection ============================================
abs$char <- abs$abs == "character(0)"
abs_final2 <- subset(abs, char == FALSE)

abs <- subset(abs, char == TRUE)
abs$char <- abs$doi == "character(0)"
doi_final2 <- subset(abs, char == FALSE)
doi_final2$number <- 1

no_resume2 <- subset(abs, char == TRUE)
no_resume2$number <- 0

# 03.3 Clean the selection without abstract and introduction ===================
doi$char <- doi$doi == "character(0)"
doi_final3 <- subset(doi, char == FALSE)

no_resume3 <- subset(doi, char == TRUE)
no_resume3$number <- 0

# 03.4 Merge all the data ======================================================

names <- c("Id", "Full_text", "Number","Removed_ref")

intro <- intro_final[,c(1:2,4:5)] #merge the introduction selections 
colnames(intro) <- names

abs <- rbind(abs_final, abs_final2) #merge the abstract selections 
abs <- abs [,c(1:2,4,6)]
colnames(abs) <- names

doi <- rbind(doi_final, doi_final2, doi_final3) #merge the selections without abstract and introduction
doi <- doi [,c(1:2,4,7)]
colnames(doi) <- names

# Not_treated data have to analyzed again to see if "abstract" or "introduction" word are not embedded with others which can make the greplx function inefficient
# Once check and modified if necessary (remove the reference part manually) you would have to upload again later in the steps.
not_treated <- rbind(no_resume, no_resume2 , no_resume3)
not_treated <- not_treated [,c(1:2,4)]

no_doi <- subset(clean, is.na(clean$doi) == TRUE) # Look if a references part as not been spltited from an article
no_doi <- no_doi [,c(1:2,4)]
not_treated <- rbind(not_treated, no_doi)
colnames(not_treated) <- c ("Id", "Full_text", "Number") 

final_data <- rbind(intro, abs, doi) #Combine all the data in one data frame


write.table(not_treated$Id, "./Export/pre_process/not_treated.txt", sep = ";", row.names = FALSE , col.names = FALSE)

# 03.5 Add the articles with embedded or no references part =====================
# Repeat the steps 2 until 2.4 step. for the "not_treated" articles once the references have been removed manually
save(list = c("clean", "corpus", "final_data", "not_treated"), file = "./Export/pre_process/Filtered_references.RData")

rm(list=ls())

# 04 First filter ##############################################################
load("./Export/pre_process/Filtered_references.RData")

# 04.1 Import the metadata (Exported from the RIS file in Zotero in CSV format) 
Metadata <- read_delim("./Data/Metadata.csv", delim =";", na = "NA") #File not included in the data set
Metadata$File.Attachments[Metadata$File.Attachments == ""] <-NA
Metadata <- Metadata %>% drop_na(File.Attachments) #Remove the ones without Pdfs

# You need to verify that your FileAttachments columns is well written only with the file name without anything else

# 04.2 Filter only Articles from scientific journals ============================
Metadata <- subset(Metadata, Metadata$Item.Type == "journalArticle") #Select only journal articles
final_data <- subset(final_data, final_data$Id %in% Metadata$File.Attachments)#Select in the data set journal articles

write_delim(Metadata, "./Export/pre_process/Metadata_clean.csv", delim = ";")

# 04.3 Function for filtering the data =========================================
filter_func <- function(x, z){
  
  list_archeo <- c("archaeology", "archeology", "archaeological", "archeological") #List of archaeological words related
  list_ML <- c("machine learning", "ml", "deep learning", "artificial intelligence") #List of ML words related
  
  #Filter for the archaeological words
  x$check <- grepl(list_archeo[1], z)
  x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
  x$archeo_value <- x$number
  
  x$check <- grepl(list_archeo[2], z)
  x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
  x$archeo_value <- x$archeo_value + x$number
  
  x$check <- grepl(list_archeo[3], z)
  x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
  x$archeo_value <- x$archeo_value + x$number
  
  x$check <- grepl(list_archeo[4], z)
  x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
  x$archeo_value <- x$archeo_value + x$number

  
  #Filter for the machine learning words
  x$check <- grepl(list_ML[1], z)
  x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
  x$ML_value <- x$number
  
  x$check <- grepl(list_ML[2], z)
  x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
  x$ML_value <- x$ML_value + x$number
  
  x$check <- grepl(list_ML[3], z)
  x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
  x$ML_value <- x$ML_value + x$number
  
  x$check <- grepl(list_ML[4], z)
  x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
  x$ML_value <- x$ML_value + x$number

  x$filtered <- ifelse(x$archeo_value > 0 & x$ML_value > 0, x$filtered <- 1, x$filtered <- 0)  

  return(x)
}

# 04.4 Apply the filtering to the articles without references part =============
first_filtered_data <- filter_func(final_data, final_data$Removed_ref)

# 04.5 Subset the different results ============================================
# Subset the article without archeo keywords
no_archeo <- subset(first_filtered_data, first_filtered_data$archeo_value == 0)

# Subset the article without ML keywords
no_ML <- subset(first_filtered_data, first_filtered_data$ML_value == 0)

# Subset the article without either archeo or ML keywords
no_combined <- subset(first_filtered_data, first_filtered_data$filtered == 0)

# Subset the article with archeo AND ML keywords
first_filtered_data <- subset(first_filtered_data, first_filtered_data$filtered == 1)

# 04.6 Export the resuts =======================================================
write.table(no_archeo[1], "./Export/first_filter/first_filter_no_archeo.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
write.table(no_ML[1], "./Export/first_filter/first_filter_no_ML.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
write.table(no_combined[1], "./Export/first_filter/first_filter_no_combined.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
write.table(first_filtered_data[1], "./Export/first_filter/first_filtered.csv", sep = "\t", row.names = FALSE , col.names = FALSE)

save(list = c("final_data", "filter_func", "first_filtered_data","no_combined", "Metadata"), file = "./Export/first_filter/First_filter.RData")
rm(list=c(ls()))

# 05 Second filter #############################################################
load("./Export/first_filter/First_filter.RData")

# 05.1 Filter the metadata =====================================================
Metadata_first_filter <- subset(Metadata, Metadata$File.Attachments %in% first_filtered_data$Id)  #Export the metadata from the first filter
write_delim(Metadata_first_filter, "./Export/first_filter/Metadata_first_filter.csv", delim = ";")

# 05.2 Merge metadata and first filtered ones ==================================
second_filtered_data <- merge(first_filtered_data, Metadata, by.x = "Id", by.y = "File.Attachments")

# Only select articles with an abstract
second_filtered_data$Abstract.Note[second_filtered_data$Abstract.Note== ""] <-NA
second_filtered_data <- subset(second_filtered_data, is.na(second_filtered_data$Abstract.Note) == FALSE) 

# 05.3 Filter the abstract =====================================================
x <- second_filtered_data
z <- x$Abstract.Note

# Function filtering the data for the abstract
abstract_filtered_data <- filter_func(x, z)

# Subset the article without archeo keywords
no_archeo_abstract <- subset(abstract_filtered_data, abstract_filtered_data$archeo_value == 0)

# Subset the article without ML keywords
no_ML_abstract <- subset(abstract_filtered_data, abstract_filtered_data$ML_value == 0)

# Subset the article without either archeo or ML keywords
no_combined_abstract <- subset(abstract_filtered_data, abstract_filtered_data$filtered == 0)

# Subset the article with archeo AND ML keywords
abstract_filtered_data <- subset(abstract_filtered_data, abstract_filtered_data$filtered == 1)

# 05.4 Export the results ======================================================
write.table(no_archeo_abstract[1], "./Export/second_filter/Abstract_filtered_no_archeo.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
write.table(no_ML_abstract[1], "./Export/second_filter/Abstract_filtered_no_ML.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
write.table(no_combined_abstract[1], "./Export/second_filter/Abstract_filtered_no_combined.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
write.table(abstract_filtered_data[1], "./Export/second_filter/Abstract_filtered.csv", sep = "\t", row.names = FALSE , col.names = FALSE)

save(list = c("no_archeo_abstract", "filter_func",  "no_combined_abstract", "no_ML_abstract", "abstract_filtered_data"), file = "./Export/second_filter/Abstract_filtered.RData")

rm(list=c("no_archeo_abstract", "no_combined_abstract", "no_ML_abstract"))

# 05.5 Filter the title ========================================================
x <- second_filtered_data
z <- x$Title

# Function filtering the data ==================================================
title_filtered_data <- filter_func(x, z)

# Subset the article without archeo keywords
no_archeo_title <- subset(title_filtered_data, title_filtered_data$archeo_value == 0)

# Subset the article without ML keywords
no_ML_title <- subset(title_filtered_data, title_filtered_data$ML_value == 0)

# Subset the article without either archeo or ML keywords
no_combined_title <- subset(title_filtered_data, title_filtered_data$filtered == 0)

# Subset the article with archeo AND ML keywords
title_filtered_data <- subset(title_filtered_data, title_filtered_data$filtered == 1)

# 05.6 Export the results ======================================================
write.table(no_archeo_title[1], "./Export/second_filter/Title_filtered_no_archeo.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
write.table(no_ML_title[1], "./Export/second_filter/Title_filtered_no_ML.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
write.table(no_combined_title[1], "./Export/second_filter/Title_filtered_no_combined.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
write.table(title_filtered_data[1], "./Export/second_filter/Title_filtered.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
save(list = c("no_archeo_title", "no_ML_title", "no_combined_title", "title_filtered_data"), file = "./Export/second_filter/Title_filtered.RData")

rm(list=c("no_archeo_title", "no_ML_title", "no_combined_title","x","z"))

# 05.7 Combine the title and abstract filters ==================================
second_filtered_data <- rbind(title_filtered_data, abstract_filtered_data)
second_filtered_data <- second_filtered_data[c(1:5)]
second_filtered_data <- distinct(second_filtered_data) #Remove duplicates

write.table(second_filtered_data[1], "./Export/second_filter/second_filtered.txt", sep = ";", row.names = FALSE , col.names = FALSE)

# 05.8 Export the metadata with the second filter ==============================
Metadata_second_filter <- subset(Metadata, Metadata$File.Attachments %in% second_filtered_data$Id)  #Export the metadata from the first filter
write_delim(Metadata_second_filter, "./Export/second_filter/Metadata_second_filter.csv", delim = ";")

save(list = c("second_filtered_data", "Metadata_second_filter"), file = "./Export/second_filter/Final_data_automatic_process.RData")
