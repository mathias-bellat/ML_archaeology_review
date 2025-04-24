####################################################################
# This script is for Filtering the pdf files extracted for         #
# statistical analysis                                             #                                                         
#                                                                  #                                                  
# Author: Mathias Bellat                                           #
# Affiliation : Tuebingen University                               #
# Creation date : 12/02/2024                                       #
# E-mail: mathias.bellat@uni-tubingen.de                           #
####################################################################


# 00 Preparation ---------------------------------------------------------------

# Folder check
getwd()

# Clean up workspace
rm(list = ls(all.names = TRUE))

# Load packages
install.packages("pacman")
library(pacman) #Easier way of loading packages
pacman::p_load(dplyr, readr, ggalluvial, stringr, plyr, tibble, rnaturalearth, RColorBrewer) # Specify required packages and download it if needed

#Show session infos
sessionInfo()

# 01 Import data sets ----------------------------------------------------------
# 01.1 Import the metada infos ################################################

info <- read_delim("./Data/ML_archaeology_metadata.csv", delim = ";")
head(info)

# 01.2 Import the observations #################################################
obs <- read_delim("./Data/ML_archaeology_info.csv", delim = ";", na = "")
head(obs)

# 01.3 Remove non reviewed papers ##############################################

# Reviewed and not reviewed papers, one papers might have several studies
not_review <- subset(obs, obs$Included!= "Yes")
review <- subset(obs, obs$Included == "Yes")

# Merge with metadata
merge <- merge(info, not_review, by = "ID")

# Select papers which have several studies to not remove them in case one of their study do fit review protocol
df.1 <- merge[grep("-1", merge$Name.y, ignore.case = TRUE ),]
df.2 <- merge[grep("-2", merge$Name.y, ignore.case = TRUE ),]
df <- rbind(df.1, df.2)

# Select the papers in question and unlist them from the papers to remove
ID <- c(df$ID)
not_review <- merge[!merge$ID %in% ID, ]
head(not_review)

# Create the reviewed metadata file
ID <- c(not_review$ID)
metadata <- info[!info$ID %in% ID, ]

# Show how many papers were included in the review and how many not

# Reviewed paper
nrow(metadata)
# Number of studies
nrow(review)
# Not reviewed paper
nrow(not_review)

# 02 Basics statistics ---------------------------------------------------------
# 02.1 Year of publication graph ###############################################
pub <- table(metadata$Year)
pub <- as.data.frame(pub)
colnames(pub) <- c("year","Freq")
pub$year <- as.numeric(as.character(pub$year))

df1 <- join(data.frame(year = 1997:2022), pub)  #Left join df years to all years 
df1[is.na(df1$Freq), "Freq"] <- 0

# Create the plot
plot <- ggplot(subset(df1, Freq != 0), aes(x = year, y = Freq))+
  geom_rect( xmin = 2019, xmax = 2022,  ymin = -Inf,
             ymax = Inf,  fill = "lightblue",  alpha = 0.03) +
  coord_cartesian(xlim =c(1997, 2023), ylim = c(0, 50)) +
  geom_line(color = "black", linewidth = 0.75) +  labs(x = "Years", y = paste0("Number of publications (n = ", sum(df1$Freq),")")) +
  geom_text(aes(label = round(Freq, 2)), vjust = -0.95, hjust = 0.95) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  scale_x_continuous(breaks = seq(2000, max(df1$year), by = 5)) +
  theme_classic()

# Check the plot
plot 

# Export the plot  
ggsave("./Export/Graph/Figure_03.png", plot = plot, width = 7, height = 6, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_03.pdf", plot = plot, width = 7, height = 6, units = "in")

# 02.2 Country list ############################################################
countries <- table(metadata$`Country of affiliation`)
countries <- as.data.frame(countries)
countries$Var1 <- as.character(countries$Var1)

# Get world map data
world <- merge(ne_countries(), countries, by.x = "iso_a3_eh", by.y = "Var1", all.x = TRUE)

# remove antarctica
world <- world[!world$iso_a3 %in% c("ATA"),]

# Manually specify breaks for creating categories
custom_breaks <- c(1, 3, 5, 10, 20, max(na.omit(world$Freq)))  # Adjust the breaks as needed
world$categories <- cut(world$Freq, breaks = custom_breaks, include.lowest = TRUE)
red_palette <- brewer.pal(5, "Reds")

# Create the plot
plot <- ggplot() +
  geom_sf(data = world, aes(fill = categories)) +
  labs(fill =  paste0("Number of publications \n per countires (n = ",nrow(metadata),")")) +
  scale_fill_manual(values = red_palette,na.value = "white",  labels=c("< 3", "3 < 5", "5 < 10", "10 < 20"," > 20", "NAs"))  +  # Adjust the color palette as needed
  theme_void() +
  theme(legend.position = c(0.05, 0.25),  # Place legend at the bottom
    legend.justification = "left",  # Center the legend
    legend.box.just = "left",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"))# Center the legend box

# Check the plot
plot 

# Export the plot  
ggsave("./Export/Graph/Figure_04.png", plot = plot, width = 16, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_04.pdf", plot = plot, width = 16, height = 10, units = "in")

# 02.3 Most prolific authors (not included in the article) #####################
# Prepare the authors dataset
authors <- as.data.frame(metadata$Authors)
authors <- authors %>% add_column(pest_matrix = authors$`metadata$Authors` %>% str_split(';', simplify = T))
authors <- as.data.frame(authors$pest_matrix)

# Create a function to remove all blank space
remove_spaces <- function(x) {
  str_replace_all(x, " ", "")
}
authors <- authors %>% mutate_all(.funs = remove_spaces)

# Split the column of authors to have only one main column
for (i in 1:length(authors)) {
  x <- as.data.frame(authors[i])
  x <- x %>% add_column(pest_matrix = x[[1]] %>% str_split(',', simplify = T))
  x <- as.data.frame(x[,2])
  authors[[i]] <- x[[1]]
}

# Create a data frame with each unique authors participation
authorsFinal <- as.data.frame(authors[[1]])
for (i in 2:length(authors)) {
  x <- as.data.frame(authors[[i]])
  colnames(x) <- colnames(authorsFinal)
  authorsFinal <- rbind(authorsFinal, x)
}

# Create a data frame with the frequencies for each authors
authorsFinal[[1]][authorsFinal[[1]] ==""] <- NA
authorsFinal <- na.omit(authorsFinal)
freq <- table(authorsFinal)
freq_df <- as.data.frame(freq)

# Show the first 6 more prolific authors, careful "Li" refers to three different authors
head(freq_df[order(-freq_df$Freq), ])

# 02.4 Most common journals ####################################################
journals <- table(metadata$Journal)
journals <- as.data.frame(journals)
colnames(journals) <- c("journal","Freq")
journals <- journals[order(-journals$Freq),]

# Plot the top six review
head(journals)

# 02.5 Open access articles ####################################################

# Number of papers
summary(as.factor(metadata$`Open-access`))

# Ratio in purcent
(sum(metadata$`Open-access` == "Yes")/nrow(metadata))*100

# 02.6 Number of articles for each subfield of archaeology #####################
cat <- review[,c(3,7)]
cat <- cat %>% add_column(pest_matrix = cat$Subfield %>% str_split(';', simplify = T))
cat_full <- as.data.frame(cat$pest_matrix)
cat <- cbind(cat[,c(1:2)], cat_full)

# Create separation function
separate <- function(df, A, B) {
  for (i in 1:nrow(df)) {
    for (j in A:B) {
      y <- df[i, ]
      y[ , A:B] <- NA  # Corrected indexing for columns
      x <- df[i, j]
      y[A] <- ifelse(is.na(x), y[j], x)
      df <- rbind(df, y)
    }
  }
  return(df)
} 

# Split the column to have every values as a frequency table
hist <- separate(cat,3,6)
hist <- hist[c(nrow(cat)+1:nrow(hist)),c(1,3)]
hist$V1[hist$V1 =="Theory"] <- NA
hist$V1[hist$V1 ==""] <- NA
hist$V1[hist$V1 =="NA"] <- NA

hist <- na.omit(hist)

# Convert as number date
hist$Year <- as.numeric(hist$Year)

# Create the frenquence table
freq_table <- table(hist$Year, hist$V1)
freq_df <- as.data.frame(freq_table)
colnames(freq_df) <- c("year", "category", "Freq")

# Remove absence of data
freq_df <- freq_df[freq_df$Freq > 0,]

# Convert the date to number
freq_df$year <- as.numeric(as.character(freq_df$year))

# Convert the categories from factors to characters
freq_df$category <- as.character(freq_df$category)

# Blindfold colors
color <- c('#00429d', '#6a4285', '#8e4575', '#a74d6b', '#b95967', '#c76767', '#d2776b', '#d88974', '#db9c80', '#daaf90', '#d4c4a3', '#c7d8ba', '#edded8', '#feecdb', '#ffffe0')

# Create the plot
plot <- ggplot(freq_df, aes(x=year, y=Freq, fill = category)) +
  geom_bar(stat = "identity", colour="white", width= 0.9, cex = 0.1) +
  geom_text(aes(x=year, y = Freq, label = Freq), vjust = -0.5, col = "white")+
  scale_fill_manual(values = color) + # Apply the custom color palette
  labs(x = "Year", y = paste0("Number of observations (n =", sum(freq_df$Freq),")"), fill = "Archaeological categories") +
  coord_cartesian(xlim =c(1997, 2022), ylim = c(0, 75)) +
  theme_bw()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) 

# Plot
plot

# Export plot
ggsave("./Export/Graph/Figure_05.png", plot = plot, width = 15, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_05.pdf", plot = plot, width = 15, height = 10, units = "in")


# 02.7 Number of articles for each families of models #####################
cat <- review[,c(3,6)]
cat <- cat %>% add_column(pest_matrix = cat$Family %>% str_split(';', simplify = T))
cat_full <- as.data.frame(cat$pest_matrix)
cat <- cbind(cat[,c(1:2)], cat_full)

# Split the column to have every values as a frequency table
family <- separate(cat,3,8)
family <- family[c(nrow(cat)+1:nrow(family)),c(1,3)]
family$V1[family$V1 ==""] <- NA
family$V1[family$V1 =="N/A"] <- NA

family <- na.omit(family)

# Convert as number date
family$Year <- as.numeric(family$Year)

# Create the frenquence table
freq_table <- table(family$Year, family$V1)
freq_df <- as.data.frame(freq_table)
colnames(freq_df) <- c("year", "family", "Freq")

# Remove absence of data
freq_df <- freq_df[freq_df$Freq > 0,]

# Convert the date to number
freq_df$year <- as.numeric(as.character(freq_df$year))

# Convert the categories from factors to characters
freq_df$category <- as.character(freq_df$family)

# Blindfold colors
color <- c('#9F0162', '#009F81', '#FF5AAF', '#00FCCF', '#8400CD', '#008DF9', '#00C2F9', '#FFB2FD', '#FF6E3A')

# Create the plot
plot <- ggplot(freq_df, aes(x=year, y=Freq, fill = family)) +
  geom_bar(stat = "identity", colour="white", width= 0.9, cex = 0.1) +
  geom_text(aes(x=year, y = Freq, label = Freq), vjust = -0.5, col = "white")+
  scale_fill_manual(values = color) + # Apply the custom color palette
  labs(x = "Year", y = paste0("Number of observations (n =", sum(freq_df$Freq),")"), fill = "Architectures categories") +
  coord_cartesian(xlim =c(1997, 2022), ylim = c(0, 75)) +
  theme_bw()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) 

# Plot
plot

# Export plot
ggsave("./Export/Graph/Figure_06.png", plot = plot, width = 15, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_06.pdf", plot = plot, width = 15, height = 10, units = "in")


# 02.8 Number of articles for each type of input data ##########################
cat <- review[,c(3,8)]
cat <- cat %>% add_column(pest_matrix = review$`Input data` %>% str_split(';', simplify = T))
cat_full <- as.data.frame(cat$pest_matrix)
cat <- cbind(cat[,c(1:2)], cat_full)

# Split the column to have every values as a frequency table
input <- separate(cat,3,4)
input <-input[c(nrow(cat)+1:nrow(input)),c(1,3)]
input$V1[input$V1 ==""] <- NA
input$V1[input$V1 =="Theory"] <- NA
input <- na.omit(input)
input <- as.data.frame(table(input$V1))
input$Var1 <- as.character(input$Var1)

# Remove under represented categories
input <- input[order(-input$Freq), ]
top5 <- input[c(1:5), ]
other_sum <- sum(input$Freq[6:nrow(input)])
input <- rbind(top5, data.frame(Var1 = "Others", Freq = other_sum))
input$Var1 <- factor(input$Var1, levels = input$Var1)
print(input)

# Blindfold colors
color <- c('#9F0162', '#009F81', '#FF5AAF', '#008DF9', '#FF6E3A', 'darkgray')


# Barplot of the input
plot <- ggplot(input, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = color) +
  labs(x = "Input categories", y = paste0("Number of observations (n =", sum(input$Freq),")")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  theme_classic() +
 theme(axis.ticks.x = element_blank())
  

# Plot
plot

# Export plot
ggsave("./Export/Graph/Figure_07.png", plot = plot, width = 6, height = 5, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_07.pdf", plot = plot, width = 6, height = 5, units = "in")

# 02.9 Number of results categories ############################################
results <- as.data.frame(table(review$Results))

# Compute percentages
results$fraction <- results$Freq / sum(results$Freq)

# Compute the cumulative percentages (top of each rectangle)
results$ymax <- cumsum(results$fraction)

# Compute the bottom of each rectangle
results$ymin <- c(0, head(results$ymax, n=-1))

# Compute label position
results$labelPosition <- (results$ymax + results$ymin) / 2

# Compute a good label
results$label <- paste0(results$Var1, "\n ", round(results$fraction*100, digits = 1), "%")

# Blindfold colors
color <- c('#D81B60', '#1E88E5', 'darkgray','#004D40', '#FFC107')

# Make the plot
plot <- ggplot(results, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  geom_label( x=4.25, aes(y=labelPosition, label=label), size=4) +
  scale_fill_manual(values = color) +
  coord_polar(theta="y") +
  xlim(c(2, 4.2)) +
  theme_void() +
  theme(legend.position = "none")

# Plot
plot

# Export plot
ggsave("./Export/Graph/Figure_08.png", plot = plot, width = 6, height = 5, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_08.pdf", plot = plot, width = 6, height = 5, units = "in")

# 03 Subfield categories alluvial diagram --------------------------------------
# 03.1 Function to concatenate the columns #####################################
separate <- function(df, A, B) {
  for (i in 1:nrow(df)) {
    for (j in A:B) {
      y <- df[i, ]
      y[ , A:B] <- NA  # Corrected indexing for columns
      x <- df[i, j]
      y[A] <- ifelse(is.na(x), y[j], x)
      df <- rbind(df, y)
    }
  }
  return(df)
} 

# 03.2 Archaeological categories concatenate ###################################

# Split the categories column by semicolon
cat <- review[,c(10:11,7)]
cat <- cat %>% add_column(pest_matrix = cat$Subfield %>% str_split(';', simplify = T))
cat_full <- as.data.frame(cat$pest_matrix)
cat_full <- cbind(cat[,c(1,2)], cat_full)


full_first <- separate(cat_full,3,6)
full_first <- full_first[-c(1:nrow(cat_full)),]
full_first <- full_first[,-c(4:6)]

# Replace white spaces NA, theory and non ML methods
full_first[,3][full_first[,3] == ""] <- NA
full_first[,3][full_first[,3] == "NA"] <- NA
full_first[,3][full_first[,3] == "Theory"] <- NA

# Remove absence of case
final <- full_first[complete.cases(full_first[,3]), ]
colnames(final) <- c("Evaluation", "Task","Category")

write.csv(final, "./Export/first_alluvial.csv", fileEncoding = "UTF-8")

# 03.3 Remove under represented tasks ##########################################

final <- as.data.frame(final)
frequency_table <- table(final$Task)
frequency_df <- as.data.frame(frequency_table)

# Define the number
x <- 5
list <- subset(frequency_df, frequency_df$Freq < x)
old <- as.vector(list$Var1)
for (i in 1:nrow(final)) {
  for (j in 1:length(old)) {
    ifelse(final[i,"Task"] == old[j], final[i,"Task"] <- "Others", final[i,"Task"] <- final[i,"Task"])
  }
}

# 03.4 Remove under represented archaeological categories ######################
frequency_table <- table(final$Category)
frequency_df <- as.data.frame(frequency_table)

x <- 5
list <- subset(frequency_df, frequency_df$Freq < x)
old <- as.vector(list$Var1)
for (i in 1:nrow(final)) {
  for (j in 1:length(old)) {
    ifelse(final[i,"Category"] == old[j], final[i,"Category"] <- "Others", final[i,"Category"] <- final[i,"Category"])
  }
}

# 03.5 Plot the first alluvial diagram #########################################
frequency_table <- table(final$Evaluation, final$Category, final$Task)

# Convert the frequency table to a data frame
frequency_df <- as.data.frame(frequency_table)
frequency <- frequency_df[frequency_df$Freq > 0,]
colnames(frequency) <- c("Evaluation","Category","Task","freq")


# Plot from Task to Categories with Evaluation in background
plot <- ggplot(data = frequency,
               aes(axis1 = Task, axis2 = Category,  y = freq)) +
  geom_alluvium(aes(fill = Evaluation),
                curve_type = "sigmoid") +
  geom_stratum(aes(fill = Evaluation), col = "black", fill="lightgrey") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 4) +
  
  scale_x_discrete(limits = c("Task","Caegories"),
                   expand = c(0.15, 0.05)) +
  labs(fill = paste0("Counted observations (n = ", sum(frequency$freq), ")")) +
  theme_void()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(), legend.text = element_text(size = 14)) 

# Plot the graph
plot

ggsave("./Export/Graph/Figure_09.png", plot = plot, width = 12, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_09.pdf", plot = plot, width = 12, height = 10, units = "in")



# 04 Families categories alluvial diagram ---------------------------------
# 04.1 Families categories concatenate ####################################
# Split the categories column by semicolon
cat <- review[,c(10:11,6)]
cat <- cat %>% add_column(pest_matrix = cat$Family %>% str_split(';', simplify = T))
cat_full <- as.data.frame(cat$pest_matrix)
cat_full <- cbind(cat[,c(1,2)], cat_full)

full_first <- separate(cat_full,3,8)
full_first <- full_first[-c(1:nrow(cat_full)),]
full_first <- full_first[,-c(4:8)]

# Replace white spaces NA, theory and non ML methods
full_first[,3][full_first[,3] == ""] <- NA
full_first[,3][full_first[,3] == "NA"] <- NA
full_first[,3][full_first[,3] == "Theory"] <- NA
full_first[,3][full_first[,3] == "N/A"] <- NA

# Remove absence of case
final <- full_first[complete.cases(full_first[,3]), ]
colnames(final) <- c("Evaluation", "Task","Architecture")

write.csv(final, "./Export/second_alluvial.csv", fileEncoding = "UTF-8")

# 04.2 Remove under represented tasks ##########################################
final <- as.data.frame(final)
frequency_table <- table(final$Task)
frequency_df <- as.data.frame(frequency_table)

# Define the number
x <- 5
list <- subset(frequency_df, frequency_df$Freq < x)
old <- as.vector(list$Var1)
for (i in 1:nrow(final)) {
  for (j in 1:length(old)) {
    ifelse(final[i,"Task"] == old[j], final[i,"Task"] <- "Others", final[i,"Task"] <- final[i,"Task"])
  }
}

# 04.3 Remove under represented archaeological categories ######################
frequency_table <- table(final$Architecture)
frequency_df <- as.data.frame(frequency_table)

x <- 5
list <- subset(frequency_df, frequency_df$Freq < x)
old <- as.vector(list$Var1)
for (i in 1:nrow(final)) {
  for (j in 1:length(old)) {
    ifelse(final[i,"Architecture"] == old[j], final[i,"Architecture"] <- "Others", final[i,"Architecture"] <- final[i,"Architecture"])
  }
}

# 04.4 Plot the second alluvial diagramm ######################################
frequency_table <- table(final$Evaluation, final$Architecture, final$Task)

# Convert the frequency table to a data frame
frequency_df <- as.data.frame(frequency_table)
frequency <- frequency_df[frequency_df$Freq > 0,]
colnames(frequency) <- c("Evaluation","Architecture","Task","freq")


# Plot from Task to Categories with Evaluation in background
plot <- ggplot(data = frequency,
               aes(axis1 = Task, axis2 = Architecture,  y = freq)) +
  geom_alluvium(aes(fill = Evaluation),
                curve_type = "sigmoid") +
  geom_stratum(aes(fill = Evaluation), col = "black", fill="lightgrey") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 4) +
  
  scale_x_discrete(limits = c("Task","Architecture"),
                   expand = c(0.15, 0.05)) +
  labs(fill = paste0("Counted observations (n = ", sum(frequency$freq), ")")) +
  theme_void()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(), legend.text = element_text(size = 14)) 

# Plot the graph
plot

ggsave("./Export/Graph/Figure_10.png", plot = plot, width = 12, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_10.pdf", plot = plot, width = 12, height = 10, units = "in")

# 05 Results categories alluvial diagram ---------------------------------------

# 05.1 Remove under represented tasks ##########################################
final <- review[,c(10:12)]
frequency_table <- table(final$Task)
frequency_df <- as.data.frame(frequency_table)

# Define the number
x <- 5
list <- subset(frequency_df, frequency_df$Freq < x)
old <- as.vector(list$Var1)
for (i in 1:nrow(final)) {
  for (j in 1:length(old)) {
    ifelse(final[i,"Task"] == old[j], final[i,"Task"] <- "Others", final[i,"Task"] <- final[i,"Task"])
  }
}

# 05.2 Plot the third alluvial diagramm ########################################
frequency_table <- table(final$Evaluation, final$Results, final$Task)

# Convert the frequency table to a data frame
frequency_df <- as.data.frame(frequency_table)
frequency <- frequency_df[frequency_df$Freq > 0,]
colnames(frequency) <- c("Evaluation","Results","Task","freq")


# Plot from Task to Categories with Evaluation in background
plot <- ggplot(data = frequency,
               aes(axis1 = Task, axis2 = Results,  y = freq)) +
  geom_alluvium(aes(fill = Evaluation),
                curve_type = "sigmoid") +
  geom_stratum(aes(fill = Evaluation), col = "black", fill="lightgrey") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 4) +
  
  scale_x_discrete(limits = c("Task","Results"),
                   expand = c(0.15, 0.05)) +
  labs(fill = paste0("Counted observations (n = ", sum(frequency$freq), ")")) +
  theme_void()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(), legend.text = element_text(size = 14)) 

# Plot the graph
plot

ggsave("./Export/Graph/Figure_11.png", plot = plot, width = 12, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_11.pdf", plot = plot, width = 12, height = 10, units = "in")
