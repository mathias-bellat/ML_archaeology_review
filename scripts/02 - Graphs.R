####################################################################
# This script is for creating the figures and graph                #                                                         
#                                                                  #                                                  
# Author: Mathias Bellat                                           #
# Affiliation : Tuebingen University                               #
# Creation date : 12/02/2024                                       #
# E-mail: mathias.bellat@uni-turbingen.de                          #
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
pacman::p_load(dplyr, readr, ggalluvial, stringr, plyr, tibble, 
patchwork, rnaturalearth, RColorBrewer, gridExtra, treemap, knitr, kableExtra) # Specify required packages and download it if needed

# 0.3 Session info =============================================================

sessionInfo()

# 01 Import data sets ##########################################################
# 01.1 Import the metadata infos ===============================================

info <- read_delim("./data/ML_archaeology_metadata.csv", delim = ";")
head(info)

# 01.2 Import the observations =================================================
obs <- read_delim("./data/ML_archaeology_info.csv", delim = ";", na = "")
head(obs)

# 01.3 Import the models used in the papers ====================================

models <- read_delim("./data/ML_archaeology_models.csv", delim = ";", na = "")
head(models)

# 01.4 Remove non reviewed papers ==============================================

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

# 02 Basics statistics #########################################################

# 02.1 Second figure on inputs =================================================

task <- data.frame(
  individual = c("Automatic Structure Detection","Artefact Classification", "Taphonomic Classification", "Archaeological Predictive Model",
                 "Architectural Element Classification", "Data Quality Improvement", "Artefact Prediction", "Environmental Reconstruction",
                 "Text Extraction", "Archaeological Structure Classification", "Architectural Reconstruction", "Sourcing", "Chronology",
                 "Image Segmentation", "Species Classification", "Biochemical Prediction", "Dispersion Models", "Movement Recognition",
                 "Spectra Prediction"),
  group = c("Task", "Task","Task","Task","Task","Task","Task","Task", "Task","Task","Task","Task","Task","Task","Task", "Task","Task","Task","Task") ,
  value = c(38, 29, 13, 9, 9, 8, 5, 5, 5, 5, 3, 4, 3, 3, 2, 2, 2, 1, 1))

architecture <- data.frame(
  individual = c("Artificial Neural Network","Bayesian Classifier", "Nearest Neighbour Classifier", "Linear Classifier",
                 "Polynomial Classifier", "Decision Trees and Rule Induction", "Ensemble Learning", "Unsupervised Learning and Clustering",
                 "Genetic Algorithm"),
  group = c("Family", "Family","Family","Family","Family","Family","Family","Family", "Family") ,
  value = c(110, 13, 22, 27, 7, 24, 71, 12, 1))

classification <- data.frame(
  individual = c(" Archaeobotany ","Archaeological Dating", "Archaeological Excavation", "Bioarchaeology",
                 "Classification & Typology", "Cognitive Archaeology", "Conservation and Cataloguing", "Ethnoarchaeology",
                 "Experimental Archaeology", "Geoarchaeology", "Historical Archaeology", "Landscape Archaeology", "Surveying",
                 "Taphonomy", "Zooarchaeology"),
  group = c("Subfield", "Subfield","Subfield","Subfield","Subfield","Subfield","Subfield","Subfield", "Subfield","Subfield","Subfield","Subfield","Subfield","Subfield","Subfield") ,
  value = c(8, 2, 6, 14, 31, 8, 35, 2, 10, 17, 3, 20, 42, 12, 12))

evaluation <- data.frame(
  individual = c("Classifciation", "Regression", "Clustering"),
  group = c("Evaluation", "Evaluation", "Evaluation"),
  value = c(111,29,6)
)
data <- rbind(architecture, classification, task, evaluation)
data$group <- as.factor(data$group)


custom_colors <- c("Family" = "#D81B60",
                   "Subfield" = "#1E88E5",
                   "Task" = "#FFC107",
                   "Evaluation" = "#004D40")

# Function to create individual plots
create_plot <- function(data, group_name) {
  filtered_data <- data %>%
    filter(group == group_name) %>%
    arrange(desc(value))
  ggplot(filtered_data, aes(x = reorder(individual, value), y = value)) +
    geom_bar(stat = "identity", fill = custom_colors[group_name], width = 0.7) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
      axis.title.x = element_blank(),
      plot.title = element_text(size = 12, face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(title = group_name,
         y = "Count") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
}
# Create individual plots
p1 <- create_plot(data, "Family")
p2 <- create_plot(data, "Subfield")
p3 <- create_plot(data, "Task")
p4 <- create_plot(data, "Evaluation")

# Arrange plots in a 2x2 grid, pairing larger and smaller groups
arranged_plot <- grid.arrange(
  p1, p2,  # Family (9 items) with Evaluation (3 items)
  p3, p4,  # Task (19 items) with Subfield (15 items)
  ncol = 2)

ggsave("./export/graph/Figure_02_raw.pdf", arranged_plot, width = 14, height = 10)
ggsave("./export/graph/Figure_02_raw.png", arranged_plot, width = 14, height = 10)

# 02.2 Year of publication graph ===============================================
pub <- table(metadata$Year)
pub <- as.data.frame(pub)
colnames(pub) <- c("year","Freq")
pub$year <- as.numeric(as.character(pub$year))

df1 <- left_join(data.frame(year = 1997:2022), pub, by = join_by(year)) 
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
ggsave("./export/graph/Figure_03_raw.png", plot = plot, width = 7, height = 6, units = "in", dpi = 600)
ggsave("./export/graph/Figure_03_raw.pdf", plot = plot, width = 7, height = 6, units = "in")

# 02.3 Country list ============================================================
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
  labs(fill =  paste0("Number of publications \nper countires (n = ",nrow(metadata),")")) +
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
ggsave("./export/graph/Figure_04.png", plot = plot, width = 16, height = 10, units = "in", dpi = 600)
ggsave("./export/graph/Figure_04.pdf", plot = plot, width = 16, height = 10, units = "in")

# 02.4 Most prolific authors (not included in the article) =====================
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

# 02.5 Most common journals ====================================================
journals <- table(metadata$Journal)
journals <- as.data.frame(journals)
colnames(journals) <- c("journal","Freq")
journals <- journals[order(-journals$Freq),]

# Plot the top six review
head(journals)

# 02.6 Open access articles ====================================================

# Number of papers
summary(as.factor(metadata$`Open-access`))

# Ratio in purcent
(sum(metadata$`Open-access` == "Yes")/nrow(metadata))*100

# 02.7 Prepare the model graph =================================================
models <- models[-74,c(1:5)]
df_counted <- models[models[3] !=0,] # Remove models with no occurrences

names(df_counted) <- c("description", "model", "value", "value.best", "family")
df_counted$family[df_counted$family == "N/A"] <- "Statistics" 

png("./export/graph/Figure_05_raw.png", width = 1600, height = 1300)
pdf("./export/graph/Figure_05_raw.pdf", width = 16, height = 13)

tm <- treemap(df_counted,
              index = c("family", "model"),
              vSize = "value",
              type = "index",
              fontsize.labels = c(11, 15), 
              fontcolor.labels = c("white", "black"),
              bg.labels = 2,
              align.labels = list(c("left", "top"), c("center", "center")),
              palette = "Pastel1",
              title = "",
              border.col = c("black", "white"),
              border.lwds = c(2, 0))
dev.off()

# 02.8 Number of articles for each subfield of archaeology =====================
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

# Blindfold colors
color <- c('#00429d', '#6a4285', '#8e4575', '#a74d6b', '#b95967', '#c76767', '#d2776b', '#d88974', '#db9c80', '#daaf90', '#d4c4a3', '#c7d8ba', '#edded8', '#feecdb', '#ffffe0')

freq_df <- freq_df %>%
  arrange(year, category) %>%
  mutate(
    cumsum_freq = ave(Freq, year, FUN = cumsum),
    pos = cumsum_freq - 0.5 * Freq)

print(freq_df)

# Create the plot
plot1 <- ggplot(freq_df, aes(x=year, y=Freq, fill = reorder(category, -as.numeric(category)))) +
  geom_bar(stat = "identity", colour="white", width= 0.9, cex = 0.1) +
  geom_text(aes(y = pos, label = ifelse(Freq > 1, Freq, "")),
            color = "white",
            size = 4) +
  scale_fill_manual(values = color) + # Apply the custom color palette
  labs(x = "Year", y = paste0("Number of observations (n =", sum(freq_df$Freq),")"), fill = "Archaeological categories") +
  coord_cartesian(xlim =c(1997, 2022), ylim = c(0, 75)) +
  annotate("text", x = 1997, y = 75, label = "A", fontface = "bold", size = 9, hjust = 1.2, vjust = 0.5) +
  theme_bw()+
  theme(legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13, face = "bold"))

# Plot 1
plot1

# 02.9 Number of articles for each families of models ==========================
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

# Blindfold colors
color <- c('#9F0162', '#009F81', '#FF5AAF', '#00FCCF', '#8400CD', '#008DF9', '#00C2F9', '#FFB2FD', '#FF6E3A')

freq_df <- freq_df %>%
  arrange(year, family) %>%
  mutate(
    cumsum_freq = ave(Freq, year, FUN = cumsum),
    pos = cumsum_freq - 0.5 * Freq)

print(freq_df)

plot2 <- ggplot(freq_df, aes(x = year, y = Freq, fill = reorder(family, -as.numeric(family)))) +
  geom_bar(stat = "identity", colour = "white", width = 0.9) +
  geom_text(aes(y = pos, label = ifelse(Freq > 1, Freq, "")),
    color = "white",
    size = 4) +
  scale_fill_manual(values = color) +
  labs(
    x = "Year",
    y = paste0("Number of observations (n = ", sum(freq_df$Freq), ")"),
    fill = "Architectures categories"
  ) +
  coord_cartesian(xlim = c(1997, 2022), ylim = c(0, 75)) +
  annotate("text", x = 1997, y = 75, label = "B", fontface = "bold", size = 9, hjust = 1.2, vjust = 0.5) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13, face = "bold"))

# Plot 2
plot2

plot <- plot1/plot2

# Combinned plot
plot

# Export plot
ggsave("./export/graph/Figure_06_raw.png", plot = plot, width = 13, height = 17, units = "in", dpi = 600)
ggsave("./export/graph/Figure_06_raw.pdf", plot = plot, width = 13, height = 17, units = "in")


# 02.10 Number of articles for each type of input data ==========================
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
plot1 <- ggplot(input, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = color) +
  labs(x = "Input categories", y = paste0("Number of observations (n =", sum(input$Freq),")")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  annotate("text", x = 6, y = 55, label = "A", fontface = "bold", size = 9, hjust = -0.2, vjust = 0.5) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"))
  

# Plot 1
plot1

# 02.11 Number of results categories ===========================================
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
plot2 <- ggplot(results, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  geom_label(x = 4.5, aes(y=labelPosition, label=label), size=4) +
  annotate("text", x = 4.55, y = max(results$ymax), label = "B", 
           fontface = "bold", size = 9, hjust = -6, vjust = 1) +
  scale_fill_manual(values = color) +
  coord_polar(theta="y") +
  xlim(c(2, 4.6)) +
  theme_void() +
  theme(legend.position = "none")

# Plot 2
plot2

# Final plot  
plot <- plot1 + plot2 + plot_layout(widths = c(1, 0.8))
plot

# Export plot
ggsave("./export/graph/Figure_07_raw.png", plot = plot, width = 13.5, height = 5, units = "in", dpi = 600)
ggsave("./export/graph/Figure_07_raw.pdf", plot = plot, width = 13.5, height = 5, units = "in")

# 03 Families categories alluvial diagram ######################################
# 03.1 Families categories concatenate =========================================
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

write.csv(final, "./export/second_alluvial.csv", fileEncoding = "UTF-8")

# 03.2 Remove under represented tasks ==========================================
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

# 03.3 Remove under represented archaeological categories ======================
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

# 03.4 Plot the second alluvial diagramm =======================================
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

# Plot the graph/
plot

ggsave("./export/graph/Figure_08_raw.png", plot = plot, width = 12, height = 10, units = "in", dpi = 600)
ggsave("./export/graph/Figure_08_raw.pdf", plot = plot, width = 12, height = 10, units = "in")

# 04 Subfield categories alluvial diagram ######################################
# 04.1 Function to concatenate the columns =====================================
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

# 04.2 Archaeological categories concatenate ###################################

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

write.csv(final, "./export/first_alluvial.csv", fileEncoding = "UTF-8")

# 04.3 Remove under represented tasks ==========================================

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

# 03.4 Remove under represented archaeological categories ======================
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

# 04.5 Plot the first alluvial diagram =========================================
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

# Plot the graph/
plot

ggsave("./export/graph/Figure_09_raw.png", plot = plot, width = 12, height = 10, units = "in", dpi = 600)
ggsave("./export/graph/Figure_09_raw.pdf", plot = plot, width = 12, height = 10, units = "in")

# 05 Results categories alluvial diagram #######################################

# 05.1 Remove under represented tasks ==========================================
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

# 05.2 Plot the third alluvial diagram =========================================
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

# Plot the graph/
plot

ggsave("./export/graph/Figure_10_raw.png", plot = plot, width = 12, height = 10, units = "in", dpi = 600)
ggsave("./export/graph/Figure_10_raw.pdf", plot = plot, width = 12, height = 10, units = "in")

# 06 Model table for annexes/glossary ##########################################
# 6.1 Prepare the data =========================================================
df_counted$approach <- "Machine learning"
df_counted$approach[df_counted$family == "Statistics"] <- "Statistics" 
df_counted$model[df_counted$family == "Statistics"]
df_counted$family[df_counted$model == "LR"] <- "Linear regression"
df_counted$family[df_counted$model == "k-MC" | df_counted$model ==  "k-MED"] <- "Dimensionality reduction"

# 6.2 Format the data ==========================================================
df_counted <- select(df_counted, approach, family, description, model, value, value.best)
df_counted <- df_counted %>%  arrange(approach, family, desc(value))

# 6.3 Plot the table ===========================================================
# Use the html format for the html quarto file and the latex format for the word and pdf

df_counted %>%
  kable("html", caption = "List of algorithms used in the papers under review organized by the approach and family of analysis, 
        along with their abbreviations and number of use. In the case the model was compared to others, we highlighted the number of time he 
        performed as the best model.", col.names = c("Approach", "Family", "Description", "Model abreviation", "Number of uses", 
                                                     "Number of time the model performed the best")) %>%
  kable_styling(full_width = FALSE) %>%
  collapse_rows(columns = c(1,2), valign = "top")
