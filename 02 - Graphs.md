

<script src="02 - Graphs_files/libs/kePrint-0.0.1/kePrint.js"></script>
<link href="02 - Graphs_files/libs/lightable-0.0.1/lightable.css" rel="stylesheet" />

# Graphics

All the input data used here are available either in the `data` folder
under `.csv` format or at the [OSF deposit]() under the name
`2-Results.xlsx`

## Preparation

``` r
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
patchwork, rnaturalearth, RColorBrewer, gridExtra, treemap) # Specify required packages and download it if needed

# 0.3 Session info =============================================================

sessionInfo()
```

    R version 4.5.1 (2025-06-13 ucrt)
    Platform: x86_64-w64-mingw32/x64
    Running under: Windows 11 x64 (build 26100)

    Matrix products: default
      LAPACK version 3.12.1

    locale:
    [1] LC_COLLATE=French_France.utf8  LC_CTYPE=French_France.utf8    LC_MONETARY=French_France.utf8 LC_NUMERIC=C                   LC_TIME=French_France.utf8    

    time zone: Europe/Paris
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices datasets  utils     methods   base     

    other attached packages:
     [1] gridExtra_2.3       RColorBrewer_1.1-3  rnaturalearth_1.1.0 patchwork_1.3.1     tibble_3.3.0        plyr_1.8.9          stringr_1.5.1       ggalluvial_0.12.5  
     [9] ggplot2_3.5.2       readr_2.1.5         dplyr_1.1.4        

    loaded via a namespace (and not attached):
     [1] vctrs_0.6.5       cli_3.6.5         rlang_1.1.6       stringi_1.8.7     renv_1.0.11       generics_0.1.4    glue_1.8.0        hms_1.1.3        
     [9] scales_1.4.0      grid_4.5.1        tzdb_0.5.0        lifecycle_1.0.4   compiler_4.5.1    pacman_0.5.1      Rcpp_1.1.0        pkgconfig_2.0.3  
    [17] rstudioapi_0.17.1 farver_2.1.2      R6_2.6.1          tidyselect_1.2.1  pillar_1.11.0     magrittr_2.0.3    tools_4.5.1       withr_3.0.2  

## Import the data

``` r
# 01 Import data sets ##########################################################
# 01.1 Import the metadata infos ===============================================

info <- read_delim("./data/ML_archaeology_metadata.csv", delim = ";")
```

    Rows: 196 Columns: 10
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ";"
    chr (9): ID, Name, Authors, Title, Abstract, Country of affiliation, Journal...
    dbl (1): Year

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(info)
```

    # A tibble: 6 × 10
      ID    Name          Year Authors Title Abstract Country of affiliati…¹ Journal
      <chr> <chr>        <dbl> <chr>   <chr> <chr>    <chr>                  <chr>  
    1 ID001 Nguifo et a…  1997 Nguifo… PLAT… The aut… FRA                    Comput…
    2 ID002 O'Sullivan …  2000 O'Sull… Agen… Agent-b… GBR                    Enviro…
    3 ID003 Amigoni and…  2009 Amigon… THE … The app… ITA                    Applie…
    4 ID004 Boon et al.…  2009 Boon, … Digi… We desc… NLD                    Interd…
    5 ID005 Toler et al…  2010 Toler-… Mult… We pres… USA                    Acm Tr…
    6 ID006 Barcelo and…  2012 Barcel… Func… Why arc… ESP                    Medite…
    # ℹ abbreviated name: ¹​`Country of affiliation`
    # ℹ 2 more variables: `Open-access` <chr>, Included <chr>

``` r
# 01.2 Import the observations =================================================
obs <- read_delim("./data/ML_archaeology_info.csv", delim = ";", na = "")
```

    Rows: 214 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ";"
    chr (14): ID, Name, Model, Best model, Family, Subfield, Input data, Input d...
    dbl  (1): Year

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(obs)
```

    # A tibble: 6 × 15
      ID    Name                Year Model `Best model` Family Subfield `Input data`
      <chr> <chr>              <dbl> <chr> <chr>        <chr>  <chr>    <chr>       
    1 ID001 Nguifo et al. 1997  1997 LEGAL <NA>         Decis… Classif… Small scale…
    2 ID002 O'Sullivan and Ha…  2000 <NA>  <NA>         <NA>   <NA>     <NA>        
    3 ID003 Amigoni and Schia…  2009 <NA>  <NA>         <NA>   <NA>     <NA>        
    4 ID013 Menze et al. 2006   2006 RF    <NA>         Ensem… Surveyi… Remote sens…
    5 ID004 Boon et al. 2009-2  2009 <NA>  <NA>         <NA>   Classif… Theory      
    6 ID004 Boon et al. 2009-1  2009 TiMBL <NA>         Unsup… Conserv… Text data   
    # ℹ 7 more variables: `Input data (detailed)` <chr>, Evaluation <chr>,
    #   Task <chr>, Results <chr>, `Pre-training` <chr>,
    #   `Pre-training (detailed)` <chr>, Included <chr>

``` r
# 01.3 Import the models used in the papers ====================================

models <- read_delim("./data/ML_archaeology_models.csv", delim = ";", na = "")
```

    New names:
    Rows: 74 Columns: 10
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: ";" chr
    (4): Model, Acronym, Familly assigned, Family dbl (5): No. of study cases,
    Frequency best, ID, No. of study cases (model f... lgl (1): ...6
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...6`

``` r
head(models)
```

    # A tibble: 6 × 10
      Model   Acronym `No. of study cases` `Frequency best` `Familly assigned` ...6 
      <chr>   <chr>                  <dbl>            <dbl> <chr>              <lgl>
    1 Random… RF                        54               20 Ensemble Learning  NA   
    2 Suppor… SVM                       26                2 Linear Classifier  NA   
    3 Feedfo… FNN                       23                4 Artificial Neural… NA   
    4 k-near… kNN                       19                1 Nearest Neighbour… NA   
    5 Convol… CNN                       14                1 Artificial Neural… NA   
    6 Residu… ResNet                    12                2 Artificial Neural… NA   
    # ℹ 4 more variables: ID <dbl>, Family <chr>,
    #   `No. of study cases (model family)` <dbl>, `No. of models` <dbl>

``` r
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
```

         ID                        Name.x Year.x
    1 ID002    O'Sullivan and Haklay 2000   2000
    2 ID003 Amigoni and Schiaffonati 2009   2009
    4 ID006      Barcelo and Almeida 2012   2012
    5 ID007          Praisler et al. 2013   2013
    6 ID011       Andrew and Shepard 2017   2017
    7 ID012              Drap et al. 2017   2017
                                                       Authors
    1                                 O'Sullivan, D; Haklay, M
    2                              Amigoni, F; Schiaffonati, V
    4                              Barcelo, JA; de Almeida, VM
    5 Praisler, Mirela; Domnisoru, Danieal; Domnisoru, Leonard
    6                                 Andrew, ME; Shephard, JM
    7   Drap, P; Papini, O; Pruno, E; Nucciotti, M; Vannini, G
                                                                                                                                           Title
    1                                                                            Agent-based models and individualism: is the world agent-based?
    2                                                                    THE MINERVA SYSTEM: A STEP TOWARD AUTOMATICALLY CREATED VIRTUAL MUSEUMS
    4                                                   Functional analysis from visual and non-visual data. An artificial intelligence approach
    5                                         Chemometric method for the automated identification of cucuteni ceramics based on ATR-FTIR spectra
    6 Semi-automated detection of eagle nests: an application of very high-resolution image data and advanced image analyses to wildlife surveys
    7                             Ontology-Based Photogrammetry Survey for Medieval Archaeology: Toward a 3D Geographic Information System (GIS)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Abstract
    1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Agent-based models (ABMs) are an increasingly popular tool in the social sciences. This trend seems likely to continue, so that they will become widely used in geography and in urban and regional planning. We present an overview of examples of these models in the life sciences, economics, planning, sociology, and archaeology. We conclude that ABMs strongly tend towards an individualist view of the social world. This point is reinforced by closer consideration of particular examples. This discussion pays attention to the inadequacy of an individualist model of society with reference to debates in social theory. We argue that because models are closed representations of an open world it is important that institutions and other social structures be explicitly included, or that their omission be explained. A tentative explanation for the bias of ABMs is offered, based on an examination of early research in artificial intelligence and distributed artificial intelligence from which disciplines the approach is derived. Some implications of these findings are discussed. We indicate some useful research directions which are beginning to tackle the individualism issue directly. We further note that the underlying assumptions of ABMs are often hidden in the implementation details. We conclude that such models must be subject to critical examination of their assumptions, and that model builders should engage with social theory if the approach is to realise its full potential.
    2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             The application of artificial intelligence (AI) tools to cultural heritage yields new technological solutions for the fruition of museums and art exhibitions. In this article we present the latest version of the Minerva system, able to support the organization of virtual museums by cooperating with curators to set a collection of works of art in an environment. The new version of Minerva organizes a part of the arche-ological finds belonging to the collection of the Archeological Museum of Milan. The role of Minerva is not to substitute the curators but to assist them in setting up a museum or an art exhibition. In this perspective, Minerva carries out automatically some tasks involved in museum organization and constitutes a unique system among those oriented to cultural heritage. The experimental activities have been conducted in cooperation with archeological experts who validated the results produced by Minerva.
    4                                                                                                                                                                                                                                                                                                                                                            Why archaeological artefacts are the way they are? In this paper we try to solve such a question by investigating the relationship between form and function. We propose new ways of studying the way behaviour in the past can be asserted on the examination of archaeological observables in the present. In any case, we take into account that there are also non-visual features characterizing ancient objects and materials (i.e., compositional information based on mass spectrometry data, chronological information based on radioactive decay measurements, etc.). Information that should make us aware of many functional properties of objects is multidimensional in nature: size, which makes reference to height, length, depth, weight and mass; shape and form, which make reference to the geometry of contours and volumes; texture, which refers to the microtopography (roughness, waviness, and lay) and visual appearance (colour variations, brightness, reflectivity and transparency) of surfaces; and finally material, meaning the combining of distinct compositional elements and properties to form a whole. With the exception of material data, the other relevant aspects for functional reasoning have been traditionally described in rather ambiguous terms, without taking into account the advantages of quantitative measurements of shape/form, and texture. Reasoning about the functionality of archaeological objects recovered at the archaeological site requires a cross-disciplinary investigation, which may also range from recognition techniques used in computer vision and robotics to reasoning, representation, and learning methods in artificial intelligence. The approach we adopt here is to follow current computational theories of object perception to ameliorate the way archaeology can deal with the explanation of human behaviour in the past (function) from the analysis of visual and non-visual data, taking into account that visual appearances and even compositional characteristics only constrain the way an object may be used, but never fully determine it.
    5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A chemometric software application was developed in order to obtain an automatic identification of original Cucuteni ceramic artifacts and distinguish them from fake ceramic samples similar to those found on the black market. The software application is based on Principal Component Analysis (PCA), a method of artificial intelligence that allowed us to build multivariate models that can be used for the efficient spectral discrimination of genuine Cucuteni ceramic samples. The spectra obtained by Fourier Transform Infrared Spectroscopy with ATR-Attenuated Total Reflection (FTIR-ATR) for a set of Cucuteni ceramic samples discovered in archaeological sites from Moldova Romania were used as input database for the detection system.
    6                                                                                                                                                                                                                                                                                                Very high-resolution (VHR) image data, including from unmanned aerial vehicle (UAV) platforms, are increasingly acquired for wildlife surveys. Animals or structures they build (e.g. nests) can be photointerpreted from these images, however, automated detection is required for more efficient surveys. We developed semi-automated analyses to map white-bellied sea eagle (Haliaeetus leucogaster) nests in VHR aerial photographs of the Houtman Abrolhos Islands, Western Australia, an important breeding site for many seabird species. Nest detection is complicated by high environmental heterogeneity at the scale of nests (similar to 1-2 m), the presence of many features that resemble nests and the variability of nest size, shape and context. Finally, the rarity of nests limits the availability of training data. These challenges are not unique to wildlife surveys and we show how they can be overcome by an innovative integration of object-based image analyses (OBIA) and the powerful machine learning one-class classifier Maxent. Maxent classifications using features characterizing object texture, geometry and neighborhood, along with limited object color information, successfully identified over 90% of high quality nests (most weathered and unusually shaped nests were also detected, but at a slightly lower rate) and labeled <2% of objects as candidate nests. Although this overestimates the occurrence of nests, the results can be visually screened to rule out all but the most likely nests in a process that is simpler and more efficient than manual photointerpretation of the full image. Our study shows that semi-automated image analyses for wildlife surveys are achievable. Furthermore, the developed strategies have broad relevance to image processing applications that seek to detect rare features differing only subtly from a heterogeneous background, including remote sensing of archeological remains. We also highlight solutions to maximize the use of imperfect or uncalibrated image data, such as some UAV-based imagery and the growing body of VHR imagery available in Google Earth and other virtual globes.
    7 This paper presents certain reflections concerning an interdisciplinary project between medieval archaeologists from the University of Florence (Italy) and computer science researchers from CNRS, National Center for Scientific Research, (France), aiming towards a connection between 3D spatial representation and archaeological knowledge. We try to develop an integrated system for archaeological 3D survey and all other types of archaeological data and knowledge by incorporating observable (material) and non-graphic (interpretive) data. Survey plays a central role, since it is both a metric representation of the archaeological site and, to a wider extent, an interpretation of it (being also a common basis for communication between the two teams). More specifically, 3D survey is crucial, allowing archaeologists to connect actual spatial assets to the stratigraphic formation processes (i.e., to the archaeological time) and to translate spatial observations into historical interpretation of the site. It is well known that laser scanner, photogrammetry and computer vision are very useful tools for archaeologists, although the integration of the representation of space, as well as archaeological time has not yet found a methodological standard of reference. We propose a common formalism for describing photogrammetric survey and archaeological knowledge stemming from ontologies: indeed, ontologies are fully used to model and store 3D data and archaeological knowledge. We equip this formalism with a qualitative representation of time, starting from archaeological stratigraphy. Stratigraphic analyses (both of excavated deposits and of upstanding structures) are closely related to Edward Cecil Harris's theory of the \\Unit of Stratigraphication\\\\ (referred to as \\\\"US\\\\", while a stratigraphic unit of an upstanding structure Unita Stratigrafica Murale, in Italian, will be referred to as \\\\"USM\\\\"). Every US is connected to the others by geometric, topological and, eventually, temporal links, and these are recorded by the 3D photogrammetric survey. However, the limitations of the Harris matrix approach led us to use another formalism for representing stratigraphic relationships, namely Qualitative Constraints Networks (QCN), which was successfully used in the domain of knowledge representation and reasoning in artificial intelligence for representing temporal relations.\\""
      Country of affiliation                                        Journal
    1                    GBR Environnement and Planning A-Economy and Space
    2                    ITA               Applied Artificial Intelligence,
    4                    ESP    Mediterranean Archaeology and Archaeometry,
    5                    ROU       European Journal of Science and Theology
    6                    AUS     Remote Sensing in Ecology and Conservation
    7                    FRA                                    Geosciences
      Open-access Included.x                        Name.y Year.y Model Best model
    1          No         No    O'Sullivan and Haklay 2000   2000  <NA>       <NA>
    2         Yes         No Amigoni and Schiaffonati 2009   2009  <NA>       <NA>
    4         Yes         No      Barcelo and Almeida 2012   2012  <NA>       <NA>
    5         Yes         No          Praisler et al. 2013   2013  <NA>       <NA>
    6         Yes         No       Andrew and Shepard 2017   2017  <NA>       <NA>
    7         Yes         No              Drap et al. 2017   2017  <NA>       <NA>
      Family                     Subfield Input data Input data (detailed)
    1   <NA>                         <NA>       <NA>                  <NA>
    2   <NA>                         <NA>       <NA>                  <NA>
    4   <NA> Cognitive Archaeology;Theory     Theory                Theory
    5   <NA>                         <NA>       <NA>                  <NA>
    6   <NA>                         <NA>       <NA>                  <NA>
    7   <NA>                         <NA>       <NA>                  <NA>
      Evaluation   Task Results Pre-training Pre-training (detailed)
    1       <NA>   <NA>    <NA>         <NA>                    <NA>
    2       <NA>   <NA>    <NA>         <NA>                    <NA>
    4     Theory Theory    <NA>         <NA>                    <NA>
    5       <NA>   <NA>    <NA>         <NA>                    <NA>
    6       <NA>   <NA>    <NA>         <NA>                    <NA>
    7       <NA>   <NA>    <NA>         <NA>                    <NA>
                       Included.y
    1 Not archaeology (Reason 12)
    2          Not ML (Reason 13)
    4                          No
    5          Not ML (Reason 13)
    6 Not archaeology (Reason 12)
    7          Not ML (Reason 13)

``` r
# Create the reviewed metadata file
ID <- c(not_review$ID)
metadata <- info[!info$ID %in% ID, ]

# Show how many papers were included in the review and how many not

# Reviewed paper
nrow(metadata)
```

    [1] 135

``` r
# Number of studies
nrow(review)
```

    [1] 147

``` r
# Not reviewed paper
nrow(not_review)
```

    [1] 61

## Statistics from the results

All the presented figures have been slightly modified with [InkScape
software](https://inkscape.org/) before publishing.

``` r
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
```

<img src="./export/graph/Figure_02_raw.png" style="width:50.0%"
data-fig-align="center" alt="Inputs categories" />

``` r
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
```

![](02---Graphs_files/figure-commonmark/statistics%20bis%20graph-1.png)

``` r
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
```

    Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    3.5.0.
    ℹ Please use the `legend.position.inside` argument of `theme()` instead.

``` r
# Check the plot
plot 
```

![](02---Graphs_files/figure-commonmark/statistics%20bis%20graph-2.png)

``` r
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
```

             authors..1.. Freq
    112 Domínguez-Rodrigo    9
    25          Baquedano    7
    252                Li    6
    344            Orengo    5
    521          Yravedra    5
    28           Bataille    4

``` r
# 02.5 Most common journals ====================================================
journals <- table(metadata$Journal)
journals <- as.data.frame(journals)
colnames(journals) <- c("journal","Freq")
journals <- journals[order(-journals$Freq),]

# Plot the top six review
head(journals)
```

                                               journal Freq
    60                                  Remote Sensing   15
    34               Journal of Archaeological Science   14
    54                                        PloS One   11
    61                              Scientific Reports    6
    37 Journal of Computer Applications in Archaeology    5
    7                       Archaeological Prospection    4

``` r
# 02.6 Open access articles ====================================================

# Number of papers
summary(as.factor(metadata$`Open-access`))
```

     No Yes 
     40  95 

``` r
# Ratio in purcent
(sum(metadata$`Open-access` == "Yes")/nrow(metadata))*100
```

    [1] 70.37037

``` r
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
```

    png 
      2 

``` r
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
```

       year                                  category Freq cumsum_freq  pos
    1  1997 Classification (of material) and Typology    1           1  0.5
    2  2006                     Landscape Archaeology    1           1  0.5
    3  2006          Surveying (and Site Prospection)    1           2  1.5
    4  2009              Conservation and Cataloguing    1           1  0.5
    5  2010              Conservation and Cataloguing    1           1  0.5
    6  2014 Classification (of material) and Typology    2           2  1.0
    7  2015  Bioarchaeology (Biological Anthropology)    2           2  1.0
    8  2015 Classification (of material) and Typology    1           3  2.5
    9  2015              Conservation and Cataloguing    1           4  3.5
    10 2015                            Geoarchaeology    1           5  4.5
    11 2016  Bioarchaeology (Biological Anthropology)    1           1  0.5
    12 2016                          Ethnoarchaeology    1           2  1.5
    13 2016                  Experimental Archaeology    1           3  2.5
    14 2016                            Zooarchaeology    1           4  3.5
    15 2017              Conservation and Cataloguing    1           1  0.5
    16 2018                             Archaeobotany    2           2  1.0
    17 2018                     Archaeological Dating    1           3  2.5
    18 2018                 Archaeological Excavation    1           4  3.5
    19 2018  Bioarchaeology (Biological Anthropology)    2           6  5.0
    20 2018 Classification (of material) and Typology    3           9  7.5
    21 2018                     Cognitive Archaeology    1          10  9.5
    22 2018              Conservation and Cataloguing    1          11 10.5
    23 2018                  Experimental Archaeology    2          13 12.0
    24 2018                            Geoarchaeology    1          14 13.5
    25 2018                    Historical Archaeology    1          15 14.5
    26 2018                     Landscape Archaeology    1          16 15.5
    27 2018          Surveying (and Site Prospection)    2          18 17.0
    28 2018                                 Taphonomy    2          20 19.0
    29 2018                            Zooarchaeology    3          23 21.5
    30 2019  Bioarchaeology (Biological Anthropology)    4           4  2.0
    31 2019 Classification (of material) and Typology    3           7  5.5
    32 2019                     Cognitive Archaeology    1           8  7.5
    33 2019              Conservation and Cataloguing    5          13 10.5
    34 2019                          Ethnoarchaeology    1          14 13.5
    35 2019                  Experimental Archaeology    5          19 16.5
    36 2019                    Historical Archaeology    1          20 19.5
    37 2019                     Landscape Archaeology    2          22 21.0
    38 2019          Surveying (and Site Prospection)    7          29 25.5
    39 2019                                 Taphonomy    6          35 32.0
    40 2019                            Zooarchaeology    3          38 36.5
    41 2020                             Archaeobotany    2           2  1.0
    42 2020                 Archaeological Excavation    1           3  2.5
    43 2020  Bioarchaeology (Biological Anthropology)    1           4  3.5
    44 2020 Classification (of material) and Typology    5           9  6.5
    45 2020                     Cognitive Archaeology    1          10  9.5
    46 2020              Conservation and Cataloguing    2          12 11.0
    47 2020                  Experimental Archaeology    1          13 12.5
    48 2020                            Geoarchaeology    8          21 17.0
    49 2020                     Landscape Archaeology    4          25 23.0
    50 2020          Surveying (and Site Prospection)    6          31 28.0
    51 2020                                 Taphonomy    1          32 31.5
    52 2020                            Zooarchaeology    2          34 33.0
    53 2021                             Archaeobotany    2           2  1.0
    54 2021                     Archaeological Dating    1           3  2.5
    55 2021                 Archaeological Excavation    2           5  4.0
    56 2021  Bioarchaeology (Biological Anthropology)    4           9  7.0
    57 2021 Classification (of material) and Typology   12          21 15.0
    58 2021                     Cognitive Archaeology    2          23 22.0
    59 2021              Conservation and Cataloguing   13          36 29.5
    60 2021                            Geoarchaeology    4          40 38.0
    61 2021                    Historical Archaeology    1          41 40.5
    62 2021                     Landscape Archaeology    7          48 44.5
    63 2021          Surveying (and Site Prospection)   14          62 55.0
    64 2021                                 Taphonomy    2          64 63.0
    65 2021                            Zooarchaeology    2          66 65.0
    66 2022                             Archaeobotany    2           2  1.0
    67 2022                 Archaeological Excavation    2           4  3.0
    68 2022 Classification (of material) and Typology    4           8  6.0
    69 2022                     Cognitive Archaeology    3          11  9.5
    70 2022              Conservation and Cataloguing   10          21 16.0
    71 2022                  Experimental Archaeology    1          22 21.5
    72 2022                            Geoarchaeology    3          25 23.5
    73 2022                     Landscape Archaeology    5          30 27.5
    74 2022          Surveying (and Site Prospection)   12          42 36.0
    75 2022                                 Taphonomy    1          43 42.5
    76 2022                            Zooarchaeology    1          44 43.5

``` r
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
```

    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.

``` r
# Plot 1
plot1
```

![](02---Graphs_files/figure-commonmark/statistics%20bis%20graph-3.png)

``` r
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
```

       year                               family Freq cumsum_freq  pos
    1  1997    Decision Trees and Rule Induction    1           1  0.5
    2  2006                    Ensemble Learning    1           1  0.5
    3  2009 Unsupervised Learning and Clustering    1           1  0.5
    4  2010    Decision Trees and Rule Induction    1           1  0.5
    5  2010                    Ensemble Learning    1           2  1.5
    6  2010                    Linear Classifier    1           3  2.5
    7  2014            Artificial Neural Network    1           1  0.5
    8  2014                  Bayesian Classifier    1           2  1.5
    9  2014    Decision Trees and Rule Induction    1           3  2.5
    10 2014                    Ensemble Learning    1           4  3.5
    11 2014         Nearest Neighbour Classifier    1           5  4.5
    12 2014 Unsupervised Learning and Clustering    1           6  5.5
    13 2015            Artificial Neural Network    1           1  0.5
    14 2015    Decision Trees and Rule Induction    1           2  1.5
    15 2015                    Linear Classifier    1           3  2.5
    16 2015         Nearest Neighbour Classifier    1           4  3.5
    17 2015                Polynomial Classifier    2           6  5.0
    18 2016            Artificial Neural Network    2           2  1.0
    19 2016    Decision Trees and Rule Induction    2           4  3.0
    20 2016                    Ensemble Learning    1           5  4.5
    21 2016                    Genetic Algorithm    1           6  5.5
    22 2016                Polynomial Classifier    1           7  6.5
    23 2017                    Linear Classifier    1           1  0.5
    24 2017         Nearest Neighbour Classifier    1           2  1.5
    25 2018            Artificial Neural Network    5           5  2.5
    26 2018                  Bayesian Classifier    3           8  6.5
    27 2018    Decision Trees and Rule Induction    4          12 10.0
    28 2018                    Ensemble Learning    8          20 16.0
    29 2018                    Linear Classifier    3          23 21.5
    30 2018         Nearest Neighbour Classifier    2          25 24.0
    31 2018                Polynomial Classifier    2          27 26.0
    32 2018 Unsupervised Learning and Clustering    1          28 27.5
    33 2019            Artificial Neural Network   13          13  6.5
    34 2019                  Bayesian Classifier    4          17 15.0
    35 2019    Decision Trees and Rule Induction    7          24 20.5
    36 2019                    Ensemble Learning   12          36 30.0
    37 2019                    Linear Classifier    9          45 40.5
    38 2019         Nearest Neighbour Classifier    8          53 49.0
    39 2019                Polynomial Classifier    1          54 53.5
    40 2019 Unsupervised Learning and Clustering    1          55 54.5
    41 2020            Artificial Neural Network   10          10  5.0
    42 2020                  Bayesian Classifier    4          14 12.0
    43 2020    Decision Trees and Rule Induction    2          16 15.0
    44 2020                    Ensemble Learning   13          29 22.5
    45 2020                    Linear Classifier    6          35 32.0
    46 2020         Nearest Neighbour Classifier    4          39 37.0
    47 2020                Polynomial Classifier    1          40 39.5
    48 2020 Unsupervised Learning and Clustering    5          45 42.5
    49 2021            Artificial Neural Network   38          38 19.0
    50 2021    Decision Trees and Rule Induction    3          41 39.5
    51 2021                    Ensemble Learning   23          64 52.5
    52 2021                    Linear Classifier    4          68 66.0
    53 2021         Nearest Neighbour Classifier    2          70 69.0
    54 2021 Unsupervised Learning and Clustering    1          71 70.5
    55 2022            Artificial Neural Network   40          40 20.0
    56 2022                  Bayesian Classifier    1          41 40.5
    57 2022    Decision Trees and Rule Induction    2          43 42.0
    58 2022                    Ensemble Learning   11          54 48.5
    59 2022                    Linear Classifier    2          56 55.0
    60 2022         Nearest Neighbour Classifier    3          59 57.5
    61 2022 Unsupervised Learning and Clustering    2          61 60.0

``` r
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
```

![](02---Graphs_files/figure-commonmark/statistics%20bis%20graph-4.png)

``` r
plot <- plot1/plot2

# Combinned plot
plot
```

![](02---Graphs_files/figure-commonmark/statistics%20bis%20graph-5.png)

``` r
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
```

                       Var1 Freq
    7 Remote sensing images   58
    8    Small scale images   30
    3 Artefact measurements   17
    9               Spectra   14
    2              3D model   10
    1                Others   19

``` r
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
```

![](02---Graphs_files/figure-commonmark/statistics%20bis%20graph-6.png)

``` r
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
```

![](02---Graphs_files/figure-commonmark/statistics%20bis%20graph-7.png)

``` r
# Final plot  
plot <- plot1 + plot2 + plot_layout(widths = c(1, 0.8))
plot
```

![](02---Graphs_files/figure-commonmark/statistics%20bis%20graph-8.png)

``` r
# Export plot
ggsave("./export/graph/Figure_07_raw.png", plot = plot, width = 13.5, height = 5, units = "in", dpi = 600)
ggsave("./export/graph/Figure_07_raw.pdf", plot = plot, width = 13.5, height = 5, units = "in")
```

## Alluvial diagrams

``` r
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
```

    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.

![](02---Graphs_files/figure-commonmark/alluvial%20diagrams%20graph-1.png)

``` r
ggsave("./export/graph/Figure_08_raw.png", plot = plot, width = 12, height = 10, units = "in", dpi = 600)
```

    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.

``` r
ggsave("./export/graph/Figure_08_raw.pdf", plot = plot, width = 12, height = 10, units = "in")
```

    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.

``` r
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
```

    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.

![](02---Graphs_files/figure-commonmark/alluvial%20diagrams%20graph-2.png)

``` r
ggsave("./export/graph/Figure_09_raw.png", plot = plot, width = 12, height = 10, units = "in", dpi = 600)
```

    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.

``` r
ggsave("./export/graph/Figure_09_raw.pdf", plot = plot, width = 12, height = 10, units = "in")
```

    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.
    Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    params$discern): Some strata appear at multiple axes.

``` r
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
```

![](02---Graphs_files/figure-commonmark/alluvial%20diagrams%20graph-3.png)

``` r
ggsave("./export/graph/Figure_10_raw.png", plot = plot, width = 12, height = 10, units = "in", dpi = 600)
ggsave("./export/graph/Figure_10_raw.pdf", plot = plot, width = 12, height = 10, units = "in")
```

## Annexe/Glossary model table

``` r
# 06 Model table for annexes/glossary ##########################################
# 6.1 Prepare the data =========================================================
df_counted$approach <- "Machine learning"
df_counted$approach[df_counted$family == "Statistics"] <- "Statistics" 
df_counted$model[df_counted$family == "Statistics"]
```

    [1] "k-MC"  "k-MED" "LR"   

``` r
df_counted$family[df_counted$model == "LR"] <- "Linear regression"
df_counted$family[df_counted$model == "k-MC" | df_counted$model ==  "k-MED"] <- "Dimensionality reduction"

# 6.2 Format the data ==========================================================
df_counted <- select(df_counted, approach, family, description, model, value, value.best)
df_counted <- df_counted %>%  arrange(approach, family, desc(value))

# 6.3 Plot the table ===========================================================
# Use the html format for the html quarto file and the latex format for the word and pdf

df_counted %>%
  kable("pipe", caption = "List of algorithms used in the papers under review organized by the approach and family of analysis, 
        along with their abbreviations and number of use. In the case the model was compared to others, we highlighted the number of time he 
        performed as the best model.", col.names = c("Approach", "Family", "Description", "Model abreviation", "Number of uses", 
                                                     "Number of time the model performed the best")) %>%
  kable_styling(full_width = FALSE) %>%
  collapse_rows(columns = c(1,2), valign = "top")
```

| Approach | Family | Description | Model abreviation | Number of uses | Number of time the model performed the best |
|:---|:---|:---|:---|---:|---:|
| Machine learning | Artificial Neural Network | Feedforward Neural Network | FNN | 23 | 4 |
|  |  | Convolutional Neural Network | CNN | 14 | 1 |
|  |  | Residual Neural Network | ResNet | 12 | 2 |
|  |  | Mask Region-based Convolutional Neural Network | MR-CNN | 9 | 1 |
|  |  | Faster Region-based Convolutional Neural Network | FR-CNN | 8 | 0 |
|  |  | Visual Geomtry Group | VGG | 8 | 2 |
|  |  | U-Net | U-Net | 7 | 4 |
|  |  | Inception Network | INC | 4 | 1 |
|  |  | AlexNet | AlexNet | 3 | 0 |
|  |  | RetinaNet | RN | 3 | 0 |
|  |  | YOLO | YOLO | 3 | 0 |
|  |  | DeepLabv3+ | DL3 | 2 | 0 |
|  |  | Semantic Segmentation Model | SegNet | 2 | 0 |
|  |  | Adaptive deep learning for fine-grained image retrieval | ADLFIG | 1 | 0 |
|  |  | Bidirectional Encoder Representations from Transformers (DNLM) | BERT | 1 | 0 |
|  |  | BiGRU-Dual Attention | BiGRU | 1 | 0 |
|  |  | BiLSTM (RNN/Recurent Neural Network) | BiLSTM | 1 | 0 |
|  |  | Dynamic Graph Convolutional Neural Network | DGCNN | 1 | 0 |
|  |  | DenseNet201 | DN201 | 1 | 0 |
|  |  | Generative Adversarial Network | GAN | 1 | 0 |
|  |  | Jason 2 | JAS2 | 1 | 0 |
|  |  | Neural Support Vector Machine | NSVM | 1 | 0 |
|  |  | Region-based Convolutional Neural Network | R-CNN | 1 | 0 |
|  |  | SimpleNet | SimpleNet | 1 | 0 |
|  |  | Single Shot MultiBox Detector | SSD | 1 | 0 |
|  | Bayesian Classifier | Naïve Bayes | NB | 11 | 0 |
|  |  | Maximum Entropy | MaxEnt | 2 | 0 |
|  | Decision Trees and Rule Induction | C5.0 | C5.0 | 7 | 2 |
|  |  | C4.5 | C4.5 | 4 | 0 |
|  |  | Decision Tree/Classification Tree | DT | 4 | 0 |
|  |  | Conditional Inference Trees | CTREE | 2 | 0 |
|  |  | Iterative Dichotomiser 3 | ID3 | 2 | 0 |
|  |  | Classification And Regression Tree | CART | 1 | 0 |
|  |  | Fast and Frugal Tree | FFT | 1 | 0 |
|  |  | Learning with Galois Lattice | LEGAL | 1 | 0 |
|  |  | Representative trees | REPTree | 1 | 0 |
|  |  | Random Tree | RT | 1 | 0 |
|  | Ensemble Learning | Random Forest | RF | 54 | 20 |
|  |  | Adaptative Boost | AdaBoost | 2 | 0 |
|  |  | Stochastic Gradient Boosting | SGB | 2 | 1 |
|  |  | eXtreme Gradient Boosting | XGB | 2 | 1 |
|  |  | Bootstrap Agreggating | BAgg | 1 | 0 |
|  |  | Discrete SuperLearner | dSL | 1 | 0 |
|  |  | Fast Random Forest | FRF | 1 | 0 |
|  |  | GradientBoosting regression tree | GboostRT | 1 | 0 |
|  |  | LogitBoost | LB | 1 | 0 |
|  |  | Quantile Random forest | qRF | 1 | 0 |
|  |  | Sequential Backward Selection-Random Forest Regression | SBS-RFR | 1 | 1 |
|  |  | SMOTE Boost | SMOTEBoost | 1 | 0 |
|  |  | Synthetic Minority Oversampling Technique + Edited Nearest Neighbour Rule | SMOTEENN | 1 | 0 |
|  |  | Super Learner | SP | 1 | 1 |
|  |  | Viola-Jones Cascade Classifier | VL-CC | 1 | 0 |
|  | Genetic Algorithm | Genetic Algorithm | GA | 1 | 0 |
|  | Linear Classifier | Support Vector Machine | SVM | 26 | 2 |
|  |  | Structured SVM | SSVM | 1 | 0 |
|  | Nearest Neighbour Classifier | k-nearest neighbors | kNN | 19 | 1 |
|  |  | Weighted k-nearest neighbors | kkNN | 3 | 0 |
|  | Polynomial Classifier | Support Vector Machine with Radial Basis Function Kernel | SVMr | 7 | 1 |
|  | Unsupervised Learning and Clustering | Affinity Propagation | AF | 1 | 0 |
|  |  | Hierarchical Cluster-Based Peak Alignment | CluPA | 1 | 0 |
|  |  | Databionic Swarm | DBS | 1 | 0 |
|  |  | Expectation-Maximisation Clustering | EMC | 1 | 0 |
|  |  | Graph-based Semi-Supervised Learning | GSSL | 1 | 1 |
|  |  | Iterative Closest Point | ICP | 1 | 0 |
|  |  | Iterative Self-Organizing Data Analysis | ISODATA | 1 | 0 |
|  |  | Nearest Centroid | NC | 1 | 0 |
|  |  | Simple Linear Iterative Clustering | SLIC | 1 | 0 |
|  |  | Self-Organizing Map | SOM | 1 | 0 |
|  |  | Tilburg Memory-Based Learning | TiMBL | 1 | 0 |
|  |  | Time series clustering | TSC | 1 | 0 |
| Statistics | Dimensionality reduction | k-Mean Clustering | k-MC | 7 | 0 |
|  |  | k-medoids clustering | k-MED | 2 | 0 |
|  | Linear regression | Linear Regression | LR | 1 | 0 |

List of algorithms used in the papers under review organized by the
approach and family of analysis, along with their abbreviations and
number of use. In the case the model was compared to others, we
highlighted the number of time he performed as the best model.
