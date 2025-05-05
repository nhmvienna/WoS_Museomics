# Set working directory
setwd("P:/OMICS/Data_Analysis/WoS_Museomics")

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(viridis) # for the color palette
library(stringr)
library(tidyr)
library(igraph)
library(ggrepel) # To prevent label overlap
library(patchwork) # For combining plots

# Read the specific worksheet
omics_data <- read_excel("data/Omics_Details.xlsx", sheet = "Data")

# Create directories for results
dir.create("results")
dir.create("results/figures")

# Basic overview
glimpse(omics_data) # Structure of the data
summary(omics_data) # Summary statistics
head(omics_data) # First few rows

# Prepare Methods data
method_data <- omics_data %>%
    mutate(Methods = ifelse(is.na(Methods) | str_trim(Methods) == "", "Not defined", Methods)) %>%
    mutate(Methods_wrapped = str_wrap(Methods, width = 30))

method_levels <- method_data %>%
    count(Methods_wrapped) %>%
    arrange(desc(n)) %>%
    pull(Methods_wrapped)

# Ensure "Not defined" is last
method_levels <- c(setdiff(method_levels, "Not defined"), "Not defined")
method_data$Methods_wrapped <- factor(method_data$Methods_wrapped, levels = method_levels)

# Prepare Marker data
loci_data <- omics_data %>%
    mutate(Marker = ifelse(is.na(Marker) | str_trim(Marker) == "", "Not defined", Marker)) %>%
    mutate(Marker_wrapped = str_wrap(Marker, width = 30))

loci_levels <- loci_data %>%
    count(Marker_wrapped) %>%
    arrange(desc(n)) %>%
    pull(Marker_wrapped)

# Ensure "Not defined" is last
loci_levels <- c(setdiff(loci_levels, "Not defined"), "Not defined")
loci_data$Marker_wrapped <- factor(loci_data$Marker_wrapped, levels = loci_levels)

# Prepare Organisms data (with corrected column name)
organism_grouping_data <- omics_data %>%
    mutate(`Organisms` = ifelse(is.na(`Organisms`) | str_trim(`Organisms`) == "", "Not defined", `Organisms`)) %>%
    mutate(OG_wrapped = str_wrap(`Organisms`, width = 30))

organism_grouping_levels <- organism_grouping_data %>%
    count(OG_wrapped) %>%
    arrange(desc(n)) %>%
    pull(OG_wrapped)

# Ensure "Not defined" is last
organism_grouping_levels <- c(setdiff(organism_grouping_levels, "Not defined"), "Not defined")
organism_grouping_data$OG_wrapped <- factor(organism_grouping_data$OG_wrapped, levels = organism_grouping_levels)

get_fill_colors <- function(levels) {
    has_not_defined <- "Not defined" %in% levels
    other_levels <- setdiff(levels, "Not defined")
    viridis_colors <- viridis(length(other_levels), option = "D")

    if (has_not_defined) {
        fill_colors <- setNames(c(viridis_colors, "grey70"), c(other_levels, "Not defined"))
    } else {
        fill_colors <- setNames(viridis_colors, other_levels)
    }

    return(fill_colors)
}

method_fill <- get_fill_colors(method_levels)
loci_fill <- get_fill_colors(loci_levels)
OG_fill <- get_fill_colors(organism_grouping_levels)

# Plot A: Methods
p1 <- ggplot(method_data, aes(x = Methods_wrapped, fill = Methods_wrapped)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = method_fill) +
    labs(title = "A) Analysis Methods", x = "Methods", y = "Count") +
    theme_bw() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

# Plot B: Marker
p2 <- ggplot(loci_data, aes(x = Marker_wrapped, fill = Marker_wrapped)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = loci_fill) +
    labs(title = "B) Marker Type", x = "Marker Type", y = "Count") +
    theme_bw() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

# Prepare Key Question data (splitting by commas)
key_question_data <- omics_data %>%
    mutate(Key_Question_split = str_split(`Key Question`, ",")) %>%
    unnest(Key_Question_split) %>%
    mutate(Key_Question_split = str_trim(Key_Question_split)) %>%
    filter(Key_Question_split != "") %>%
    count(Key_Question_split) %>%
    arrange(desc(n)) # Sort by count in descending order

# Ensure "Not defined" is last and grey for Key Question
key_question_data$Key_Question_split <- factor(key_question_data$Key_Question_split,
    levels = c(setdiff(unique(key_question_data$Key_Question_split), "Not defined"), "Not defined")
)

# Prepare fill color for Key Question (after splitting)
key_question_fill <- get_fill_colors(key_question_data$Key_Question_split)

# Plot C: Organism Grouping
p3 <- ggplot(organism_grouping_data, aes(x = OG_wrapped, fill = OG_wrapped)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = OG_fill) +
    labs(title = "C) Organisms", x = "Organisms", y = "Count") +
    theme_bw() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


# Plot D: Key Question
p4 <- ggplot(key_question_data, aes(x = Key_Question_split, y = n, fill = Key_Question_split)) +
    geom_col(show.legend = FALSE) + # Use geom_col to ensure correct heights
    scale_fill_manual(values = key_question_fill) +
    labs(title = "D) Key Question", x = "Key Question", y = "Count") +
    theme_bw() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

# Combine A + B in top row, C in bottom row
combined_plot <- (p1 | p2) / p3 + plot_layout(heights = c(1, 1))

# Save the combined plot
ggsave("results/figures/Methods_Marker_Organism_Grouping_Key_Question_Histograms.pdf", combined_plot, width = 12, height = 8, dpi = 300)
ggsave("results/figures/Methods_Marker_Organism_Grouping_Key_Question_Histograms.png", combined_plot, width = 12, height = 8, dpi = 300)
