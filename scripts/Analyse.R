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

# Read the specific worksheet
omics_data <- read_excel("data/Omics_WoS.xlsx", sheet = "Data")

dir.create("results")
dir.create("results/figures")

# Basic overview
glimpse(omics_data) # Structure of the data
summary(omics_data) # Summary statistics
head(omics_data) # First few rows

# Total number of unique journals
n_total_journals <- omics_data %>%
    filter(!is.na(`Source Title`)) %>%
    distinct(`Source Title`) %>%
    nrow()

# Number of journals with >= 4 articles
n_journals_4plus <- omics_data %>%
    filter(!is.na(`Source Title`)) %>%
    count(`Source Title`) %>%
    filter(n >= 4) %>%
    nrow()

# Print results
cat("Total number of journals:", n_total_journals, "\n")
cat("Number of journals with >= 4 articles:", n_journals_4plus, "\n")

# 1. Create a pie chart of search terms
# Exclude year 2025 from the analysis
# Filter the data
filtered_data <- omics_data %>%
    filter(
        !is.na(search),
        `Publication Year` != 2025,
        `Publication Type` != "B"
    )

# Count occurrences of each search term
search_counts <- filtered_data %>%
    count(search, name = "count") %>%
    arrange(desc(count))

# Calculate total n
total_n <- sum(search_counts$count)

# Calculate label positions for placing inside pie
search_counts <- search_counts %>%
    mutate(
        fraction = count / sum(count),
        ymax = cumsum(fraction),
        ymin = c(0, head(ymax, n = -1)),
        label_pos = (ymax + ymin) / 2,
        label = count
    )

# Pie chart with counts as labels
Pie_Search <- ggplot(search_counts, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = search)) +
    geom_rect() +
    geom_text(aes(x = 0.5, y = label_pos, label = label), size = 4, color = "white", fontface = "bold") +
    coord_polar(theta = "y") +
    scale_fill_viridis_d(option = "D") +
    theme_void() +
    labs(
        title = paste0("Distribution of Search Terms (n = ", total_n, ")"),
        fill = "Search Term"
    ) +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5)
    )

# Save plot
ggsave("results/figures/Pie_Search_Terms.png", plot = Pie_Search, width = 8, height = 6, dpi = 300)
ggsave("results/figures/Pie_Search_Terms.pdf", plot = Pie_Search, width = 8, height = 6, dpi = 300)

# 2. Create a histogram of search terms per publication year
# Exclude 'taxonomics' and year 2025 from the analysis
Term_by_year <- omics_data %>%
    filter(
        !is.na(search),
        !is.na(`Publication Year`),
        search != "taxonomics",
        `Publication Year` != 2025
    ) %>%
    count(`Publication Year`, search) %>%
    ggplot(aes(x = `Publication Year`, y = n, fill = search)) +
    geom_col(position = "stack") +
    scale_fill_viridis_d(option = "D") +
    labs(
        title = "Histogram of Search Terms per Publication Year",
        # subtitle = "Excluding 'taxonomics' and year 2025",
        x = "Publication Year", y = "Count", fill = "Search Term"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("results/figures/Histogram_of_Search_Terms_per_Publication_Year.png", plot = Term_by_year, width = 8, height = 4, dpi = 300)
ggsave("results/figures/Histogram_of_Search_Terms_per_Publication_Year.pdf", plot = Term_by_year, width = 8, height = 4, dpi = 300)

# 3. Create a histogram of research areas per publication year
# Exclude 'taxonomics' and year 2025 from the analysis
filtered_data <- omics_data %>%
    filter(
        !is.na(search),
        !is.na(`Source Title`),
        !is.na(`Publication Year`),
        search != "taxonomics",
        `Publication Year` != 2025,
        `Publication Type` != "B"
    )

# Count total articles per journal
journal_total <- filtered_data %>%
    count(`Source Title`, name = "total_articles")

# Journals with >=4 articles
main_journals <- journal_total %>%
    filter(total_articles >= 4) %>%
    pull(`Source Title`)

# Filter dataset by main journals only
filtered_main <- filtered_data %>%
    filter(`Source Title` %in% main_journals)

# Identify search terms missing from main journals
search_terms_missing <- setdiff(
    unique(filtered_data$search),
    unique(filtered_main$search)
)

# Add back journals related to the missing search terms
rescue_data <- filtered_data %>%
    filter(search %in% search_terms_missing)

# Combine both datasets
final_filtered <- bind_rows(filtered_main, rescue_data)

# Prepare data for plotting
plot_data <- final_filtered %>%
    count(search, `Source Title`) %>%
    mutate(`Source Title Wrapped` = str_wrap(`Source Title`, width = 30))

# Sort journals by total counts
journal_order <- plot_data %>%
    group_by(`Source Title`) %>%
    summarise(total = sum(n)) %>%
    arrange(desc(total)) %>%
    pull(`Source Title`)

plot_data <- plot_data %>%
    mutate(`Source Title Wrapped` = factor(
        `Source Title Wrapped`,
        levels = str_wrap(journal_order, width = 30)
    ))

# Plot
Hist_Journal_Stacked <- ggplot(plot_data, aes(x = `Source Title Wrapped`, y = n, fill = search)) +
    geom_col(position = "stack") +
    scale_fill_viridis_d(option = "D") +
    labs(
        title = "Stacked Articles per Journal by Search Term",
        x = "Journal", y = "Number of Articles", fill = "Search Term"
    ) +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(face = "bold")
    )

# Save
ggsave("results/figures/Stacked_Journals_Search_Terms.png", plot = Hist_Journal_Stacked, width = 12, height = 8, dpi = 300)
ggsave("results/figures/Stacked_Journals_Search_Terms.pdf", plot = Hist_Journal_Stacked, width = 12, height = 8, dpi = 300)

# 4. Create a network of research areas per Journal
# Exclude 'taxonomics' and year 2025 from the analysis
# Clean and filter the data
filtered_data <- omics_data %>%
    filter(!is.na(`Research Areas`), !is.na(`Source Title`)) %>%
    mutate(
        # Split Research Areas into individual areas
        Research_Areas = str_split(`Research Areas`, ";")
    ) %>%
    unnest(Research_Areas) %>%
    mutate(
        # Clean and convert research areas to lowercase
        Research_Areas = str_trim(str_to_lower(Research_Areas))
    )

# Create edges (research areas -> journal)
edges <- filtered_data %>%
    select(`Source Title`, Research_Areas) %>%
    rename(Research_Area = Research_Areas) %>%
    filter(!is.na(Research_Area)) %>%
    distinct()

# Create the graph object
graph <- graph_from_data_frame(edges, directed = FALSE)

# Create a layout for the nodes
layout <- layout_with_fr(graph)

# Extract node and edge data for ggplot
node_data <- data.frame(
    name = V(graph)$name,
    x = layout[, 1],
    y = layout[, 2]
)

# Wrap node labels by 30 characters
node_data$label <- str_wrap(node_data$name, width = 30)

# Define a lookup table for node formatting based on node type
lookup_table <- data.frame(
    name = c(filtered_data$`Source Title`, unique(filtered_data$Research_Areas)),
    type = c(rep("journal", length(filtered_data$`Source Title`)), rep("research_area", length(unique(filtered_data$Research_Areas)))),
    stringsAsFactors = FALSE
)

# Get viridis min/max colors
viridis_colors <- viridis(2)
color_search <- viridis_colors[2] # max
color_research <- viridis_colors[1] # min

# Assign color and font face based on the lookup table
node_data <- node_data %>%
    left_join(lookup_table, by = "name") %>%
    mutate(
        color = ifelse(type == "journal", color_search, color_research),
        fontface = ifelse(type == "journal", "plain", "bold"), # Bold for research areas, plain for journals
        text_size = ifelse(type == "journal", 3, 4.5)
    )

# Keep only unique journal nodes (no duplicates)
node_data_unique <- node_data %>%
    filter(type == "journal") %>%
    distinct(name, .keep_all = TRUE)

# Merge unique journal nodes with the rest of the nodes (research areas)
node_data <- bind_rows(
    node_data_unique,
    node_data %>% filter(type == "research_area")
)

# Extract edge data
edge_data <- as.data.frame(get.edgelist(graph))
colnames(edge_data) <- c("from", "to")

# Merge with node data to get coordinates for the edges
edge_data <- edge_data %>%
    left_join(node_data, by = c("from" = "name")) %>%
    rename(from_x = x, from_y = y) %>%
    left_join(node_data, by = c("to" = "name")) %>%
    rename(to_x = x, to_y = y)

# Plot the network using ggplot2
PLOT <- ggplot() +
    # Plot edges using geom_segment()
    geom_segment(
        data = edge_data, aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
        color = "gray", alpha = 0.5
    ) +
    # Plot nodes using geom_point()
    geom_point(data = node_data, aes(x = x, y = y, color = color), size = 3) +
    # Add labels to nodes
    geom_text_repel(
        data = node_data, aes(x = x, y = y, label = label, fontface = fontface),
        size = 3, box.padding = 0.5, point.padding = 0.5, max.overlaps = 20
    ) +
    scale_color_identity() + # Use the custom colors
    theme_void() +
    labs(title = "Research Area Network by Journal") +
    theme(plot.title = element_text(face = "bold"))

ggsave("results/figures/Research_Area_Network_by_Journal.png", plot = PLOT, width = 15, height = 12, dpi = 300)
ggsave("results/figures/Research_Area_Network_by_Journal.pdf", plot = PLOT, width = 15, height = 12, dpi = 300)

# 5. Create a network of research areas by search term
# Clean and filter the data
filtered_data <- omics_data %>%
    filter(
        !is.na(`Research Areas`),
        !is.na(search),
        !str_detect(tolower(search), "taxonomics"),
        `Publication Year` <= 2024
    ) %>%
    mutate(
        Research_Areas = str_split(`Research Areas`, ";"),
        Search_Term = search
    ) %>%
    unnest(Research_Areas) %>%
    mutate(
        Research_Areas = str_trim(str_to_lower(Research_Areas))
    )

# Create edges (Research Area -> Search Term)
edges <- filtered_data %>%
    select(Search_Term, Research_Areas) %>%
    rename(Research_Area = Research_Areas) %>%
    distinct()

# Build graph
graph <- graph_from_data_frame(edges, directed = FALSE)
layout <- layout_with_fr(graph)

# Get node data and positions
node_data <- data.frame(name = V(graph)$name, x = layout[, 1], y = layout[, 2])
node_data$label <- str_wrap(node_data$name, width = 30)

# Create lookup for node type
lookup_table <- data.frame(
    name = c(unique(filtered_data$Search_Term), unique(filtered_data$Research_Areas)),
    type = c(
        rep("search", length(unique(filtered_data$Search_Term))),
        rep("research_area", length(unique(filtered_data$Research_Areas)))
    ),
    stringsAsFactors = FALSE
)

# Get viridis min/max colors
viridis_colors <- viridis(2)
color_search <- viridis_colors[2] # max
color_research <- viridis_colors[1] # min

# Assign node formatting: color, font, size
node_data <- node_data %>%
    left_join(lookup_table, by = "name") %>%
    mutate(
        color = ifelse(type == "search", color_search, color_research),
        fontface = ifelse(type == "search", "bold", "plain"),
        text_size = ifelse(type == "search", 4.5, 3)
    )

# Create edge coordinates
edge_data <- as.data.frame(get.edgelist(graph))
colnames(edge_data) <- c("from", "to")

edge_data <- edge_data %>%
    left_join(node_data, by = c("from" = "name")) %>%
    rename(from_x = x, from_y = y) %>%
    left_join(node_data, by = c("to" = "name")) %>%
    rename(to_x = x, to_y = y)

# Plot the network
Search_Research <- ggplot() +
    geom_segment(
        data = edge_data, aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
        color = "gray70", alpha = 0.5
    ) +
    geom_point(data = node_data, aes(x = x, y = y, color = color), size = 5) +
    geom_text_repel(
        data = node_data, aes(x = x, y = y, label = label, fontface = fontface, size = text_size),
        box.padding = 0.5, point.padding = 0.5, max.overlaps = 20, show.legend = FALSE
    ) +
    scale_color_identity() +
    scale_size_identity() +
    theme_void() +
    labs(title = "Research Area Network by Search Term") +
    theme(plot.title = element_text(face = "bold", size = 14))

ggsave("results/figures/Research_Area_Network_by_Search_Term.png", plot = Search_Research, width = 10, height = 6, dpi = 300)
ggsave("results/figures/Research_Area_Network_by_Search_Term.pdf", plot = Search_Research, width = 10, height = 6, dpi = 300)

# 6. Create a stacked bar plot of research areas by search term
# Filter (exclude taxonomics and 2025)
filtered_data <- omics_data %>%
    filter(
        !is.na(search),
        !is.na(`Research Areas`),
        !is.na(`Publication Year`),
        search != "taxonomics",
        `Publication Year` != 2025
    )

# Separate multiple research areas
research_data <- filtered_data %>%
    separate_rows(`Research Areas`, sep = ";\\s*") %>%
    filter(`Research Areas` != "") %>%
    mutate(`Research Areas Wrapped` = str_wrap(`Research Areas`, width = 30))

# Count and order
plot_data <- research_data %>%
    count(`Research Areas Wrapped`, search, name = "n")

# Order bars by total count (descending)
ordered_areas <- plot_data %>%
    group_by(`Research Areas Wrapped`) %>%
    summarise(total = sum(n)) %>%
    arrange(desc(total)) %>%
    pull(`Research Areas Wrapped`)

plot_data <- plot_data %>%
    mutate(`Research Areas Wrapped` = factor(`Research Areas Wrapped`, levels = ordered_areas))

# Plot with stacked bars
Hist_Research_Stacked <- ggplot(plot_data, aes(x = `Research Areas Wrapped`, y = n, fill = search)) +
    geom_col(position = "stack") +
    scale_fill_viridis_d(option = "D") +
    labs(
        title = "Research Areas by Search Term",
        # subtitle = "Stacked by search term; excludes 'taxonomics' and articles from 2025",
        x = "Research Area", y = "Number of Articles"
    ) +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(face = "bold")
    )

# Save the plot
ggsave("results/figures/Research_Areas_by_Search_Stacked.png", plot = Hist_Research_Stacked, width = 14, height = 8, dpi = 300)
ggsave("results/figures/Research_Areas_by_Search_Stacked.pdf", plot = Hist_Research_Stacked, width = 14, height = 8, dpi = 300)
