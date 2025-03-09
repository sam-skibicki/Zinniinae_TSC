# plotting paralog report from HybPiper

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(patchwork)
library(moments)  # For skewness calculation

# Read paralog count data
paralog_data <- read_tsv("paralog_report.tsv")

# Convert to long format for ggplot heatmap
paralog_long <- paralog_data %>%
    pivot_longer(-Species, names_to = "Gene", values_to = "ParalogCount") %>%
    mutate(Species = factor(Species, levels = sort(unique(Species))))  # Ensure alphabetical order

# Define reversed Viridis color scale with light gray for zero
heatmap_plot <- ggplot(paralog_long, aes(x = Gene, y = Species, fill = ParalogCount)) +
    geom_tile() +
    scale_fill_gradientn(
        colors = c("#D9D9D9", viridis(50, option = "C", direction = 1)),  # Reverse: dark for small, bright for large
        values = c(0, seq(0.02, 1, length.out = 50)),  # Adjust mapping
        limits = c(0, max(paralog_long$ParalogCount, na.rm = TRUE))
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),  # Remove gene labels
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()) +
    labs(title = "Paralog Count Heatmap", fill = "Paralog Count")

# Compute summary metrics
paralogy_stats <- paralog_long %>%
    group_by(Species) %>%
    summarise(
        TotalParalogs = sum(ParalogCount, na.rm = TRUE),
        MeanParalogs = mean(ParalogCount, na.rm = TRUE),
        ParalogsSkew = skewness(ParalogCount, na.rm = TRUE)
    ) %>%
    arrange(Species)  # Ensure alphabetical order

# Total paralogs plot (Bottom-left, with species labels)
total_plot <- ggplot(paralogy_stats, aes(x = Species, y = TotalParalogs)) +
    geom_col(fill = "darkblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Total Paralogs per Species", x = "Species", y = "Total Paralogs")

# Paralog skew plot (Bottom-center, no species labels)
skew_plot <- ggplot(paralogy_stats, aes(x = Species, y = ParalogsSkew)) +
    geom_col(fill = "darkred") +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    labs(title = "Paralog Skew per Species", x = NULL, y = "Skewness")

# Mean paralogs per gene plot (Bottom-right, no species labels)
mean_plot <- ggplot(paralogy_stats, aes(x = Species, y = MeanParalogs)) +
    geom_col(fill = "darkgreen") +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    labs(title = "Mean Paralogs per Gene", x = NULL, y = "Mean Paralogs")

# Arrange plots: Heatmap on top, 3 summary plots in one row below
final_plot <- heatmap_plot / (total_plot | skew_plot | mean_plot)

# Display final plot
print(final_plot)
