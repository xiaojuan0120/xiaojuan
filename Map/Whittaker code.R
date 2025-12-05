# 1. Load necessary R packages
suppressMessages(suppressWarnings(library(glue)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(grid)))
install.packages("ggtext")
suppressMessages(suppressWarnings(library(ggtext)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(reshape2)))
suppressMessages(suppressWarnings(library(ggsignif)))
suppressMessages(suppressWarnings(library(patchwork)))


# 3. Load the dataset
input_data <- read.csv('α-diversities.csv')

# 4. Transform dataset into long format
transformed_data <- melt(input_data, id = c('Group', 'Treatment')) %>%
  mutate(
    Treatment = factor(Treatment, levels = c('Abundant', 'Rare')),
    Group = factor(Group, levels = c('ACE','Shannon','Simpson'))
  )

# 5. Function to create a single plot for a specific group
generate_group_plot <- function(data, age_group, ylim_range, y_ticks, signif_position, plot_title, color_fill, gradient_colors) {
  # Create a gradient background
  background_gradient <- rasterGrob(
    colorRampPalette(gradient_colors)(256),
    width = unit(1, 'npc'),
    height = unit(1, 'npc'),
    interpolate = TRUE
  )
  
  # Create the plot
  ggplot(data = subset(data, Group == age_group), aes(x = Treatment, y = value, fill = Treatment)) +
    annotation_custom(background_gradient, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    geom_boxplot(width = 0.72) +
    geom_signif(
      comparisons = list(c('Abundant', 'Rare')),
      map_signif_level = function(p) { paste('italic(P) == ', sprintf('%.4g', p)) },
      y_position = signif_position,
      test = 't.test',
      textsize = 3.6,
      tip_length = 0.03,
      parse = TRUE
    ) +
    scale_fill_manual(values = c('#999999', ifelse(age_group != 'ACE', color_fill, '#4180C3'))) +
    scale_y_continuous(limits = ylim_range, breaks = y_ticks, expand = c(0, 0)) +
    labs(title = plot_title, x = NULL, y = expression("Bacterial α-diversities")) +
    theme_classic(base_size = 10) +
    theme(
      plot.margin = margin(t = 5, r = 10, b = 0, l = 10),
      plot.title = element_textbox_simple(
        size = 10,
        color = '#000000',
        halign = 0.5,
        lineheight = 1.25,
        fill = color_fill,
        width = 1.2,
        padding = margin(5, 0, 5, 0),
        margin = margin(0, 0, 20, 0)
      ),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = 'black'),
      axis.text.y = element_text(color = 'black'),
      legend.position = 'none'
    )
}

# 6. Define plotting parameters for each age group
plot_configs <- list(
  list(
    age_group = 'ACE',
    title = 'ACE',
    ylim = c(0, 1000),
    yticks = seq(0, 1000, 200),
    signif_y = 800,
    fill_color = '#FFDD8E',
    gradient_bg = c('#FFFFFF', '#F8F6C4', '#FFDD8E')
  ),
  list(
    age_group = 'Shannon',
    title = 'Shannon',
    ylim = c(0, 10),
    yticks = seq(0, 10, 2),
    signif_y = 8,
    fill_color = '#2C9678',
    gradient_bg = c('#FFFFFF', '#70CDBE', '#2C9678')
  ),
  list(
    age_group = 'Simpson',
    title = 'Simpson',
    ylim = c(0, 1),
    yticks = seq(0, 1, 0.2),
    signif_y = 0.8,
    fill_color = '#AC99D2',
    gradient_bg = c('#FFFFFF', '#B3A6C6', '#AC99D2')
  )
)

# 7. Generate individual plots for each group (only leftmost plot shows Y-axis label)
group_plots <- lapply(seq_along(plot_configs), function(i) {
  config <- plot_configs[[i]]
  
  # Generate the plot
  plot <- generate_group_plot(
    data = transformed_data,
    age_group = config$age_group,
    ylim_range = config$ylim,
    y_ticks = config$yticks,
    signif_position = config$signif_y,
    plot_title = config$title,
    color_fill = config$fill_color,
    gradient_colors = config$gradient_bg
  )
  
  # Hide Y-axis text and ticks for all but the first plot
  if (i > 1) {
    plot <- plot + theme(axis.title.y = element_blank())
  }
  return(plot)
})

# 8. Combine all plots into a single figure
gg <- wrap_plots(group_plots, ncol = 5)
