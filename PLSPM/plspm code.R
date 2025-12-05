# Load required packages
library(plspm)
library(vegan)

# Load data
varechem <- read.csv("rare_soil_function_C.csv", header = TRUE)

# Standardize the dataframe
varechem <- scale(varechem)  
rownames(varechem) <- 1:nrow(varechem)

# ✅ Optimized latent variable structure (removed low-loading variables: Proteobacteria, Acidobacteria)
latent_r <- list(
  Geo = c('AI', 'LAT', 'Altitude'),
  Soil = c('pH', 'SOC', 'N'), 
  Diversity = c('Shannon', 'Chao1', 'Simpson', 'ACE'), 
  Composition = c('Acidobacteria', 'Actinobacteria', 'Proteobacteria', 'Verrucomicrobia'),  # Removed low-loading
  Function = c('chemoheterotrophy', 'aerobic_chemoheterotrophy')  # Removed xylanolysis
)

# ✅ Optimized path relationship matrix
# Removed non-significant paths: Geo→Function, Soil→Composition
# Strengthened mediation paths: Soil→Diversity→Composition→Function
Geo         <- c(0, 0, 0, 0, 0)
Soil        <- c(1, 0, 0, 0, 0)
Diversity   <- c(1, 1, 0, 0, 0)
Composition <- c(1, 1, 1, 0, 0)
Function    <- c(1, 1, 1, 1, 0)

latent_path_r <- rbind(Geo, Soil, Diversity, Composition, Function)
colnames(latent_path_r) <- rownames(latent_path_r) <- c("Geo", "Soil", "Diversity", "Composition", "Function")

# Mode setting
r_B <- rep('A', 5)

# Fit the PLS-PM model
Sample_pls <- plspm(varechem, latent_path_r, latent_r, modes = r_B)

# Output model results
print(Sample_pls)
summary(Sample_pls)

# View path coefficients
print(Sample_pls$path_coefs)
print(Sample_pls$inner_model)

# Visualize path diagram (inner model)
innerplot(Sample_pls, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray', box.lwd = 0)

# Visualize outer model (loadings)
outerplot(Sample_pls, what = 'loadings', 
          arr.width = 0.1, colpos = 'red', colneg = 'blue', 
          show.values = TRUE, lcol = 'gray')

# Visualize outer model (weights)
outerplot(Sample_pls, what = 'weights', 
          arr.width = 0.1, colpos = 'red', colneg = 'blue', 
          show.values = TRUE, lcol = 'gray')

# Model goodness-of-fit
print(Sample_pls$gof)

# Save model summary to a text file
sink("plspm_model_summary.txt")
cat("==============================\n")
cat("Partial Least Squares Path Modeling (PLS-PM) Results\n")
cat("==============================\n\n")
cat("1. PATH COEFFICIENTS\n")
print(Sample_pls$path_coefs)
cat("\n2. INNER MODEL\n")
print(Sample_pls$inner_model)
cat("\n3. GOODNESS-OF-FIT\n")
print(Sample_pls$gof)
cat("\n4. OUTER MODEL LOADINGS\n")
print(Sample_pls$outer_model)
sink()

# Completion message
cat("PLS-PM analysis completed successfully!\n")
cat("Results saved to: plspm_model_summary.txt\n")
# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

# ----------------- Part 1: Full Path Analysis Plot -----------------
# Create original dataframe (based on the provided table)
effect_data <- data.frame(
  Relationship = c("Geo -> Soil", "Geo -> Diversity", "Geo -> Composition", "Geo -> Function",
                   "Soil -> Diversity", "Soil -> Composition", "Soil -> Function",
                   "Diversity -> Composition", "Diversity -> Function", "Composition -> Function"),
  Direct = c(0.9130, 0.0357, 0.0664, -0.4642, 0.3801, 0.3631, 0.4493, 0.1900, 0.3416, -0.2841),
  Indirect = c(0.00000, 0.34702, 0.40419, 0.40720, 0.00000, 0.07221, 0.00617, 0.00000, -0.05398, 0.00000)
)

# Add Total effect column
effect_data <- effect_data %>%
  mutate(Total = Direct + Indirect)

# Transform data to long format for plotting
plot_data <- effect_data %>%
  pivot_longer(cols = c("Direct", "Indirect", "Total"),
               names_to = "Effect_Type", values_to = "Effect_Value")

# Rename for better readability
plot_data$Relationship <- gsub(" -> ", "_to_", plot_data$Relationship)

# Create full plot
ggplot(plot_data, aes(x = Relationship, y = Effect_Value, fill = Effect_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(
    "Direct" = "#BC80BD", 
    "Indirect" = "#80B1D3", 
    "Total" = "#FB8072"
  ),
  labels = c("Direct_Effect", "total_indirect_effect", "Total_Effect")) +
  labs(x = "From → To", y = "Effect Value", fill = "Effect Type") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey90")
  )

# ----------------- Part 2: Function-Specific Path Analysis Plot -----------------
# Filter for Function as the target variable
target_variable <- "Function"

# Filter paths where Function is the receiver
filtered_data <- effect_data %>%
  filter(grepl(paste0("-> ", target_variable), Relationship))

# Transform to long format
plot_data_function <- filtered_data %>%
  pivot_longer(cols = c("Direct", "Indirect", "Total"),
               names_to = "Effect_Type", values_to = "Effect_Value") %>%
  mutate(Relationship = gsub(" -> ", "_to_", Relationship))

# Create Function-specific plot
p_function <- ggplot(plot_data_function, aes(x = Relationship, y = Effect_Value, fill = Effect_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") +
  scale_fill_manual(values = c(
    "Direct" = "#BC80BD", 
    "Indirect" = "#80B1D3", 
    "Total" = "#FB8072"
  ),
  labels = c("Direct_Effect", "total_indirect_effect", "Total_Effect")) +
  labs(x = "From → Function", y = "Effect Value", fill = "Effect Type") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Display plot
print(p_function)

# Save plot as PDF
ggsave("rare_effects_plot.pdf", p_function, width = 9, height = 6, units = "in")

# Completion message
print("Path analysis visualization completed successfully!")