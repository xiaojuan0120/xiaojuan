library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("Abundant ACE-Environment Regression data.csv")

data_long <- data %>%
  pivot_longer(cols = -ACE, names_to = "variable", values_to = "value")



model_Ph <- lm(Ph ~ ACE, data = data)
summary_Ph <- summary(model_Ph)
r_squared_Ph <- round(summary_Ph$r.squared, 2)
p_value_Ph <- signif(summary_Ph$coefficients[2, 4], 3)
formula_Ph <- paste0("y = ", round(coef(model_Ph)[1], 2), " + ", round(coef(model_Ph)[2], 2), "x")

model_SOC <- lm(SOC ~ ACE, data = data)
summary_SOC <- summary(model_SOC)
r_squared_SOC<- round(summary_SOC$r.squared, 2)
p_value_SOC<- signif(summary_Ph$coefficients[2, 4], 3)
formula_SOC<- paste0("y = ", round(coef(model_SOC)[1], 2), " + ", round(coef(model_SOC)[2], 2), "x")

model_N <- lm(N ~ ACE, data = data)
summary_N <- summary(model_N)
r_squared_N <- round(summary_N$r.squared, 2)
p_value_N <- signif(summary_N$coefficients[2, 4], 3)
formula_N <- paste0("y = ", round(coef(model_N)[1], 2), " + ", round(coef(model_N)[2], 2), "x")

model_LAT <- lm(LAT ~ ACE, data = data)
summary_LAT <- summary(model_LAT)
r_squared_LAT <- round(summary_LAT$r.squared, 2)
p_value_LAT <- signif(summary_LAT$coefficients[2, 4], 3)
formula_LAT <- paste0("y = ", round(coef(model_LAT)[1], 2), " + ", round(coef(model_LAT)[2], 2), "x")

model_Altitude <- lm(Altitude ~ ACE, data = data)
summary_Altitude <- summary(model_Altitude)
r_squared_Altitude <- round(summary_Altitude$r.squared, 2)
p_value_Altitude <- signif(summary_Altitude$coefficients[2, 4], 3)
formula_Altitude <- paste0("y = ", round(coef(model_Altitude)[1], 2), " + ", round(coef(model_Altitude)[2], 2), "x")

model_NDVI <- lm(NDVI ~ ACE, data = data)
summary_NDVI <- summary(model_NDVI)
r_squared_NDVI <- round(summary_NDVI$r.squared, 2)
p_value_NDVI <- signif(summary_NDVI$coefficients[2, 4], 3)
formula_NDVI <- paste0("y = ", round(coef(model_NDVI)[1], 2), " + ", round(coef(model_NDVI)[2], 2), "x")

model_AI <- lm(AI ~ ACE, data = data)
summary_AI <- summary(model_AI)
r_squared_AI <- round(summary_AI$r.squared, 2)
p_value_AI <- signif(summary_AI$coefficients[2, 4], 3)
formula_AI <- paste0("y = ", round(coef(model_AI)[1], 2), " + ", round(coef(model_AI)[2], 2), "x")

labels <- data.frame(
  variable = c("Ph", "SOC", "N", "LAT", "Altitude", "NDVI", "AI"),
  formula = c(formula_Ph, formula_SOC, formula_N, formula_LAT, formula_Altitude, formula_NDVI, formula_AI),
  r_squared = c(r_squared_Ph, r_squared_SOC, r_squared_N, r_squared_LAT, r_squared_Altitude, r_squared_NDVI, r_squared_AI),
  p_value = c(p_value_Ph, p_value_SOC, p_value_N, p_value_LAT, p_value_Altitude, p_value_NDVI, p_value_AI)
)

colors <- c(
  "Ph" = "#1597A5", 
  "SOC" = "#FB9A99", 
  "N" = "#FDBF6F", 
  "LAT" = "#33A02C", 
  "Altitude" = "#A6CEE3", 
  "NDVI" = "#FF7F00", 
  "AI" = "#CAB2D6"
)
# 绘图
ggplot(data_long, aes(x = value, y = ACE, color = variable)) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_manual(values = colors) +
  geom_smooth(data = filter(data_long, variable == "Ph"), method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#1597A5",fill="#1597A5") +
  geom_smooth(data = filter(data_long, variable == "SOC"), method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#FB9A99",fill="#FB9A99") +
  geom_smooth(data = filter(data_long, variable == "N"), method = "lm", formula = y ~ x, se = TRUE, color = "#FDBF6F",fill = "#FDBF6F") +
  geom_smooth(data = filter(data_long, variable == "LAT"), method = "lm", formula = y ~ x, se = TRUE, color = "#33A02C",fill= "#33A02C") +
  geom_smooth(data = filter(data_long, variable == "Altitude"), method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#A6CEE3",fill="#A6CEE3") +
  geom_smooth(data = filter(data_long, variable == "NDVI"), method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#FF7F00",fill="#FF7F00") +
  geom_smooth(data = filter(data_long, variable == "AI"), method = "lm", formula = y ~ x, se = TRUE, color = "#CAB2D6",fill = "#CAB2D6") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 10))+
  # 添加注释
  geom_text(data = labels, aes(x = Inf, y = Inf, 
                               label = paste(formula, "\nR² = ", 
                                             r_squared, "\nP = ", p_value)),
            hjust = 1.1, vjust = 1.1, size = 3.5, color = "black")
p <- ggplot(data_long, aes(x = value, y = bnti, color = variable)) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_manual(values = colors) +
  geom_smooth(data = filter(data_long, variable == "Ph"), method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#1597A5",fill="#1597A5") +
  geom_smooth(data = filter(data_long, variable == "SOC"), method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#FB9A99",fill="#FB9A99") +
  geom_smooth(data = filter(data_long, variable == "N"), method = "lm", formula = y ~ x, se = TRUE, color = "#FDBF6F",fill = "#FDBF6F") +
  geom_smooth(data = filter(data_long, variable == "LAT"), method = "lm", formula = y ~ x, se = TRUE, color = "#33A02C",fill= "#33A02C") +
  geom_smooth(data = filter(data_long, variable == "Altitude"), method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#A6CEE3",fill="#A6CEE3") +
  geom_smooth(data = filter(data_long, variable == "NDVI"), method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#FF7F00",fill="#FF7F00") +
  geom_smooth(data = filter(data_long, variable == "AI"), method = "lm", formula = y ~ x, se = TRUE, color = "#CAB2D6",fill = "#CAB2D6") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 10))+
  # 添加注释
  geom_text(data = labels, aes(x = Inf, y = Inf, 
                               label = paste(formula, "\nR² = ", 
                                             r_squared, "\nP = ", p_value)),
            hjust = 1.1, vjust = 1.1, size = 3.5, color = "black")
p
dev.off()