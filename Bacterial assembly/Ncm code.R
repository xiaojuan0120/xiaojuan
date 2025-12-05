# 清除环境变量
rm(list = ls())

# 加载所需的包
library(ggplot2)
install.packages("ggimage")
library(ggimage)
library(plyr)
library(Hmisc)
library(minpack.lm)
library(stats4)

spp <- read.csv("Abundant.csv", head = TRUE, row.names = 1)

spp <- t(spp)

N <- mean(apply(spp, 1, sum))

p.m <- apply(spp, 2, mean)

p.m <- p.m[p.m != 0]

p <- p.m / N

spp.bi <- 1 * (spp > 0)

freq <- apply(spp.bi, 2, mean)
freq <- freq[freq != 0]

C <- merge(p, freq, by = 0)
C <- C[order(C[, 2]), ]
C <- as.data.frame(C)
C.0 <- C[!(apply(C, 1, function(y) any(y == 0))), ]
p <- C.0[, 2]
freq <- C.0[, 3]
names(p) <- C.0[, 1]
names(freq) <- C.0[, 1]

d = 1 / N
m.fit <- nlsLM(freq ~ pbeta(d, N * m * p, N * m * (1 - p), lower.tail = FALSE), start = list(m = 0.1))

summary(m.fit)

m.ci <- confint(m.fit, 'm', level = 0.95)

freq.pred <- pbeta(d, N * coef(m.fit) * p, N * coef(m.fit) * (1 - p), lower.tail = FALSE)

pred.ci <- binconf(freq.pred * nrow(spp), nrow(spp), alpha = 0.05, method = "wilson", return.df = TRUE)

Rsqr <- 1 - (sum((freq - freq.pred)^2)) / (sum((freq - mean(freq))^2))

bacnlsALL <- data.frame(p, freq, freq.pred, pred.ci[, 2:3])
head(bacnlsALL)

bacnlsALL$group <- with(bacnlsALL, ifelse(freq < Lower, "#a57eb6",
                                          ifelse(freq > Upper, "#f6d378", "#f3bbb1")))
p1 <- ggplot(data = bacnlsALL) +
  geom_line(aes(x = log(p), y = freq.pred), size = 0.6, linetype = 1) +
  geom_line(aes(x = log(p), y = Lower), size = 0.6, linetype = 2) +
  geom_line(aes(x = log(p), y = Upper), size = 0.6, linetype = 2) +
  geom_point(aes(x = log(p), y = freq, color = group), size = 1) +
  xlab("log10(mean relative abundance)") +
  ylab("occurrence frequency") +
  scale_colour_manual(values = c("#a57eb6", "#f6d378", "#f3bbb1")) +
  annotate("text", x = -7.5, y = 0, label = paste("Nm=", round(N, 3)), size = 4) +
  annotate("text", x = -7.5, y = 0.10, label = paste("R2=", round(Rsqr, 3)), size = 4) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 12),
    text = element_text(family = "sans", size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

data <- as.data.frame(table(bacnlsALL$group))
colnames(data) <- c("group", "nums")

data$type <- c("mid", "low", "high")

data$percentage <- round(data$nums / sum(data$nums) * 100, 1)

data$label <- paste(data$type, paste(data$percentage, "%", sep = ''))

p2 <- ggplot(data, aes(x = "", y = nums, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(
    values = c("#f6d378", "#a57eb6", "#f3bbb1"),
    labels = data$label
  ) +
  theme_void() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 8)
  )

library(patchwork)
p <- p1 + geom_subview(subview = p2, x = -12, y = 0.9, w = 4, h = 4)

print(p)
ggsave("1.pdf", p1, height = 4, width = 5)
ggsave("2.pdf", p2, height = 4, width = 5)
