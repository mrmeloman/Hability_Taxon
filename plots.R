library("ggplot2")
library(gridExtra)

mambac.x <- c()
mambac.y <- c()

maxeig.x <- c()
maxeig.y <- c()

lmode.x <- c()
lmode.y <- c()


mambac.y.range <- c()
maxeig.y.range <- c()
lmode.y.range <- c()

#Double empirical values to allow the simulated polygon to be more smooth
mambac.x <- rep(mambac.x, each = 2)
mambac.y <- rep(mambac.y, each = 2)

mamb <- data.frame(mambac.x, mambac.y)
names(mamb)[names(mamb) == "mambac.x"] <- "xvalues"
names(mamb)[names(mamb) == "mambac.y"] <- "yvalues"

mambac.polygon.x1 <- c()
mambac.polygon.x2 <- c()
mambac.polygon.y1 <- c()
mambac.polygon.y2 <- c()

mambac.polygon.x <- c(mambac.polygon.x1, mambac.polygon.x2)
mambac.polygon.y <- c(mambac.polygon.y1, mambac.polygon.y2)

#X is the same as in data
mambac.line1.y <- rep(c(), each = 2)

#X is the same as in data
mambac.line2.y <- rep(c(), each = 2)

m1 <- ggplot(mamb, aes(x = xvalues, y = yvalues)) +
  geom_polygon(inherit.aes = FALSE, aes(x = mambac.polygon.x, y = mambac.polygon.y), fill = "#0b5db5") +
  geom_line(size = 1.5) +
  geom_line(aes(y = mambac.line1.y), size = 1.2, color = "#0b5db5") +
  geom_line(aes(y = mambac.line2.y), size = 1.2, color = "#0b5db5") +
  ylim(mambac.y.range) +
  ggtitle(label = "Categorical comparison data", subtitle = "MAMBAC") +
  xlab("Cut") + ylab("Mean Difference") +
  theme(
    plot.title = element_text(size=16, hjust = 0.5),
    plot.subtitle = element_text(size=12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black", size = 0.9)
  )

m2 <- ggplot(mamb, aes(x = xvalues, y = yvalues)) +
  geom_polygon(inherit.aes = FALSE, aes(x = mambac.polygon.x, y = mambac.polygon.y), fill = "#0b5db5") +
  geom_line(size = 1.5) +
  geom_line(aes(y = mambac.line1.y), size = 1.2, color = "#0b5db5") +
  geom_line(aes(y = mambac.line2.y), size = 1.2, color = "#0b5db5") +
  ylim(mambac.y.range) +
  ggtitle(label = "MAXEIG") +
  xlab("Score") + ylab("Eigen Value") +
  theme(
    plot.title = element_text(size=12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black", size = 0.9)
  )  

m3 <- ggplot(mamb, aes(x = xvalues, y = yvalues)) +
  geom_polygon(inherit.aes = FALSE, aes(x = mambac.polygon.x, y = mambac.polygon.y), fill = "#0b5db5") +
  geom_line(size = 1.5) +
  geom_line(aes(y = mambac.line1.y), size = 1.2, color = "#0b5db5") +
  geom_line(aes(y = mambac.line2.y), size = 1.2, color = "#0b5db5") +
  ylim(mambac.y.range) +
  ggtitle(label = "L-Mode") +
  xlab("Factor Score") + ylab("Density") +
  theme(
    plot.title = element_text(size=12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black", size = 0.9)
  )

m4 <- ggplot(mamb, aes(x = xvalues, y = yvalues)) +
  geom_polygon(inherit.aes = FALSE, aes(x = mambac.polygon.x, y = mambac.polygon.y), fill = "#5aa007") +
  geom_line(size = 1.5) +
  geom_line(aes(y = mambac.line1.y), size = 1.2, color = "#5aa007") +
  geom_line(aes(y = mambac.line2.y), size = 1.2, color = "#5aa007") +
  ylim(mambac.y.range) +
  ggtitle(label = "Dimensional comparison data", subtitle = "MAMBAC") +
  xlab("Cut") + ylab("Mean Difference") +
  theme(
    plot.title = element_text(size=16, hjust = 0.5),
    plot.subtitle = element_text(size=12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black", size = 0.9),
    axis.line.y = element_blank()
  )  

m5 <- ggplot(mamb, aes(x = xvalues, y = yvalues)) +
  geom_polygon(inherit.aes = FALSE, aes(x = mambac.polygon.x, y = mambac.polygon.y), fill = "#5aa007") +
  geom_line(size = 1.5) +
  geom_line(aes(y = mambac.line1.y), size = 1.2, color = "#5aa007") +
  geom_line(aes(y = mambac.line2.y), size = 1.2, color = "#5aa007") +
  ylim(mambac.y.range) +
  ggtitle(label = "MAXEIG") +
  xlab("Score") + ylab("Eigen Value") +
  theme(
    plot.title = element_text(size=12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black", size = 0.9),
    axis.line.y = element_blank()
  )    

m6 <- ggplot(mamb, aes(x = xvalues, y = yvalues)) +
  geom_polygon(inherit.aes = FALSE, aes(x = mambac.polygon.x, y = mambac.polygon.y), fill = "#5aa007") +
  geom_line(size = 1.5) +
  geom_line(aes(y = mambac.line1.y), size = 1.2, color = "#5aa007") +
  geom_line(aes(y = mambac.line2.y), size = 1.2, color = "#5aa007") +
  ylim(mambac.y.range) +
  ggtitle(label = "L-Mode") +
  xlab("Factor Score") + ylab("Density") +
  theme(
    plot.title = element_text(size=12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black", size = 0.9),
    axis.line.y = element_blank()
  )    


grid.arrange(m1, m4, m2, m5, m3, m6, nrow = 3)

