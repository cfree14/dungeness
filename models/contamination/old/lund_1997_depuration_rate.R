
# Packages
library(tidyverse)

# Lund 1997 data
# μg = ppm 
data <- tibble(day=c(1,7,14,21),
               n=c(8,6,7,7),
               da_ppm=c(69.5, 43.4, 19, 7.6),
               perc=c(100, 62, 27, 11))

# Fit exponential decay
lmfit <- lm(log(da_ppm) ~ day, data)

# Predictions for plotting
x <- seq(1,30,1)
y <- exp(predict(lmfit, newdata = data.frame(day=x)))
preds <- tibble(x=x,
                y=y)

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.subtitle = element_text(size=9),
                  plot.title=element_text(size=11),
                  legend.position="none",
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data and fit
g <- ggplot(data, aes(x = day, y = da_ppm)) + 
  geom_point() + 
  geom_line(preds, mapping=aes(x=x, y=y, color = "red")) +
  geom_hline(yintercept=30, linetype="dotted") +
  annotate("text", x=30, y=32, hjust=1, label="Management action level (30 ppm)", size=2) +
  labs(x="Day", y="Domoic acid concentration (ppm)", 
       title="Depuration of domoic acid from Dungeness crabs",
       subtitle=expression("Lund et al. 1997"*italic("Journal of Shellfish Research"))) +
  theme_bw() + my_theme
g

# Export
plotdir <- "models/contamination/figures"
ggsave(g, filename=file.path(plotdir, "FigSX_lund_depuration_rate.png"), width=4, height=4, units="in", dpi=600)
