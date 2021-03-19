# load files
library(tidyverse)
library(patchwork)
library(kableExtra)
library(lfe)
library(lmtest)
library(xtable)
library(did)
library(magrittr)
library(ggthemes)
library(Manu)

# set my ggplot theme
theme_set(
  theme_clean() + 
    theme(plot.background = element_blank(),
          legend.background = element_rect(color = "white"))
  )

## Link to folder you want to output too. Please no o ne hack my overleaf folder.
dropbox <- "/Users/Andrew/Dropbox/Apps/Overleaf/bakerlarckerwang/Write_Up/"

# Plot 1 - Trends ---------------------------------------------------------

# make data
data <- tibble(
  Y = c(2, 5, 1, 2),
  Unit = c("Treat", "Treat", "Control", "Control"),
  T = c(0, 1, 0, 1)
)

# Make plot 1 - change in trends over time
plot1a <- data %>% 
  ggplot(aes(x = T, y = Y, group = Unit, color = Unit)) + 
  geom_point() + 
  geom_line() + 
  geom_label(aes(label = Y), hjust = 0.5, vjust = -0.5, color = "black") + 
  labs(x = "", y = "Outcome \n Variable") + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) +
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Pre", "Post")) + 
  ggtitle('Panel A') + 
  ylim(c(0, 6)) + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# Make plot 2 - remove baseline differences
# make a dataset that removes first difference
data_fd <- data %>% 
  group_by(Unit) %>% 
  mutate(Y = Y - Y[which(T == 0)])

# plot
plot1b <- data %>% 
  ggplot(aes(x = T, y = Y, group = Unit, color = Unit)) + 
  geom_point(alpha = 1/2) + 
  geom_line(alpha = 1/2, linetype = "dashed") + 
  geom_point(data = data_fd, aes(x = T, y = Y, group = Unit, color = Unit)) +
  geom_line(data = data_fd, aes(x =  T, y = Y, group = Unit, color = Unit)) +
  annotate("label", x = 1, y = 1, label = "1") + 
  annotate("label", x = 1, y = 3, label = "3") + 
  # these colors correspond to certain birds native to new zealand. This is the 
  # only asymptotically correct manner to pick color palettes.
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) +
  labs(x = "", y = "") + 
  ylim(c(0, 6)) + 
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Pre", "Post")) + 
  ggtitle('Panel B') + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# Make plot 3 - difference in differences
# dataset with the first differences
data_dd <- tribble(
  ~Unit, ~Diff,
  "Treat", 3, 
  "Control", 1
)

# plot
plot1c <- data_dd %>% 
  ggplot(aes(x = Unit, y = Diff, group = Unit, fill = Unit)) + 
  geom_col() + 
  scale_fill_manual(values = c("#A7473A", "#4B5F6C")) +
  labs(x = "", y = expression(Delta)) + 
  ylim(0, 3.5) + 
  geom_label(aes(label = Diff), fill = "white", vjust = -0.2) + 
  annotate("segment", x = 2, xend = 2, y = 1, yend = 3, color = "white", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 2, xend = 2, y = 3, yend = 1, color = "white", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 1.5, xend = 2.5, y = 1, yend = 1, color = "white") +
  annotate("label", x = 2, y = 2, label = "Treatment Effect \n = 2") + 
  ggtitle('Panel C') + 
  labs(y = expression(Delta), x = "") + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360, size = 14),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# combine subplots
plot1 <- plot1a + plot1b + plot1c

# save
ggsave(plot1, filename = paste(dropbox, "DID_schema.png", sep = ""), dpi = 500,
       width = 10, height = 4)

# Plot - GB1 ---------------------------------------------------------
# Goodman-bacon decomp, overall information
data <- tibble(
  time = 0:100,
  U = seq(5, 12, length.out = 101),
  l = seq(10, 17, length.out = 101) + c(rep(0, 85), rep(15, 16)),
  k = seq(18, 25, length.out = 101) + c(rep(0, 34), rep(10, 67))
) %>% 
  pivot_longer(-time, names_to = "series", values_to = "value")

# plot
GB1 <- data %>% 
  ggplot(aes(x = time, y = value, group = series, color = series, shape = series)) + 
  geom_line(size = 2) + geom_point(size = 2) +
  geom_vline(xintercept = c(34, 85)) +
  labs(x = "Time", y = "Units \n of y") +
  scale_x_continuous(limits = c(0, 100), breaks = c(34, 85), 
                     labels = c(expression('t'['k']^'*'), expression('t'['l']^'*')), 
                     expand = c(0, 0)) + 
  annotate("text", x = 10, y = 21, label = expression('y'^'k'), size = 9) +
  annotate("text", x = 50, y = 16, label = expression('y'^'l'), size = 9) +
  annotate("text", x = 90, y = 14, label = expression('y'^'U'), size = 9) +
  annotate('label', x = 17, y = 4, label = 'T1') +
  annotate('label', x = 60, y = 4, label = 'T2') +
  annotate('label', x = 93, y = 4, label = 'T3') +
  annotate("segment", x = 1, xend = 33, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 33, xend = 1, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 35, xend = 84, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 84, xend = 35, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) + 
  annotate("segment", x = 86, xend = 99, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 99, xend = 86, y = 2, yend = 2, color = "black", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.position = 'none',
        plot.background = element_blank(),
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.ticks.y = element_blank())

# save  
ggsave(GB1, filename = paste(dropbox, "GB_Full.png", sep = ""), dpi = 500,
       width = 10, height = 6)

# Plot - GB2 ---------------------------------------------------------

# function to make subplots
make_subplot <- function(omit, keep_dates, colors, breaks, break_expressions, series, 
                         series_x, series_y, break_names, break_loc, arrow_start, arrow_stop, title){
  
  data %>% 
    filter(series != omit & time >= keep_dates[1] & time <= keep_dates[2]) %>% 
    ggplot(aes(x = time, y = value, group = series, color = series, shape = series)) + geom_line() + geom_point() +
    geom_vline(xintercept = breaks) + 
    labs(x = "Time", y = "Units \n of y") +
    scale_x_continuous(limits = c(0, 105), breaks = breaks, 
                       labels = break_expressions, 
                       expand = c(0, 0)) + 
    annotate("text", x = series_x[1], y = series_y[1], label = series[1]) +
    annotate("text", x = series_x[2], y = series_y[2], label = series[2]) +
    annotate('label', x = break_loc[1], y = 5, label = break_names[1]) +
    annotate('label', x = break_loc[2], y = 5, label = break_names[2]) +
    annotate("segment", x = arrow_start[1], xend = arrow_stop[1], y = 2, yend = 2, color = "black", 
             arrow = arrow(length = unit(0.1, "inches"))) +
    annotate("segment", x = arrow_stop[1], xend = arrow_start[1], y = 2, yend = 2, color = "black", 
             arrow = arrow(length = unit(0.1, "inches"))) +
    annotate("segment", x = arrow_start[2], xend = arrow_stop[2], y = 2, yend = 2, color = "black", 
             arrow = arrow(length = unit(0.1, "inches"))) +
    annotate("segment", x = arrow_stop[2], xend = arrow_start[2], y = 2, yend = 2, color = "black", 
             arrow = arrow(length = unit(0.1, "inches"))) + 
    scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
    scale_color_manual(values = c(colors[1], colors[2])) +  
    ggtitle(title) + 
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          legend.position = 'none',
          plot.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          axis.ticks.y = element_blank())
 }

# make the four subplots
p1 <- make_subplot(omit = "l", keep_dates = c(0, 100), colors = c('#A7473A', '#51806a'), breaks = 34, 
                   break_expressions = expression('t'['k']^'*'), 
                   series = c(expression('y'^'k'), expression('y'^'U')),
                   series_x = c(10, 90), series_y = c(23, 16), 
                   break_names = c('T1', 'T2 + T3'), break_loc = c(17, 66), 
                   arrow_start = c(1, 35), arrow_stop = c(33, 99), 
                   title = bquote(paste('A. Early Group vs. Untreated Group')))

p2 <- make_subplot(omit = "k", keep_dates = c(0, 100), colors = c('#4B5F6C', '#51806a'), breaks = 85, 
                   break_expressions = expression('t'['l']^'*'), 
                   series = c(expression('y'^'l'), expression('y'^'U')),
                   series_x = c(50, 90), series_y = c(18, 16), 
                   break_names = c('T1 + T2', 'T3'), break_loc = c(50, 95), 
                   arrow_start = c(1, 86), arrow_stop = c(84, 99), 
                   title = bquote(paste('B. Late Group vs. Untreated Group')))

p3 <- make_subplot(omit = "U", keep_dates = c(0, 84), colors = c('#A7473A', '#4B5F6C'), breaks = c(34, 85), 
                   break_expressions = c(expression('t'['k']^'*'), expression('t'['l']^'*')), 
                   series = c(expression('y'^'k'), expression('y'^'l')),
                   series_x = c(10, 50), series_y = c(23, 18), 
                   break_names = c('T1', 'T2'), break_loc = c(17, 60), 
                   arrow_start = c(1, 35), arrow_stop = c(33, 84), 
                   title = bquote(paste('C. Early Group vs. Late Group, before ', 't'['l']^'*', sep = " ")))

p4 <- make_subplot(omit = "U", keep_dates = c(34, 100), colors = c('#A7473A', '#4B5F6C'), breaks = c(34, 85), 
                   break_expressions = c(expression('t'['k']^'*'), expression('t'['l']^'*')), 
                   series = c(expression('y'^'k'), expression('y'^'l')),
                   series_x = c(60, 50), series_y = c(36, 18), 
                   break_names = c('T2', 'T3'), break_loc = c(60, 95), 
                   arrow_start = c(35, 86), arrow_stop = c(84, 99), 
                   title = bquote(paste('D. Late Group vs. Early Group, after ', 't'['k']^'*', sep = " ")))

# combine plots
GB2 <- p1 + p2 + p3 + p4 + plot_layout(nrow = 2)

# save
ggsave(GB2, filename = paste(dropbox, "GB_subs.png", sep = ""), dpi = 500,
       width = 10, height = 6)