# load packages
library(tidyverse)
library(patchwork)
library(kableExtra)
library(lfe)
library(lmtest)
library(xtable)
library(did)
library(magrittr)
library(ggthemes)
library(bacondecomp)
library(multcomp)
library(fastDummies)
library(ggforce)

# set plot features
select <- dplyr::select
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))
dropbox <- "/Users/Andrew/Dropbox/Apps/Overleaf/bakerlarckerwang/Write_Up/"

# set seed - CS method uses bootstraping
set.seed(2140851)

# Make Data  ---------------------------------------------
# Data 1 - One Treatment Period, Constant Treatment Effects --------------
make_data1 <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 0.5),
    # generate state
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    # generate treatment groups
    group = case_when(
      state %in% 1:25 ~ 1,
      state %in% 26:50 ~ 2
    ),
    evertreated = ifelse(group == 2, "T", "C"),
    # avg yearly treatment effects by group
    hat_gamma = case_when(
      group == 2 ~ 2,
      TRUE ~ 0
    )) %>%
    # generate unit specific yearly treatment effects 
    rowwise() %>% 
    mutate(gamma = ifelse(group == 2, rnorm(1, hat_gamma, .2), 0)) %>% 
    ungroup()
  
  # year fixed effects 
  year <- tibble(
    year = 1980:2015,
    year_fe = rnorm(36, 0, 0.5))
  
  # full interaction of unit X year 
  crossing(unit, year) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(nrow(.), 0, 0.5),
           treat = ifelse(evertreated == "T" & year >= 1998, 1, 0),
           tau = ifelse(treat == 1, gamma, 0)) %>%
    # calculate the dep variable
    mutate(dep_var = unit_fe + year_fe + tau + error)
}

# make data
data1 <- make_data1()

# Data 2 - One Treatment Period, Dynamic Treatment Effects --------------
make_data2 <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 0.5),
    # generate state
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    # generate treatment groups
    group = case_when(
      state %in% 1:25 ~ 1,
      state %in% 26:50 ~ 2
    ),
    evertreated = ifelse(group == 2, "T", "C"),
    # avg yearly treatment effects by group
    hat_gamma = case_when(
      group == 2 ~ .3,
      TRUE ~ 0
    )) %>%
    # generate unit specific yearly treatment effects 
    rowwise() %>% 
    mutate(gamma = rnorm(1, hat_gamma, .2), 0) %>% 
    ungroup()
  
  # year fixed effects 
  year <- tibble(
    year = 1980:2015,
    year_fe = rnorm(36, 0, 0.5))
  
  # full interaction of unit X year 
  crossing(unit, year) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(nrow(.), 0, 0.5),
           treat = ifelse(evertreated == "T" & year >= 1998, 1, 0),
           tau = ifelse(treat == 1, gamma, 0)) %>%
    # calculate the dep variable
    group_by(unit) %>% 
    mutate(cumtau = cumsum(tau)) %>% 
    mutate(dep_var = unit_fe + year_fe + cumtau + error)
}

# make data
data2 <- make_data2()

# Data 3 - Multiple Treatment Periods and Constant Equal Treatment Effects --------------
make_data3 <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 0.5),
    # generate state
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    # generate treatment groups
    group = case_when(
      state %in% 1:17 ~ 1989,
      state %in% 18:35 ~ 1998,
      state %in% 35:50 ~ 2007
    ),
    # avg yearly treatment effects by group
    hat_gamma = case_when(
      group == 1989 ~ 3,
      group == 1998 ~ 3,
      group == 2007 ~ 3
    )) %>%
    # generate unit specific yearly treatment effects 
    rowwise() %>% 
    mutate(gamma = rnorm(1, hat_gamma, .2), 0) %>% 
    ungroup()
  
  # year fixed effects 
  year <- tibble(
    year = 1980:2015,
    year_fe = rnorm(36, 0, 0.5))
  
  # full interaction of unit X year 
  crossing(unit, year) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(nrow(.), 0, 0.5),
           treat = ifelse(year >= group, 1, 0),
           tau = ifelse(treat == 1, gamma, 0)) %>%
    # calculate the dep variable
    mutate(dep_var = unit_fe + year_fe + tau + error)
}

# make data
data3 <- make_data3()

# Data 4 - Multiple Treatment Periods and Constant Different Treatment Effects --------------
make_data4 <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 0.5),
    # generate state
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    # generate treatment groups
    group = case_when(
      state %in% 1:17 ~ 1989,
      state %in% 18:35 ~ 1998,
      state %in% 35:50 ~ 2007
    ),
    # avg yearly treatment effects by group
    hat_gamma = case_when(
      group == 1989 ~ 5,
      group == 1998 ~ 3,
      group == 2007 ~ 1
    )) %>%
    # generate unit specific yearly treatment effects 
    rowwise() %>% 
    mutate(gamma = rnorm(1, hat_gamma, .2), 0) %>% 
    ungroup()
  
  # year fixed effects 
  year <- tibble(
    year = 1980:2015,
    year_fe = rnorm(36, 0, 0.5))
  
  # full interaction of unit X year 
  crossing(unit, year) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(nrow(.), 0, 0.5),
           treat = ifelse(year >= group, 1, 0),
           tau = ifelse(treat == 1, gamma, 0)) %>%
    # calculate the dep variable
    mutate(dep_var = unit_fe + year_fe + tau + error)
}

# make data
data4 <- make_data4()

# Data 5 - Multiple Treatment Periods and Dynamic Equal Treatment Effects --------------
make_data5 <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 0.5),
    # generate state
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    # generate treatment groups
    group = case_when(
      state %in% 1:17 ~ 1989,
      state %in% 18:35 ~ 1998,
      state %in% 35:50 ~ 2007
    ),
    # avg yearly treatment effects by group
    hat_gamma = case_when(
      group == 1989 ~ .3,
      group == 1998 ~ .3,
      group == 2007 ~ .3
    )) %>%
    # generate unit specific yearly treatment effects 
    rowwise() %>% 
    mutate(gamma = rnorm(1, hat_gamma, .2), 0) %>% 
    ungroup()
  
  # year fixed effects 
  year <- tibble(
    year = 1980:2015,
    year_fe = rnorm(36, 0, 0.5))
  
  # full interaction of unit X year 
  crossing(unit, year) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(nrow(.), 0, 0.5),
           treat = ifelse(year >= group, 1, 0),
           tau = ifelse(treat == 1, gamma, 0)) %>%
    # calculate the dep variable
    group_by(unit) %>% 
    mutate(cumtau = cumsum(tau)) %>% 
    mutate(dep_var = unit_fe + year_fe + cumtau + error)
}

# make data
data5 <- make_data5()

# Data 6 - Multiple Treatment Periods and Dynamic Treatment Effects --------------
make_data6 <- function(...) {
  
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:1000, 
    unit_fe = rnorm(1000, 0, 0.5),
    # generate state
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    # generate treatment groups
    group = case_when(
      state %in% 1:17 ~ 1989,
      state %in% 18:35 ~ 1998,
      state %in% 35:50 ~ 2007
    ),
    # avg yearly treatment effects by group
    hat_gamma = case_when(
      group == 1989 ~ .5,
      group == 1998 ~ .3,
      group == 2007 ~ .1
    )) %>%
    # generate unit specific yearly treatment effects 
    rowwise() %>% 
    mutate(gamma = rnorm(1, hat_gamma, .2), 0) %>% 
    ungroup()
  
  # year fixed effects 
  year <- tibble(
    year = 1980:2015,
    year_fe = rnorm(36, 0, 0.5))
  
  # full interaction of unit X year 
  crossing(unit, year) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(nrow(.), 0, 0.5),
           treat = ifelse(year >= group, 1, 0),
           tau = ifelse(treat == 1, gamma, 0)) %>%
    # calculate the dep variable
    group_by(unit) %>% 
    mutate(cumtau = cumsum(tau)) %>% 
    mutate(dep_var = unit_fe + year_fe + cumtau + error)
}

# make data
data6 <- make_data6()

# set the colors. Again, must be based on Kiwi birds
colors <- c("Earlier vs. Later Treated - DiD Estimate" = "#A7473A", 
            "Later vs. Earlier Treated - DiD Estimate" = "#4B5F6C", 
            "Earlier vs. Later Treated - True Value" = "#A7473A", 
            "Later vs. Earlier Treated - True Value" = "#4B5F6C")

# Make Plots for Outcome Paths --------------------------------------------
plot1 <- data1 %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/10, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(evertreated, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = factor(evertreated),
        color = factor(evertreated)), size = 1) + 
  labs(x = "", y = "Value", color = "Group") + 
  geom_vline(xintercept = 1997.5, color = '#4B5F6C',
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) + 
  ggtitle("Simulation 1") + 
  labs(subtitle = expression(paste("Not Staggered + Constant ", tau))) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

plot2 <- data2 %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/10, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(evertreated, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = factor(evertreated),
        color = factor(evertreated)), size = 1) + 
  labs(x = "", y = "", color = "Group") + 
  ylim(c(0-5, 10)) + 
  geom_vline(xintercept = 1997.5, color = '#4B5F6C',
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) + 
  ggtitle("Simulation 2") + 
  labs(subtitle = expression(paste("Not Staggered + Dynamic ", tau))) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

plot3 <- data3 %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/10, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "", color = "Group") + 
  geom_vline(xintercept = 1988.5, color = "#A7473A",
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 1997.5, color = "#4B5F6C", 
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 2006.5, color = "#51806a", 
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  ggtitle("Simulation 3") + 
  labs(subtitle = expression(paste("Staggered + Constant/Equal ", tau))) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

plot4 <- data4 %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/10, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "Value", color = "Group") + 
  geom_vline(xintercept = 1988.5, color = "#A7473A",
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 1997.5, color = "#4B5F6C" , 
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 2006.5, color = "#51806a" , 
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  ggtitle("Simulation 4") + 
  labs(subtitle = expression(paste("Staggered + Constant/Unequal ", tau))) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

plot5 <- data5 %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/10, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "", color = "Group") + 
  geom_vline(xintercept = 1988.5, color = "#A7473A",
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 1997.5, color = "#4B5F6C" , 
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 2006.5, color = "#51806a" , 
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  scale_y_continuous(limits = c(-1, 10)) + 
  ggtitle("Simulation 5") + 
  labs(subtitle = expression(paste("Staggered + Dynamic/Equal ", tau))) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

plot6 <- data6 %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/10, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>% 
      group_by(group, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = factor(group),
        color = factor(group)), size = 1) + 
  labs(x = "", y = "", color = "Group") + 
  geom_vline(xintercept = 1988.5, color = "#A7473A",
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 1997.5, color = "#4B5F6C" , 
             linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 2006.5, color = "#51806a" , 
             linetype = "dashed", size = 1) + 
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  scale_y_continuous(limits = c(-1, 20)) + 
  ggtitle("Simulation 6") + 
  labs(subtitle = expression(paste("Staggered + Dynamic/Unequal ", tau))) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# do simulations, estimate ATT and plot ---------------------------------
# make function to do this by data type 
dosims <- function(i, fun) {
  # make data from function
  dt <- fun()
  # estimate model and take what you need
  felm(dep_var ~ treat | unit + year | 0 | state, data = dt) %>% 
    broom::tidy(conf.int = TRUE) %>% 
    select(estimate) %>% 
    mutate(sim = i)
}

# estimate over the three datasets 500 sims
simdata1 <- map_dfr(1:500, .f = dosims, fun = make_data1)
simdata2 <- map_dfr(1:500, .f = dosims, fun = make_data2)
simdata3 <- map_dfr(1:500, .f = dosims, fun = make_data3)
simdata4 <- map_dfr(1:500, .f = dosims, fun = make_data4)
simdata5 <- map_dfr(1:500, .f = dosims, fun = make_data5)
simdata6 <- map_dfr(1:500, .f = dosims, fun = make_data6)

# make plots 
# get the true treatment effects
tau1 <- 2
tau2 <- mean(cumsum(rep(0.3, length(1998:2015))))
tau3 <- 3
tau4 <- sum(c(17/50, 17/50, 16/50)*(c(5, 3, 1)))
tau5 <- 
  # ATT for treated group in 1989
  (17/50) * mean(cumsum(rep(0.3, length(1989:2015)))) + 
  # ATT for treated group in 1998
  (17/50) * mean(cumsum(rep(0.3, length(1998:2015)))) + 
  # ATT for treated group in 2007
  (16/50) * mean(cumsum(rep(0.3, length(2007:2015))))
tau6 <- 
  # ATT for treated group in 1989
  (17/50) * mean(cumsum(rep(0.5, length(1989:2015)))) + 
  # ATT for treated group in 1998
  (17/50) * mean(cumsum(rep(0.3, length(1998:2015)))) + 
  # ATT for treated group in 2007
  (16/50) * mean(cumsum(rep(0.1, length(2007:2015))))

# make our three plots
# from simulation 1
plot7 <- simdata1 %>% 
  ggplot(aes(x = estimate)) + 
  geom_density(fill = "#377EB8", alpha = 1/5) + 
  geom_vline(xintercept = tau1, linetype = "dashed", color = "#E41A1C", size = 1) + 
  ggtitle("Simulation 1") + 
  labs(subtitle = expression(paste("Not Staggered + Constant ", tau))) +
  labs(x = "", y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# from simulation 2
plot8 <- simdata2 %>% 
  ggplot(aes(x = estimate)) + 
  geom_density(fill = "#377EB8", alpha = 1/5) + 
  geom_vline(xintercept = tau2, linetype = "dashed", color = "#E41A1C", size = 1) + 
  ggtitle("Simulation 2") + 
  labs(subtitle = expression(paste("Not Staggered + Dynamic ", tau))) +
  labs(y = "", x = expression(widehat(delta^'DD'))) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# from simulation 3
plot9 <- simdata3 %>% 
  ggplot(aes(x = estimate)) + 
  geom_density(fill = "#377EB8", alpha = 1/5) + 
  geom_vline(xintercept = tau3, linetype = "dashed", color = "#E41A1C", size = 1) + 
  #geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) + 
  ggtitle("Simulation 3") + 
  labs(subtitle = expression(paste("Staggered + Constant/Equal ", tau))) +
  labs(x = "", y = "") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# from simulation 4
plot10 <- simdata4 %>% 
  ggplot(aes(x = estimate)) + 
  geom_density(fill = "#377EB8", alpha = 1/5) + 
  geom_vline(xintercept = tau4, linetype = "dashed", color = "#E41A1C", size = 1) + 
  ggtitle("Simulation 4") + 
  labs(subtitle = expression(paste("Staggered + Constant/Unequal ", tau))) +
  labs(x = "", y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# from simulation 5
plot11 <- simdata5 %>% 
  ggplot(aes(x = estimate)) + 
  geom_density(fill = "#377EB8", alpha = 1/5) + 
  geom_vline(xintercept = tau5, linetype = "dashed", color = "#E41A1C", size = 1) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) + 
  ggtitle("Simulation 5") + 
  labs(subtitle = expression(paste("Staggered + Dynamic/Equal ", tau))) +
  labs(y = "", x = expression(widehat(delta^'DD'))) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# from simulation 6
plot12 <- simdata6 %>% 
  ggplot(aes(x = estimate)) + 
  geom_density(fill = "#377EB8", alpha = 1/5) + 
  geom_vline(xintercept = tau6, linetype = "dashed", color = "#E41A1C", size = 1) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) + 
  ggtitle("Simulation 6") + 
  labs(subtitle = expression(paste("Staggered + Dynamic/Unequal ", tau))) +
  labs(x = "", y = "") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# plot treatment paths - three good ones
Sims_1_3_trends <- plot1 + plot2 + plot3

#save
ggsave(Sims_1_3_trends, filename = paste(dropbox, "Sims_1_3_trends.png", sep = ""), dpi = 500,
       width = 10, height = 4)

# plot estimates of TWFE DD
Sims_1_3_dist <- plot7 + plot8 + plot9

# save
ggsave(Sims_1_3_dist, filename = paste(dropbox, "Sims_1_3_dist.png", sep = ""), dpi = 500,
       width = 10, height = 4)

# plot treatment paths - three bad ones
Sims_4_6_trends <- plot4 + plot5 + plot6

# save
ggsave(Sims_4_6_trends, filename = paste(dropbox, "Sims_4_6_trends.png", sep = ""), dpi = 500,
       width = 10, height = 4)

# plot estimates of TWFE DD
Sims_4_6_dist <- plot10 + plot11 + plot12

# save
ggsave(Sims_4_6_dist, filename = paste(dropbox, "Sims_4_6_dist.png", sep = ""), dpi = 500,
       width = 10, height = 4)

# Figure - GB Decomposition -------------------------------------------------------------
# calculate the bacon decomposition without covariates
bacon_out_4 <- bacon(dep_var ~ treat,
                   data = data4,
                   id_var = "unit",
                   time_var = "year") %>% 
  mutate(treated = substr(treated, 3, 4), 
         untreated = substr(untreated, 3, 4),
         name = glue::glue("T = '{treated} \n C = '{untreated}")) %>%
  mutate(weight2 = if_else(treated %in% c("98", "89"), 17/100, 16/100),
         estimate2 = case_when(
           treated == "89" & untreated == "98" ~ 5.1,
           treated == "89" & untreated == "07" ~ 4.9,
           treated == "98" & untreated == "07" ~ 3.1,
           treated == "98" & untreated == "89" ~ 2.9,
           treated == "07" & untreated == "98" ~ 1.1,
           treated == "07" & untreated == "89" ~ 0.9
         )) %>% 
  pivot_longer(cols = c(weight, weight2),
               names_to = "weight", values_to = "weight_vl") %>% 
  pivot_longer(cols = c(estimate, estimate2), 
               names_to = "estimate", values_to = "estimate_vl") %>% 
  filter(weight == "weight" & estimate == "estimate" | 
           weight == "weight2" & estimate == "estimate2") %>% 
  mutate(identifier = case_when(
    type == "Later vs Earlier Treated" & weight == "weight" ~ "Later vs. Earlier Treated - DiD Estimate",
    type == "Later vs Earlier Treated" & weight == "weight2" ~ "Later vs. Earlier Treated - True Value",
    type == "Earlier vs Later Treated" & weight == "weight" ~ "Earlier vs. Later Treated - DiD Estimate",
    type == "Earlier vs Later Treated" & weight == "weight2" ~ "Earlier vs. Later Treated - True Value"))

bacon_out_5 <- bacon(dep_var ~ treat,
                     data = data5,
                     id_var = "unit",
                     time_var = "year") %>% 
  mutate(treated = substr(treated, 3, 4), 
         untreated = substr(untreated, 3, 4),
         name = glue::glue("T = '{treated} \n C = '{untreated}")) %>% 
  mutate(weight2 = if_else(treated %in% c("98", "89"), 17/100, 16/100),
         estimate2 = case_when(
           treated == "89" & untreated == "98" ~ mean(cumsum(rep(0.3, length(1989:1997)))),
           treated == "89" & untreated == "07" ~ mean(cumsum(rep(0.3, length(1989:2006)))),
           treated == "98" & untreated == "07" ~ mean(cumsum(rep(0.3, length(1998:2006)))),
           treated == "98" & untreated == "89" ~ mean(cumsum(rep(0.3, length(1998:2015)))),
           treated == "07" & untreated == "98" ~ mean(cumsum(rep(0.3, length(2007:2015)))),
           treated == "07" & untreated == "89" ~ mean(cumsum(rep(0.3, length(2007:2015))))
         )) %>% 
  pivot_longer(cols = c(weight, weight2),
               names_to = "weight", values_to = "weight_vl") %>% 
  pivot_longer(cols = c(estimate, estimate2), 
               names_to = "estimate", values_to = "estimate_vl") %>% 
  filter(weight == "weight" & estimate == "estimate" | 
           weight == "weight2" & estimate == "estimate2") %>% 
  mutate(identifier = case_when(
    type == "Later vs Earlier Treated" & weight == "weight" ~ "Later vs. Earlier Treated - DiD Estimate",
    type == "Later vs Earlier Treated" & weight == "weight2" ~ "Later vs. Earlier Treated - True Value",
    type == "Earlier vs Later Treated" & weight == "weight" ~ "Earlier vs. Later Treated - DiD Estimate",
    type == "Earlier vs Later Treated" & weight == "weight2" ~ "Earlier vs. Later Treated - True Value"))

bacon_out_6 <- bacon(dep_var ~ treat,
                     data = data6,
                     id_var = "unit",
                     time_var = "year") %>% 
  mutate(treated = substr(treated, 3, 4), 
         untreated = substr(untreated, 3, 4),
         name = glue::glue("T = '{treated} \n C = '{untreated}")) %>% 
  mutate(weight2 = if_else(treated %in% c("98", "89"), 17/100, 16/100),
         estimate2 = case_when(
           treated == "89" & untreated == "98" ~ mean(cumsum(rep(0.5, length(1989:1997)))),
           treated == "89" & untreated == "07" ~ mean(cumsum(rep(0.5, length(1989:2006)))),
           treated == "98" & untreated == "07" ~ mean(cumsum(rep(0.3, length(1998:2006)))),
           treated == "98" & untreated == "89" ~ mean(cumsum(rep(0.3, length(1998:2015)))),
           treated == "07" & untreated == "98" ~ mean(cumsum(rep(0.1, length(2007:2015)))),
           treated == "07" & untreated == "89" ~ mean(cumsum(rep(0.1, length(2007:2015))))
         )) %>% 
  pivot_longer(cols = c(weight, weight2),
               names_to = "weight", values_to = "weight_vl") %>% 
  pivot_longer(cols = c(estimate, estimate2), 
               names_to = "estimate", values_to = "estimate_vl") %>% 
  filter(weight == "weight" & estimate == "estimate" | 
           weight == "weight2" & estimate == "estimate2") %>% 
  mutate(identifier = case_when(
    type == "Later vs Earlier Treated" & weight == "weight" ~ "Later vs. Earlier Treated - DiD Estimate",
    type == "Later vs Earlier Treated" & weight == "weight2" ~ "Later vs. Earlier Treated - True Value",
    type == "Earlier vs Later Treated" & weight == "weight" ~ "Earlier vs. Later Treated - DiD Estimate",
    type == "Earlier vs Later Treated" & weight == "weight2" ~ "Earlier vs. Later Treated - True Value"))

### merge in the true values 
# set colors, fills, and shapes for the decomp plot
colors <- c("Earlier vs. Later Treated - DiD Estimate" = "#A7473A", 
            "Later vs. Earlier Treated - DiD Estimate" = "#4B5F6C", 
            "Earlier vs. Later Treated - True Value" = "#A7473A", 
            "Later vs. Earlier Treated - True Value" = "#4B5F6C")

fills <- c("Earlier vs. Later Treated - DiD Estimate" = "#A7473A", 
           "Later vs. Earlier Treated - DiD Estimate" = "#4B5F6C", 
           "Earlier vs. Later Treated - True Value" = "white", 
           "Later vs. Earlier Treated - True Value" = "white",
           "07" = "#51806a")

shapes <- c("Earlier vs. Later Treated - DiD Estimate" = 21, 
            "Later vs. Earlier Treated - DiD Estimate" = 24,
            "Earlier vs. Later Treated - True Value" = 21,
            "Later vs. Earlier Treated - True Value" = 24)

# sim4 plot
sim4 <- bacon_out_4 %>% 
  arrange(desc(weight)) %>% 
  ggplot(aes(x = weight_vl, y = estimate_vl, shape = identifier, color = identifier, fill = identifier)) +
  geom_point(size = 2) + 
  geom_path(aes(group = name), arrow = arrow(length = unit(0.1, "inches"), ends = "last")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("text", label = "T = '89 \n C = '98", x = .11, y = 4.3, color = "#A7473A") +
  annotate("text", label = "T = '89 \n C = '07", x = .20, y = 4.8, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '07", x = .195, y = 3.7, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '89", x = .215, y = 2.45, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '89", x = .205, y = 1, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '98", x = .11, y = 1.65, color = "#4B5F6C") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = fills) + 
  scale_shape_manual(values = shapes) +
  labs(x = "Weight", y = expression(widehat(delta^'DD'))) + 
  ggtitle("Simulation 4") + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  guides(color = guide_legend(nrow = 2))

# sim 5 plot
sim5 <- bacon_out_5 %>% 
  arrange(desc(weight)) %>% 
  ggplot(aes(x = weight_vl, y = estimate_vl, shape = identifier, color = identifier, fill = identifier)) +
  geom_point(position = position_jitter(width = 0, height = 0.3, seed = 3), size = 2) + 
  geom_path(aes(group = name), arrow = arrow(length = unit(0.1, "inches"), ends = "last"),
            show.legend = FALSE, position = position_jitter(width = 0, height = 0.3, seed = 3)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("text", label = "T = '89 \n C = '98", x = .11, y = 2, color = "#A7473A") +
  annotate("text", label = "T = '89 \n C = '07", x = .21, y = 2.5, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '07", x = .215, y = .8, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '89", x = .215, y = -1.9, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '89", x = .165, y = -2, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '98", x = .113, y = -2, color = "#4B5F6C") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = fills) + 
  scale_shape_manual(values = shapes) +
  labs(x = "Weight", y = "") + 
  ggtitle("Simulation 5") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  guides(color = guide_legend(nrow = 2))

# sim 6  plot
sim6 <- bacon_out_6 %>% 
  arrange(desc(weight)) %>% 
  ggplot(aes(x = weight_vl, y = estimate_vl, shape = identifier, color = identifier, fill = identifier)) +
  geom_point(size = 2) + 
  geom_path(aes(group = name), arrow = arrow(length = unit(0.1, "inches"), ends = "last"),
            show.legend = FALSE) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("text", label = "T = '89 \n C = '98", x = .11, y = 4.2, color = "#A7473A") +
  annotate("text", label = "T = '89 \n C = '07", x = .20, y = 4.5, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '07", x = .21, y = 2, color = "#A7473A") +
  annotate("text", label = "T = '98 \n C = '89", x = .216, y = -5.2, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '89", x = .153, y = -5, color = "#4B5F6C") +
  annotate("text", label = "T = '07 \n C = '98", x = .12, y = -2.9, color = "#4B5F6C") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = fills) + 
  scale_shape_manual(values = shapes) +
  labs(x = "Weight", y = "") + 
  geom_mark_circle(aes(fill = treated, description = "Bad \n 2x2 Below", filter = treated == "07" & untreated == "89" & weight == "weight"),
                   con.type = "straight", label.buffer = unit(17, 'mm'), expand = unit(10, "mm"),
                   label.fontsize = 8, con.arrow = arrow(length = unit(0.1, "inches")), label.fill = "#51806a30", show.legend = FALSE) + 
  ggtitle("Simulation 6") + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  guides(color = guide_legend(nrow = 2))

# make subplot showing 2007 treated v. 1989 control 
colors2 <- c("Treated" = "#A7473A", "Control" = "#4B5F6C")

# make subplot
subplot <- data6 %>% 
  filter((group == 2007 | group == 1989) & year >= 1989) %>% 
  mutate(group = if_else(group == 2007, "Treated", "Control")) %>% 
  ggplot(aes(x = year, y = dep_var, group = unit)) +
  # unit specific lines
  geom_line(alpha = 1/10, color = "grey") + 
  # group specific averages
  geom_line(
    data = . %>%
      group_by(group, year) %>% 
      summarize(dep_var = mean(dep_var)),
    aes(x = year, y = dep_var, group = group,
        color = group), size = 1) + 
  scale_color_manual(values = colors2) + 
  ylim(c(-1, 17)) + 
  labs(x = "", y = "Value") + 
  geom_vline(xintercept = 2006.5, color = "#A7473A" , 
             linetype = "dashed", size = 1) + 
  ggtitle("Biased 2x2 Estimate From Simulation 6") + 
  labs(subtitle = expression(paste("Treated = ",'G'['2007'], "; Control = ", 'G'['1989']))) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360))

# combine and save  
GB_decomp_sims <- (sim4 + sim5 + sim6) / (subplot) 
ggsave(GB_decomp_sims, filename = paste(dropbox, "GB_decomp_sims.png", sep = ""), dpi = 500,
       width = 10, height = 8)

# Callaway Sant'Anna -------------------------------------------------------------
# create a lead/lag indicators
data <- data6 %>% 
  # variable with relative year from treatment
  mutate(rel_year = year - group) %>% 
  # drop observations after 2006 bc all treated 
  filter(year <= 2006) %>% 
  dplyr::arrange(group, unit, year) %>% 
  ungroup()

# first get percentage contribution to each lead/lag indicator by treatment cohort for weights
# we will need this for the Abraham/Sun method, as well as the true treatment indicator
# calculate weights
weights <- data %>% 
  mutate(rel_year = year - group) %>% 
  # drop 2007 adopters
  filter(group != 2007) %>% 
  group_by(group, rel_year) %>% 
  count %>% 
  ungroup() %>% 
  group_by(rel_year) %>% 
  mutate(total = sum(n),
         perc = n / total) %>% 
  # keep just the variables we need
  select(rel_year, group, perc) %>% 
  ungroup() %>% 
  # get rid of negative numbers because glmt is weird
  mutate(rel_year2 = rel_year - min(rel_year)) %>% 
  rowwise() %>% 
  # add variable equal to coefficient from regression
  mutate(term = paste0("cohort_", group, "_", rel_year2)) %>% 
  ungroup()

# make a dataset with the theoretical values to merge in
true_effect <- weights %>% 
  # add in the multiples
  mutate(
    multiple = case_when(
      rel_year < 0 ~ 0,
      rel_year >= 0 ~ rel_year + 1),
    # add in the tau_g values 
    tau_g = case_when(
      group == 1989 ~ .5,
      group == 1998 ~ .3,
      group == 2007 ~ .1),
    # multiply the two 
    effect = multiple*tau_g) %>% 
  #collapse by  time period 
  group_by(rel_year) %>% 
  summarize(true_tau = weighted.mean(effect, w = perc)) %>% 
  # make the time variable for merging
  mutate(t = rel_year)

# create a first treated variable which is 0 for the 2007 cohort
data <- data %>% 
  mutate(first_treat = if_else(group == 2007, 0, group))

# run the CS algorithm
CS_out <- att_gt(yname = "dep_var", 
                 data = data,
                 gname = "first_treat",
                 idname = "unit", 
                 tname = "year", 
                 clustervars = "state",
                 bstrap = T, 
                 cband = T,
                 est_method = "reg",
                 control_group = "notyettreated",
                 print_details = F)

# get the event study estimates
es <- aggte(CS_out, type = "dynamic")

# plot
CS_ES <- tibble(
  t = es$egt,
  estimate = es$att.egt,
  se = es$se.egt,
  conf.low = estimate - 1.96*se,
  conf.high = estimate + 1.96*se
) %>% 
  #keep just years -5 to 5
  filter(t %>% between(-5, 5)) %>% 
  left_join(true_effect) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_line(aes(x = t, y = true_tau, color = "True Effect"), linetype = "dashed") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups),
              color = "lightgrey", alpha = 1/4) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = "Estimated Effect"), show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = expression(widehat(delta)['it'])) +
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) + 
  ggtitle("Callaway & Sant'Anna") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# Abraham and Sun -------------------------------------------------------------
## Make cohort-relative time dummies
dataAS <- data %>% 
  # replace rel_year = NA if last treated group
  mutate(rel_year2 = if_else(group == 2007, as.numeric(NA), rel_year),
         rel_year2 = rel_year2 - min(rel_year2, na.rm = TRUE)) %>% 
  # make a variable that is the interaction of group and relative year
  rowwise() %>% 
  mutate(cohort = if_else(group == 2007, as.character(NA), paste0(group, "_", rel_year2))) %>% 
  ungroup() %>% 
  dummy_cols(select_columns = "cohort", ignore_na = TRUE) %>% 
  mutate_at(vars(starts_with("cohort_")), ~replace_na(., 0))

# put the covariates into a vector form
# get the relative time indicators we use form the weights file
covs <- weights %>% filter(rel_year != -1 & rel_year > min(rel_year)) %>% pull(term)

# estimate the saturated model
fit <- felm(as.formula(paste("dep_var ~ ", paste(covs, collapse = "+"), "| unit + year | 0 | state")), 
            data = dataAS, exactDOF = TRUE)

# get the coefficients and make a dataset for plotting
coefs <- fit$coefficients %>%
  # add in coefficient name to tibble
  as_tibble(rownames = "term") %>% 
  # bring in weights
  left_join(., weights)

# get the relevant coefficients and weights into a string to get the linear combination
get_lincom <- function(ll) {
  # get just the coefficients for a specific lead lag
  cf2 <- coefs %>% filter(rel_year == ll)
  # paste the function that goes into the linear combination function
  F <- paste(paste(cf2$perc, cf2$term, sep = " * ", collapse = " + "), " = 0")
  # take linear combination and put into a data frame
  broom::tidy(
    confint(glht(fit, linfct = F)),
    conf.int = TRUE
  ) %>% mutate(rel_year = ll)
}

# run over all lead/lags
AS_plot <- map_df(c(-5:-2, 0:5), get_lincom) %>% 
  # add time variable
  mutate(t = c(-5:-2, 0:5))

#Plot the results
SA_ES <- AS_plot %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1 (all zeros)
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  left_join(true_effect) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_line(aes(x = t, y = true_tau, color = "True Effect"), linetype = "dashed") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups),
              color = "lightgrey", alpha = 1/4) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = "Estimated Effect"), show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = expression(widehat(delta)['it'])) +
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) + 
  ggtitle("Sun & Abraham") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# CLDZ -------------------------------------------------------------
# get the treatment cohorts
obs <- data %>% 
  filter(group != 2007) %>% 
  pull(group) %>% 
  unique()

# Make the estimating equation
covs <- c(paste0("`rel_year_", -5:-2, "`"),
          paste0("rel_year_", 0:5))

# make the formula
formula_cldz2 <- as.formula(paste("dep_var ~", paste(covs, collapse = " + "), 
                                  "| factor(unit):factor(df) + factor(year):factor(df) | 0 | state_df"))

# make formula to create the dataset
getdata <- function(i) {
  
  #keep what we need
  data %>% 
    # keep treated units and all units not treated within -5 to 5
    filter(group == i | group > i + 5) %>% 
    # keep just year -5 to 5
    filter(year >= i - 5 & year <= i + 5) %>%
    # create an indicator for the dataset
    mutate(df = i) %>% 
    # make dummies
    mutate(rel_year = if_else(group == i, rel_year, as.numeric(NA))) %>% 
    dummy_cols(select_columns = "rel_year", ignore_na = TRUE) %>% 
    mutate_at(vars(starts_with("rel_year")), ~replace_na(., 0))
}

# get data stacked
stacked_data <- map_df(obs, getdata) %>% mutate(state_df = paste(state, df))

# estimate the model on our stacked data
Stacked_ES <- stacked_data %>% 
  # fit the model
  do(broom::tidy(felm(formula_cldz2, data = ., exactDOF = TRUE, cmethod = "reghdfe"), 
                 conf.int = TRUE, se = "cluster")) %>% 
  # make a relative time variable
  filter(term %in% covs) %>% 
  mutate(t = c(-5:-2, 0:5)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  left_join(true_effect) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_line(aes(x = t, y = true_tau, color = "True Effect"), linetype = "dashed") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups),
              color = "lightgrey", alpha = 1/4) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = "Estimated Effect"), show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Relative Time", y = expression(widehat(delta)['it'])) +
  scale_color_manual(values = c("#A7473A", "#4B5F6C")) + 
  ggtitle("Stacked Regression") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 360, hjust = 0.5, vjust = 0.5))

# combine and save
NEW_DID_SIMS <- CS_ES + SA_ES + Stacked_ES
ggsave(NEW_DID_SIMS, filename = paste(dropbox, "NEW_DID_SIMS.png", sep = ""), dpi = 500,
       width = 10, height = 4)