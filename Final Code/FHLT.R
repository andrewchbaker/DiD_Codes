# load packages
library(tidyverse)
library(lfe)
library(kableExtra)
library(bacondecomp)
library(ggthemes)
library(did)
library(patchwork)
library(fastDummies)
library(fixest)

# set themes and output location
select <- dplyr::select
theme_set(theme_clean() + theme(plot.background = element_blank()))
# save out into dropbox folder
dropbox <- "/Users/Andrew/Dropbox/Apps/Overleaf/bakerlarckerwang/Write_Up/"
options(knitr.kable.NA = '')

# set seed for CS bootstrap estimator to be replicable
set.seed(20210215)

# load the data
data <- haven::read_dta(here::here("Reps/FHLT", 'reformdata.dta'))

# function to get significance stars
make_stars <- function(t, dof) {
  if (2 * pt(-t, df=dof) < 0.01) {
    ptstar <- "***"
  } else if (2 * pt(-t, df=dof) < 0.05) {
    ptstar <- "**"
  } else if (2 * pt(-t, df=dof) < 0.1) {
    ptstar <- "*"
  } else {
    ptstar <- ""
  }
  return(ptstar)
}

# function to get info from models
get_info <- function(est, modelname, type, variable) {
  broom::tidy(est, se = "cluster") %>% 
    filter(term == variable) %>% 
    select(estimate, statistic) %>% 
    mutate(mod = modelname, type = type) 
}

# estimate the two models
mod1 <- feols(qw ~ post + itenforce + postto + divtax + capgaintax + loggdppc + fdi + rulelaw + lntaw + logage +
               debttaw + cashtoaw + ppesalesw + forsale2w + rdsales2w + capextaw + ch + cl + iq | year + code,
              cluster = "ccode", data = data)

mod2 <- feols(qw ~ post1 + itenforce + postto + divtax + capgaintax + loggdppc + fdi + rulelaw + lntaw + logage +
               debttaw + cashtoaw + ppesalesw + forsale2w + rdsales2w + capextaw + ch + cl + iq | year + code, 
             cluster = "ccode", data = data)

# estimate the two models without controls
mod3 <- feols(qw ~ post | year + code, 
             cluster = "ccode", data = data)

mod4 <- feols(qw ~ post1 | year + code,
             cluster = "ccode", data = data)

# show the with and without controls side by side
FHLT_table <- bind_rows(
  get_info(mod1, "Major Reform", "Controls", "post"),
  get_info(mod2, "First Reform", "Controls", "post1"),
  get_info(mod3, "Major Reform", "No Controls", "post"),
  get_info(mod4, "First Reform", "No Controls", "post1"),
) %>%
  rowwise() %>% 
  # make estimate and statistic into three digits with stars
  mutate(estimate = paste0(as.character(format(round(estimate, 3), nsmall = 3)), make_stars(statistic, 10000)),
         statistic = paste0("(", as.character(format(round(statistic, 2), nsmall = 2)), ")")) %>%
  ungroup() %>% 
  # push and pull
  pivot_longer(cols = c(estimate, statistic),
               names_to = "variable",
               values_to = "value") %>% 
  pivot_wider(id_cols = variable, 
              names_from = c(mod, type), 
              values_from = c(value)) %>% 
  select(-variable) %>% 
  # add in rows for controls and such
  bind_rows(
    tibble(
      `Major Reform_Controls` = c(rep("Yes", 3), "196,016", 
                                  as.character(format(round(broom::glance(mod1)$adj.r.squared, 3), nsmall = 3))),
      `First Reform_Controls` = c(rep("Yes", 3), "196,016", 
                                  as.character(format(round(broom::glance(mod2)$adj.r.squared, 3), nsmall = 3))),
      `Major Reform_No Controls` = c("No", rep("Yes", 2), "196,016",
                                     as.character(format(round(broom::glance(mod3)$adj.r.squared, 3), nsmall = 3))),
      `First Reform_No Controls` = c("No", rep("Yes", 2), "196,016", 
                                     as.character(format(round(broom::glance(mod4)$adj.r.squared, 3), nsmall = 3)))
    )
  ) %>% 
  mutate(Variable = c("Post", NA_character_, "Control variables", "Firm fixed effects", 
                      "Year fixed effects", "N", "Adj. R2")) %>% 
  select(Variable, everything()) %>% 
  # make and report table
  kable("latex", align = 'lcccc', booktabs = T,
        col.names = c("Variable", rep(c("Major Reform", "First Reform"), 2)),
        label = "FHLT_table", 
        caption = "The Impact of Board Reforms on Firm Value") %>% 
  kable_styling(position = "center", latex_options = c("HOLD_position")) %>% 
  add_header_above(c(" " = 1, "With Covariates" = 2, "Without Covariates" = 2)) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 4))

# save
write_lines(FHLT_table, path = paste(dropbox, "FHLT_table.tex", sep = ""))

# Event Study + Timing Graphs ---------------------------------------------
enacts <- bind_rows(
  # major reforms by country year
  data %>% 
    group_by(ccode, year) %>% 
    summarize(reform_type = "Major Reforms",
              post = mean(post),
              count = n()),
  # first reforms by country year
  data %>% 
    group_by(ccode, year) %>% 
    summarize(reform_type = "First Reforms",
              post = mean(post1),
              count = n())
) %>% 
  mutate(reform_type = factor(reform_type, 
                              levels = c("Major Reforms", "First Reforms")))

# make the timing plot
FHLT_TIMING <- enacts %>% 
  mutate(post = if_else(post == 1, "Post", "Pre"),
         post = factor(post, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(x = year, y = ccode)) + 
  geom_tile(aes(fill = as.factor(post), alpha = count)) + 
  scale_alpha(range = c(0.5, 1)) + 
  scale_fill_manual(values = c("#4B5F6C", "#A7473A")) + 
  labs(x = "Year", y = "Country") + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(color = "white")) + 
  guides(alpha = FALSE) + 
  facet_wrap(~reform_type)

# make the event study estimates
# function to estimate the event study DID by reform type and with and without covariates
run_es <- function(reformtype, covtype, title, lastyear) {
  
  # make relative time dummies with data
  dt <- data %>%
    # drop after last treated year
    filter(year < lastyear) %>% 
    mutate(rel_year = year - {{reformtype}},
           rel_year = if_else({{reformtype}} == lastyear, NA_real_, rel_year)) %>% 
    # make dummies
    fastDummies::dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
                            ignore_na = TRUE) %>% 
    mutate(across(starts_with("rel_year_"), ~replace_na(., 0)))
  
  # get the relative year indicators 
  yrs <- sort(unique(dt$rel_year))
  # drop most negative and time t = -1
  yrs <- yrs[which(yrs != min(yrs) & yrs != -1)]
  
  # make formula
  if (covtype == "covs") {
    covariates <- c(paste0("`", "rel_year_", yrs, "`"), base_covs)
    form <- as.formula(paste0("qw ~", paste0(covariates, collapse = " + "),
                              "| year + code"))
  } else {
    covariates <- c(paste0("`", "rel_year_", yrs, "`"))
    form <- as.formula(paste0("qw ~", paste0(covariates, collapse = " + "),
                              "| year + code"))
  }
  
  # estimate the model
  mod <- feols(form, cluster = "ccode", data = dt)
  
  # estimate the model and plot
  broom::tidy(mod, conf.int = TRUE, se = "cluster") %>%
    filter(str_detect(term, "rel_year")) %>% 
    # add in the relative time variable
    mutate(t = yrs) %>% 
    filter(t %>% between(-5, 5)) %>% 
    select(t, estimate, conf.low, conf.high) %>% 
    bind_rows(
      tibble(
        t = -1, estimate = 0, conf.low = 0, conf.high = 0
      )
    ) %>% 
    # make two different periods for the connection
    mutate(group = as.factor(case_when(
      t < 0 ~ 1,
      t >= 0 ~ 2
    ))) %>% 
    ggplot(aes(x = t, y = estimate)) + 
    geom_point(fill = "white", shape = 21) + geom_line() + 
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  linetype = "longdash", show.legend = FALSE) + 
    geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
    geom_vline(xintercept = -1,  linetype = "longdash", color = "gray") + 
    labs(y = "Effect", x = "Years Relative to Reform") + 
    scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
    ggtitle(title) + 
    theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
          plot.title = element_text(hjust = 0.5))
}

# estimate the two event studies
FHLT_ES1 <- run_es(reform, "nocovs", "(A)", 2007)
FHLT_ES2 <- run_es(firstreform, "nocovs", "(B)", 2006) 

# combine the plots
FHLT_ES <- FHLT_ES1 + FHLT_ES2

# combine the timing plot and the event study plots and save
FHLT_TIMING_ES <- FHLT_TIMING + FHLT_ES + plot_layout(nrow = 2, heights = c(1.5, 1))

# save
ggsave(FHLT_TIMING_ES, filename = paste(dropbox, "FHLT_TIMING_ES.png", sep = ""), dpi = 500,
       width = 8, height = 8)

# Remedies ----------------------------------------------------------------
# Callaway Sant'anna
# make id variable
ids <- tibble(
  code = unique(data$code)
) %>% 
  mutate(firm = 1:n())

# bring in id
data_cs <- data %>% 
  left_join(ids, by = "code") %>% 
  mutate(ccode = group_indices(., ccode))

# run estimate
cs1 <- att_gt(yname = "qw",
              data = data_cs,
              tname = "year",
              idname = "firm",
              gname = "reform",
              clustervars = "ccode",
              bstrap = T,
              cband = T,
              est_method = "reg",
              xformla = NULL,
              control_group = "notyetreated",
              print_details = FALSE, 
              panel = TRUE,
              allow_unbalanced_panel = TRUE,
              cores = 4)

# make the dynamic event study
es1 <- aggte(cs1, type="dynamic", na.rm = TRUE, min_e = -5, max_e = 5)

# get the pre and post F
# run new code which modifies the CS code to get the F stat value for any combination of lead/lag indics
source(here::here("Code/Final Codes", "aggte_anyF.R"))

# estimate pre model which will aggregate overall ATT just in -5 to -1
es1_pre <- aggte_anyF(cs1, type = "dynamic", min_e = -5, max_e = 5, min_e2 = -5, max_e2 = -1, na.rm = TRUE)

# get estimates and p-value for pre and post
pre_att <- format(round(es1_pre$overall.att, 3), nsmall = 3)
pre_p <- format(round(pnorm(abs(es1_pre$overall.att/es1_pre$overall.se), lower.tail = FALSE)*2, 3), nsmall = 3)

post_att <- format(round(es1$overall.att, 3), nsmall = 3)
post_p <- format(round(pnorm(abs(es1$overall.att/es1$overall.se), lower.tail = FALSE)*2, 3), nsmall = 3)

# make the text that goes into the plot
text_pre <- bquote(widehat(delta^'Pre') ==.(pre_att)~"; "~p^'Pre'==.(pre_p))
text_post <- bquote(widehat(delta^'Post') ==.(post_att)~"; "~p^'Post'==.(post_p))

# plot
FHLT_CS1 <- tibble(
  t = es1$egt,
  estimate = es1$att.egt,
  se = es1$se.egt,
  conf.low = estimate - 1.96*se,
  conf.high = estimate + 1.96*se
) %>% 
  # make two different periods for the connection
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(A)") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, group = group, color = group), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual(values = c("#4B5F6C", "#A7473A")) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -1,  linetype = "longdash", color = "gray") + 
  annotate("text", x = 1.5, y = -0.4, parse = F, label = text_pre, color = '#4B5F6C') +
  annotate("text", x = 1.5, y = -0.5, parse = F, label = text_post, color = '#A7473A') +
  labs(y = "Effect", x = "Years Relative to Reform") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# first reforms
# run CS estimator
cs2 <- att_gt(yname = "qw",
              data = data_cs,
              tname = "year",
              idname = "firm",
              gname = "firstreform",
              clustervars = "ccode",
              bstrap = T,
              cband = T,
              est_method = "reg",
              xformla = NULL,
              control_group = "notyetreated",
              print_details = FALSE, 
              panel = TRUE,
              allow_unbalanced_panel = TRUE,
              cores = 4)

# make the dynamic event study
es2 <- aggte(cs2, type="dynamic",  min_e = -5, max_e =5, na.rm = TRUE)

# estimate pre model which will aggregate overall ATT just in -5 to -1
es2_pre <- aggte_anyF(cs2, type = "dynamic", min_e = -5, max_e = 5, min_e2 = -5, max_e2 = -1, na.rm = TRUE)

# get estimates and p-value for pre and post
pre_att <- format(round(es2_pre$overall.att, 3), nsmall = 3)
pre_p <- format(round(pnorm(abs(es2_pre$overall.att/es2_pre$overall.se), lower.tail = FALSE)*2, 3), nsmall = 3)

post_att <- format(round(es2$overall.att, 3), nsmall = 3)
post_p <- format(round(pnorm(abs(es2$overall.att/es2$overall.se), lower.tail = FALSE)*2, 3), nsmall = 3)

# make the text that goes into the plot
text_pre <- bquote(widehat(delta^'Pre') ==.(pre_att)~"; "~p^'Pre'==.(pre_p))
text_post <- bquote(widehat(delta^'Post') ==.(post_att)~"; "~p^'Post'==.(post_p))

# plot
FHLT_CS2 <- tibble(
  t = es2$egt,
  estimate = es2$att.egt,
  se = es2$se.egt,
  conf.low = estimate - 1.96*se,
  conf.high = estimate + 1.96*se
) %>% 
  filter(t %>% between(-5, 5)) %>% 
  # make two different periods for the connection
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  ggtitle("(B)") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, group = group, color = group), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual(values = c("#4B5F6C", "#A7473A")) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -1,  linetype = "longdash", color = "gray") + 
  annotate("text", x = 2, y = -0.55, parse = F, label = text_pre, color = '#4B5F6C') +
  annotate("text", x = 2, y = -0.7, parse = F, label = text_post, color = '#A7473A') +
  labs(y = "Effect", x = "Years Relative to Reform") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# stacked regression with full exclusion
stacked <- function(reformvar, lastyear, title) {
  
  # get treated years that we can estimate
  treats <- data %>% 
    filter({{reformvar}} < lastyear) %>% 
    pull({{reformvar}}) %>% 
    unique() %>% 
    sort()
  
  # function to get treat-year specific datasets
  make_dt <- function(tyr) {
    data %>% 
      filter(year < lastyear) %>% 
      # keep firms in the adopt year pre-treatment observations
      filter({{reformvar}} == tyr | ({{reformvar}} > tyr & year < {{reformvar}})) %>% 
      # keep just years t -5 to t + 5
      filter(year %>% between(tyr - 5, min(tyr + 5, lastyear - 1))) %>% 
      # replace adopt year to NA to make dummies
      mutate(newyear = if_else({{reformvar}} == tyr, {{reformvar}}, NA_real_),
             rel_year = year - newyear) %>% 
      select(code, year, ccode, newyear, rel_year, qw) %>% 
      mutate(dt = as.character(tyr))
  }
  
  # run over out treated years
  stacked_data <- map_dfr(treats, make_dt) %>% 
    dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
               ignore_na = TRUE) %>% 
    mutate(across(starts_with("rel_year_"), ~replace_na(., 0))) %>% 
    mutate(cluster = paste0(ccode, "_", dt))
  
  # make formula
  yrs <- sort(unique(stacked_data$rel_year))
  
  # drop time t = -1
  yrs <- yrs[which(yrs != -1)]
  
  # make covariates and formula
  covariates <- c(paste0("`", "rel_year_", yrs, "`"))
  form <- as.formula(paste0("qw ~", paste0(covariates, collapse = " + "),
                              "| year^dt + code^dt"))
  
  # estimate the model and plot
  # estimate the model
  mod <- feols(form, cluster = "cluster", data = stacked_data)
  
  # plot
  plot <- broom::tidy(mod, conf.int = TRUE, se = "cluster") %>%
    filter(str_detect(term, "rel_year")) %>% 
    # add in the relative time variable
    mutate(t = yrs) %>% 
    filter(t %>% between(-5, 5)) %>% 
    select(t, estimate, conf.low, conf.high) %>% 
    bind_rows(
      tibble(
        t = -1, estimate = 0, conf.low = 0, conf.high = 0
      )
    ) %>% 
    # make two different periods for the connection
    mutate(group = as.factor(case_when(
      t < 0 ~ 1,
      t >= 0 ~ 2
    ))) %>% 
    ggplot(aes(x = t, y = estimate)) + 
    geom_point(fill = "white", shape = 21) + geom_line() + 
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, group = group, color = group), 
                  linetype = "longdash", show.legend = FALSE) + 
    scale_color_manual(values = c("#4B5F6C", "#A7473A")) + 
    geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
    geom_vline(xintercept = -1,  linetype = "longdash", color = "gray") + 
    labs(y = "Effect", x = "Years Relative to Reform") + 
    scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
    ggtitle(title) + 
    theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
          plot.title = element_text(hjust = 0.5))
  
  # estimate submodels to get the p-values and averages
  # make pre data - only years -5 to -1
  pre_data <- stacked_data %>%
    filter(is.na(rel_year) | rel_year <= -1) %>% 
    mutate(treat = if_else(!is.na(rel_year) & rel_year < -1, 1, 0))
  
  # make post data - years -1 to + 5
  post_data <- stacked_data %>% 
    filter(is.na(rel_year) | rel_year >= -1) %>% 
    mutate(treat = if_else(!is.na(rel_year) & rel_year > -1, 1, 0))
  
  # get estimate and p value from pre and post regs
  pre_mod <- broom::tidy(feols(qw ~ treat | year^dt + code^dt, cluster = "cluster", data = pre_data))
  post_mod <- broom::tidy(feols(qw ~ treat | year^dt + code^dt, cluster = "cluster", data = post_data))
  
  # get estimates and p-value for pre and post
  pre_att <- format(round(pre_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
  pre_p <- format(round(pre_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)
  
  post_att <- format(round(post_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
  post_p <- format(round(post_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)
  
  # make the text that goes into the plot
  text_pre <- bquote(widehat(delta^'Pre') ==.(pre_att)~"; "~p^'Pre'==.(pre_p))
  text_post <- bquote(widehat(delta^'Post') ==.(post_att)~"; "~p^'Post'==.(post_p))
  
  # export
  list(plot = plot, text_pre = text_pre, text_post = text_post)
  
}

# estimate the two event studies
stack1 <- stacked(reform, 2007, "(C)")

# annotate plot with the F stat values
FHLT_stack1 <- stack1$plot + 
  annotate("text", x = -2, y = -0.35, parse = F, label = stack1$text_pre, color = '#4B5F6C') +
  annotate("text", x = -2, y = -0.42, parse = F, label = stack1$text_post, color = '#A7473A')

# do same for plot 2
stack2 <- stacked(firstreform, 2006, "(D)")

# annotate plot with the F stat values
FHLT_stack2 <- stack2$plot + 
  annotate("text", x = 2.5, y = -0.3, parse = F, label = stack2$text_pre, color = '#4B5F6C') +
  annotate("text", x = 2.5, y = -0.38, parse = F, label = stack2$text_post, color = '#A7473A')

# combine and save
FHLT_CS_STACK <- FHLT_CS1 + FHLT_CS2 + FHLT_stack1 + FHLT_stack2 + plot_layout(nrow = 2)

ggsave(FHLT_CS_STACK, filename = paste(dropbox, "FHLT_CS_STACK.png", sep = ""), dpi = 800,
       width = 10, height = 20/3)