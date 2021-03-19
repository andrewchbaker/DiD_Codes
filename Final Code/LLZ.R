# load packages
library(car)
library(tidyverse)
library(lfe)
library(kableExtra)
library(RPostgres)
library(multcomp)
library(fastDummies)
library(lubridate)
library(ggthemes)
library(patchwork)
library(fixest)

# set standards
options(knitr.kable.NA = '')
select <- dplyr::select
theme_set(theme_clean() + theme(plot.background = element_blank()))
dropbox <- "/Users/Andrew/Dropbox/Apps/Overleaf/bakerlarckerwang/Write_Up/"

# Connect to WRDS Server --------------------------------------------------
# need to type in your password here
wrds <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  user = '',
                  password = '',
                  dbname = 'wrds',
                  sslmode = 'require')

# Binary indicator  -------------------------------------------------------
# load data
data_tab2 <- haven::read_dta(here::here("Reps/LLZ/JAR2018_data/Table 2.dta"))
data_tab3 <- haven::read_dta(here::here("Reps/LLZ/JAR2018_data/Table 3.dta"))

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

# estimate model as is
# the covariates
covs <- c("post", "mis_rd ", "rd_to_sales_2", "intangible_to_at_2", "advertise_to_sale_2", "lnat_2", "hhi", 
         "bign", "manextyear", "seonextyear", "gdpgr", "unemployment")
# formula
form1 <- as.formula(paste0("lnhide_ratio ~", paste0(covs, collapse = " + "), "| year + gvkey + ba_state"))

# model as is
mod1 <- feols(form1, cluster = "ba_state", data = data_tab2)

# model with three FEs but no covariates
form2<- as.formula(paste0("lnhide_ratio ~", "post", "| year + gvkey + ba_state"))
mod2 <- feols(form2, cluster = "ba_state", data = data_tab2)

# model with two levels of fixed effects
form3 <- as.formula(paste0("lnhide_ratio ~", paste0(covs, collapse = " + "), "| year + gvkey"))
mod3 <- feols(form3, cluster = "ba_state", data = data_tab2)

# model with 2 FEs and no covariates
form4 <- as.formula(paste0("lnhide_ratio ~", "post", "| year + gvkey"))
mod4 <- feols(form4, cluster = "ba_state", data = data_tab2)

# function to get info from models
get_info <- function(mod, modelname) {
  bind_cols(
    broom::tidy(mod, se = "cluster") %>% 
      filter(term == "post") %>% 
      select(estimate, std.error, statistic),
    broom::glance(mod, se = "cluster") %>% 
      select(nobs, adj.r.squared)
    ) %>% 
    mutate(mod = modelname)
}

# make table
# bind info needed from models
LLZ_table <- bind_rows(
  get_info(mod1, "mod1"),
  get_info(mod2, "mod2"),
  get_info(mod3, "mod3"),
  get_info(mod4, "mod4"),
) %>% 
  rowwise() %>% 
  # make estimate and statistic into three digits with stars
  mutate(estimate = paste0(as.character(format(round(estimate, 3), nsmall = 3)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 3), nsmall = 3)), ")"),
         nobs = scales::comma_format()(nobs),
         adj.r.squared = as.character(format(round(adj.r.squared, 3), nsmall = 3))) %>%
  # pivot longer and then wider
  select(-statistic) %>% 
  pivot_longer(cols = -mod) %>% 
  pivot_wider(names_from = mod, values_from = value) %>% 
  # add in other info
  bind_rows(
    tribble(
      ~name, ~mod1, ~mod2, ~mod3, ~mod4,
      "Firm FE", "Y", "Y", "Y", "Y",
      "Year FE", "Y", "Y", "Y", "Y",
      "State FE", "Y", "Y", "N", "N",
      "Covariates", "Y", "N", "Y", "N"
    )
  ) %>% 
  # rename left column and re-sort
  mutate(newname = c("IDD", "", "N", "Adjusted R2", "Firm FE",
                     "Year FE", "State FE", "Covariates")) %>% 
  select(newname, mod1, mod2, mod3, mod4) %>% 
  # make and report table
  kable("latex", align = 'lcccc', booktabs = T,
        col.names = c(" ", "(1)", "(2)", "(3)", "(4)"),
        label = "LLZ_table", linesep = "",
        caption = "IDD and Nondisclosure of Customer Information") %>% 
  kable_styling(position = "center", latex_options = c("HOLD_position"))

# save
write_lines(LLZ_table, path = paste(dropbox, "LLZ_table.tex", sep = ""))

# Event Study Did ---------------------------------------------------------
# first make treatment timing plot
# bring in datadate info for gvkey to make lead lag indicators
# download comp data
comp <- tbl(wrds, sql("SELECT gvkey, fyear, datadate, indfmt, datafmt, popsrc, consol, cik FROM comp.funda")) %>% 
  # filter as per usual
  filter(indfmt == 'INDL' & datafmt == 'STD' & popsrc == 'D' & consol == 'C' & !is.na(fyear)) %>% 
  distinct() %>% 
  collect()

comp <- comp %>% 
  # make the year variable - use the maximum datadate in the year prior
  mutate(year = lubridate::year(datadate) + 1) %>% 
  group_by(gvkey, year) %>% 
  filter(datadate == max(datadate)) %>% 
  ungroup()

# merge in
data_tab2 <- data_tab2 %>% 
  left_join(comp %>% select(gvkey, year, datadate))

# make adopt year and relative year
data_tab2 <- data_tab2 %>% 
  # add in adopt year
  mutate(adopt_year = case_when(
    ba_state == "AR" ~ 1997,
    ba_state == "CT" ~ 1996,
    ba_state == "DE" ~ 1964,
    ba_state == "FL" ~ 1960,
    ba_state == "GA" ~ 1998,
    ba_state == "IL" ~ 1989,
    ba_state == "IN" ~ 1995,
    ba_state == "IA" ~ 1996,
    ba_state == "KS" ~ 2006,
    ba_state == "MA" ~ 1994,
    ba_state == "MI" ~ 1966,
    ba_state == "MN" ~ 1986,
    ba_state == "MO" ~ 2000,
    ba_state == "NJ" ~ 1987,
    ba_state == "NY" ~ 1919,
    ba_state == "NC" ~ 1976,
    ba_state == "OH" ~ 2000,
    ba_state == "PA" ~ 1982,
    ba_state == "TX" ~ 1993,
    ba_state == "UT" ~ 1998,
    ba_state == "WA" ~ 1997,
    TRUE ~ NA_real_
  )) %>% 
  # drop after repeal
  mutate(adopt_year = case_when(
    ba_state == "FL" & datadate >= ymd(20000521) ~ NA_real_,
    ba_state == "MI" & datadate >= ymd(20010430) ~ NA_real_,
    ba_state == "TX" & datadate >= ymd(20020403) ~ NA_real_,
    TRUE ~ adopt_year
  )) %>% 
  # make relative year
  mutate(rel_year = year - adopt_year) 

# make relative timing plot
LLZ_timing <- data_tab2 %>% 
  # get post indicator and number by state year
  group_by(ba_state, year) %>% 
  summarize(post = mean(post),
            count = n()) %>% 
  # there are some partials - deal with those
  mutate(alpha = if_else(post > 0, count*post, as.numeric(count)),
         post = if_else(post <= 0, "No IDD", "IDD")) %>% 
  # make it so they plot in the right direction
  mutate(post = factor(post, levels = c("No IDD", "IDD"))) %>% 
  # plot
  ggplot(aes(x = year, y = fct_rev(ba_state))) + 
  geom_tile(aes(fill = as.factor(post), alpha = alpha)) + 
  scale_alpha(range = c(0.5, 1)) + 
  scale_fill_manual(values = c("#4B5F6C", "#A7473A")) + 
  labs(x = "Year", y = "State") + 
  ggtitle("(A)") + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(color = "white"),
        plot.title = element_text(hjust = 0.5)) + 
  guides(alpha = FALSE)

# make event study plots 2 ways - one with 2wfe and one with 3wfe
# make event study data - drop all firms in states that passed the law before 1994
es_data <- data_tab2 %>% 
  filter(is.na(adopt_year) | adopt_year > 1994) %>% 
  # make dummies
  fastDummies::dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
                          ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0)))
  
# get the relative year indicators 
yrs <- sort(unique(es_data$rel_year))
# drop most negative and time t = -1
yrs <- yrs[which(yrs != min(yrs) & yrs != -1)]

# make covariates
covariates <- c(paste0("`", "rel_year_", yrs, "`"))
form <- as.formula(paste0("lnhide_ratio ~", paste0(covariates, collapse = " + "),
                          "| year + gvkey + ba_state"))

# estimate model
mod <- feols(form, cluster = "ba_state", data = es_data)

# first es plot - 3 levels of fixed effects
LLZ_ES1 <- broom::tidy(mod, conf.int = TRUE, se = "cluster") %>% 
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
  labs(y = "Effect", x = "Years Relative to IDD") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  ggtitle("(B)") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# now with 2wfe
form <- as.formula(paste0("lnhide_ratio ~", paste0(covariates, collapse = " + "),
                          "| year + gvkey"))
mod <- feols(form, cluster = "ba_state", data = es_data)

# plot
LLZ_ES2 <- broom::tidy(mod, conf.int = TRUE, se = "cluster") %>% 
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
  labs(y = "Effect", x = "Years Relative to IDD") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  ggtitle("(C)") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# combine
LLZ_TIMING_ES <- LLZ_timing + (LLZ_ES1 / LLZ_ES2)

# save
ggsave(LLZ_TIMING_ES, filename = paste(dropbox, "LLZ_TIMING_ES.png", sep = ""), dpi = 800,
       width = 10, height = 8)

# Remedies ----------------------------------------------------------------
# no covariates using just controls or full exclusion
# get treated years that we can estimate
treats <- es_data %>% 
  filter(!is.na(adopt_year)) %>% 
  pull(adopt_year) %>% 
  unique() %>% 
  sort()

# function to get treat-year specific cohorts
make_dt_full <- function(tyr) {
  es_data %>% 
    # keep firms in the adopt year, never treateds, or treated after tyr + 5
    filter(adopt_year == tyr | adopt_year > tyr + 5 | is.na(adopt_year)) %>% 
    # keep just years t - 5  to t + 5
    filter(year %>% between(tyr - 5, tyr + 5)) %>% 
    # replace adopt year to NA to make dummies
    mutate(rel_year = if_else(adopt_year == tyr, rel_year, NA_real_)) %>% 
    select(gvkey, year, ba_state, lnhide_ratio, rel_year) %>% 
    mutate(dt = as.character(tyr))
}

# make stacked data
stacked_data_full <- map_dfr(treats, make_dt_full) %>% 
  dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(ba_state, "_", dt))

# make formula for no covariate case
covs <- c(paste0("`", "rel_year_", c(-5:-2, 0:5), "`"))
form <- as.formula(paste0("lnhide_ratio ~", paste0(covs, collapse = " + "),
                          "| year^dt + gvkey^dt + ba_state^dt"))

# estimate model
mod <- feols(form, cluster = "cluster", data = stacked_data_full)

# estimate submodels to get the p-values and averages
# make pre data - only years -5 to 0
pre_data_full <- stacked_data_full %>% 
  mutate(dt = as.numeric(dt)) %>% 
  filter(is.na(rel_year) | rel_year <= -1) %>% 
  mutate(treat = if_else(!is.na(rel_year) & rel_year < -1, 1, 0))

# make post data - years 0 to + 10
post_data_full <- stacked_data_full %>% 
  mutate(dt = as.numeric(dt)) %>% 
  filter(is.na(rel_year) | rel_year >= -1) %>% 
  mutate(treat = if_else(!is.na(rel_year) & rel_year > -1, 1, 0))

# get estimate and p value from pre and post regs
pre_mod <- broom::tidy(feols(lnhide_ratio ~ treat |year^dt + gvkey^dt + ba_state^dt, cluster = "cluster", data = pre_data_full))
post_mod <- broom::tidy(feols(lnhide_ratio ~ treat | year^dt + gvkey^dt + ba_state^dt, cluster = "cluster", data = post_data_full))

# get estimates and p-value for pre and post
pre_att <- format(round(pre_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
pre_p <- format(round(pre_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)

post_att <- format(round(post_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
post_p <- format(round(post_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)

# make the text that goes into the plot
text_pre <- bquote(widehat(delta^'Pre') ==.(pre_att)~"; "~p^'Pre'==.(pre_p))
text_post <- bquote(widehat(delta^'Post') ==.(post_att)~"; "~p^'Post'==.(post_p))

# estimate model and make plot
LLZ_stack1 <- broom::tidy(mod, conf.int = TRUE, se = "cluster") %>% 
  # add in the relative time variable
  mutate(t = c(-5:-2, 0:5)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in time t = -1
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
  annotate("text", x = -1, y = 0.12, parse = F, label = text_pre, color = '#4B5F6C') +
  annotate("text", x = -1, y = 0.10, parse = F, label = text_post, color = '#A7473A') +
  labs(y = "Effect", x = "Years Relative to IDD") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  ggtitle("(A)") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# Do partial data
# function to get treat-year specific cohorts 
make_dt_partial <- function(tyr) {
  es_data %>% 
    # keep firms in the adopt year, never treateds, or treated after tyr + 5
    filter(adopt_year == tyr | (adopt_year > tyr & year < adopt_year) | is.na(adopt_year)) %>% 
    # keep just years t - 5  to t + 5
    filter(year %>% between(tyr - 5, tyr + 5)) %>% 
    # replace adopt year to NA to make dummies
    mutate(rel_year = if_else(adopt_year == tyr, rel_year, NA_real_)) %>% 
    select(gvkey, year, ba_state, lnhide_ratio, rel_year) %>% 
    mutate(dt = as.character(tyr))
}

# make stacked data
stacked_data_partial <- map_dfr(treats, make_dt_partial) %>% 
  dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(ba_state, "_", dt))

# estimate model
mod <- feols(form, cluster = "cluster", data = stacked_data_partial)

# estimate submodels to get the p-values and averages
# make pre data - only years -5 to 0
pre_data_partial <- stacked_data_partial %>% 
  mutate(dt = as.numeric(dt)) %>% 
  filter(is.na(rel_year) | rel_year <= -1) %>% 
  mutate(treat = if_else(!is.na(rel_year) & rel_year < -1, 1, 0))

# make post data - years 0 to + 10
post_data_partial <- stacked_data_partial %>% 
  mutate(dt = as.numeric(dt)) %>% 
  filter(is.na(rel_year) | rel_year >= -1) %>% 
  mutate(treat = if_else(!is.na(rel_year) & rel_year > -1, 1, 0))

# get estimate and p value from pre and post regs
pre_mod <- broom::tidy(feols(lnhide_ratio ~ treat |year^dt + gvkey^dt + ba_state^dt, cluster = "cluster", data = pre_data_partial))
post_mod <- broom::tidy(feols(lnhide_ratio ~ treat | year^dt + gvkey^dt + ba_state^dt, cluster = "cluster", data = post_data_partial))

# get estimates and p-value for pre and post
pre_att <- format(round(pre_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
pre_p <- format(round(pre_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)

post_att <- format(round(post_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
post_p <- format(round(post_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)

# make the text that goes into the plot
text_pre <- bquote(widehat(delta^'Pre') ==.(pre_att)~"; "~p^'Pre'==.(pre_p))
text_post <- bquote(widehat(delta^'Post') ==.(post_att)~"; "~p^'Post'==.(post_p))

# estimate model and make plot
LLZ_stack2 <- broom::tidy(mod, conf.int = TRUE, se = "cluster") %>% 
  # add in the relative time variable
  mutate(t = c(-5:-2, 0:5)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in time t = -1
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
  annotate("text", x = -1, y = 0.12, parse = F, label = text_pre, color = '#4B5F6C') +
  annotate("text", x = -1, y = 0.10, parse = F, label = text_post, color = '#A7473A') +
  labs(y = "Effect", x = "Years Relative to IDD") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  ggtitle("(B)") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

## Now do same thing but with two way fixed effects
form <- as.formula(paste0("lnhide_ratio ~", paste0(covs, collapse = " + "),
                          "| year^dt + gvkey^dt"))

# estimate model
mod <- feols(form, cluster = "cluster", data = stacked_data_full)

# get estimate and p value from pre and post regs
pre_mod <- broom::tidy(feols(lnhide_ratio ~ treat |year^dt + gvkey^dt, cluster = "cluster", data = pre_data_full))
post_mod <- broom::tidy(feols(lnhide_ratio ~ treat | year^dt + gvkey^dt, cluster = "cluster", data = post_data_full))

# get estimates and p-value for pre and post
pre_att <- format(round(pre_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
pre_p <- format(round(pre_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)

post_att <- format(round(post_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
post_p <- format(round(post_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)

# make the text that goes into the plot
text_pre <- bquote(widehat(delta^'Pre') ==.(pre_att)~"; "~p^'Pre'==.(pre_p))
text_post <- bquote(widehat(delta^'Post') ==.(post_att)~"; "~p^'Post'==.(post_p))

# estimate model and make plot
LLZ_stack3 <- broom::tidy(mod, conf.int = TRUE, se = "cluster") %>% 
  # add in the relative time variable
  mutate(t = c(-5:-2, 0:5)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in time t = -1
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
  annotate("text", x = -1, y = 0.12, parse = F, label = text_pre, color = '#4B5F6C') +
  annotate("text", x = -1, y = 0.10, parse = F, label = text_post, color = '#A7473A') +
  labs(y = "Effect", x = "Years Relative to IDD") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  ggtitle("(C)") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# finally on partial data
## Now do with two way fixed effects
form <- as.formula(paste0("lnhide_ratio ~", paste0(covs, collapse = " + "),
                          "| year^dt + gvkey^dt"))

# estimate model
mod <- feols(form, cluster = "cluster", data = stacked_data_partial)

# get estimate and p value from pre and post regs
pre_mod <- broom::tidy(feols(lnhide_ratio ~ treat |year^dt + gvkey^dt, cluster = "cluster", data = pre_data_partial))
post_mod <- broom::tidy(feols(lnhide_ratio ~ treat | year^dt + gvkey^dt, cluster = "cluster", data = post_data_partial))

# get estimates and p-value for pre and post
pre_att <- format(round(pre_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
pre_p <- format(round(pre_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)

post_att <- format(round(post_mod %>% filter(term == "treat") %>% pull(estimate), 3), nsmall = 3)
post_p <- format(round(post_mod %>% filter(term == "treat") %>% pull(p.value), 3), nsmall = 3)

# make the text that goes into the plot
text_pre <- bquote(widehat(delta^'Pre') ==.(pre_att)~"; "~p^'Pre'==.(pre_p))
text_post <- bquote(widehat(delta^'Post') ==.(post_att)~"; "~p^'Post'==.(post_p))

# estimate model and make plot
LLZ_stack4 <- broom::tidy(mod, conf.int = TRUE, se = "cluster") %>% 
  # add in the relative time variable
  mutate(t = c(-5:-2, 0:5)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in time t = -1
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
  annotate("text", x = -1, y = 0.12, parse = F, label = text_pre, color = '#4B5F6C') +
  annotate("text", x = -1, y = 0.10, parse = F, label = text_post, color = '#A7473A') +
  labs(y = "Effect", x = "Years Relative to IDD") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  ggtitle("(D)") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5))

# combine plots
LLZ_STACK <- LLZ_stack1 + LLZ_stack2 + LLZ_stack3 + LLZ_stack4 + plot_layout(nrow = 2)

# save
ggsave(LLZ_STACK, filename = paste(dropbox, "LLZ_STACK.png", sep = ""), dpi = 800,
       width = 10, height = 8)