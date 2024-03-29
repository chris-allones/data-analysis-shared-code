
#| label: setup
#| include: false
# Setting working directory
setwd("~/GitHub-repo/phd-thesis")

# libraries
library(tidyverse)
library(janitor)
library(gtsummary)
library(kableExtra)
library(psych)
library(EFAtools)
library(seminr)
library(ggpmisc)
library(ggstatsplot)
library(patchwork)
library(frontier)

# sourced codes
source("data_cleaning.R")
source("custom_themes.R")


# DT global options
# options(DT.options = list(searching = FALSE))

#| label: data management
#| echo: false

# Loading_data
mlip <- read_csv("data/20220801_mlip_data.csv")


# Social capital and collective actions
  
## EFA

## parallel analysis plot
fa.parallel(sc_ca_mlip, fa = "fa")


## factor extraction
sc_ca_fa <- 
  fa(r = sc_ca_mlip, 
     nfactors = 5, 
     rotate = "varimax", 
     fm = "ml")

print(sc_ca_fa$loadings, sort = TRUE, cutoff = 0.4)


## Items descriptive

## legend and color
legend_color <- c("#500207", "#9f040e", "#e30613","#c63c46", "#fa4c58", "#fc9ca2", "#ffd3b6")
### label for social capital
legend_lab1 <- c("Strongly agree", "6", "5", "4", "3", "2", "Strongly disagree")
### label for collective action
legend_lab2 <- c("Always", "6", "5", "4", "3", "2", "Never happened")
### label for perceived farming risks
legend_lab3 <- c("With very great impact", "6", "5", "4", "3", "2", "No impact")
### label for human capital
legend_lab4 <- c("Very confident", "6", "5", "4", "3", "2", "Not confident")
### label for irrigation satisfaction
legend_lab5 <- c("Excellent", "6", "5", "4", "3", "2", "Very poor")

## functions to plotting items
plot_items <- 
  function(select_factor, legend_color, legend_label){
    sc_ca_items %>% 
      filter(factor == select_factor) %>% 
      ggplot(aes(x = prop_response, y = items, fill = response_2)) +
      geom_col(size = 1, width = 0.5) +
      geom_text(
        aes(label = round(prop_response, 0)),
        position = position_stack(vjust = 0.5),
        color = "white") +
      scale_fill_manual(values = legend_color,
                        labels = legend_label) +
      guides(fill = guide_legend(nrow = 1,label.position = "top", reverse = TRUE)) +
      facet_wrap(~ factor, scales = "free", ncol = 1) +
      theme_minimal() +
      theme(plot.margin = margin(rep(20, 4)),
            plot.title = element_text(hjust = 0.5, size = 14, color = "gray20",
                                      face = "bold", margin = margin(b = 20)),
            plot.title.position = "plot",
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(margin = margin(t=10), size = 14),
            axis.text = element_text(size = 10, color = "grey20"),
            strip.text = element_text(size = 12, face = "bold"),
            strip.clip = "off",
            legend.position = "bottom",
            legend.text = element_text(size = 12)) +
      labs(fill = element_blank(),
           x = "Percent of water users")
  }

 

### Social capital

  
## Social capital
items_social_capital <- plot_items(select_factor = "(a) Social capital", 
                                   legend_color = legend_color, 
                                   legend_label = legend_lab1)

ggsave(plot = items_social_capital, filename =  "plot/items_social_capital.jpeg", dpi = 400, width = 7.5, height = 5)
 

![](plot/items_social_capital.jpeg){fig-align="center"}

### Collective action

  
## Collective action
items_collective_action <- 
  plot_items(select_factor = "(b) Collective action", 
             legend_color = legend_color, 
             legend_label = legend_lab2)

ggsave(plot = items_collective_action, filename = "plot/items_collective_action.jpeg", dpi = 400, width = 7, height = 5)
 

![](plot/items_collective_action.jpeg){fig-align="center"}

### Perceived farming risks

  
## Perceived farming risks
items_perceived_risks <- 
  plot_items(select_factor = "(c) Perceived farming risks", 
             legend_color = legend_color, 
             legend_label = legend_lab3)

ggsave(plot = items_perceived_risks, filename = "plot/items_perceived_risks.jpeg", dpi = 400, width = 7, height = 4)
 

![](plot/items_perceived_risks.jpeg){fig-align="center"}

### Human capital

  
## Human capital
items_human_capital <- 
  plot_items(select_factor = "(e) Human capital", 
             legend_color = legend_color, 
             legend_label = legend_lab4)

ggsave(plot = items_human_capital, filename = "plot/items_human_capital.jpeg", dpi = 400, width = 7, height = 3.5)
 

![](plot/items_human_capital.jpeg){fig-align="center"}

### Irrigation satisfaction

  
## Irrigation satisfaction
items_irrigation_sat <- 
  plot_items(select_factor = "(d) Irrigation satisfaction", 
             legend_color = legend_color, 
             legend_label = legend_lab5)

ggsave(plot = items_irrigation_sat, filename = "plot/items_irrigation_sat.jpeg", dpi = 400, width = 7, height = 4)
 

![](plot/items_irrigation_sat.jpeg){fig-align="center"}

#### Combining items

  

(items_social_capital / items_collective_action / items_perceived_risks) + plot_layout(heights = c(1.3, 1.2, 1))

ggsave("plot/items_combined.jpeg", dpi = 400, width = 8, height = 14)

(items_irrigation_sat / items_human_capital) + plot_layout(heights = c(1.2, 0.8))
ggsave("plot/items_combined2.jpeg", dpi = 400, width = 8, height = 8)

 

## PLS-SEM model 1.1

  

## measurement model
mm_model_1 <- 
  constructs(
    composite("Social capital", multi_items("l", c(30:32, 34:37))),
    composite("Perceived risks", multi_items("i", c(6, 9:12))),
    composite("Human capital", multi_items("j", c(17, 20, 21))),
    composite("Collective action", multi_items("m", 54:59)),
    composite("Irrigation satisfaction", multi_items("n", c(64, 68:71)))
  )

## structural model
sm_model_1 <- 
  relationships(
    paths(from = c("Social capital", "Perceived risks", "Human capital"), 
          to = "Collective action"),
    paths(from = "Collective action",to = "Irrigation satisfaction")
  )


## estimating the pls-sem model
pls_sem_model_1 <- 
  estimate_pls(
    measurement_model = mm_model_1,
    structural_model = sm_model_1,
    data = sc_ca_mlip
  )


## pls-sem model summary
pls_sem_model_1_summary <- summary(pls_sem_model_1)

## plotting pls-sem model
plot(pls_sem_model_1)

 

### Bootstrapped PLS-SEM

  

## bootstrapping model
bootstrapped_pls_sem_1 <- bootstrap_model(seminr_model = pls_sem_model_1, nboot = 1e3)

## bootstrapped summary results
bootsrapped_summary_1 <- summary(bootstrapped_pls_sem_1, alpha = 0.10)
bootsrapped_summary_1$bootstrapped_paths %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

  
plot(bootstrapped_pls_sem_1)
 

  
bootsrapped_summary_1$bootstrapped_total_paths %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

### Factor loadings

  
bootsrapped_summary_1$bootstrapped_loadings %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

  
bootsrapped_summary_1$bootstrapped_loadings %>% 
  data.frame() %>% rownames_to_column("factor") %>% 
  clean_names() %>% 
  ggplot(aes(bootstrap_mean, factor)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = x5_ci, xmax = x95_ci), width = 0.3) +
  geom_vline(xintercept = 0.3, lty = "dashed", color = "gray60") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.1)) +
  theme_1 +
  theme(panel.grid = element_blank()) +
  labs(x = "Bootstrap mean and 95% confidence interval",
       y = "Factor items")

ggsave("plot/fct1_loadings.jpeg", dpi = 400, width = 8, height = 6)
 

### Validity and reliability

  
pls_sem_model_1_summary$reliability %>%
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

  
png("plot/fct1_reliability_validity.jpeg", units = "in", width = 6, height = 4, res = 400)
pls_sem_model_1_summary$reliability %>% plot()
dev.off()
dev.new()
 

### Discriminant validity

  
pls_sem_model_1_summary$validity$fl_criteria %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

## PLS-SEM model 1.2

  
## structural model 2
sm_model_2 <- 
  relationships(
    paths(from = c("Social capital", "Perceived risks", "Human capital"), 
          to = "Collective action"),
    paths(from = c("Social capital", "Perceived risks", "Human capital", "Collective action"),
          to = "Irrigation satisfaction")
  )


## estimating the pls-sem model
pls_sem_model_2 <- 
  estimate_pls(
    measurement_model = mm_model_1,
    structural_model = sm_model_2,
    data = sc_ca_mlip
  )


## pls-sem model summary
pls_sem_model_2_summary <- summary(pls_sem_model_2)

## plotting pls-sem model
plot(pls_sem_model_2)
 

### Bootstrapped PLS-SEM 2

  

## bootstrapping model
bootstrapped_pls_sem_2 <- bootstrap_model(seminr_model = pls_sem_model_2, nboot = 1e3)

## bootstrapped summary results
bootsrapped_summary_2 <- summary(bootstrapped_pls_sem_2, alpha = 0.10)
bootsrapped_summary_2$bootstrapped_paths %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

  
plot(bootstrapped_pls_sem_2)
 

  
bootsrapped_summary_2$bootstrapped_total_paths %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

  
bootsrapped_summary_2$bootstrapped_total_paths
 

### Factor loadings

  
bootsrapped_summary_2$bootstrapped_loadings %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

### Validity and reliability

  
pls_sem_model_2_summary$reliability %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

### Discriminant validity

  
pls_sem_model_2_summary$validity$fl_criteria %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

# Production efficiency measurement

## Input-output

  
## plotting input-output relationship
input_output <- 
  mlip %>% 
  select(prod, area, seed_q, fert_q, fl_labor_hr, hl_labor_hr) %>%
  filter(prod <= 22000) %>% 
  # mutate(prod = prod / 1e3) %>% 
  pivot_longer(cols = area:hl_labor_hr) %>% 
  mutate(name = case_when(name == "area" ~ "area planted",
                          name == "seed_q" ~ "seed",
                          name == "fert_q" ~ "fertilizer",
                          name == "fl_labor_hr" ~ "family labor",
                          name == "hl_labor_hr" ~ "hired labor",
                          TRUE ~ name)) %>% 
  mutate(name = str_to_sentence(name)) %>% 
  mutate(name = factor(name, levels = c("Area planted", "Seed", "Fertilizer", "Family labor", "Hired labor"))) %>% 
  ggplot(aes(log(value + 1), log(prod + 1))) +
  geom_smooth(span = 1.5) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")),
               coef.digits = 3,
               size = 3)  +
  
  scale_y_continuous(expand = c(0,0), limits = c(7, 11)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  facet_wrap(~ name, scales = "free") +
  labs(y = "Logged output",
       x = "Logged quantity") +
  theme_1

## saving plot
ggsave(plot = input_output, filename = "plot/input_output.jpeg", dpi = 300, width = 8, height = 4)

 

![](plot/input_output.jpeg){fig-align="center"}

### Input-ouput by irrigation branch

  

## function for plotting input-output relationship
input_output_plot <- function(input_name = "n_input"){
  mlip %>% 
    select(farm_loc, prod, area, seed_q, fert_q, fl_labor_hr, hl_labor_hr) %>%
    filter(prod <= 22000) %>% 
    pivot_longer(cols = area:hl_labor_hr) %>% 
    mutate(name = case_when(name == "area" ~ "area planted",
                            name == "seed_q" ~ "seed",
                            name == "fert_q" ~ "fertilizer",
                            name == "fl_labor_hr" ~ "family labor",
                            name == "hl_labor_hr" ~ "hired labor",
                            TRUE ~ name)) %>% 
    mutate(name = str_to_sentence(name)) %>% 
    mutate(name = factor(name, levels = c("Area planted", "Seed", "Fertilizer", "Family labor", "Hired labor"))) %>% 
    filter(name == input_name) %>% 
    ggplot(aes(log(value + 1), log(prod + 1))) +
    geom_smooth(span = 1.5) +
    stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                   after_stat(rr.label), sep = "*\", \"*")),
                 coef.digits = 4,
                 size = 3.5)  +
    
    scale_y_continuous(expand = c(0,0), limits = c(7, 11)) +
    scale_x_continuous(expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    facet_wrap(~ farm_loc, scales = "free") +
    labs(y = input_name,
         x = element_blank()) +
    theme_2
}

## plotting output-input relation by input used
p_area <- input_output_plot(input_name = "Area planted")
p_seed <- input_output_plot(input_name = "Seed")
p_fert <- input_output_plot(input_name = "Fertilizer")
p_fl <- input_output_plot(input_name = "Family labor")
p_hl <- input_output_plot(input_name = "Hired labor")

## combining plots
input_output_branch <- p_area / p_seed / p_fert / p_fl / p_hl

## saving plots
ggsave(plot = input_output_branch, filename = "plot/input_output_branch.jpeg", dpi = 400, width = 8.5, height = 8)
 

![](plot/input_output_branch.jpeg){fig-align="center"}

  

## filtering 
mlip_diff_data <- 
  mlip %>% 
  select(farm_loc, prod, area, seed_q, fert_q, fl_labor_hr, hl_labor_hr) %>%
  filter(prod <= 22000) %>% 
  filter(seed_q < 50) %>% 
  filter(hl_labor_hr < 100)

## AOV: production
aov_area <- 
  mlip_diff_data %>% 
  ggbetweenstats(
    x = farm_loc,
    y = area,
    results.subtitle = TRUE, 
    bf.message = FALSE) +
  coord_cartesian(clip = "off") +
  labs(y = "Area", x = element_blank()) +
  theme(axis.title.y.right = element_blank(),
        plot.subtitle = element_text(size = 8))


## AOV: production
aov_prod <- 
  mlip_diff_data %>% 
  ggbetweenstats(
    x = farm_loc,
    y = prod,
    results.subtitle = TRUE, 
    bf.message = FALSE) +
  coord_cartesian(clip = "off") +
  labs(y = "Production", x = element_blank()) +
  theme(axis.title.y.right = element_blank(),
        plot.subtitle = element_text(size = 8))

## AOV: seed
aov_seed <- 
  mlip_diff_data %>% 
  ggbetweenstats(
    x = farm_loc,
    y = seed_q,
    results.subtitle = TRUE, 
    bf.message = FALSE) +
  coord_cartesian(clip = "off") +
  labs(y = "Seed", x = element_blank()) +
  theme(axis.title.y.right = element_blank(),
        plot.subtitle = element_text(size = 7.5))

## AOV: fertilizer
aov_fert <- 
  mlip_diff_data %>% 
  ggbetweenstats(
    x = farm_loc,
    y = fert_q,
    results.subtitle = TRUE, 
    bf.message = FALSE) +
  coord_cartesian(clip = "off") +
  labs(y = "Fertilizer", x = element_blank()) +
  theme(axis.title.y.right = element_blank(),
        plot.subtitle = element_text(size = 7.5))

## AOV: family labor
aov_fl <- 
  mlip_diff_data %>% 
  ggbetweenstats(
    x = farm_loc, 
    y = fl_labor_hr,
    results.subtitle = TRUE, 
    bf.message = FALSE) +
  coord_cartesian(clip = "off") +
  labs(y = "Family labor", x = element_blank()) +
  theme(axis.title.y.right = element_blank(),
        plot.subtitle = element_text(size = 8))

## AOV: hired labor
aov_hl <- 
  mlip_diff_data %>% 
  ggbetweenstats(
    x = farm_loc,
    y = hl_labor_hr,
    results.subtitle = TRUE, 
    bf.message = FALSE) +
  coord_cartesian(clip = "off") +
  labs(y = "Hired labor", x = element_blank()) +
  theme(axis.title.y.right = element_blank(),
        plot.subtitle = element_text(size = 8))


## combining plots
aov_inputs <- 
  combine_plots(
    list(aov_area, aov_prod, aov_seed, aov_fert, aov_fl, aov_hl),
    plotgrid.args = list(ncol = 2))

ggsave(plot = aov_inputs, filename = "plot/aov_inputs.jpeg", dpi = 400, width = 8.5, height = 9.5)

 

![](plot/aov_inputs.jpeg){fig-align="center"}

## Technical efficiency

  

## Cobb-Douglas formula
cd_fxn <- as.formula(log(prod)~ log(seed_q) + log(fert_q) + log(fl_labor_hr)
                     + log(hl_labor_hr))

## Estimating efficiency score
sfa_cd <- sfa(formula = cd_fxn, data = mlip_prod)

## summary results
summary(sfa_cd, extraPar = TRUE)
 

  

eff_score <- efficiencies(sfa_cd) %>% data.frame() %>%  tibble()
mlip_prod_eff <- bind_cols(mlip_prod, efficiencies(sfa_cd))

mlip_prod_eff %>% 
  ggplot(aes(efficiency, fill = farm_loc)) +
  geom_histogram(alpha = 0.5) +
  scale_fill_manual(values = c("#286351", "#ad581a", "#6f6c93")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  theme_1 +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_text(size = 12)) +
  labs(fill = NULL,
       y = element_blank(),
       x = "TE index of E(exp(-u) | e)")

ggsave("plot/eff_score.jpeg", dpi = 300, width = 6, height = 4.5)

 

## Allocative efficiency

## Cost efficiency

  

cost_fxn <- 
  as.formula(log(m_cost) ~ log(seed_p) + log(fert_p) + log(fl_labor_w) + log(hl_labor_w) + log(prod))


sfa_cost <- sfa(formula = cost_fxn, data = mlip_cost_prod)
sfa_cost %>% summary(extraPar = TRUE)

lrtest(sfa_cost)

efficiencies(sfa_cost) %>% hist()

 

# Linking PIM, social capital & collective irrigation management

## PLS-SEM model 2.1

  

## joining efficiency score with factor items
sc_ca_eff_mlip <- 
  bind_cols(sc_ca_mlip, mlip %>% select(qid)) %>% 
  left_join(mlip_prod_eff %>% select(qid, efficiency)) %>%
  relocate(qid, efficiency, .before = l30) %>% 
  mutate(efficiency = 1 - efficiency)

## measurement model
mm_model_21 <- 
  constructs(
    composite("Social capital", multi_items("l", c(30:32, 34:37))),
    composite("Perceived risks", multi_items("i", c(6, 9:12))),
    composite("Human capital", multi_items("j", c(17, 20, 21))),
    composite("Collective action", multi_items("m", 54:59)),
    composite("Irrigation satisfaction", multi_items("n", c(64, 68:71))),
    composite("Efficiency", single_item("efficiency"))
  )

## structural model
sm_model_21 <- 
  relationships(
    paths(from = c("Social capital", "Perceived risks", "Human capital"), 
          to = "Collective action"),
    paths(from = "Collective action",to = "Irrigation satisfaction"),
    paths(from = c("Collective action", "Irrigation satisfaction"), to = c("Efficiency"))
  )


## estimating the pls-sem model
pls_sem_model_21 <- 
  estimate_pls(
    measurement_model = mm_model_21,
    structural_model = sm_model_21,
    data = sc_ca_eff_mlip
  )


## pls-sem model summary
pls_sem_model_21_summary <- summary(pls_sem_model_21)

## plotting pls-sem model
plot(pls_sem_model_21)

 

### Bootstrapped PLS-SEM

  

## bootstrapping model
bootstrapped_pls_sem_21 <- bootstrap_model(seminr_model = pls_sem_model_21, nboot = 1e3)

## bootstrapped summary results
bootsrapped_summary_21 <- summary(bootstrapped_pls_sem_21, alpha = 0.10)
bootsrapped_summary_21$bootstrapped_paths %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

  
plot(bootstrapped_pls_sem_21)
 

  
## bootstrapped summary results
bootsrapped_summary_21$bootstrapped_total_paths %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

### Factor loadings

  
bootsrapped_summary_21$bootstrapped_loadings %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

  
bootsrapped_summary_21$bootstrapped_loadings %>% 
  data.frame() %>% rownames_to_column("factor") %>% 
  clean_names() %>% 
  ggplot(aes(bootstrap_mean, factor)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = x5_ci, xmax = x95_ci), width = 0.3) +
  geom_vline(xintercept = 0.3, lty = "dashed", color = "gray60") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.1)) +
  theme_1 +
  theme(panel.grid = element_blank()) +
  labs(x = "Bootstrap mean and 95% confidence interval",
       y = "Factor items")

ggsave("plot/fct2_loadings.jpeg", dpi = 400, width = 8, height = 6)
 

### Validity and reliability

  
pls_sem_model_21_summary$reliability %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

  
png("plot/fct2_reliability_validity.jpeg", units = "in", width = 6.2, height = 3.9, res = 400)
pls_sem_model_21_summary$reliability %>% plot()
dev.off()
dev.new()
 

### Discriminant validity

  
pls_sem_model_21_summary$validity$fl_criteria %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

## PLS-SEM model 2.2

  

## joining efficiency score with factor items
sc_ca_eff_mlip <- 
  bind_cols(sc_ca_mlip, mlip %>% select(qid)) %>% 
  left_join(mlip_prod_eff %>% select(qid, efficiency)) %>%
  relocate(qid, efficiency, .before = l30) %>% 
  mutate(efficiency = efficiency)

## measurement model
mm_model_22 <- 
  constructs(
    composite("Social capital", multi_items("l", c(30:32, 34:37))),
    composite("Perceived risks", multi_items("i", c(6, 9:12))),
    composite("Human capital", multi_items("j", c(17, 20, 21))),
    composite("Collective action", multi_items("m", 54:59)),
    composite("Irrigation satisfaction", multi_items("n", c(64, 68:71))),
    composite("Efficiency", single_item("efficiency"))
  )

## structural model
sm_model_22 <- 
  relationships(
    paths(from = c("Social capital", "Perceived risks", "Human capital"), 
          to = "Collective action"),
    paths(from = "Collective action",to = "Irrigation satisfaction"),
    paths(from = c("Collective action", "Irrigation satisfaction", "Perceived risks", "Human capital"), 
          to = c("Efficiency"))
  )


## estimating the pls-sem model
pls_sem_model_22 <- 
  estimate_pls(
    measurement_model = mm_model_22,
    structural_model = sm_model_22,
    data = sc_ca_eff_mlip
  )


## pls-sem model summary
pls_sem_model_22_summary <- summary(pls_sem_model_22)

## plotting pls-sem model
plot(pls_sem_model_22)

 

### Bootstrapped PLS-SEM

  

## bootstrapping model
bootstrapped_pls_sem_22 <- bootstrap_model(seminr_model = pls_sem_model_22, nboot = 1e3)

## bootstrapped summary results
bootsrapped_summary_22 <- summary(bootstrapped_pls_sem_22, alpha = 0.10)
bootsrapped_summary_22$bootstrapped_paths %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

  
plot(bootstrapped_pls_sem_22)
 

  
bootsrapped_summary_22$bootstrapped_total_paths %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

### Factor loadings

  
bootsrapped_summary_22$bootstrapped_loadings %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

### Validity and reliability

  
pls_sem_model_22_summary$reliability %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 

### Discriminant validity

  
pls_sem_model_22_summary$validity$fl_criteria %>% 
  kable(digits = 3) %>% 
  row_spec(0, bold = TRUE) %>% 
  kable_minimal() %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover"))
 
