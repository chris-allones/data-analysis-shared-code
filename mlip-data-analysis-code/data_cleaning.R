
# loading libraries
library(tidyverse)

# Loading_data
mlip <- read_csv("data/20220801_mlip_data.csv")


# Data wrangling

## social capital and collective actions
sc_ca_mlip <- 
  mlip %>% 
  select(
    # social capital
    l30:l32, l34:l37,
    # collective action
    m54:m59,
    # perceived farming risks
    i1:i12,
    # human capital
    j13:j21,
    ## irrigation satisfaction
    n64, n66:n71
  ) %>% 
  ## removed based from EFA results
  select(-i2, -(j13:j16), -j18, -j19, -i1, -(i3:i5), -i7, -i8, -n66, -n67)


## social items data prep for plotting

sc_ca_items <- 
  sc_ca_mlip %>% 
  pivot_longer(cols = l30:n71, 
               names_to = "items",
               values_to = "response") %>% 
  mutate(factor = case_when(str_detect(items, "l") ~ "(a) Social capital",
                            str_detect(items, "m") ~ "(b) Collective action",
                            str_detect(items, "i") ~ "(c) Perceived farming risks",
                            str_detect(items, "j") ~ "(e) Human capital",
                            str_detect(items, "n") ~ "(d) Irrigation satisfaction",
                            TRUE ~ items
                            )) %>%
  mutate(items = case_when(# social capital
                           items == "l30" ~ "In the village, people trust each other.",
                           items == "l31" ~ "The relationship among people in this village/ neighborhood is generally harmonious.",
                           items == "l32" ~ "The people in this village/ neighborhood are helping each other.",
                           items == "l34" ~ "Members of the water user group go along with each other.",
                           items == "l35" ~ "Members of the water user group trust each other.",
                           items == "l36" ~ "Members of the water user group are willing to offer help with each other.",
                           items == "l37" ~ "Members are sharing and exchanging ideas to improve farming skills.",
                           
                           # collective actions
                           items == "m54" ~ "Members agree and comply with the rules and regulations of WUG.",
                           items == "m55" ~ "Members cooperate in maintaining the irrigation canal and infrastructure.",
                           items == "m56" ~ "Members can provide production information to the irrigation officer before the next irrigation schedule.",
                           items == "m57" ~ "Members can use the irrigation according to the scheduled cycle of irrigation.",
                           items == "m58" ~ "Members take care of the irrigation infrastructure to avoid leakage and other damages. ",
                           items == "m59" ~ "Members strictly comply with the rules and regulations of the irrigation group.",
                           
                           # perceived farming risks
                           items == "i6" ~ "Erratic rainfall pattern.",
                           items == "i9" ~ "Uncertain price of rice.",
                           items == "i10" ~ "Uncertain price of inputs (e.g., fertilizer, seeds, and other).",
                           items == "i11" ~ "Uncertain demand.",
                           items == "i12" ~ "Influence of middlemen.",
                           
                           # human capital
                           items == "j17" ~ "Can take a leadership role in the water user group.",
                           items == "j20" ~ "Can perform record keeping (e.g., recording of expenses, sales and farm transaction).",
                           items == "j21" ~ "Can perform necessary farm finance (e.g., computation of farm profit or losses).",
                           
                           # irrigation satisfaction
                           items == "n64" ~ "Clear scheduling of irrigation.",
                           items == "n68" ~ "On-time irrigation based on the scheduled irrigation.",
                           items == "n69" ~ "Collection of water charges.",
                           items == "n70" ~ "Perceived water quality provided by the irrigation.",
                           items == "n71" ~ "Condition of irrigation canal and ditches.",
                           TRUE ~ items)) %>% 
  relocate(factor, .before = items) %>% 
  count(factor, items, response) %>% 
  group_by(items) %>% 
  mutate(prop_response = n / sum(n) * 100) %>% 
  ungroup() %>% 
  mutate(items = str_wrap(items, 50)) %>% 
  mutate(response_2 = factor(response, levels = 1:7) %>% fct_rev())


## production data 

mlip_prod <- 
  mlip %>% 
  filter(prod <= 22000) %>% 
  mutate(seed_q = seed_q * area,
         fert_q = fert_q * area,
         fl_labor_hr = fl_labor_hr * area,
         hl_labor_hr = hl_labor_hr * area) %>% 
  select(qid, farm_loc, age, educ_level, hh_mem, offarm_inc, soil_qual, topog,
         area, prod, seed_q, fert_q, pest_q,
         pestq_dum, fl_labor_hr, hl_labor_hr, area,
         input_mrkt_dist, prod_mrkt_dist, yrs_farming) %>% 
  # mean scaling inputs
  mutate(prod = prod / mean(prod),
         seed_q = seed_q / mean(seed_q),
         fert_q = fert_q / mean(fert_q),
         fl_labor_hr = fl_labor_hr / mean(fl_labor_hr),
         hl_labor_hr = hl_labor_hr / mean(hl_labor_hr))


## cost data
mlip_cost_prod <- 
  mlip %>% 
  select(qid, farm_loc, prod, seed_q, seed_p, fert_q, fert_p, fl_labor_hr, fl_labor_w, hl_labor_hr, hl_labor_w) %>% 
  mutate(cost = seed_q*seed_p + fert_q*fert_p +  fl_labor_hr*fl_labor_w + hl_labor_hr*hl_labor_w) %>% 
  mutate(m_cost = cost / mean(cost, na.rm = TRUE))
