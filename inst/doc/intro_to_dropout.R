## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(dropout)

## ----message=FALSE------------------------------------------------------------
library(dplyr)
data("flying")

## -----------------------------------------------------------------------------
drop_summary(flying, "location_census_region")

## ----fig.cap="Comparative Analysis of Age and Gender against Dropout Rates"----
library(ggplot2)

flying %>%
  drop_detect("smoking_violation") %>% 
  bind_cols(flying, .) %>% 
  filter(!is.na(gender)) %>% 
  mutate(age = factor(age, levels = c("18-29", "30-44", "45-60", "> 60"))) %>% 
  ggplot(aes(x=age, fill=dropout)) +
  geom_bar(position="dodge") +
  facet_grid(gender ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

