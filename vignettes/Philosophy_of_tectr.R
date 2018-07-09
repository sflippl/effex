## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----makeittalk, out.width = "50%", echo = FALSE-------------------------
knitr::include_graphics(path = "figures/1_makeittalk.png")

## ----makeittalk_extended, out.width = "70%", echo = FALSE----------------
knitr::include_graphics(path = "figures/1_makeittalk_extended.png")

## ----separate, out.width = "40%", echo = FALSE---------------------------
knitr::include_graphics(path = "figures/1_separate_a1.png")
knitr::include_graphics(path = "figures/1_separate_a2.png")

## ----eval = FALSE--------------------------------------------------------
#  df <- get_variable("worldbank-gdp", "worldbank-population")

## ----eval = FALSE--------------------------------------------------------
#  library(worldbank)
#  df <- get_variable("worldbank-gdp")
#  ggdistribution(df$worldbank__gdp) # I will explain these implementational details
#                                    # in another vignette

## ----separate_b, echo = FALSE, out.width = "70%"-------------------------
knitr::include_graphics(path = "figures/1_separate_b.png")

## ----eval = FALSE--------------------------------------------------------
#  gdp <- new_variable(
#    class = "continuous"
#    source = "Worldbank",
#    description_path = "desc/wb/gdp.Rmd",
#    suffix = "$"
#  ) %>%
#    add_scale(x = "log10", colour = "Blues") %>%
#    add_model(lm_conflict) %>%
#    add_model(mboost_conflict, effect = c("bbs(., center = TRUE, df = 1)",
#                                          "bols(.)"))

## ----eval = FALSE--------------------------------------------------------
#  ggplot(df, aes(x = gdp)) + geom_standard()

## ----eval = FALSE--------------------------------------------------------
#  gdp <- gdp %>% change_geom("standard", x = "hist")

## ----eval = FALSE--------------------------------------------------------
#  ggplot(df, aes(x = gdp, y = population, colour = continent)) +
#    geom_standard()

