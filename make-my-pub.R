
# make my pub -------------------------------------------------------------
# runs a cutting edge analysis on some penguin data: what explains penguin weight?

# setup -------------------------------------------------------------------

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(here)
library(palmerpenguins)

results_name <- "v1.1" # specify the name of the saved results

results_path <-
  here("results", results_name) # create path to results

if (!dir.exists(results_path)) {
  # create results directory if it's missing
  dir.create(results_path, recursive = TRUE)
}

pub_theme <- ggplot2::theme_light(base_size = 14)

theme_set(pub_theme)



# explore data ------------------------------------------------------------

weight_plot <- penguins %>% 
  ggplot(aes(body_mass_g,fill = sex)) + 
  geom_density(alpha = 0.75) + 
  facet_grid(island ~ species) + 
  scale_x_continuous(guide = guide_axis(n.dodge = 2))

weight_things_plot <- penguins %>% 
  ggplot(aes(bill_length_mm, flipper_length_mm,fill = body_mass_g)) + 
  geom_point(size = 4, shape = 21, alpha = 0.75) + 
  scale_fill_viridis_c()


# fit a model -------------------------------------------------------------


model_a <- lm(body_mass_g ~ sex + island, data = penguins)

model_a_summary <- broom::tidy(model_a)

model_b <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm, data = penguins)

model_b_summary <- broom::tidy(model_b)


# save results ------------------------------------------------------------

plots <-
  ls()[grepl("_plot", ls())] # find all things that end in _plot

# just a little trick. Combining file.path with the output of here keeps the formatting consistent

models <-   ls()[grepl("model_", ls())] # find all things that end in _plot

save(file = file.path(results_path, "penguin-plots.Rdata"),
     list = plots) # save plots

save(file = file.path(results_path, "penguin-models.Rdata"),
     list = models) # save plots


# knit report -------------------------------------------------------------

output_format = c(
  "bookdown::html_document2",
  "bookdown::pdf_document2",
  "bookdown::word_document2"
)

if (any(grepl("pdf",output_format ))){
  
  if (tinytex::tinytex_root() == ""){
    tinytex::install_tinytex()
  }
  
}

# render reports
rmarkdown::render(
  here::here("documents", "my-pub.Rmd"),
  params = list(results_name = results_name),
  output_format = output_format,
  output_dir = results_path # put the reports in the results directory
)



