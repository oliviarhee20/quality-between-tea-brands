library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(ri2)
library(flextable)

set.seed(12345)

# Randomization code

options <- c("Bigelow", "Harney")

random_choice <- sample(options, size = 1)

print(random_choice) # Repeat five times for each round

# Load and organize data

tea_data <- read.csv("tea_data.csv") %>%
  mutate(Z    = as.integer(tea_brand == "harney"),
         pair = paste(assistant_number, tea_type))

long <- tea_data %>%
  pivot_longer(c(flavor_score, strength_score),
               names_to = "outcome", values_to = "rating")

# Show flavor and strength outcomes

long %>%
  count(outcome, tea_brand, rating) %>%
  pivot_wider(names_from = rating, values_from = n,
              values_fill = 0, names_sort = TRUE) %>%
  kable()

# DiM ATE identification and randomization inference

estimate <- function(d, y) {
  d   <- d[!is.na(d[[y]]), ]
  tt  <- t.test(d[[y]][d$Z == 1], d[[y]][d$Z == 0])
  dec <- declare_ra(blocks = d$pair)
  ri  <- conduct_ri(as.formula(paste(y, "~ Z")), declaration = dec,
                    sharp_hypothesis = 0, data = d,
                    sims = 1000, progress_bar = FALSE)
  data.frame(ATE     = unname(tt$estimate[1] - tt$estimate[2]),
             SE      = tt$stderr,
             CI_low  = tt$conf.int[1],
             CI_high = tt$conf.int[2],
             p_t     = tt$p.value,
             p_RI    = summary(ri)$two_tailed_p_value)
}

results <- expand.grid(tea_type = c("all teas", "herbal", "black", "green"),
                       outcome  = c("flavor_score", "strength_score"),
                       stringsAsFactors = FALSE)
results <- cbind(results, do.call(rbind, Map(function(g, y) {
  d <- if (g == "all teas") tea_data else filter(tea_data, tea_type == g)
  estimate(d, y)
}, results$tea_type, results$outcome)))

kable(results, digits = 3, row.names = FALSE)

f  <- results %>% filter(tea_type == "all teas", outcome == "flavor_score")
s  <- results %>% filter(tea_type == "all teas", outcome == "strength_score")
r2 <- function(x) sprintf("%.2f", x)

# Print results table

library(flextable)

results <- data.frame(
  tea_type = rep(c("all teas", "herbal", "black", "green"), 2),
  outcome  = rep(c("flavor", "strength"), each = 4),
  ATE      = c(-0.20, 0.20, 0.20, -1.00, -0.33, -0.20, -0.40, -0.40),
  SE       = c(0.41, 0.58, 0.63, 0.85, 0.40, 0.71, 0.45, 0.78),
  CI_low   = c(-1.05, -1.19, -1.28, -2.98, -1.15, -1.87, -1.50, -2.19),
  CI_high  = c(0.65, 1.59, 1.68, 0.98, 0.48, 1.47, 0.70, 1.39),
  p_t      = c(0.63, 0.74, 0.76, 0.28, 0.41, 0.79, 0.41, 0.62),
  p_RI     = c(0.73, 1.00, 1.00, 0.38, 0.46, 1.00, 0.75, 0.81)
)

ft <- bg(colformat_double(flextable(results), digits = 2), bg = "white", part = "all")
save_as_image(ft, path = "tea_table.png")

# Graphic (Coppock 2021)

ci95 <- function(y) {
  y <- y[!is.na(y)]
  if (length(y) < 2 || sd(y) == 0)
    return(data.frame(y = mean(y), ymin = mean(y), ymax = mean(y)))
  ci <- t.test(y)$conf.int
  data.frame(y = mean(y), ymin = ci[1], ymax = ci[2])
}

panels  <- c("all teas", "herbal", "black", "green")
plot_df <- bind_rows(mutate(long, panel = "all teas"),
                     mutate(long, panel = tea_type)) %>%
  mutate(panel = factor(panel, levels = panels))
labels  <- results %>%
  mutate(panel = factor(tea_type, levels = panels),
         label = sprintf("ATE = %.2f [%.2f, %.2f]\nRI p = %.2f",
                         ATE, CI_low, CI_high, p_RI))

ggplot(plot_df, aes(tea_brand, rating, colour = tea_brand)) +
  geom_point(position = position_jitter(width = 0.15, height = 0.1, seed = 1),
             alpha = 0.6) +
  stat_summary(fun.data = ci95, position = position_nudge(x = 0.35)) +
  geom_text(data = labels, aes(x = 1.5, y = 6.2, label = label),
            inherit.aes = FALSE, size = 3) +
  facet_grid(outcome ~ panel) +
  scale_colour_manual(values = c(bigelow = "blue", harney = "red"),
                      guide = "none") +
  scale_y_continuous(breaks = 1:5) +
  coord_cartesian(ylim = c(0.5, 6.6)) +
  labs(x = NULL, y = "Rating (1-5)",
       caption = "Points -- cups of tea; bars -- mean with 95% CI") +
  theme_minimal()