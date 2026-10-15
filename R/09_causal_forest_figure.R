# 09_causal_forest_figure.R
# Causal forest effects of secondary completion on NEET by province and wealth
# quintile (women 15-64), from saved results. Report version with title in
# outputs/figures/, slide version without title in outputs/figures/slides/.
suppressPackageStartupMessages({ library(tidyverse); library(scales) })
load("outputs/causal_forest_results.RData")
FIGS <- "outputs/figures"; SL <- file.path(FIGS, "slides")
dir.create(SL, showWarnings = FALSE, recursive = TRUE)
ur <- "#E2231A"; ugr <- "#6C7279"
ate <- cf_results$ate_neet
ate_est <- 100 * unname(ate[1])
cat(sprintf("ATE on NEET: %.1f pp", ate_est))
if (length(ate) > 1) cat(sprintf(" (SE %.1f pp)", 100 * unname(ate[2])))
cat("\n")
d <- cf_results$cate_neet_all %>%
  filter(dimension %in% c("Province", "Wealth Quintile"), !is.na(group)) %>%
  mutate(across(c(mean_cate, ci_lo, ci_hi), ~ .x * 100))
prov <- d %>% filter(dimension == "Province") %>% arrange(desc(mean_cate))
quin <- d %>% filter(dimension == "Wealth Quintile") %>% slice(n():1)
dd <- bind_rows(prov, quin) %>%
  mutate(key = factor(paste(dimension, group, sep = "__"), levels = unique(paste(dimension, group, sep = "__"))))
print(dd %>% select(dimension, group, mean_cate, ci_lo, ci_hi, n) %>% mutate(across(where(is.numeric), ~ round(.x, 1))))
p <- ggplot(dd, aes(mean_cate, key)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = ate_est, linetype = "dotted", colour = "#333") +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi), orientation = "y", width = 0.3, linewidth = 0.6, colour = ur) +
  geom_point(size = 3.2, colour = ur, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f pp", mean_cate), x = ci_hi + 0.4), hjust = 0, size = 3.2, fontface = "bold") +
  facet_wrap(~ dimension, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub(".*__", "", x)) +
  scale_x_continuous(labels = label_number(suffix = " pp"), expand = expansion(mult = c(0.05, 0.35))) +
  labs(title = "Causal forest: effect of secondary completion on NEET, by province and wealth",
       subtitle = sprintf("Women 15-64 | mean predicted effect per group | dotted line = average effect (%.1f pp)", ate_est),
       x = "Change in NEET probability (pp)", y = NULL,
       caption = "NLSS IV 2022/23. Generalised random forest. Bars: +/-1.96 SE of the mean prediction in each group.") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, colour = "#222"),
        plot.subtitle = element_text(colour = ugr), plot.caption = element_text(size = 9, colour = ugr, hjust = 0),
        panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", size = 12))
ggsave(file.path(FIGS, "cf1_cate_province_wealth.png"), p, width = 12, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(SL, "cf1_cate_province_wealth.png"), p + labs(title = NULL), width = 12, height = 5.5, dpi = 300, bg = "white")
cat("saved cf1_cate_province_wealth (report + slide versions)\n")
