################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------

# Load data --------------------------------------------------------------------
combined <- readRDS(file = here("data", "estimation_panels", "monthly_estimation_panel.rds"))

## PROCESSING ##################################################################

# Individual regressions with no lag
ind <- feols(asinh(count) ~ asinh(mass_ton) | client + year + month,
             panel.id = ~client + ym,
             vcov = "NW",
             data = combined)

# Individual regressions with lag
ind_lag <- feols(asinh(count) ~ l(asinh(mass_ton)) | client + year + month,
                 panel.id = ~client + ym,
                 vcov = "NW",
                 data = combined)

# Aggregate regressions
agg <- feols(asinh(count) ~ asinh(mass_ton) | year + month,
             panel.id = ~client + ym,
             vcov = "NW",
             data = combined %>% 
               group_by(ym, year, month, mass_ton) %>% 
               summarize(count = sum(count, na.rm = T),
                         .groups = "drop") %>% 
               mutate(client = 1))


agg_lag <- feols(asinh(count) ~ l(asinh(mass_ton)) | year + month,
                 panel.id = ~client + ym,
                 vcov = "NW",
                 data = combined %>% 
                   group_by(ym, year, month, mass_ton) %>% 
                   summarize(count = sum(count, na.rm = T),
                             .groups = "drop") %>% 
                   mutate(client = 1))



contemp <- list(ind, agg)
lags <- list(ind_lag, agg_lag)

modelsummary::modelsummary(list("Panel A) Contemporaneous esstimates" = contemp,
                                "Panel B) Lag-1 (months) estimates" = lags),
                           shape = "rbind",
                           coef_rename = c("asinh(mass_ton)" = "asinh[Sargassum biomass (tons)]",
                                           "l(asinh(mass_ton), 1)" = "asinh[Sargassum biomass (tons)]"),
                           stars = panelsummary:::econ_stars(),
                           gof_omit = "With|IC|FE|Std.",output = "latex",
                           title = "Estimaes of the elasticity of # tanks sold with respect to estimated sargassum biomass (tons).",
                           notes = "Column 1 shows client-level estimates, and column 2 shows industry-level estimates.
                           The top panel shows contemporaneous estimates, and the bottom panel shows industry-wide estimates.
                           All models include year and month fixed effects. Client-level models also include client-level fixed effects.
                           Standard errors are panel-robust and heteroskedasticity consistent, calculated a la  Newey-West (Lag of 3 preiods).")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ggplot(combined,
       aes(x = ym, y = count)) + 
  geom_point()

ggplot(combined,
       aes(x = ym, y = mass_ton)) + 
  geom_line() +
  scale_x_continuous(breaks = ym(paste(c(2012:2023), "01")))

ggplot(combined, aes(x = asinh(mass_ton), y = asinh(count))) + 
  geom_smooth(method = "lm", aes(group = client), se = F, linewidth = 0.1) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red")

tanks_time <- combined %>% 
  group_by(ym, year, month, mass_ton) %>% 
  summarize(count = sum(count, na.rm = T),
            .groups = "drop") %>% 
  ggplot(aes(x = ym, y = count)) + 
  geom_hline(aes(yintercept = mean(count, na.rm = T) + sd(count, na.rm = T)), linetype = "dashed") +
  geom_hline(aes(yintercept = mean(count, na.rm = T) - sd(count, na.rm = T)), linetype = "dashed") +
  geom_line() +
  geom_point() +
  theme_minimal(base_size = 10) +
  labs(x = "Period (year-month)",
       y = "# Scuba tank fills")

ggsave(plot = tanks_time,
       filename = "tanks_time.pdf",
       width = 5,
       height = 4)


sargassum_time <- combined %>% 
  select(ym, mass_ton) %>% 
  distinct() %>% 
  ggplot(aes(x = ym, y = mass_ton)) + 
  geom_hline(aes(yintercept = mean(mass_ton, na.rm = T) + sd(mass_ton, na.rm = T)), linetype = "dashed") +
  geom_hline(aes(yintercept = mean(mass_ton, na.rm = T) - sd(mass_ton, na.rm = T)), linetype = "dashed") +
  geom_line() +
  geom_point() +
  theme_minimal(base_size = 10) +
  labs(x = "Period (year-month)",
       y = "Sargassum biomass (tons)")

ggsave(plot = sargassum_time,
       filename = "sargassum_time.pdf",
       width = 5,
       height = 4)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------


# Which year had the biggest imapct?
combined %>% 
  mutate(predicted = predict(ind, newdata = .),
         predicted_wos = predict(ind, newdata = mutate(., mass_ton = 0))) %>% 
  mutate(difference = predicted - predicted_wos) %>% 
  group_by(year, mass_ton) %>% 
  summarize(difference = sum(difference)) %>% 
  ggplot(aes(x = year, y = difference)) +
  geom_col() +
  labs(x = "Year",
       y = "Losses (Estimated missing # of dives)") +
  scale_x_continuous(breaks = c(2012:2023))



feols(asinh(count) ~ asinh(mass_ton) | csw(0, client, year, month),
      panel.id = ~client + ym,
      vcov = "NW",
      data = combined) %>% 
  set_names(c(1:4)) %>% 
  modelsummary::modelsummary(coef_rename = c("asinh(mass_ton)" = "asinh[Sargassum biomass (tons)]"),
                             stars = panelsummary:::econ_stars(),
                             gof_omit = "With|IC|Std.",
                             output = "latex",
                             title = "Estimaes of the elasticity of # tanks sold with respect to estimated sargassum biomass (tons) under different fixed-effects specifications.",
                             notes = "Column 4 here is the same as column 1 in Table 1. Columns 1:3 show specifications where fixed-effects are added step-wise.
                             Note that the coefficien tof interest is always negative and significant, and between -0.6 and -0.1.")

