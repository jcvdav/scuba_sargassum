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
scuba <- readRDS(file = here("data", "processed", "monthly_scuba.rds")) %>% 
  complete(ym, client, fill = list(count = 0))
sargassum <- read_table(file = here("data/raw/Sargassum_ts_cut.txt")) %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(date)) %>% 
  mutate(month = month(date),
         year = year(date),
         ym = ymd(paste(year, month, "15"))) %>% 
  select(ym, mass_ton)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

combined <- sargassum %>% 
  left_join(scuba, by = join_by("ym")) %>% 
  complete(ym, client, fill = list(count = 0)) %>% 
  replace_na(replace = list(count = 0, mass_ton = 0)) %>% 
  drop_na(client) %>% 
  mutate(year = year(ym),
         month = month(ym),
         count = count,
         mass_ton = mass_ton) %>% 
  filter(!(count == 0 & mass_ton == 0),
         !year == 2024)

## EXPORT ######################################################################
saveRDS(object = combined,
        file = here("data", "estimation_panels", "monthly_estimation_panel.rds"))
