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
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
files <- list.files(here("data", "raw"), pattern = "Ventas", full.names = T)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
my_read <- function(file){
  # browser()
  data <- read_fwf(file = file,
           skip = 16,
           col_types = "c",
           skip_empty_rows = T,
           col_positions = fwf_widths(widths = c(19, 12, 22, 15, 17, 18, 18))) %>% 
    set_names(c("date", "doc", "id", "client", "count", "unit_price", "total_price"))
  
  # Find end of S80s
  nrow <- min(which(str_detect(data$id, "Subtotales")))
  
  data <- head(data, nrow) %>% 
    mutate(date = as.character(date),
           doc = as.numeric(doc),
           id = as.character(id),
           client = as.character(client),
           count = as.numeric(count),
           unit_price = as.numeric(unit_price),
           total_price = as.numeric(unit_price))
  
  return(data)
  
}

my_read(files[5])

data <- map_dfr(files[1:12], my_read)

clean_data <- data %>% 
  filter(count < 0) %>% 
  mutate(date = str_replace(date, "Ene", "Jan"),
         date = str_replace(date, "Abr", "Apr"),
         date = str_replace(date, "Ago", "Aug"),
         date = dmy(date),
         week = week(date),
         year = year(date),
         month = month(date),
         ym = ymd(paste(year, month, "15")),
         count = -1 * as.numeric(count)) %>% 
  drop_na()

monthly_scuba <- clean_data %>% 
  group_by(client, ym) %>% 
  summarize(count = sum(count),
            .groups = "drop")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

p1 <- ggplot(clean_data, aes(x = week, y = count, color = factor(year), group = year)) + 
  stat_summary(geom = "line", fun = "sum", na.rm = T) +
  theme_bw() +
  labs(x = "Week of year",
       y = "# of tanks per week")

p2 <- ggplot(clean_data, aes(x = ym, y = count)) + 
  stat_summary(geom = "line", fun = "sum", na.rm = T) +
  theme_bw() +
  labs(x = "Year",
       y = "# of tanks per month")

cowplot::plot_grid(p1, p2, ncol = 2)

ggplot(clean_data, aes(x = week, y = count, color = factor(year), group = year)) + 
  stat_summary(geom = "line", fun = "sum", na.rm = T) +
  theme_bw() +
  labs(x = "Week of year",
       y = "# of tanks per week",
       color = "Year") +
  facet_wrap(~year_group, ncol = 1)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(clean_data,
        file = here("data", "processed", "scuba_data_clean.rds"))

saveRDS(monthly_scuba,
        file = here("data", "processed", "monthly_scuba.rds"))
