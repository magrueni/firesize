
library(tidyverse)
library(sf)

cntrs <- list.files("data/patches", ".csv$") %>% 
  str_sub(., 14, nchar(.) - 4)

k <- 0
out <- vector("list", length(cntrs))

for (i in list.files("data/patches", ".csv$", full.names = TRUE)) {
  k <- k + 1
  out[[k]] <- read_csv(i) %>%
    mutate(patch = as.integer(patch))
}

nfire <- out %>% map(nrow) %>% unlist()

a <- out[which(nfire > 0)] %>%
  set_names(cntrs[which(nfire > 0)]) %>%
  bind_rows(.id = "country")

clim <- read_csv("data/climate/era5_sm_vpd_summer_anomaly.csv")

grid <- read_sf("data/climate/climategrid_epsg3035.gpkg")

a_sf <- st_as_sf(a, coords = c("x", "y"))
st_crs(a_sf) <- st_crs(grid)
patches_grid <- st_intersection(a_sf, grid)

a <- patches_grid %>% st_drop_geometry()

a <- a %>% left_join(clim, by = c("year", "gridindex"))

# patch-level

ggplot(a %>% sample_frac(0.1), 
       aes(x = sm_z, y = area, col = vpd_z)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10() +
  scale_color_gradient2()

f <- lm(log(area) ~ sm_z * vpd_z, data = a)

summary(f)

effects::effect("sm_z:vpd_z", f) %>%
  as.data.frame() %>%
  ggplot(.) +
  geom_line(aes(x = sm_z, y = fit, col = factor(vpd_z))) +
  scale_color_brewer(palette = "RdBu", direction = -1)

# grid-level

a_grid <- a %>%
  group_by(gridindex, country, year) %>%
  summarize(mean = mean(area),
            median = median(area),
            sum = sum(area),
            var = var(area),
            sd = sd(area),
            varcoef = sd / mean,
            iqr = IQR(area),
            qdcoef = iqr / median,
            q75 = quantile(area, 0.75),
            q95 = quantile(area, 0.95)) %>%
  ungroup()

a_grid <- a_grid %>% 
  right_join(clim, by = c("year", "gridindex"))

ggplot(a_grid %>% filter(!is.na(mean)), 
       aes(x = vpd, y = median)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10() +
  facet_wrap(~country, scales = "free")

