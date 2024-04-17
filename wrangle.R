## Create cohause wide tibble 
## 

library(tidyverse)
library(fpp3)

# read in cohaus use and generation data
cohaus <- read_csv("data/cohaus.csv") |> 
  mutate(dttm = parse_date_time(dttm, "dmy_HM"))

# cohaus data has missing observations, build wide tibble using left joins
dttm <- seq(from = dmy_hm("01092021 00:00"), to = dmy_hm("29022024 23:30"), by = "30 min")

# look up function that returns NA if kWh data is missing
kWhLookup <- function(x = dmy_hm("01012022 09:00"), source = "APT") {
  tmp <- cohaus |> filter(dttm == x, sourceID == source) |> 
    pull(kWh)
  ifelse(is_null(tmp), NA, tmp)
}

# create vectors of kWh data for each sink / (source)
APT <- map2_dbl(dttm, "APT", kWhLookup)
CS <- map2_dbl(dttm, "CS", kWhLookup)
EV <- map2_dbl(dttm, "EV", kWhLookup)
HP <- map2_dbl(dttm, "HP", kWhLookup)
PV <- map2_dbl(dttm, "PV", kWhLookup)

# create wide tibble

cohaus_wide <- tibble(
  dttm = dttm,
  APT = APT,
  CS = CS,
  EV = EV,
  HP = HP,
  PV = PV
)

cohaus_wide <- cohaus_wide |> 
  fill(!dttm, .direction = "down") |> 
  mutate(date = date(dttm),
         hour = hour(dttm)) |> 
  group_by(date,hour) |> 
  summarise(dttm = min(dttm),
            APT = sum(APT),
            CS = sum(CS),
            EV = sum(EV),
            HP = sum(HP),
            Source = sum(PV),
            Sink = sum(APT,CS,EV,HP)
            ) |>
  ungroup() |>
  select(dttm,APT,CS,EV,HP,Source,Sink) |> 
  arrange(dttm) 
  

# write to rds
cohaus_wide |> write_rds("data/cohaus_wide.rds")


# tidy data 
cohaus_long <- cohaus_wide |> 
  pivot_longer(cols = -dttm, 
               names_to = "sourceID", 
               values_to = "kWh")

# plot a month of use (sink) and generation (source)
cohaus_long |> 
  filter(dttm < dmy("01102021"),
         sourceID %in% c("Sink", "Source")) |> 
  ggplot(aes(x = dttm, y = kWh, color = sourceID)) +
  geom_line()



