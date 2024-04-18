## Create cohause wide tibble 
## 

library(tidyverse)
library(fpp3)
library(readxl)
library(suncalc)

# read in cohaus use and generation data
cohaus <- read_csv("data/cohaus.csv") |> 
  mutate(dttm = parse_date_time(dttm, "dmy_HM"))

# cohaus data has missing observations, rebuild dttm with 30 min intervals
dttm <- seq(from = dmy_hm("01012022 00:00"), 
            to = dmy_hm("29022024 23:30"), 
            by = "30 min") |> 
  as_datetime()

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

glimpse(cohaus_wide)

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
            Gen = -sum(PV),
            Use = sum(APT,CS,EV,HP),
            NetUse = Use - Gen,
            Import = pmax(NetUse,0),
            Export = -pmin(NetUse,0)
            ) |>
  ungroup() |>
  select(dttm,APT,CS,EV,HP,Gen,Use,NetUse,Import,Export) |> 
  arrange(dttm) 

# read in nz public holidays (including auckland anniversary)
#
aklhols <- read_excel("data/aklhols.xlsx") |> 
  mutate(ObsDate = as_date(ObsDate))

# create table of sunrise and sunsets for Auckland, NZ 
#
getAklSunrise <- function(x = Sys.Date()) {
  tmp <- getSunlightTimes(date = x, 
                          lon = 174.7402, # lon & lat for Grey Lynn
                          lat = -36.8645, 
                          keep = c("sunrise","sunset"), 
                          tz = "Pacific/Auckland"
  )
  return(as_datetime(tmp[,4]))
}

getAklSunset <- function(x = Sys.Date()) {
  tmp <- getSunlightTimes(date = x, 
                          lon = 174.7402, 
                          lat = -36.8645, 
                          keep = c("sunrise","sunset"), 
                          tz = "Pacific/Auckland"
  )
  return(as_datetime(tmp[,5]))
}

# dummy variable if sun is up
# 
isSunUp <- function(x) {
  x <- force_tz(x, tzone = "Pacific/Auckland")
  ifelse(as_datetime(x) > getAklSunrise(as_date(x)) &
           as_datetime(x) < getAklSunset(as_date(x)), 
         1, 0)
}

sunUp <- cohaus_wide$dttm |> 
  map_lgl(isSunUp)

# dummy variable if work day
#                    
isWorkDay <- function(x) {
  ifelse(lubridate::wday(x) %in% c(1,7), FALSE, # 1 = Sun, 7 = Sat
         ifelse(as_date(x) %in% aklhols$ObsDate, FALSE, TRUE))
}

workDay <- cohaus_wide$dttm |> 
  map_lgl(isWorkDay)

# add sunUp and workday to cohaus_wide 
cohaus_wide <- cohaus_wide |> 
  add_column(sunUp = as_factor(sunUp),
             workDay = as_factor(workDay))

# read in NIWA temps and wind data from Auckland MOTAT EWS station
# 
motattemp <- read_csv("data/motattemp.csv", 
                      col_types = cols(Date = col_date(format = "%Y%m%d"), 
                                       Hour = col_time(format = "%H")))

motatwind <- read_csv("data/motatwind.csv", 
                      col_types = cols(Date = col_date(format = "%Y%m%d"), 
                                       Hour = col_time(format = "%H")))

# join motat temp and wind tibbles, compute ambient temperature
# formula: australian bureau of meteorology
# https://www.weather.gov/media/epz/wxcalc/vaporPressure.pdf
# 
weather <- motattemp |> left_join(motatwind, by = c("Date","Hour")) |> 
  fill(!c(Date,Hour), .direction = "down") |> 
  mutate(dttm = ymd(Date) + hms(Hour),
         rho = RH/100 * 6.105 * exp(17.27 * Tdry/(Tdry + 237.7)),
         AT = Tdry + 0.33 * rho - 0.7 * Spd - 4) |> 
  select(dttm, Tdry, RH, Spd, AT)

cohaus_wide <- cohaus_wide |> 
  left_join(weather, by = "dttm")

cohaus_wide <- cohaus_wide |> 
  fill(Tdry:AT, .direction = "down")


# create dummy variables for hour, week, month, quarter, year 
# 
periodDummy <- function(x = now(), y = "hour") {
  x <- force_tz(x, tzone = "Pacific/Auckland")
  ifelse(y == "hour", paste0("H",sprintf("%02d", hour(x))),
         ifelse(y == "week", paste0("W",sprintf("%02d", week(x))),
                ifelse(y == "month", paste0("M",sprintf("%02d", month(x))),
                       ifelse(y == "quarter", paste0("Q",sprintf("%02d", quarter(x))),
                              ifelse(y == "year", paste0("Y",sprintf("%02d", year(x))), NA)
                              )
                       )
                )
         )
}

hour <- map2_chr(cohaus_wide$dttm, "hour", periodDummy)
week <- map2_chr(cohaus_wide$dttm, "week", periodDummy)
month <- map2_chr(cohaus_wide$dttm, "month", periodDummy)
quarter <- map2_chr(cohaus_wide$dttm, "quarter", periodDummy)
year <- map2_chr(cohaus_wide$dttm, "year", periodDummy)

cohaus_wide <- cohaus_wide |>
  mutate(hour = as_factor(hour),
         week = as_factor(week),
         month = as_factor(month),
         quarter = as_factor(quarter),
         year = as_factor(year)
         )

# write to rds
cohaus_wide |> write_rds("data/cohaus_wide.rds")

glimpse(cohaus_wide)


