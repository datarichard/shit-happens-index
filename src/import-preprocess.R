library(tidyverse)
library(haven)

home <- c("~/Dropbox (Sydney Uni)/HILDA/shit-happens-index")

#### Import HILDA data ####
filepaths <- list.files(
  path = '~/Dropbox (Sydney Uni)/HILDA/data',
  pattern = '^Combined.*.dta$',
  full.names = TRUE
)

hilda <- list()
for (pathtofile in filepaths) {
  df <- read_dta(pathtofile)
  hilda <- append(hilda, list(df))
  cat('.')
}

#### Gather events ####
source("~/Dropbox (Sydney Uni)/HILDA/src/gather_hilda.R")

# We need the year and quarter of each event
gather_event_year <- function(eventcode) {
  codeq <- paste0(eventcode, 'q', seq(4))
  gather_hilda(hilda, c(eventcode, codeq)) %>%
    spread(key = code, value = val) %>%
    mutate_at(eventcode, funs(case_when(. < 0 ~ NA_real_, 
                                        TRUE ~ as.numeric(.)))) %>%
    mutate_at(eventcode, funs(. - 1)) %>%
    mutate_at(codeq, funs(case_when(. < 0 ~ NA_real_,
                                    TRUE ~ as.numeric(.)))) %>% 
    mutate(rowsums = rowSums(.[3:7], na.rm = TRUE)) %>%
    filter(rowsums > 0) %>% 
    rowwise() %>%
    mutate(event_year = which(letters == wave) + 2000) %>%
    ungroup() %>%
    select(-rowsums) -> df
  
  colnames(df) <- c("xwaveid", "wave", "annual", "post03",
                    "post06", "post09", "post12", "event_year")
  
  df %>%
    mutate(post12 = ifelse(rowSums(.[4:7]) == 0, 1, post12)) %>%
    select(-annual, -wave) %>%
    gather(month, val, post03:post12) %>%
    filter(val > 0) %>%
    select(-val) -> df
  
  return(df)
}

# Select the events (death of spouse, bankruptcy, divorce)
named_events <- list(
  Widowed = "ledsc",    
  Divorced = "lesep",
  Bankruptcy = "lefnw",
  Jailed = "lejls",
  Attacked = "levio",
  Injured = "leins",
  Reconciled = "lercl", 
  Fired = "lefrd", 
  Family_harmed = "leinf",   
  Robbed = "lepcm",
  Friend_died = "ledfr",
  Relative_died = "ledrl",
  Relative_jailed = "lejlf",
  Home_lost = "ledhm",
  Moved = "lemvd",
  Hired = "lejob",
  Promoted = "leprm",
  Retired = "lertr",
  Money_gained = "lefni",
  Pregnant = "leprg",
  Childbirth = "lebth",
  Married = "lemar"
)

event_years <- map(.x = named_events,
                   .f = ~gather_event_year(.x)) 

# Once we have the event years, we need to gather the wellbeing observations 
# around those time points for each person

#### Gather wellbeing ####
mhi5 <- gather_hilda(hilda, c('ghmh')) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
  spread(code, val) %>%
  rowwise() %>%
  mutate(year = which(letters == wave) + 2000) %>%
  ungroup()

#### Join wellbeing with events ####
mhi5_by_events <- map(.x = event_years, .f = ~{
  left_join(mhi5, .x, by = c("xwaveid")) %>%
    group_by(xwaveid) %>%
    filter(any(!is.na(event_year))) %>%
    ungroup() %>%
    mutate(delta_year = year - event_year)}
) 

#### Flag people with obs. before & after event ####
# We can now use delta_year to flag people with at least one wellbeing score 
# before and after the event. We will also create flags to filter people report-
# ing multiple instances of the same event-type, or instances of the same event-
# type which are close together (i.e., a more restrictive subset of multi).
# 
# Finally we join with basic demographics (sex and age)
# 
# Gather and join demographic variables
sex <- gather_hilda(hilda, 'hgsex') %>%
  group_by(xwaveid) %>%
  summarise(sex = round(mean(val))) %>%
  transmute(
    xwaveid,
    female = sex - 1,
  )

age_and_sex <- gather_hilda(hilda, 'hgage') %>% 
  spread(code, val) %>%
  left_join(sex)

# Final flags and join
map(.x = mhi5_by_events, .f = ~{
  na.omit(.x) %>% # remove missing outcome data
    group_by(xwaveid) %>%
    mutate(multi = length(unique(event_year)) > 1) %>% # flag any multiple event
    filter(delta_year >= -3 & delta_year <= 3) %>%     # select obs within 3-years
    mutate(close = any(rle(year)[[1]] > 1)) %>%        # flag events close in time
    group_by(xwaveid, event_year) %>%                  # 
    mutate(                                            # 
      before = any(delta_year < 0),                    # obs. before event
      after = any(delta_year > 0)                      # obs. after event
    ) %>%                                              # 
    # filter(before & after) %>%              # select events obs. before & after
    ungroup() %>%
    mutate(time = case_when(                # wellbeing observed:
      delta_year == -3 ~ "pre36",           # more than three years before event
      delta_year == -2 ~ "pre24",           # more than two years before event
      delta_year == -1 ~ "pre12",           # more than one year before event
      delta_year ==  0 ~ month,             # less than one year after event
      delta_year ==  1 ~ "post24",          # less than two years after event
      delta_year ==  2 ~ "post36",          # less than three years after event
      delta_year ==  3 ~ "post48")          # less than four years after event
    ) %>%
    select(xwaveid, wave, year, ghmh, event_year, time, multi, close) %>%
    left_join(age_and_sex)}
  ) %>% 
  write_rds("data/mhi5_by_all_lifeevents.rds")



  

