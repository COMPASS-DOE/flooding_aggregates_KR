## functions to import and process respiration data
## 
## 

source("2-code/0-packages.R")

library(tidyverse)
library(beepr)
#library(googledrive)
theme_set(theme_bw(base_size = 14))

import_licor_data = function(FILEPATH){

  filePaths <- list.files(path = FILEPATH, pattern = ".82z", full.names = TRUE, recursive = TRUE)
  licor_dat <- do.call(bind_rows, lapply(filePaths, function(path) {
    
    data = read.csv(unzip(path, "data.csv"), skip = 1) %>% dplyr::select(DATE, TIME, CO2_DRY, CH4_DRY)
    port1_value = jsonlite::read_json(unzip(path, "metadata.json"))$`LI-8250`$PORT
    port2_value = jsonlite::read_json(unzip(path, "metadata.json"))$`8250-01`$PORT
    
    data %>% mutate(port1 = port1_value,
                    port2 = port2_value,
                    #datetime = ymd_hms(paste(DATE, TIME)),
                    source = basename(path),
                    #CO2_DRY = as.numeric(CO2_DRY),
                    #CH4_DRY = as.numeric(CH4_DRY)
                    )
    
  }))

}
licor_data = import_licor_data(FILEPATH = "1-data/respiration/licor_data")
licor_time = read.csv("1-data/respiration/licor_time.csv")

licor_map = data.frame(
  sample = c("sample1", "sample2", "sample3", "sample4", "sample5", "sample6", "ambient7", "ambient8"),
  port = c(1, 2, 3, 4, 5, 6, 7, 8)
)


licor_time_processed = 
  licor_time %>% 
  mutate(datetime = mdy_hm(paste(licor_date, licor_time))) %>% 
  dplyr::select(licor_action, datetime) %>% 
  drop_na() %>% 
  filter(licor_action %in% c("start", "stop")) %>% 
  mutate(group = round(0.1+ row_number()/2)) %>% 
  pivot_wider(names_from = "licor_action", values_from = "datetime")


process_licor_data = function(licor_data, licor_map){
  
  licor_columns = 
    licor_data %>% 
    janitor::clean_names() %>% 
    dplyr::select(date, time, port1, co2_dry, ch4_dry) %>% 
    mutate(
           datetime = ymd_hms(paste(date, time)),
           co2_dry = as.numeric(co2_dry),
    ) %>% 
    rename(port = port1) %>% 
    filter(!is.na(co2_dry)) %>% 
    filter(co2_dry >= 0) 
  
  licor_processed_ppm_TEMP = 
    licor_columns %>%
    left_join(licor_map) %>% 
    rename(co2_ppm = co2_dry,
           ch4_ppb = ch4_dry) %>% 
    arrange(date, time) %>% 
#    mutate(timepoint = case_when(datetime > ymd_hms("2025-12-14 13:00:00") & datetime < ymd_hms("2025-12-15 9:00:00") ~ "baseline",
#                             # datetime > ymd_hms("2026-01-12 12:00:00") & datetime < ymd_hms("2026-01-13 9:00:00") ~ "2-dry ",
#                             datetime > ymd_hms("2026-01-13 12:00:00") & datetime < ymd_hms("2026-01-14 8:00:00") ~ "W1",
#                             # datetime > ymd_hms("2026-03-10 13:30:00") & datetime < ymd_hms("2026-03-10 13:50:00") ~ "4-dry",
#                             datetime > ymd_hms("2026-03-10 14:00:00") & datetime < ymd_hms("2026-03-11 10:00:00") ~ "W2",
#                             # datetime > ymd_hms("2026-04-28 10:50:00") & datetime < ymd_hms("2026-04-28 12:30:00") ~ "6-dry",
#                             datetime > ymd_hms("2026-04-28 15:00:00") & datetime < ymd_hms("2026-04-29 11:00:00") ~ "W3",
#    )) %>% 
    mutate(port = case_when(port >= 7 ~ 8, .default = port)) %>% 
#    filter(port %in% c(1, 2, 3, 4, 8)) %>% 
#    filter(!is.na(timepoint)) %>% 
    force()
  
  # Whenever the valve position changes, that's a new sample starting
  newsample <- licor_processed_ppm_TEMP$port != c(NA, head(licor_processed_ppm_TEMP$port, -1))
  newsample[is.na(newsample)] <- FALSE
  licor_processed_ppm_TEMP$sample_number = cumsum(newsample)
  
  ## the ambient is a mess because it was run after each sample, for flushing.
  ## so the initial values for each ambient reading are high, but it goes down to actual ambient values by the end of the reading
  ## so, we'll just keep the final reading of each sample_number
#  ambient = 
#    licor_processed_ppm_TEMP %>% 
#    filter(core_name == "Ambient") %>%
#    group_by(sample_number) %>% 
#    filter(datetime == max(datetime)) %>% 
#    filter(core_name == "Ambient") %>%
#    group_by(sample_number) %>% 
#    filter(datetime == max(datetime))
#  
#  ## now add this back to the sample data
#  ## so the final file will have the full timeseries of soil cores and one reading per sample_number for ambient
#  licor_processed_ppm = 
#    licor_processed_ppm_TEMP %>% 
#    filter(core_name != "Ambient") %>%
#    bind_rows(ambient)
#  
#  licor_processed_ppm

  licor_processed_ppm_TEMP
  }
licor_processed_ppm = process_licor_data(licor_data, licor_map)

licor_processed_ppm_subset = 
  licor_processed_ppm %>% 
  subset(datetime <= licor_time_processed$stop & datetime >= licor_time_processed$start) %>% 
  filter(datetime >= "2026-09-16 13:06:00" & datetime <= "2026-09-16 23:06:00") %>% 
  group_by(sample_number) %>% 
  mutate(max = max(co2_ppm)) %>% 
  filter(co2_ppm == max) %>% 
  dplyr::select(-max) %>% 
  ungroup() %>% 
  mutate(time_elapsed = difftime(datetime, min(datetime), units = "mins"),
         time_elapsed_mins = as.numeric(time_elapsed))


licor_processed_ppm_subset %>% 
  filter(co2_ppm < 2000) %>% 
  filter(sample == "sample3") %>% 
  ggplot(aes(x = time_elapsed_mins, y = co2_ppm)) +
  geom_point()+
  geom_line()+
  facet_wrap(~sample)+
  xlim(0, 605)




future_code = function(){
  
  
  
  fit_slope = function(licor_processed_ppm){
    
    # Whenever the valve position changes, that's a new sample starting
    newsample <- licor_processed_ppm$port != c(NA, head(licor_processed_ppm$port, -1))
    newsample[is.na(newsample)] <- FALSE
    licor_processed_ppm$sample_number = cumsum(newsample)
    
    rate = 
      licor_processed_ppm %>% 
      mutate(datetime = as_datetime(paste(date, time))) %>% 
      group_by(sample_number) %>% 
      dplyr::mutate(elapsed_sec = as.double(difftime(datetime, min(datetime), units = "secs"))) %>% 
      group_by(core_name, timepoint, sample_number) %>% 
      dplyr::summarize(datetime = min(datetime),
                       co2_ppm_s = lm(co2_ppm ~ elapsed_sec)$coefficients["elapsed_sec"],
                       ch4_ppb_s = lm(ch4_ppb ~ elapsed_sec)$coefficients["elapsed_sec"],
                       co2_max_ppm = max(co2_ppm),
                       sample_elapsed = max(elapsed_sec))  %>% 
      filter(sample_elapsed < 200)  %>% 
      group_by(core_name, timepoint) %>% 
      dplyr::mutate(elapsed_minutes = as.double(difftime(datetime, min(datetime), units = "mins")))
    
    
    ##  rate %>% 
    ##    filter(!core_name %in% c("FOR_20", "FOR_21", "ambient")) %>% 
    ##    ggplot(aes(x = datetime, y = co2_ppm_s, color = core_name))+
    ##    geom_point()+
    ##    facet_wrap(~water_treatment, ncol = 1)+
    ##    #  ylim(440, 600)+
    ##    NULL 
    ##  
    ##  
    ##  rate %>% 
    ##    filter(!core_name %in% c("FOR_20", "FOR_21", "ambient")) %>% 
    ##    ggplot(aes(x = datetime, y = ch4_ppb_s, color = core_name))+
    ##    geom_point()+
    ##    facet_wrap(~water_treatment, ncol = 1)+
    ##    #  ylim(440, 600)+
    ##    NULL 
    ##  
    ##  ## 8 hours of GHG
    
    rate
    
  }
  
  
  compute_rates <- function(licor_processed_ppm, volume_cm3 = 81.07, tair_C = 21, pressure_kPa = 101.325) {
    
    ## We want to compute rate of change (CO2 ppm/s and CH4 ppb/s),
    ## and then convert this to µmol/s using the ideal gas law:
    ## A = dC/dt * V * Pa/RT (cf. Steduto et al. 2002), where
    ## 	A is the flux (µmol/g/s)
    ##	  dC/dt is raw respiration as above (mole fraction/s)
    ## 	V is total chamber volume (cm3)
    ##	  Pa is atmospheric pressure (kPa)
    ##	  R is universal gas constant (8.3 x 10^3 cm3 kPa mol-1 K-1)
    ##	  T is air temperature (K)
    
    R <- 8.3145e+3			# cm3 kPa K−1 mol−1
    
    rate = 
      licor_processed_ppm %>% 
      filter(core_name != "Ambient") %>% 
      mutate(datetime = as_datetime(paste(date, time))) %>% 
      group_by(sample_number) %>% 
      dplyr::mutate(elapsed_sec = as.double(difftime(datetime, min(datetime), units = "secs"))) %>% 
      group_by(core_name, sample_number, timepoint) %>% 
      dplyr::summarize(datetime = min(datetime),
                       co2_ppm_s = lm(co2_ppm ~ elapsed_sec)$coefficients["elapsed_sec"],
                       ch4_ppb_s = lm(ch4_ppb ~ elapsed_sec)$coefficients["elapsed_sec"],
                       co2_max_ppm = max(co2_ppm),
                       sample_elapsed = max(elapsed_sec))  %>% 
      filter(sample_elapsed < 200)  %>% 
      group_by(core_name, timepoint) %>% 
      dplyr::mutate(elapsed_hr = as.double(difftime(datetime, min(datetime), units = "hours")),
                    # Respiration, µmol/time unit via ideal gas law
                    co2_umol_s = co2_ppm_s * volume_cm3 * pressure_kPa / (R * (tair_C + 273.15)),
                    co2_umol_hr = co2_umol_s * 3600
      )
    
    # Compute ppm/time unit change in gas concentration
    #m <- lm(gas_ppm ~ time)
    #gas_ppm_time <- unname(coefficients(m)["time"])
  }
  cumulative_evolution = function(licor_processed_rates){
    
    fn = function(licor_processed_rates){
      co2_interp = 
        approx(licor_processed_rates$elapsed_hr, 
               licor_processed_rates$co2_umol_hr, 
               ties = mean, rule = 2) %>% 
        as.tibble() %>% 
        mutate(delta_hr = x - lag(x),
               co2_umol = y * delta_hr) %>% 
        summarize(co2_umol_cum = sum(co2_umol, na.rm = T)) %>% 
        force()
    }
    
    # core_weights = weights_processed %>% dplyr::select(core_name, soil_od_g)
    
    cumulative = 
      licor_processed_rates %>% 
      filter(elapsed_hr <= 20) %>% 
      filter(elapsed_hr > 0) %>% 
      group_by(core_name, timepoint) %>% 
      do(fn(.)) %>% 
      #    left_join(core_weights) %>% 
      #    mutate(co2_umol_cum_g = co2_umol_cum/soil_od_g) %>% 
      force()
    
  }
  
  
}

