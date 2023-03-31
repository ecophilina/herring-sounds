# combine long-duration acoustic indices into one dataframe for plotting in R -----
library(tidyverse)
library(purrr)
library(lubridate)

# set these for a specific machine and dataset

# raw wav files
# where did we put the towsey outputs?
towsey_directory <- "wdata/"
# where to put the compiled dataframe?
output_parent_directory <- "data/"

# # wav files subjected to Raven's broadband denoise adaptive filter
# # where did we put the towsey outputs?
# towsey_directory <- "wdata/bbdenoise/"
# # where to put the compiled dataframe?
# output_parent_directory <- "data/bbdenoise/"
# 
# 
# # wav files subjected to Raven's narrowband denoise adaptive filter
# # where did we put the towsey outputs?
# towsey_directory <- "wdata/nbdenoise/"
# # where to put the compiled dataframe?
# output_parent_directory <- "data/nbdenoise/"


dir.create(file.path(output_parent_directory))

# requires get-acoustic-indices.R to have been run for all sites in this list
list_sites <- tribble(
  ~site_description, ~site_file_name,
  "Neck Point (2021)", "neckpt",
  "Denman (2020)", "denman",
  "Collishaw (2020)", "collishaw"
)
list_sites$towsey_directory <- towsey_directory

# this function first moves summary csv to separate folder and then merges them into single dataframe

combine_towsey_summary_tabs <- function(file_directory, summary_tables) {
  # browser()
  # create file for summary tables
  dir.create(file.path(summary_tables))
  
  # move the summary tables to a separate folder
  files_to_move <- list.files(file_directory,
                              pattern = "Towsey.Acoustic.Indices.csv",
                              full.names = TRUE, recursive = T, include.dirs = T
  )
  
  filesstrings::file.move(files_to_move, summary_tables)
  
  # list remaining csv files, all should contain various indices with 3 letter code in file name
  files <- list.files(summary_tables,
                      pattern = ".csv",
                      full.names = TRUE, recursive = T, include.dirs = T
  )
  all_data <- map_df(files, ~ read.csv(.x) %>% mutate(file = basename(.x)))
  # browser()
  d <- all_data %>% separate(file,
                             # starting from right and using negatives to allow stid lengths to differ
                             into = c("trap_id", "yr", "mnth", "day", "hr", "min", "sec", "ext"),
                             # uses same thresholds as for index specific csv shifted by 4
                             sep = c(-37, -35, -33, -31, -29, -27, -25) - 4
  ) %>%
    # use to check if working
    # select(Index, trap_id, yr, mnth, d, hr, min, sec, program, index_type, ext)
    mutate(
      # sec = "00", 
      trap_id = as.numeric(trap_id),
      # this is the date-time when the file started recording
      file_dt = ymd_hms(paste(yr, mnth, day, hr, min, "00")),
      minintofile = ResultMinute,
      secintofile = ResultStartSeconds,
      # calculate datetime for each interval
      datetime = file_dt + (minintofile * 60),
      # convert interval to seconds and double to fill in gaps for plotting
      # works if half of each hr were actually sampled
      plot_time = file_dt + (minintofile * 60 * 2),
      # datetime of each interval
      yr = year(datetime),
      mnth = month(datetime),
      day = day(datetime),
      hr = hour(datetime),
      min = minute(datetime),
      sec = second(datetime)
    )
  d
}

# this runs above to combine summaries for all sites and adds in annotations ----
# currently not adjusting for daylight savings time
combine_with_annotations <- function(site_description, site_file_name, towsey_directory) {
  # browser()
  file_directory <- paste0(towsey_directory, site_file_name)
  summary_tables <- paste0(towsey_directory, site_file_name, "_sumtab")
  
  s <- combine_towsey_summary_tabs(
    file_directory,
    summary_tables
  )
  s$site <- site_description
  # browser()
  if(site_file_name == "denman") {
    d <- read.csv("raw-annotations/Denman_1min_200306.csv", stringsAsFactors = F) %>%
      mutate(site = "Denman (2020)")
    d2<-read.csv("raw-annotations/Denman_15min_200308_PE2.csv", stringsAsFactors = F) %>% 
      mutate(site = "Denman (2020)") 
  }
  
  if(site_file_name == "collishaw") {
    d <-read.csv("raw-annotations/Collishaw_1min_200306.csv", stringsAsFactors = F) %>%
      mutate(site = "Collishaw (2020)") %>% rename(herring.hs = herring.j)
    d2<-read.csv("raw-annotations/Collishaw_15min_200308_PE2.csv", stringsAsFactors = F) %>% 
      mutate(site = "Collishaw (2020)") 
  }

  if(site_file_name == "neckpt") {
    # found errors in file names on March 10, 23:30, 11th 00:00, and 14th 00:00 and 03:00
    d <- read.csv("raw-annotations/NeckPt_1min_210311_PE.csv", stringsAsFactors = F) %>%
      mutate(
        site = "Neck Point (2021)",
        # only non-zero records are 1? so replaced by NAs
        herring.frt = as.integer(herring.frt)
      ) %>%
      rename(herring.hs = herring.j)
    d2<-read.csv("raw-annotations/NeckPt_15min_210313_PE2.csv", stringsAsFactors = F) %>% 
      mutate(site = "Neck Point (2021)") 
  }

  d <- d %>%
    separate(filename,
             sep = c("\\."),
             into = c("soundtrap", "datetime", "filetype"),
             remove = F, extra = "merge"
    ) %>%
    separate(datetime,
             into = c("year", "month", "day", "hr", "min", "sec"),
             sep = c(-10, -8, -6, -4, -2),
             remove = FALSE
    ) %>%
    mutate(
      secintofile = samp.start.min * 60, # needed to correct error in csv data
      minintofile = secintofile / 60,
      # this is the date-time the file started recording
      file_dt = ymd_hms(paste(year, month, day, hr, min, "00")),
      # convert interval to seconds and double to fill in gaps
      datetime = file_dt + (minintofile * 60),
      bin15min = ifelse(min < 30, 0, 30)
    ) %>%
    select(-datetime, -filetype, -year, -month, -day, -hr, -min, -sec)

 # browser()
 
  d2 <- d2 %>%
    separate(filename, sep=c("\\."), 
             into=c("soundtrap","datetime","filetype"),
             remove = F, extra = "merge") %>%
    separate(datetime,
             into = c("year","month","day","hr","min","sec"),
             sep=c(-10,-8,-6,-4,-2),
             remove=FALSE)%>%
    mutate(samp.tot.sec = 900,
           # this is the date-time the file started recording
           file_dt = ymd_hms(paste(year, month, day, hr, min, "00")),
           # d=as.numeric(day),
           # hr=as.numeric(hr),
           min=as.numeric(min),
           # sec=as.numeric(sec),
           bin15min = ifelse(min < 30, 0, 30)
           ) %>% select(-datetime, -year, -month, -day, -hr, -min, -sec, -filetype) 
  # %>%
  #   mutate(
  #     # correct daylight savings time to standard time
  #     hr=case_when(
  #       datetime > "200308013002" & datetime < "210000000000" ~ hr - 1,
  #       datetime > "210314013110" ~ hr - 1,
  #       TRUE ~ hr
  #     ),
  #     d=case_when(
  #       hr < 0 ~ d - 1,
  #       TRUE ~ d
  #     ),
  #     hr=case_when(
  #       hr < 0 ~ 23,
  #       TRUE ~ hr
  #     ),
  #     time = round(hr + (min/60), 1) #+ (sec/60/60)
  #   ) #%>% group_by(site) %>% mutate(
  #   #   daysintosample = (d-min(d))+ time/24 
  #   # ) %>% ungroup()

  dat1 <- inner_join(s, d)
  dat2 <- inner_join(s, d2)
  dat <- bind_rows(dat1,dat2)
}

data <- purrr::pmap_dfr(list_sites, combine_with_annotations)
saveRDS(data, paste0(output_parent_directory, "towsey-summary-scores4.rds"))



# put all indices together in one file for plotting false colour spectrograms----
# combines remaining csv into one dataframe for each site
combine_towsey_data <- function(file_directory) {
  message(
    "Make sure you have first run 'combine_towsey_summary_tabs()' on this directory",
    " otherwise dataframe will contain flaws."
  )
  # list remaining csv files, all should contain various indices with 3 letter code in file name
  files <- list.files(file_directory,
                      pattern = ".csv",
                      full.names = TRUE, recursive = T, include.dirs = T
  )
  # files
  
  all_data <- map_df(files, ~ read.csv(.x) %>% mutate(file = basename(.x)))
  
  d <- all_data %>%
    separate(file,
             # starting from right and using negatives to allow stid lengths to differ
             into = c("trap_id", "yr", "mnth", "day", "hr", "min", "sec", "program", "index_type", "ext"),
             sep = c(-37, -35, -33, -31, -29, -27, -25, -7, -4)
    ) %>%
    # use to check if working
    # select(Index, trap_id, yr, mnth, d, hr, min, sec, program, index_type, ext)
    mutate(
      trap_id = as.numeric(trap_id),
      # this is the date-time when the file started recording
      file_dt = ymd_hms(paste(yr, mnth, day, hr, min, "00")),
      minintofile = Index,
      # calculate datetime for each interval
      datetime = file_dt + (minintofile * 60),
      # convert interval to seconds and double to fill in gaps for plotting
      # works if half of each hr were actually sampled
      plot_time = file_dt + (minintofile * 60 * 2),
      # datetime of each interval
      yr = year(datetime),
      mnth = month(datetime),
      day = day(datetime),
      hr = hour(datetime),
      min = minute(datetime),
      sec = second(datetime)
    ) %>%
    select(-Index, -ext)
  
  d_long <- d %>% pivot_longer(1:(ncol(d) - 13), names_to = "freq_bin", values_to = "score")
  d_long$freq_bin_num <- as.numeric(str_replace(d_long$freq_bin, "c", ""))
  d_long
}


# list_sites <- tribble(
#   ~site_description, ~site_file_name,
# #   "Neck Point (2021)", "neckpt"
# #   # "Denman (2020)", "denman",
#   "Collishaw (2020)", "collishaw"
# )
# list_sites$towsey_directory <- towsey_directory


combine_towsey_data_for_all <- function(site_description, site_file_name, towsey_directory) {
  file_directory <- paste0(towsey_directory, site_file_name)
  
  ld <- combine_towsey_data(file_directory)
  ld$site <- site_description
  
  saveRDS(ld, paste0(output_parent_directory, "towsey-indices-", site_file_name, ".rds"))
}

purrr::pmap_dfr(list_sites, combine_towsey_data_for_all)
# because this saves each internally # A tibble: 0 × 0 is expected
