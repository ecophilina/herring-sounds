# combine long-duration acoustic indices into one dataframe for plotting in R -----
library(tidyverse)
library(purrr)
library(lubridate)


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
  
  d <- all_data %>% separate(file,
                             # starting from right and using negatives to allow stid lengths to differ
                             into = c("trap_id", "yr", "mnth", "day", "hr", "min", "sec", "ext"),
                             # uses same thresholds as for index specific csv shifted by 4
                             sep = c(-37, -35, -33, -31, -29, -27, -25) - 4
  ) %>%
    # use to check if working
    # select(Index, trap_id, yr, mnth, d, hr, min, sec, program, index_type, ext)
    mutate(
      trap_id = as.numeric(trap_id),
      # this is the date-time when the file started recording
      file_dt = ymd_hms(paste(yr, mnth, day, hr, min, sec)),
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
      file_dt = ymd_hms(paste(yr, mnth, day, hr, min, sec)),
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

# set these for a specific machine
# where did we put the towsey outputs?
towsey_directory <- "wdata/"
# where to put the compiled dataframe?
output_parent_directory <- "data/"

list_sites <- tribble(
  ~site_description, ~site_file_name,
  # "Neck Point (2021)", "neckpt",
  "Denman (2020)", "denman",
  "Collishaw (2020)", "collishaw"
)
list_sites$towsey_directory <- towsey_directory


combine_with_annotations <- function(site_description, site_file_name, towsey_directory) {
  # browser()
  file_directory <- paste0(towsey_directory, site_file_name)
  summary_tables <- paste0(towsey_directory, site_file_name, "_sumtab")
  
  s <- combine_towsey_summary_tabs(
    file_directory,
    summary_tables
  )
  s$site <- site_description
  
  if(site_file_name == "denman") {
    d <- read.csv("raw-annotations/Denman_1min_200306.csv", stringsAsFactors = F) %>%
      mutate(site = "Denman (2020)")
  }
  
  if(site_file_name == "collishaw") {
    d <-read.csv("raw-annotations/Collishaw_1min_200306.csv", stringsAsFactors = F) %>%
      mutate(site = "Collishaw (2020)") %>% rename(herring.hs = herring.j)
  }
  
  if(site_file_name == "neckpt") {
    d <- read.csv("raw-annotations/NeckPt_1min_210311.csv", stringsAsFactors = F) %>%
      mutate(
        site = "Neck Point (2021)",
        # only non-zero records are 1? so replaced by NAs
        herring.frt = as.integer(herring.frt)
      ) %>%
      rename(herring.hs = herring.j)
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
      file_dt = ymd_hms(paste(year, month, day, hr, min, sec)),
      # convert interval to seconds and double to fill in gaps
      datetime = file_dt + (minintofile * 60),
      # year = as.numeric(year),
      # month = as.numeric(month),
      # day = as.numeric(day),
      hour = as.numeric(hr),
      minute = as.numeric(min),
      second = as.numeric(sec),
      decimal_time = hour + (minute / 60) + (minintofile / 60),
      bin15min = ifelse(min < 30, 0, 30)
    ) %>%
    select(-datetime, -filetype, -year, -month, -day, -hr, -min, -sec)
  
  dat <- left_join(s, d) 
  dat
}

data <- purrr::pmap_dfr(list_sites, combine_with_annotations)
saveRDS(data, paste0(output_parent_directory, "towsey-summary-scores.rds"))


# put all indices together in one file for plotting false colour spectrograms
combine_towsey_data_for_all <- function(site_description, site_file_name, towsey_directory) {
  file_directory <- paste0(towsey_directory, site_file_name)
  
  ld <- combine_towsey_data(file_directory)
  ld$site <- site_description
  
  saveRDS(ld, paste0(output_parent_directory, "towsey-indices-", site_file_name, ".rds"))
}

purrr::pmap_dfr(list_sites, combine_towsey_data_for_all)

