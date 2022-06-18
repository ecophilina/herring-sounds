# get long-duration acoustic indices using Towsey's program

## for sarcher
# program_location <- 'C:\\Users\\sarcher\\AP\\AnalysisPrograms.exe'
# 
## Set the directory containing the files
# source_directory <- "data\\2018"
## The directory to store the results
# base_output_directory <- "wdata"

# for shortraker
program_location <- 'C:\\Users\\shortraker\\AP\\AnalysisPrograms.exe'

# Set the directory containing the files

# for denman
source_directory <- "D:\\Herring2020\\Denman\\psdfiles"
base_output_directory <- "wdata\\denman"

# for collishaw
source_directory <- "D:\\Herring2020\\Collishaw\\psdfiles"
base_output_directory <- "wdata\\collishaw"


# Get a list of audio files inside the directory
files <- list.files(source_directory, pattern = "*.wav", full.names = TRUE)

# create output directory
dir.create(file.path(base_output_directory))
# iterate through each file
for(file in files) {
  message("Processing ", file) 
  
  # get just the name of the file
  file_name <- basename(file)
  
  # make a folder for results
  output_directory <- normalizePath(file.path(base_output_directory, file_name))
  dir.create(output_directory, recursive = TRUE)
  
  # prepare command
  command <- sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" ', file, output_directory)
  
  # finally, execute the command
  # system2('C:\\Users\\sarcher\\AP\\AnalysisPrograms.exe', command)
  system2(program_location, command)
}

# combine outputs into one dataframe for plotting in R -----

library(tidyverse)
library(purrr)
library(lubridate)
library(ggsidekick)

combine_towsey_data <- function(file_directory = "wdata/denman", 
                                summary_tables = "wdata/denman_sumtab"
                                ){

dir.create(file.path(summary_tables))
  
# move the summary tables to a separate folder
files_to_move = list.files(file_directory, pattern='Towsey.Acoustic.Indices.csv', 
                           full.names=TRUE, recursive = T, include.dirs = T)

filesstrings::file.move(files_to_move, summary_tables)

# list remaining csv files, all should contain various indices with 3 letter code in file name
files = list.files(file_directory, pattern='.csv', full.names=TRUE, recursive = T, include.dirs = T)
# files

all_data <- map_df(files, ~read.csv(.x) %>% mutate(file = basename(.x)))

d <- all_data %>% separate(file,
    # starting from right and using negatives to allow stid lengths to differ
    into=c("trap_id","yr","mnth","d","hr","min","sec","program","index_type", "ext"),
    sep = c(-37,-35,-33,-31,-29,-27,-25,-7,-4))%>%
  # select(Index, trap_id, yr, mnth, d, hr, min, sec, program, index_type, ext)# pause here to check
  mutate(
    trap_id = as.numeric(trap_id),
    interval = Index,
    file_dt=ymd_hms(paste(yr,mnth,d,hr,min,sec)),# this is the date-time the file started recording
    datetime = file_dt + (interval*60), # convert interval to seconds and double to fill in gaps 
    time = file_dt + (interval*60*2), # convert interval to seconds and double to fill in gaps 
    yr=year(datetime),
    mnth=month(datetime),
    d=day(datetime),
    hr=hour(datetime),
    min=minute(datetime),
    sec=second(datetime)) %>% select(-Index, -ext) #%>% 
  
d_long <- d %>% pivot_longer(1:(ncol(d)-13), names_to = "freq_bin", values_to = "score")
d_long$freq_bin_num <- as.numeric(str_replace(d_long$freq_bin, "c", ""))

d_long
} 

d_long <- combine_towsey_data() 
d_long$site <- "Denman (2020)"
saveRDS(d_long, "data/towsey-indices-denman.rds")

# for later:
# d_long <- combine_towsey_data(file_directory = "wdata/collishaw", 
#                               summary_tables = "wdata/collishaw_sumtab") 
# d_long$site <- "Collishaw (2020)"
# saveRDS(d_long, "data/towsey-indices-collishaw.rds")
# 
# d_long <- combine_towsey_data(file_directory = "wdata/neckpt", 
#                               summary_tables = "wdata/neckpt_sumtab") 
# d_long$site <- "Neck Point (2021)"
# saveRDS(d_long, "data/towsey-indices-neckpt.rds")

unique(d_long$index_type)


# explore results and plot -----

# view one index at a time
d_long %>% filter(index_type == "ACI") %>% 
  ggplot(aes(time, freq_bin_num, colour = score, fill = score)) + 
  # geom_tile() +
  geom_raster() +
  scale_colour_viridis_c(option = "turbo") +
  scale_fill_viridis_c(option = "turbo") +
  coord_cartesian(expand = FALSE) +
  theme_sleek() + theme(panel.background = element_rect(fill = "black"))


d_long %>% ggplot() + geom_density(aes(score)) + 
  facet_wrap(~index_type, scales = "free") +
  theme_sleek()


false_colour_plot <- function(
  indices = c( "ENT", "EVN", "ACI")
){
  
d_set <- d_long %>% filter(index_type %in% indices) %>% 
  pivot_wider(names_from = "index_type", values_from = "score") 
# %>%
#   mutate(ACI_scaled = ACI/max(ACI), ENT_scaled = ENT/max(ENT), EVN_scaled = EVN/max(EVN))
# browser()
# d_set[!complete.cases(d_set),] #Returns zero rows, no pixel is lacking any data

r1 <- range(d_set[[indices[1]]])
r2 <- range(d_set[[indices[2]]])
r3 <- range(d_set[[indices[3]]])
  
# # Values must be btw 0-1
d_set$r <- d_set[[indices[1]]]/max(d_set[[indices[1]]])
d_set$g <- d_set[[indices[2]]]/max(d_set[[indices[2]]])
d_set$b <- d_set[[indices[3]]]/max(d_set[[indices[3]]])

range(d_set$r)
range(d_set$g)
range(d_set$b)

# if all values are negative could jsut use absolute values?
if(min(r1) <= 0 & max(r1) <= 0){
  d_set$r <- 1 - abs(d_set[[indices[1]]])/max(abs(d_set[[indices[1]]]))
}
if(min(r2) <= 0 & max(r2) <= 0){
  d_set$g <- 1 - abs(d_set[[indices[2]]])/max(abs(d_set[[indices[2]]]))
}
if(min(r3) <= 0 & max(r3) <= 0){
  d_set$b <- 1 - abs(d_set[[indices[3]]])/max(abs(d_set[[indices[3]]]))
}

ggplot(data=filter(d_set, interval != 0) , 
       aes(time, freq_bin_num, fill = rgb(r, g, b, maxColorValue = 1))) + 
  geom_raster() +
  scale_colour_identity() +
  scale_fill_identity() +
  coord_cartesian(expand = FALSE) +
  theme_sleek() + theme(panel.background = element_rect(fill = "black"))

}

false_colour_plot()    
# alternative order on default choices
# false_colour_plot(c("EVN", "ENT", "ACI"))       

unique(d_long$index_type)
# "ACI" 
# "BGN": range is all negative so must take absolute value first?
# "CVR" "DIF" "ENT" "EVN" "OSC" "PMN" "RHZ" "RNG" "RPS" "RVT" "SPT" "SUM"
false_colour_plot(c("OSC", "CVR", "BGN"))    
false_colour_plot(c("PMN", "RHZ", "RNG"))    
false_colour_plot(c("RPS", "RVT", "SPT"))  
false_colour_plot(c("SUM",  "ENT", "DIF"))  
false_colour_plot(c("RPS", "ENT", "ACI"))    
