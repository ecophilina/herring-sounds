# get long-duration acoustic indices using Towsey's program

## for sarcher
# program_location <- 'C:\\Users\\sarcher\\AP\\AnalysisPrograms.exe'
# 
## Set the directory containing the files
# source_directory <- "data\\2018"
## The directory to store the results
# base_output_directory <- "wdata"

# Set the directory containing the files

# for denman
source_directory <- "D:\\Herring2020\\Denman\\psdfiles\\denoise_broadband"
base_output_directory <- "wdata\\bbdenoise\\denman"

# for denman narrowband denoise 
source_directory <- "D:\\Herring2020\\Denman\\psdfiles\\denoise_narrowband"
base_output_directory <- "wdata\\nbdenoise\\denman"

# for collishaw
source_directory <- "D:\\Herring2020\\Collishaw\\psdfiles\\denoise_broadband_psdfiles"
base_output_directory <- "wdata\\bbdenoise\\collishaw"

# for collishaw narrowband denoise 
source_directory <- "D:\\Herring2020\\Collishaw\\psdfiles\\denoise_narrowband"
base_output_directory <- "wdata\\nbdenoise\\collishaw"

# for neck pt
source_directory <- "D:\\Herring2021\\NeckPt1\\psdfiles\\denoise_broadband"
base_output_directory <- "wdata\\bbdenoise\\neckpt"

# for neck pt narrowband denoise 
source_directory <- "D:\\Herring2021\\NeckPt1\\psdfiles\\denoise_narrowband"
base_output_directory <- "wdata\\nbdenoise\\neckpt"

# Get a list of audio files inside the directory
files <- list.files(source_directory, pattern = "*.wav", full.names = TRUE)

# for shortraker
program_location <- 'C:\\Users\\shortraker\\AP\\AnalysisPrograms.exe'

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
