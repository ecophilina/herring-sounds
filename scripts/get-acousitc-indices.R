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
