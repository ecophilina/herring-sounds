# removing the tones from the 2018 herring deployments

remove.tones(infolder = "D:/Herring/Pilot_deployment/67391491/Herring_2018_673914191_psd",
             outfolder = "D:/Herring/Pilot_deployment/67391491/Herring_2018_673914191_psd/tone_removed")


# trying concatenate function
source("scripts/concatenate_psd_files.R")
merge.psd(infolder="D:/Herring/Pilot_deployment/67391491/Herring_2018_673914191_psd/tone_removed",
          file.type="rds",
          soundtrapID=67391491,
          date.min="2018-03-08 10:00:00",
          date.max="2018-03-08 11:00:00",
          outfolder="C:/Users/sarcher/Documents/Manuscripts/herring-sounds/data")


d<-read_rds("C:/Users/sarcher/Documents/Manuscripts/herring-sounds/data/67391491_from_201838100_to_201838110.rds")

d[1:5,1:5]
