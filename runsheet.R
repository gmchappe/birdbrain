#sourcing all programs
source("C:\\Users\\gmcha\\OneDrive\\Documents\\disc golf programs\\birdbrain26\\bbsetup.R")
source("C:\\Users\\gmcha\\OneDrive\\Documents\\disc golf programs\\birdbrain26\\bbinput.R")
source("C:\\Users\\gmcha\\OneDrive\\Documents\\disc golf programs\\birdbrain26\\bbpoints.R")
source("C:\\Users\\gmcha\\OneDrive\\Documents\\disc golf programs\\birdbrain26\\bbsham.R")

# source the correct round file (or both, just be aware) depending on if you're using UDisc or PDGA
source("C:\\Users\\gmcha\\OneDrive\\Documents\\disc golf programs\\birdbrain26\\bbround_udisc.R")
source("C:\\Users\\gmcha\\OneDrive\\Documents\\disc golf programs\\birdbrain26\\bbround_pdga.R")

# FILL IN THE FOLLOWING INPUTS:

# create output folder and set it as working directory. This will be where the round recaps go.
setwd("C:\\Users\\gmcha\\OneDrive\\Documents\\disc golf programs\\birdbrain26\\output")

# input <- folder where results/exports are saved (just makes the input for in a little less clunky)
input <- "C:\\Users\\gmcha\\OneDrive\\Documents\\disc golf programs\\birdbrain26\\input"

# output <- GoogleSheet where the league outputs will go
out <- 'https://docs.google.com/spreadsheets/d/1_NvpAOZSjCd-hvwM_3MCx6DKh8aHnvdPXY-3zuKeaV0/edit?gid=0'

# league setup
# only to be done once at the start of the season!
# see README for options if you've run this in a previous season.
bbsetup(link = out,
        first = 1)

# Now fill in the schedule on the generated GoogleSheet.

# league rounds (fill in 1-X, and then the links to the individual round spreadsheets as they are created/downloaded to the input folder)
bbround_udisc(1,paste0(input,'round1_blind'),out)
bbround_udisc(2,paste0(input, 'round2_blind'),out)
bbround_udisc(3,paste0(input, 'round3_blind'),out)
bbround_udisc(4,paste0(input, 'round4_blind'),out)
bbround_udisc(5,paste0(input, 'round5_blind'),out)
bbround_udisc(6,paste0(input, 'round6_blind'),out)
bbround_udisc(7,paste0(input, 'round7_blind'),out)
bbround_udisc(8,paste0(input, 'round8_blind'),out)
bbround_udisc(9,paste0(input, 'round9_blind'),out)
bbround_udisc(10,paste0(input,'round10_blind'),out)
bbround_udisc(11,paste0(input,'round11_blind'),out)
bbround_udisc(12,paste0(input,'round12_blind'),out)
bbround_udisc(13,paste0(input,'round13_blind'),out)
bbround_udisc(14,paste0(input,'round14_blind'),out)
bbround_udisc(15,paste0(input,'round15_blind'),out)
bbround_udisc(16,paste0(input,'round16_blind'),out)
bbround_udisc(17,paste0(input,'round17_blind'),out)
bbround_udisc(18,paste0(input,'round18_blind'),out)
bbround_udisc(19,paste0(input,'round19_blind'),out)
bbround_udisc(20,paste0(input,'round20_blind'),out)

# at any point between rounds, you can run bbpools() to re-adjust the pool assignments and slope/rating of your league.
# this will automatically occur for the first time at round 11.
# i'd recommend every 5-15 rounds, depending on the number of unique league members you have.
#bbpools(link = out)