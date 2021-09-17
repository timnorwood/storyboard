
setwd("N:\\set\\your\\working\\directory")

# Get Care Opinion data
source( "Scripts\\20200903 GetStories v2.R")

# Create Flexdashboards
library(rmarkdown)
load("Output\\storyFrameSC.rda")
load("Output\\tagFrameSC.rda")
  
renderHBStoryboard <- function(hdr, label, POname, shpName, OutName) {
  rmarkdown::render("20200616_storyboard_v0.9.1.Rmd", params = list(
    hdr = hdr,
    label = label,
    POname = POname,
    shpName = shpName,
    DataFrom = as.Date("2017-01-01"),  
    CloudsFrom = as.Date("2020-09-01")), # update date
    output_file = OutName)
}

# Create storyboard for a NHS board
storyIDs = storyFrameSC$PostID[storyFrameSC$NACorg == "SS9"]
storyFrame = storyFrameSC[storyFrameSC$PostID %in% storyIDs,]
tagFrame = tagFrameSC[tagFrameSC$PostID %in% storyIDs,]
renderHBStoryboard(hdr = "Storyboard NHS Lothian", label = "LO", POname = "SS9", shpName = "Lothian", OutName = 'storyboard_LO.html')
renderHBStoryboard(label = "Ayr", POname = "SA9", shpName = "Ayrshire and Arran", OutName = 'storyboard_UHA.html')

# Create storyboard for a hospital
storyIDs = storyFrameSC$PostID[storyFrameSC$NACS %in% "A111H"]
storyFrame = storyFrameSC[storyFrameSC$PostID %in% storyIDs,]
tagFrame = tagFrameSC[tagFrameSC$PostID %in% storyIDs,]
renderHBStoryboard(hdr = "Storyboard Crosshouse", label = "Crosshouse", POname = "SA9", shpName = "Ayrshire and Arran", OutName = 'storyboard_UHC.html')

