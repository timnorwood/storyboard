
## Thanks to Chris Beeley for his code for using  both API 1 and 2.
## The code get stories and tagged information by using Care Opinions APIs. 
## I tried moving solely to using API 2 but would have had to set up a loop to get
## tag and service information for each story, which would be very slow. 
## See discussion at https://chrisbeeley.net/?p=904

################ Read data using API 1 ########################
library(RCurl)
library(XML)
library(tidyverse)
library(readxl)

setwd("N:\\add\\your\\working\\directory")

APIkey = "add your own API key"
API2key = "add your own API key"

### set the initial value of skip and make an empty dataframe and empty XML to keep everything
skip = 0
tagFrame = data.frame()
nacFrame = data.frame()
storyFrame = data.frame()
myXML = NULL
opinion = "holder"
  
### for as long as the API returns results, parse XML, convert to dataframe, and stick together dataframes
repeat {
  myXML = try(getURL(
    paste(
      "https://www.careopinion.org.uk/api/v1/opinions?healthservice=",
      "nhs-scotland"
      ,
      "&apikey=",
      APIkey,
      "&skip=",
      skip,
      "&take=100",
      sep = ""
    )
  ))
  
  if (myXML == "")
  {
    cat("Repeat loop break 1. ", skip, " files read")
    
    break
  }
    
    # Clean text to remove odd characters
    #myXML <- sapply(myXML,function(row) iconv(row, "latin1", "ASCII", sub=""))
    myXML =  iconv(myXML, "latin1", "ASCII", sub="")
    
    nodes = xmlParse(myXML)
    
    ### note the use of *[local-name() = 'XXX']
    ### this is because I couldn't get the namespace working
    
    opinion = xpathApply(nodes, "//*[local-name() = 'Opinion']/*[local-name() = 'Body']", xmlValue)
    PostID = xpathApply(nodes, "//*[local-name() = 'Opinion']/*[local-name() = 'PostingId']", xmlValue)
    date = substr(unlist(xpathApply(nodes, "//*[local-name() = 'Opinion']/*[local-name() = 'dtSubmitted']", xmlValue)), 1, 10)
    
    polarity = xpathApply(nodes, "//*[local-name() = 'Tags']//*[local-name() = 'Tag']/*[local-name() = 'Polarity']", xmlValue)
    tagGroup = xpathApply(nodes, "//*[local-name() = 'Tags']//*[local-name() = 'Tag']/*[local-name() = 'TagGroup']", xmlValue)
    tagName = xpathApply(nodes, "//*[local-name() = 'Tags']//*[local-name() = 'Tag']/*[local-name() = 'TagName']", xmlValue)
    sizes = xpathApply(nodes, "//*[local-name() = 'Tags']", xmlSize)
    
    NACorg = xpathSApply(nodes, "//*[local-name() = 'OrganisationNACS']", xmlValue)
    NACtype = xpathSApply(nodes, "//*[local-name() = 'HealthService']//*[local-name() = 'Type']", xmlValue)
    service = xpathSApply(nodes, "//*[local-name() = 'SiteNACS']", xmlValue)
    postcode = xpathSApply(nodes, "//*[local-name() = 'Postcode']", xmlValue)
    NACsname = xpathSApply(nodes, "//*[local-name() = 'Name']", xmlValue)    
    NACsizes = xpathSApply(nodes, "//*[local-name() = 'HealthServices']", xmlSize)
    
    ### this function exists because some stories have more than one NACS/tag code.
    ### So what it does is repeats stories (using the size variable)
    ### to make it the same length as the NACS, meaning stories are
    ### doubled next to each unique NACS. Then you can cbind
    
    ### For TAGS
     listMat = mapply(function(w, x, y, z){
     rep(c(w, x, z), y)
     }, PostID, opinion, sizes, date)

     finalMat = matrix(unlist(listMat), ncol=3, byrow=TRUE)
     colnames(finalMat) = c("PostID","Story", "Date")
     final = data.frame(finalMat,
                        "tagGroup" = unlist(tagGroup),
                        "tagName" = unlist(tagName),
                        "polarity" = unlist(polarity),
                        stringsAsFactors = FALSE
                        )
     tagFrame = rbind(tagFrame, final)
    
    
    ### For NACS
    listMat = mapply(function(w, x, y, z){
      rep(c(w, x, z), y)
    }, PostID, opinion, NACsizes, date)
    
    finalMat = matrix(unlist(listMat), ncol=3, byrow=TRUE)
    colnames(finalMat) = c("PostID","Story", "Date")
    final = data.frame(finalMat,
                       "NACS" = unlist(service),
                       "NACSname" = unlist(NACsname),
                       "postcode" = unlist(postcode),
                       "NACorg" = unlist(NACorg),
                       "NACtype" = unlist(NACtype),
                       stringsAsFactors = FALSE
                       )
    nacFrame = rbind(nacFrame, final)
    
    cat("Got files up to ", skip+length(opinion),"\r")
    if (length(opinion) < 100) 
    { cat("Repeat loop break 2. ", skip+length(opinion)," files read");
      break }
    skip=skip+100  
   
 } #Repeat end
 
save(nacFrame, file = "Output\\nacFrame.rda")
save(tagFrame, file = "Output\\tagFrame.rda")



################ Read data using API 2 ########################
library(httr)    # for GET(...)
library(rjson)   # for fromJSON(...)

# produce empty list to lappend
opinionList = list()

# set skip at 0 and make continue TRUE until no stories are returned 
skip = 7 # Note the first 6 criticalities are NULL so skip these
continue = TRUE

while(continue){
  opinions = GET(paste0("https://www.careopinion.org.uk/api/v2/opinions?take=100&skip=",skip),
                 add_headers(Authorization = API2key))
  if(length(content(opinions)) == 0){ # if there are no stories then stop
    continue = FALSE
  }
  opinionList = c(opinionList, content(opinions)) # add the stories to the list
  # increase skip, and repeat
  cat('\r',"Getting lines",skip,"to",skip+99,"from Care Opinion")
  #Sys.sleep(0.3) # need to add this for the API limit of 5 requests per second
  skip = skip + 100
}
cat('\n')

# I keep getting a message saying "Sorry! Something went wrong." Removes entries with this error.
cat(NROW(opinionList[opinionList == "Sorry! Something went wrong."]),'entries removed because they were empty \n') 
opinionList = opinionList[opinionList != "Sorry! Something went wrong."]

# Got an error later when some list elements contained one element not 13
cat(NROW(opinionList[lengths(opinionList) != 13]),'more entries removed because they were empty \n') 
opinionList = opinionList[lengths(opinionList) == 13]

# Pick out the data we want from opinionList
# if there are no new stories this entire bit is skipped
if(length(opinionList) > 0){
  PostID = lapply(opinionList, "[[", "id")
  title = lapply(opinionList, "[[", "title")
  story = lapply(opinionList, "[[", "body")
  criticality = lapply(opinionList, "[[", "criticality")
  date = lapply(opinionList, "[[", "dateOfSubmission")
  progress = lapply(opinionList, "[[", "progress")
  #authorRole = lapply(opinionList, "[[", "authorRole")
  
  storyData = data.frame("PostID" = unlist(PostID),
                         "Title" = unlist(title),
                         #"PO" = unlist(story),
                         #"Date" = as.Date(substr(unlist(date), 1, 10)),
                         "criticality" = unlist(criticality),
                         "progress" = unlist(progress),
                         #"authorRole" = unlist(authorRole),
                         stringsAsFactors = FALSE
  )
}

save(storyData, file = "Output\\storyData.rda")

################ services data ########################

# produce empty list to lappend
serviceList = list()

# set skip at 0 and make continue TRUE until no stories are returned 
skip = 0
continue = TRUE

while(continue){
  service = GET(paste0("https://www.careopinion.org.uk/api/v2/healthservices?type=hospital&take=100&skip=",skip),
                 add_headers(Authorization = API2key))
  if(length(content(service)) == 0){ # if there are no stories then stop
    continue = FALSE
  }
  serviceList = c(serviceList, content(service)) # add the stories to the list
  # increase skip, and repeat
  cat('\r',"Getting services",skip,"to",skip+99,"from Care Opinion")
  #Sys.sleep(0.3) # need to add this for the API limit of 5 requests per second
  skip = skip + 100
}
cat('\n')

# Pick out the data we want from serviceList
# if there are no new stories this entire bit is skipped
if(length(serviceList) > 0){
  nacs = lapply(serviceList, "[[", "nacs")
  name = lapply(serviceList, "[[", "name")
  type = lapply(serviceList, "[[", "type")
  #organisation = lapply(serviceList, "[[", "organisation")

  serviceData = data.frame("nacs" = unlist(nacs),
                         "name" = unlist(name),
                         "type" = unlist(type),
                         #"organisation" = unlist(organisation),
                         stringsAsFactors = FALSE
  )
}

save(serviceData, file = "Output\\serviceData.rda")


################ tagFrame ########################

#load(file = "Output\\tagFrame.rda")

## Change polarity and tagName from lookup
tagLookup = read_excel("Input\\20200611 tagLookup.xlsx", sheet = "tagLookup") %>%
  mutate(tagName = str_to_sentence(tagName),
         Rename = str_to_sentence(Rename))
orderedtags = read_excel("Input\\20200611 tagLookup.xlsx", sheet = "orderedtags") %>%
  mutate(tagLevel = str_to_sentence(tagLevel)) %>% 
  pull(tagLevel)
 
tagFrameSC = tagFrame %>% 
  mutate(tagName = str_to_sentence(str_trim(as.character(tagName))),
          PostID = as.numeric(as.character(PostID)),
          Date = as.Date(Date, format = "%Y-%m-%d"),
          polarity = as.numeric(as.character(polarity))) %>%  # Convert to number
  left_join(tagLookup) %>% 
  mutate(
          polarity = ifelse(!is.na(myPolarity), myPolarity, polarity),
          tagName = ifelse(!is.na(Rename), Rename, tagName),
          tagName = factor(tagName, levels = unique(c(orderedtags,unique(tagName),"Unclassified"))))  %>% #DOUBKE "UNCLASSIFIED" ADDITION IS CORRECT
  select(PostID,Date,polarity,tagName,tagGroup,tagClass)

## Save tagFrame 
save(tagFrameSC, file = "Output\\tagFrameSC.rda")


################ storyFrame ########################

#load(file = "Output\\nacFrame.rda")
#load(file = "Output\\storyData.rda")

### Add service level groupings from lookup
nacLookup = read.csv(paste("Input\\20200521 nacLookup.csv"), stringsAsFactors = FALSE, header=TRUE)
nacFrame = merge(nacFrame,nacLookup, all.x=TRUE)

### Match on an overall polarity and criticality etc to storyFrame
# Calculate overall polarity
 tagPolSumDF = tagFrameSC %>% 
   #mutate(PostID = as.numeric(as.character(PostID))) %>%   
   group_by(PostID) %>% 
   dplyr::summarise(tagPolSum = sum(polarity)) 

# Add overall polarity and criticality etc
 storyFrameSC = nacFrame %>% 
   mutate(PostID = as.numeric(as.character(PostID)),
          Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
   left_join(tagPolSumDF) %>% 
   left_join(storyData) %>% 
   mutate(criticality = ifelse(is.na(criticality), "not known", criticality),
          NACSgroup = ifelse(is.na(NACSgroup), "Other", NACSgroup),
          Date = as.Date(Date, format = "%Y-%m-%d"))

## Match on criticality etc from API2
#  storyFrame = merge(storyFrame,storyData, all.x = T)
#  storyFrame$criticality[is.na(storyFrame$criticality)] = "not known"
#  storyFrame$NACSgroup[is.na(storyFrame$NACSgroup)] = "Other" # Make NACSgroup NAs 'Other'

### Save StoryFrame
save(storyFrameSC, file = "Output\\storyFrameSC.rda")

print("Data read and saved!")
 
 
################ Lookup maintainance ################

### Write list of emotion tags without polarity - assign polarity if reasonable and add to tagLookup.xlsx
# write.csv(unique(tagFrame$tagName[tagFrame$tagGroup == "Emotion" & tagFrame$polarity == 0]), file = "input\\EmotionsCHECK.csv")

### Write list of negative tags from critical stories not in orderedtags - rename and assign tagClass and add to tagLookup.xlsx
allFrame = merge(storyFrameSC,tagFrameSC, by.x="PostID", by.y="PostID")
allFrame %>% 
   filter(polarity == -1,
          criticality %in% c("minimally critical", "mildly critical",
                             "moderately critical", "strongly critical", "severely critical"),
          is.na(tagClass),
          !tagGroup %in% c("Cause of disease", "Emotion", "Condition","Part of body"),
          Date.y > as.Date("2019-06-01")) %>% 
   group_by(tagName) %>%
   dplyr::summarise(count = n()) %>%
   write.csv((file = "Input\\tagCHECK.csv"))


### Write list of NACS services without a group - assign NACSgroup and add to nacLookup.csv
# check specialty at https://www.medschools.ac.uk/studying-medicine/after-medical-school/specialties
nacFrame %>% 
  filter(NACtype == "service", is.na(NACSgroup)) %>% 
  distinct(NACSname) %>% 
  write.csv(., file = "input\\nacsCHECK.csv")
