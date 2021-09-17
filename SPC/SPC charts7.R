# Function to produce Control Charts
ControlChart = function(num, denom, subgroup, type, title, ylabel, xlabel, scale, start) {
  
  library(ggplot2)
  
  subgroup = factor(subgroup, levels=unique(subgroup))
    
  dataDF = data.frame(num,denom,subgroup)
  dataDF$measure = dataDF$num / dataDF$denom
  head(dataDF)
  
#This function sets scaler for the y axis (e.g 100 changes 0.5 to 50%)
scaler <- function(){
  function(x)x*scale
}

if (length(subgroup) < 20)  {
  ggplot() +
    geom_line(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
    geom_point(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
    #  scale_x_date(breaks = "1 month") +
    #scale_y_continuous(labels=scaler(), limits=c(max(min(dataDF$measure)*0.66,0), uplim)) +
    xlab(xlabel) + ylab(ylabel) +
    ggtitle(title)
  #print("SPC charts should have at least 20 points. No charts produced.")
} else {
  
if (type == "p") {
      sdp = function(pct, denom) {
      s = sqrt((pct*(1-pct))/denom)
      return(s)
      } # End of p Function
      uplim = min(max(dataDF$measure)*1.3, 1) # Set y axis limit for chart
} else if (type == "u") {
      sdp = function(measure, denom) {
        s = sqrt(measure/denom)
      return(s)     
      } #End of u Function  
      uplim = max(dataDF$measure)*1.3
} else if (type == "c") {
      sdp = function(count, denom) {
        s = sqrt(count/denom)
      return(s)     
      } #End of c Function  
      uplim = max(dataDF$measure)*1.3
} else if (type == "I") {
      sdp = function(MR, denom) {
        s = sum(MR)/denom
      return(s)     
      } #End of I Function  
      uplim = max(dataDF$measure)*1.3
} else {
  print ("chart type not recognised")  
} #End of If


# Start used to change the position of the first baseline test

  shiftpos = start
  newshiftpos = start
  chrt = 0
  
  ### Calculate mean and standard deviations for first [1:20]
  dataDF$mean = sum(dataDF$num[start:(start+19)]) / sum(dataDF$denom[start:(start+19)]) # Mean using sum of numerators and denominators
  dataDF$sd = sdp(dataDF$mean, dataDF$denom) 
  dataDF$baselines = as.numeric(NA) # Create baseline variable but don't calculate until checked for 20 points with no shift
  
  if (type == "I") {
    dataDF$MR = abs(c(head(dataDF$num,-1) - tail(dataDF$num,-1),NA))
    dataDF$sd = mean(na.omit(dataDF$MR[start:(start+19)]))
    lim3 = 2.66
    lim2 = 1.77
  } else {
    lim3 = 3
    lim2 = 2
  } 
  
  for (loops in 1:12) {
    
    #Calculate points above or below mean and number each run
    dataDF$abovebelow = 0
    dataDF$abovebelow[dataDF$mean == 0] = -1   # If the mean is zero treat as below
    dataDF$abovebelow[dataDF$measure < dataDF$mean] = -1
    dataDF$abovebelow[dataDF$measure > dataDF$mean] = 1
    keep <- function(x) {
      L <- c(TRUE,x[-1L]!=0) # The first row must be preserved if zero or not
      idx <- c(0, which(L))[cumsum(L) + 1]
      return(x[idx])
    } #function to preserve last non zero number
    dataDF$abovebelowpreserved <- keep(dataDF$abovebelow)
    
    #Number the runs above or below the mean
    runChange = dataDF$abovebelowpreserved[-1L] != dataDF$abovebelowpreserved[-nrow(dataDF)] #List change points in data with TRUE
    runChange[dataDF$mean[-1L] != dataDF$mean[-nrow(dataDF)]] = TRUE #Start new run at rephase points
    dataDF$runNo = c(0,cumsum(runChange)) +1 #Number the runs
    
    # Create data frame with run lengths - excluding points on the center line
    runlengthDF = aggregate(measure ~ runNo, data=dataDF[dataDF$abovebelow != 0,], NROW)
    colnames(runlengthDF) <- c("runNo", "runlength")
    
    # Merge to one data frame - removeing any existing runlength column
    dataDF = merge(dataDF[,!(names(dataDF) %in% c("runlength"))], runlengthDF, all.x =TRUE)
    dataDF$runlength[dataDF$abovebelowpreserved == 0] = 0 # For cases where data starts with a zero
    dataDF$runlength[dataDF$mean == 0 & dataDF$measure == 0] = 0 # For cases where there's a run of zeros on a zero mean
    
    # Create a column with data for runs 8 or longer only
    dataDF$highlight = NA
    dataDF$highlight[dataDF$runlength >= 8] = dataDF$measure[dataDF$runlength >= 8]
    dataDF$highlight[dataDF$measure > dataDF$mean+(lim3*dataDF$sd)] = dataDF$measure[dataDF$measure > dataDF$mean+(lim3*dataDF$sd)]
    dataDF$highlight[dataDF$measure < dataDF$mean-(lim3*dataDF$sd)] = dataDF$measure[dataDF$measure < dataDF$mean-(lim3*dataDF$sd)]
    

    # Calculate the position of the next minimum 8 point shift (returns zero if no more shifts)
    newshiftpos = min(which.max(dataDF$runlength >= 8 & dataDF$runNo > dataDF$runNo[shiftpos]),nrow(dataDF))
    newsusshiftpos = min(which.max(dataDF$runlength >= 8 & dataDF$runNo > dataDF$runNo[shiftpos]),nrow(dataDF))
    
    ### If 20 points from shift HAVE NO shifts within them, then set as new baseline. Also if data starts with a shift.
    # if ( ((shiftpos+19) < newshiftpos) | (loops == 1 & newshiftpos==1) ) {
    #   dataDF$baselines[shiftpos:min(nrow(dataDF),(shiftpos+19))] = dataDF$mean[shiftpos:min(nrow(dataDF),(shiftpos+19))]
    #   #dataDF$baselines[shiftpos:(shiftpos+19)] = dataDF$mean[shiftpos:(shiftpos+19)]
    #   }
    ### If 20 points from shift HAVE NO shifts within them, then set as new baseline.
    #if (((shiftpos+11) < newshiftpos)) {dataDF$baselines[shiftpos:(shiftpos+11)] = dataDF$mean[shiftpos:(shiftpos+11)]}
    
    ### Mark as new baseline if:
    # (a) there are 20 data points (not counting blanks or NAs); and
    # (b) the 20 points are stable (i.e. no shifts starting within the 20 points)
    # (c) allow if 20 points start with a shift
    if ((shiftpos+19 <= length(num[!is.na(num)])) & ((shiftpos+19 < newshiftpos) | newshiftpos==1))
    {dataDF$baselines[shiftpos:(shiftpos+19)] = dataDF$mean[shiftpos:(shiftpos+19)]}
    
    # if (count 12 NA in $highlight AND shiftpos+11 <= nrows)
    if ( (sum(is.na(dataDF$highlight[shiftpos:(shiftpos+19)])) >= 20) & 
         (shiftpos+19 <= length(dataDF$highlight))  )
    {dataDF$baselines[shiftpos:(shiftpos+19)] = dataDF$mean[shiftpos:(shiftpos+19)]}
    
    chrt = chrt + 1
    
    # Plot
    print(
      ggplot() +
        geom_line(aes(x=subgroup, y=mean, group = 1), data=dataDF, linetype = "longdash", colour = "darkgray") +
        geom_line(aes(x=subgroup, y=mean+(lim2*sd), group = 1), data=dataDF, linetype = "longdash", colour = "#fad6d6") +
        geom_line(aes(x=subgroup, y=mean+(lim3*sd), group = 1), data=dataDF, linetype = "longdash", colour = "#f7c0c0" ) +   
        geom_line(aes(x=subgroup, y=mean-(lim2*sd), group = 1), data=dataDF, linetype = "longdash", colour = "#fad6d6") +
        geom_line(aes(x=subgroup, y=mean-(lim3*sd), group = 1), data=dataDF, linetype = "longdash", colour = "#f7c0c0") + 
        geom_line(aes(x=subgroup, y=baselines, group = 1), data=dataDF, linetype = "solid", colour = "darkgray") +
        geom_line(aes(x=subgroup, y=baselines+(lim2*sd), group = 1), data=dataDF, linetype = "solid", colour = "#fad6d6") +
        geom_line(aes(x=subgroup, y=baselines+(lim3*sd), group = 1), data=dataDF, linetype = "solid", colour = "#f7c0c0") +   
        geom_line(aes(x=subgroup, y=baselines-(lim2*sd), group = 1), data=dataDF, linetype = "solid", colour = "#fad6d6") +
        geom_line(aes(x=subgroup, y=baselines-(lim3*sd), group = 1), data=dataDF, linetype = "solid", colour = "#f7c0c0") + 
        geom_line(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
        geom_point(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
        geom_point(aes(x=subgroup, y=highlight, group = 1), data=dataDF, colour = "red") +    
        theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
        #  scale_x_date(breaks = "1 month") +
        scale_y_continuous(labels=scaler(), limits=c(max(min(dataDF$measure)*0.66,0), uplim)) +
        xlab(xlabel) + ylab(ylabel) +
        ggtitle(title)
    ) # ggplot chart
    

    if (newsusshiftpos == 1) break
    
    
    ### Calculate temporary mean from the next sustained shift
    #if (dataDF$runlength[newshiftpos] >= 9) {
    dataDF$mean[newsusshiftpos:nrow(dataDF)] = mean(dataDF$measure[newsusshiftpos:min(newsusshiftpos+19,newsusshiftpos+dataDF$runlength[newsusshiftpos]-1, nrow(dataDF))])  # mean
    dataDF$baselines[newsusshiftpos:nrow(dataDF)] = NA
    if (type == "I") {
      dataDF$sd[newsusshiftpos:nrow(dataDF)] = mean(na.omit(dataDF$MR[newsusshiftpos:min(newsusshiftpos+19,newsusshiftpos+dataDF$runlength[newsusshiftpos]-1, nrow(dataDF))]))
      }
    #}

    shiftpos = newsusshiftpos

    
} # End of For loop

} # End of >20 points If 

} # End of ControlChart function

# Create dataframe  with some random data
num = c(rnorm(10, mean=15.1, sd=4.01), rnorm(20, mean=14.0, sd=3.1),rnorm(10, mean=20.5, sd=5.03))
denom = rnorm(length(num), mean=10000, sd=500)
subgroup = c(1:length(num))
type = "u"
xlabel = "x label"
ylabel = "y label"
scale = 1
title = "Titeloi!"
ControlChart(num,denom,subgroup,"u","Random data", "Number", "point", 1,1)

