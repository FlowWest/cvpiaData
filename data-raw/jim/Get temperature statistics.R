
setwd("C:/Users/Jim/Box Sync/CVPIA/Phase II/Fall Chinook Model v2016_17/Process inputs/Process_CC_temps")

# get the first few rows with the info
heads<-read.csv("C:/Users/Jim/Downloads/Full5QOutputs.csv", stringsAsFactors =F, nrows=8)

# select the header and add Date_time column heading
clmn_names<-c("Date_time",names(heads)[-1])

### I wanted to just grab some relevent columns for my own edification 
### this file links the column names to the ones I want
Leave<-read.csv("Check out columns to keep.csv")
keep<-subset(Leave, Jim_Keep != "")

# now read in full data
biggie<-read.csv("C:/Users/Jim/Downloads/Full5QOutputs.csv", header = FALSE, stringsAsFactors =F, skip=12)
# name the columns
names(biggie)<- clmn_names
# select those I want
biggie<-biggie[,as.character(keep[,1])]
# rename them as something I understand
names(biggie) <-as.character(keep[,3])

# extract dates and hrs
test<-matrix(unlist(strsplit(biggie[,1], " ")), ncol=2, byrow=T)
old<-unique(test[,2]); new<-c(6,12,18,24)
library(gsubfn)
test[,2]<-gsubfn("\\S+", setNames(as.list(new), old), test[,2])
biggie<-cbind(test,biggie)
names(biggie)[1:2]<-c("Date","Hr")

#coerce the date
biggie$Date <- as.Date(biggie$Date, format="%m/%d/%Y")
# create year by stripping the year value in Date and make it numeric
biggie$Year <-as.numeric(format(biggie$Date, format = "%Y"))
# create month by stripping the month value in Date and make it numeric
biggie$Month <-as.numeric(format(biggie$Date, format = "%m"))
# make month numeric
biggie$Hr<- as.numeric(biggie$Hr)
## just clean her up a bit
biggie<- biggie[,c(1,35,36,2,4:34)]

# Degrees F to C all stats need to be in C not F
biggie[,5:35] <- (biggie[,5:35] - 32)*0.5556

# get stream average values when multiple readings, I'm guessing at these locations
biggie$AMERICAN <-rowMeans(biggie[,c("AMERICAN.A", "AMERICAN.B")])
biggie$CLEAR.CREEK <-rowMeans(biggie[,c("CLEAR.CREEK.A", "CLEAR.CREEK.B")])
biggie$UPPER.SAC <-rowMeans(biggie[,c("UPPER.SAC.A", "UPPER.SAC.B")])
biggie$UP.MID.SAC <-rowMeans(biggie[,c("UP.MID.SAC.A", "UP.MID.SAC.B")])
biggie$TUOLUMNE <-rowMeans(biggie[,c("TUOLUMNE.C", "TUOLUMNE.A", "TUOLUMNE.B")])
biggie$SAN.JOAQUIN <-rowMeans(biggie[,c("SAN.JOAQUIN.B", "SAN.JOAQUIN.A", "SAN.JOAQUIN.C")])
biggie$MERCED <-rowMeans(biggie[,c("MERCED.B", "MERCED.A")])
# reorder columns
biggie<-biggie[,c(1:4,9:14,18:19,21,23,25,36:41)]
## END OF DATAFRAME MANIPULATION, WE DOT THE CALCULATIONS OF STATS BELOW

# Get daily averages and maxes
daily.meanz<-aggregate(biggie[c(5:21)], by= biggie[c(1,2,3)], mean)
daily.max<-aggregate(biggie[c(5:21)], by= biggie[c(1,2,3)], max)

# NOW CALCULATE STATISTICS FOR MODEL
## Temperature stat # 1: proportion daily means temperatures > 22 C Oct- Nov
## you have so much data here that I'm not going to worry about using probability
## distributions to calculate statistics just get frequencies and averages and SD
hot<-daily.meanz
hot[,4:20]<-ifelse(hot[,4:20] > 22,1,0)
hot<-hot[hot$Month > 9 & hot$Month < 12,]
PR.GE23<-aggregate(hot[c(4:20)], by = hot[c(2)], mean)
# get means and among year SD
PR.GE23.STAT<- cbind(colMeans(PR.GE23[,-1]),apply(PR.GE23[,-1],2, sd))
colnames(PR.GE23.STAT) = c("Mean","SD")

## Temperature stat # 2: mean and sd degree days for adult fish heading to spawning grounds, Sept-Nov
## I just need it for each trib and segment (e.g., Upper-mid Sacramento, Lower-mid Sac, etc.)
## by month I will take care of routing fish and assigning units
## sum average daily thermal units in C, these are degree days with 0 C as baseline
DD.mo.yr<-aggregate(daily.meanz[c(4:20)], by = daily.meanz[c(2,3)], sum, na.rm=T)
DD.mo.yr<-DD.mo.yr[DD.mo.yr$Month>8 & DD.mo.yr$Month<12,]

# Calculate monthly means and SD
DD.mo.yr.mn<-aggregate(DD.mo.yr[c(3:19)], by = DD.mo.yr[c(2)], mean, na.rm=T)
DD.mo.yr.mn$Month<-paste(month.abb[DD.mo.yr.mn$Month],"mean", sep = "_")
DD.mo.yr.sd<-aggregate(DD.mo.yr[c(3:19)], by = DD.mo.yr[c(2)], sd, na.rm=T)
DD.mo.yr.sd$Month<-paste(month.abb[DD.mo.yr.sd$Month],"SD", sep = "_")
# Put in a single file
DD.mo.yr.STAT<-cbind(t(DD.mo.yr.mn[,-1]),t(DD.mo.yr.sd[,-1]))
colnames(DD.mo.yr.STAT)= c(DD.mo.yr.mn$Month,DD.mo.yr.sd$Month)

# Temperature stat # 3: Prob that 10-day maximum temperature > 25oC during rearing, Jan-August
# calculate 10 day running average maximum
require(RcppRoll)
Roll<-daily.max[5:31010,1:3]
for(jj in 4:20) Roll<-cbind(Roll,roll_mean(x= daily.max[,jj], n = 10, align = c("center"), na.rm = TRUE))
names(Roll)<-names(daily.max)

# 10 day max > 25?
Roll[,4:20]<-ifelse(Roll[,4:20] > 25,1,0)

# Find if there was there a 10 day period in the month > 25
juve.max25<- aggregate(Roll[4:20], by = Roll[c(2:3)], max, na.rm=T)
juve.max25<-juve.max25[juve.max25$Month<9,]

# Calculate monthly means and SD
juve.max25.mn<-aggregate(juve.max25[c(3:19)], by = juve.max25[c(2)], mean, na.rm=T)
juve.max25.mn$Month<-paste(month.abb[juve.max25.mn$Month],"mean", sep = "_")
juve.max25.sd<-aggregate(juve.max25[c(3:19)], by = juve.max25[c(2)], sd, na.rm=T)
juve.max25.sd$Month<-paste(month.abb[juve.max25.sd$Month],"SD", sep = "_")
# Put in a single file
juve.max25.STAT<-cbind(t(juve.max25.mn[,-1]),t(juve.max25.sd[,-1]))
colnames(juve.max25.STAT)= c(juve.max25.mn$Month,juve.max25.sd$Month)

## Temperature stat # 4: Prop average daily temperature > 20oC during rearing, Jan-August
hot<-daily.meanz
hot[,4:20]<-ifelse(hot[,4:20] > 20,1,0)
hot<-hot[hot$Month < 8,]
PR.GT20<-aggregate(hot[c(4:20)], by = hot[c(2,3)], mean, na.rm=T)
# get means and among year SD
PR.GT20.mn<-aggregate(PR.GT20[c(3:19)], by = PR.GT20[c(2)], mean, na.rm=T)
PR.GT20.mn$Month<-paste(month.abb[PR.GT20.mn$Month],"mean", sep = "_")
PR.GT20.sd<-aggregate(PR.GT20[c(3:19)], by = PR.GT20[c(2)], sd, na.rm=T)
PR.GT20.sd$Month<-paste(month.abb[PR.GT20.sd$Month],"SD", sep = "_")

# put in a single file
PR.GT20.STAT<-cbind(t(PR.GT20.mn[,-1]),t(PR.GT20.sd[,-1]))
colnames(PR.GT20.STAT)= c(PR.GT20.mn$Month,PR.GT20.sd$Month)

# Temperature stat # 5: Average median monthly temperature at Bend Bridge, North Delta (Rio Vista gage), 
# and Cent S Delta (SAN JOAQUIN R A PRISONERS PT gage) in Jan- Aug 
# here I'll pretend that LOW.MID.SAC is Rio Vista to demonstrate

median.mnth<-aggregate(daily.meanz[c(12)], by = daily.meanz[c(2,3)], median)

med.mnth.STAT<-cbind(c(1:12),tapply(median.mnth$LOW.MID.SAC,median.mnth$Month, mean, na.rm = T),
                     tapply(median.mnth$LOW.MID.SAC,median.mnth$Month, sd, na.rm = T))

colnames(med.mnth.STAT)<-c("Month", "Mean","SD")

Juv.OUTM.S