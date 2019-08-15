setwd("C:/Users/duartead/Box Sync/CVPIA/Mean fall Chinook model v 2017_18/ModelCalibrationNew")

require(plyr)
require(rJava)
require(openxlsx)


# install.packages('devtools')
# devtools::install_github("FlowWest/cvpiaFlow")
# devtools::install_github("FlowWest/cvpiaTemperature")
# devtools::install_github("FlowWest/cvpiaHabitat")
# 
# devtools::install_github("FlowWest/cvpiaData")
# 
# devtools::install_github('FlowWest/cvpiaCalibration')

inps <- load_baseline_data("fall")$inps

library("cvpiaTemperature")
library("cvpiaHabitat")
library("cvpiaFlow")
library("cvpiaData")

grandTab<-read.csv("data-raw/grandtab/grandtab_SacPAS_InRiverChinook_12_26_18.csv")
names(grandTab)<-tolower(names(grandTab))
head(grandTab,n=10)
summary(grandTab)

# Fall and spring run from 1998 to 2017
grandTab<-grandTab[which(grandTab$year<2018 & grandTab$year>1997),]
grandTab<-grandTab[which(grandTab$run=="Fall" | grandTab$run=="Spring" ),]

###############################################################################
#Feather and Yuba Rivers do not seperate spring and fall run
#in totals, here we apply the proportion of spring hatchery return
#adults that are used to adjust the totals for 2010 - 2012
###############################################################################

prop.spring<-data.frame(prp=c(0.076777295, 0.056932196, 0.081441457), Year=c(2010,2011,2012))
prp<-mean(prop.spring[,1])
stuff<-grandTab[which(grandTab$watershed=="Feather River" | grandTab$watershed=="Yuba River"),-5]
stuff<-stuff[-1*which(is.na(stuff$count)==TRUE),] # take out NA values
stuff<-aggregate(x=stuff$count,by=list(year=stuff$year,watershed=stuff$watershed),FUN=sum) # I summed them based because only 2 years tried to separate spring and fall
names(stuff)[3]<-"count"
stuff$count<-ceiling(stuff$count*(1-prp)) # get the proportion of the fall run

# first filter grandtab to only the fall run then remove the current values for feather
# and yuba and append the new computed values. 
grandTab<-grandTab[which(grandTab$run=="Fall"),]
grandTab<-grandTab[which(grandTab$watershed !="Feather River" & grandTab$watershed !="Yuba River"),c(1,4,2)]
grandTab<-rbind(grandTab,stuff)

###############################################################################
#look for and remove duplicates - this has to do with how Grand Tab breaks up a couple of the tribs
###############################################################################
ind<-duplicated(grandTab[,1:2])
grandTab[ind,] #dulplicates, upper sac and battle creek

BattleCreek<-grandTab[which(grandTab$watershed=="Battle Creek"),]
grandTab<-grandTab[-1*which(grandTab$watershed=="Battle Creek"),]
sum(BattleCreek$count,na.rm=TRUE)
BattleCreek<-aggregate(x=BattleCreek$count,by=list(year=BattleCreek$year,watershed=BattleCreek$watershed),FUN=sum,na.rm=TRUE) # I summed them 
names(BattleCreek)[3]<-"count"
sum(BattleCreek$count,na.rm=TRUE)
grandTab<-rbind(grandTab,BattleCreek)

UpperSacramentoRiver<-grandTab[which(grandTab$watershed=="Upper Sacramento River"),]
grandTab<-grandTab[-1*which(grandTab$watershed=="Upper Sacramento River"),]
sum(UpperSacramentoRiver$count,na.rm=TRUE)
UpperSacramentoRiver<-aggregate(x=UpperSacramentoRiver$count,by=list(year=UpperSacramentoRiver$year,watershed=UpperSacramentoRiver$watershed),FUN=sum,na.rm=TRUE) # I summed them 
names(UpperSacramentoRiver)[3]<-"count"
sum(UpperSacramentoRiver$count,na.rm=TRUE)
grandTab<-rbind(grandTab,UpperSacramentoRiver)

ind<-duplicated(grandTab[,1:2])
grandTab[ind,] #we're good

shed.escapee<-grandTab

###############################################################################
#set hatchery fish
###############################################################################
hatchery<-read.csv("data-raw/grandtab/hatchery expansion.csv")
names(hatchery)<-tolower(names(hatchery))
hatchery<-hatchery[-1*which(hatchery$source=="made up"),]
prop.hatch<-aggregate(x=hatchery$hatchery,by=list(watershed=hatchery$trib),FUN=mean) # took mean based on values found here - http://www.casalmon.org/salmon-snapshots/history/battle-creek
prop.hatch<-merge(inps[,1:3],prop.hatch,all=T)
names(prop.hatch)[4]<-"prop.Hatch"
prop.hatch$prop.Hatch[which(is.na(prop.hatch$prop.Hatch)==TRUE)]<-mean(prop.hatch$prop.Hatch,na.rm=TRUE)
prop.hatch<-prop.hatch[order(prop.hatch$order),]
write.csv(prop.hatch,"HatcheryProp_FallRun.csv",row.names=FALSE)

###############################################################################
#fill in missing values
###############################################################################
valley.escapee<-aggregate(x=shed.escapee$count,by=list(year=shed.escapee$year),FUN=sum) #see what years have a complete data set
complete.Years<-valley.escapee$year[which(is.na(valley.escapee$x)==FALSE)]

shed.com.esc<-shed.escapee[which(shed.escapee$year %in% complete.Years),]
valley.escapee<-valley.escapee[which(valley.escapee$year %in% complete.Years),]
names(valley.escapee)[2]<-"tot.count"

#proportion of run that belongs to each trib (only for tribs which Grand Tab had data for)
prop.escapee<-merge(shed.com.esc,valley.escapee,all=T)
prop.escapee$prop.esc<-prop.escapee$count/prop.escapee$tot.count
prop.escapee<-aggregate(x=prop.escapee$prop.esc,by=list(watershed=prop.escapee$watershed),FUN=mean) #mean proportion of fish that come from each trib
names(prop.escapee)[2]<-"prop.escape"
min(prop.escapee$prop.escape)

#use reference trib to estimate total escapement each year
tot.escape<-shed.escapee[which(shed.escapee$watershed=="Upper Sacramento River"),] #treat Upper Sacramento River as reference because of data completeness, no hatchery, and large contribution to annual numbers
tot.escape$total.Escape<-ceiling(tot.escape$count/prop.escapee$prop.escape[which(prop.escapee$watershed=="Upper Sacramento River")])
tot.escape<-tot.escape[,c(1,4)]
plot(tot.escape$year,tot.escape$total.Escape,type='l')

#make complete list of tribs with spawning habitat and calibration years
all.years<-1998:2017
all.watersheds<-load_baseline_data("fall")
all.watersheds<-data.frame(all.watersheds$inps$watershed,all.watersheds$inps$order,all.watersheds$IChab.spawn[,1,1])
names(all.watersheds)<-c("watershed","order","s_hab")
all.sp.watersheds<-all.watersheds$watershed[which(is.na(all.watersheds$s_hab)==FALSE)]
all.comb<-merge(all.years,all.sp.watersheds)
names(all.comb)<-c("year","watershed")
all.comb<-merge(all.comb,prop.escapee,all=T)

#If no data to estimate the proportion of the run that goes to that trib, assume trib is equal to the smallest estimated 
#proportion. This makes it so all fall-run tribs have fish to calibrate to.
#Note that I opted to not rescale the proportions to sum to 1 because it is such a low number.
all.comb$prop.escape<-ifelse(is.na(all.comb$prop.escape)==TRUE,min(prop.escapee$prop.escape),all.comb$prop.escape)

#fill in missing data based on total escapement and proportion in each trib
all.comb<-merge(all.comb,tot.escape,all=T)
all.comb<-merge(all.comb,shed.escapee,all=T)
all.comb$est.count<-ifelse(
  is.na(all.comb$count)==TRUE,
  round(all.comb$total.Escape*all.comb$prop.escape,0),
  all.comb$count
)

###############################################################################
#create csv file for calibration
###############################################################################
all.watersheds<-all.watersheds[,c(1,2)]
n.sheds<-length(all.watersheds$watershed)
n.years<-length(all.years)
tater<-matrix(NA,nrow=n.sheds,ncol=n.years)
for(ii in 1:n.sheds){
  # ii=1
  this<-all.comb[which((all.comb$watershed==all.watersheds$watershed[ii])==TRUE),]
  if(dim(this)[1]>0){
    cat("watershed: ", as.character(all.watersheds$watershed[ii]), " has ", dim(this)[1], " dims\n")
    for(jj in 1:n.years){
      # jj=1
      that<-this$count[which(this$year==all.years[jj])]
      tater[ii,jj]<-that
    } 
  }
}
points(all.years,apply(tater,2,sum,na.rm=TRUE),type='l',col=2) #not a match because black line was based only on reference trib (upper sac) and red line used the actual counts when they were available.

tada<-data.frame(all.watersheds,tater)
tada[is.na(tada)]<-0
names(tada)<-c("watershed","order",all.years)
write.csv(tada,"Calibration2018_NaturalAdults_FallRun_NoFillIns.csv",row.names=FALSE)
