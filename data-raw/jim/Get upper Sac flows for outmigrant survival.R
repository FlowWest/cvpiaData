sac.at.bend.bridge <- read.delim("C:/Users/Jim/Downloads/sac at bend bridge.txt", stringsAsFactors=FALSE)
summary(sac.at.bend.bridge)

sac.at.bend.bridge$Q.cms<-sac.at.bend.bridge$Q.cfs*0.0283168

sac.at.bend.bridge$Date<- as.Date(as.character(sac.at.bend.bridge$Date), format="%m/%d/%Y")
# create year by stripping the year value in Date1 and make it numeric
sac.at.bend.bridge$year <-as.numeric(format(sac.at.bend.bridge$Date, format = "%Y"))
# create month by stripping the month value in Date1 and make it numeric
sac.at.bend.bridge$month <-as.numeric(format(sac.at.bend.bridge$Date, format = "%m"))

medQ<-aggregate(sac.at.bend.bridge[c(3)], by=sac.at.bend.bridge[c(5,6)], median )

floz<-NULL
for(y in 1970:1990){
  work<- subset(medQ, year == y)
  floz<-cbind(floz,work[,3])
}

write.csv(floz,"Wet up Sac Q.csv", row.names=F)  

floz<-NULL
for(y in 1924:1944){
  work<- subset(medQ, year == y)
  floz<-cbind(floz,work[,3])
}

write.csv(floz,"Dry up Sac Q.csv", row.names=F)  

