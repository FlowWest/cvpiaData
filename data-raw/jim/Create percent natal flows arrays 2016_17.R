shd.ord<- read.csv("data-raw/jim/watershed order2016.csv") #file needed to ensure that arrays are formatted correctly

shd.ord<-shd.ord[order(shd.ord$Order),]

flood.dat<- read.csv("data-raw/jim/all trib flows by year month.csv")

flood.dat$Sacramento.River.MidL <- flood.dat$Sacramento.River..Lower.*0.9
library(lubridate)
flood.dat %>%
  gather(watershed, flow, -Date) %>%
  mutate(Date = mdy(Date), year = year(Date), month = month(Date)) %>%
  filter(month == 10) %>%
  select(year, month, watershed, flow) %>% View()


names(flood.dat)

flood.dat$Date <- as.Date(as.character(flood.dat$Date), format="%m/%d/%Y")
# create year by stripping the year value in Date1 and make it numeric
flood.dat$year <-as.numeric(format(flood.dat$Date, format = "%Y"))
# create month by stripping the month value in Date1 and make it numeric
flood.dat$month <-as.numeric(format(flood.dat$Date, format = "%m"))
# create month by stripping the month value in Date1 and make it numeric
flood.dat$day <-as.numeric(format(flood.dat$Date, format = "%d"))

flood.dat<-flood.dat[order(flood.dat$Date),]

flow.dat<-subset(flood.dat, month == 10)

### put data in proper order
dater2<-flow.dat[,c(as.character(shd.ord$Fld.shed.nam), "year")]

flow.dat %>%


dater2<- na.omit(dater2)

names(dater2)

dater2[,c("Sacramento.River..Upper.", "Antelope.Creek",
          "Battle.Creek","Bear.Creek", "Big.Chico.Creek",
          "Butte.Creek", "Clear.Creek", "Cottonwood.Creek",
          "Cow.Creek", "Deer.Creek", "Elder.Creek",
          "Mill.Creek", "Paynes.Creek",
          "Stony.Creek", "Thomes.Creek")] <- dater2[,c("Sacramento.River..Upper.",
                                                       "Antelope.Creek",
                                                       "Battle.Creek", "Bear.Creek",
                                                       "Big.Chico.Creek",
                                                       "Butte.Creek", "Clear.Creek",
                                                       "Cottonwood.Creek", "Cow.Creek",
                                                       "Deer.Creek", "Elder.Creek",
                                                       "Mill.Creek", "Paynes.Creek",
                                                       "Stony.Creek", "Thomes.Creek")]/dater2[,c("Sacramento.River..Upper.Mid.")]
# Sacramento tribs
dater2[,c("Bear.River")]<- dater2[,c("Bear.River")]/dater2[,c("Feather.River")]
dater2[,c("Yuba.River")]<- dater2[,c("Yuba.River")]/dater2[,c("Feather.River")]
dater2[,c("Feather.River")]<-dater2[,c("Feather.River")]/ dater2[,c("Sacramento.River.MidL")]
dater2[,c("American.River")]<-dater2[,c("American.River")]/dater2[,c("Sacramento.River..Lower.")]

# delta tributaries
dater2[,c("Calaveras.River","Cosumnes.River","Mokelumne.River")]<-1

dater2[,c("Merced.River", "Stanislaus.River", "Tuolumne.River")] <- dater2[,c("Merced.River", "Stanislaus.River", "Tuolumne.River")]/dater2[,c("San.Joaquin.River")]


for(i in 1:31) dater2[,i]<-ifelse(dater2[,i]>1,1,dater2[,i])

table(flood.dat$year)
flood.trix<-NULL
for(y in 1970:1990){
  #y = 1924;
  work<- subset(dater2, year == y)
  ### acres to square m changer 4046.86
  flood.trix<-cbind(flood.trix,t(work[,1:31]))
}

write.csv(flood.trix,"Wet return flow.csv", row.names=F)

flood.trix<-NULL
for(y in 1924:1944){
  #y = 1924;
  work<- subset(dater2, year == y)
  ### acres to square m changer 4046.86
  flood.trix<-cbind(flood.trix,t(work[,1:31]))
}

write.csv(flood.trix,"Dry return flow.csv", row.names=F)
