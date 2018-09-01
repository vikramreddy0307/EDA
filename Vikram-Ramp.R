library(readxl)
orig_data= read_excel("Shanrohi.xlsx")[c(-1,-1445),]
colnames(orig_data)=c('date','time','speed','fuel_economy','rpm','coolant_temp')

#install.packages('lubricate')
library(lubridate)
orig_data$time <- factor(orig_data$time)

# parese date
a <- hms(as.character(orig_data$time))

# get hours
orig_data$hour=hour(a)

# get minutes
minute(a)
switch_on=data.frame(orig_data[orig_data$speed==0 & orig_data$rpm==0,])

Stable_data=orig_data[orig_data$speed==0 & orig_data$rpm !=0,]

data=orig_data[,3:7]
data[data ==0] <- NA
#data[which(is.na(data)),] 
data=data[apply(data[,1:4],1,function(x)any(!is.na(x))),]
data[is.na(data)] <- 0


library(stringr)
data$speed=gsub(' N',"",data$speed)
data$coolant_temp=gsub("F","",data$coolant_temp)
data[,1:4]=lapply(data[,1:4],as.numeric)
#hist(data$fuel_economy)
data[data$fuel_economy==242320.4 & data$speed==15776,]=NA
data=na.omit(data)
data$speed_bin=cut(data$speed, breaks = c(-Inf,0,10,20,30,40,50,60,70), labels = c('Stable','(0-10]','(10-20]','(20-30]',
                                                                                   '(30-40]','(40-50]','(50-60]','(60-70]'))


data$rpm_bin=cut(data$rpm,breaks=c(500,1000,1500,2000,2500,3000,3500,4000),labels= 
                   c('(500-1000]','(1000-1500]','(1500-2000]','(2000-2500]','(2500-3000]','(3000-3500]','(3500-4000]'))

data$economy=factor(ifelse(data$fuel_economy<17,'less than 17','Greater than 17' ))
data$hour=as.factor(data$hour)

library(ggplot2)
ggplot(data,aes(fuel_economy))+geom_histogram(aes(fill=..count..),bins=10)


sum(is.na(data))
#density plot of fuel_economy for RPM and Speed ranges
ggplot(data,aes(x=fuel_economy,y=..density..,fill=speed_bin))+geom_density()+
  ggtitle("Denisty plot of fuel economy over 
          various range of speed\n") + 
  theme(plot.title=element_text(size=18))
ggplot(data,aes(x=fuel_economy,y=..density..,fill=rpm_bin))+geom_density()+
  ggtitle("Denisty plot of fuel economy over 
          various range of RPM\n") + 
  theme(plot.title=element_text(size=18))
ggplot(data,aes(x=coolant_temp))+geom_histogram(aes(fill=..count..),bins=70)+
  ggtitle("Histogram of coolant temp\n") + 
  theme(plot.title=element_text(size=18))
ggplot(data,aes(x=coolant_temp,y=..density..,fill=speed_bin))+geom_density()
ggplot(data,aes(x=coolant_temp,y=..density..,fill=rpm_bin))+geom_density()


#rpm vs fuel economy
ggplot(data,aes(x=rpm_bin,y=fuel_economy,colour=rpm_bin))+geom_point()+
  annotate('rect',ymin=17,ymax=23,xmax='(500-1000]',xmin='(3500-4000]',alpha=0.3,fill='green')+
  annotate('text',y=24,x='(2000-2500]',label="Range of RPM's which gave the ideal mileage")+geom_hline(yintercept = 17.4)


# 3D Exploded Pie Chart
#install.packages('plotrix')
library(plotrix)
slices = c(113,111,223,329,34,2)
lbls=c('(500-1000]','(1000-1500]','(1500-2000]','(2000-2500]','(2500,3000]','(3500-4000]')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of RPM of the car(Total data) ")

#performance of car(mileage) over different RPM ranges
ggplot(data,aes(x=rpm_bin,y=fuel_economy,colour=rpm_bin))+geom_point(shape=20)+
  geom_hline(yintercept = 17.4)+
  annotate('text',y=24,x='(2000-2500]',label='Ideal mileage is 
              obtained only in 2500-4000 Rpm ranges')

#speed of the car over different RPM ranges
ggplot(data,aes(x=rpm_bin,y=speed,colour=rpm_bin))+geom_point()+
  ggtitle("variation in speed of the car driven over different RPM ranges.\n") + 
  theme(plot.title=element_text(size=18))

#  how the car has performed(mileage) with RPM over the speed ranges
ggplot(data,aes(x=rpm_bin,y=fuel_economy,colour=speed_bin))+geom_point(shape=20)+
  geom_hline(yintercept = 17.4)+ggtitle("RPM vs Fuel_economy with different speed ranges\n") + 
  theme(plot.title=element_text(size=18))

#fuel economy between 17 to 23 in findinf ideal RPM
fuel_economy_data=data[data$fuel_economy>=17 & data$fuel_economy<23,]
slices = c(1,71,11)
lbls=c('(1500-2000]-','(2000-2500]-','(2500-3000]-')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main=" RPM of the car driven which resulted
      mileage bewtween 17 and 23 in total data")

# data with rpm(2000-2500] and mileage between 17 and 23
economy_rpm_fuel=fuel_economy_data[fuel_economy_data$rpm_bin=='(2000-2500]' | fuel_economy_data$rpm_bin=='(2500-3000]',]

slices = c(1,3,5,51,22)
lbls=c('(10-20)-','(20-30)-','(30-40)-','(40-50)-','(50-60)-')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="range of speed of the car driven Which resulted in
      mileage between 17 and 23 under rpm of 2000-3000]")

############
#
#
# Analysing the RPM where the car has not performed well
#
#
##########



#data for which the fuel_economy is less than 17
less_economy=data[data$fuel_economy<17,]
slices = c(113,111,222,258,14,2)
lbls=c('(500_1000]-','(1000-1500]-','(1500-2000]-','(2000-2500]-','(2500-3000]-','(3500-4000]')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of RPM of the car which resulted in
      mileage less than 17 in total data")


less_economy_speed=less_economy[less_economy$rpm>1000 &less_economy$rpm<2500,]
table(less_economy_speed$speed_bin)
slices = c(8,14,62,78,141,224,63)
lbls=c('Stable','(0-10]-','(10-20)-','(20-30)-','(30-40)-','(40-50)-','(50-60)-')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of speed of the car which resulted in
      mileage less than in 17  under RPM of 1000-2500 in total data")

ggplot(data,aes(x=rpm,y=speed,colour=economy))+geom_point()+
  annotate('rect',xmin=2000,xmax=3000,ymin=30,ymax=60,alpha=0.3,fill='green')+
  annotate('text',x=3000,y=30,label='Region where fuel_economy 
           is varying for same rpm and speed',color="blue")+
  ggtitle("speed vs rpm  with respect to fuel_economy\n") + 
  theme(plot.title=element_text(size=18))+
  annotate('rect',xmin=500,xmax=2000,ymin=0,ymax=45,alpha=0.3,fill='red')+
  annotate('text',x=2500,y=10,label='Region where fuel_economy 
           is less for this ranges of rpm and speed',color="blue")








############################
#
#
#speed vs fuel economy
#
#
###########################


#speed vs fuel economy
ggplot(data,aes(x=speed_bin,y=fuel_economy,colour=speed_bin))+geom_point()+
  annotate('rect',ymin=17,ymax=23,xmax='Stable',xmin='(60-70]',alpha=0.3,fill='green')+
  annotate('text',y=24,x='(20-30]',label="Region explaining
           Range of speeds which gave the ideal mileage")+geom_hline(yintercept = 17.4)


#pie chaart of speeds of the car driven
table(data$speed_bin)
slices = c(69,39,79,89,148,277,108,3)
lbls=c('Stable','(0-10)-','(10-20)-','(20-30)-','(30-40)-','(40-50)-','(50-60)-','(60-70)-')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of speed of the car driven 
      in various speed ranges")


#performance of car(mileage) over different speed ranges
ggplot(data,aes(x=speed_bin,y=fuel_economy,colour=speed_bin))+geom_point(shape=20)+
  geom_hline(yintercept = 17.4)+
  annotate('text',y=24,x='30',label='Ideal mileage is 
           obtained only in  speed ranging from 30-60')

# RPM of the car over different speed ranges
ggplot(data,aes(x=speed_bin,y=rpm,colour=speed_bin))+geom_point()+
  ggtitle("variation in rpm of the car driven over 
          different speed ranges.\n") + 
  theme(plot.title=element_text(size=18))

#  how the car has performed(mileage) with RPM over the speed ranges
ggplot(data,aes(x=speed_bin,y=fuel_economy,colour=rpm_bin))+geom_point(shape=20)+
  geom_hline(yintercept = 17.4)+ggtitle("Speed vs fuel economy for different 
                                        RPM ranges\n") + 
  theme(plot.title=element_text(size=18))




# pie chart of speed with obtained mileage between 17 and 23
table(fuel_economy_data$speed_bin)
slices = c(1,3,5,52,22)
lbls=c('(10-20)-','(20-30)-','(30-40)-','(40-50)-','(50-60)-')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="Speed of the car driven for obtaining
      mileage bewtween 17 and 23 in total data")



#data with speed=40-60 and mileage between 17 and 23table(less_economy$speed_bin)
economy_speed_fuel=fuel_economy_data[fuel_economy_data$speed_bin==50 |
                                       fuel_economy_data$speed_bin==60,]
table(economy_speed_fuel$rpm_bin)
slices = c(1,63,10)
lbls=c('(1500-2000]-','(2000-2500]-','(2500-3000]-')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="RPM of the car driven  with in speed 40- 60 with obtained 
      mileage between 17 and 23 \n.")

########################
#
#
# Analysing the speed where the car has not performed well
#
#
###############################
table(less_economy$speed_bin)

slices = c(69,39,78,86,143,225,79,1)
lbls=c('Stable-','(0-10]-','(10-20]-','(20-30]-','(30-40]-','(40-50]-','(50-60]-','(60-70]-')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="speed of the car driven  with obtained mileage less than 17")

#pints where mileage will be min
ggplot(less_economy,aes(x=rpm_bin,y=speed,colour=economy))+geom_point()+
  ggtitle("Speed vs rpm  for fuel_economy <17 \n")

ggplot(fuel_economy_data,aes(x=rpm,y=speed))+geom_point()

slices = c(113,111,222,258,14,2)
lbls=c('(500_1000]-','(1000-1500]-','(1500-2000]-','(2000-2500]-','(2500-3000]-','(3500-4000]')
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct) # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of RPM of the car for obtaining
      mileage less than in 17 in total data")
#combination of speed and rpm to get the mileage greater than 17
ggplot(data,aes(x=rpm,y=speed,colour=economy))+geom_point()+
  annotate('rect',xmin=2000,xmax=3000,ymin=30,ymax=60,alpha=0.3,fill='green')+
  annotate('text',x=3000,y=30,label=' Green Region where fuel_economy 
           is varying(>17 and <17) for same rpm and speed',color="blue")+
  ggtitle("speed vs rpm  with respect to fuel_economy\n") + 
  theme(plot.title=element_text(size=18))+
  annotate('rect',xmin=500,xmax=2000,ymin=0,ymax=45,alpha=0.3,fill='red')+
  annotate('text',x=2500,y=10,label=' Red Region where fuel_economy 
           is less(<17) for this ranges of rpm and speed',color="blue")


#Explaning the car being brough to neutral while driving
Stable_data$hour=as.factor(Stable_data$hour)
Stable_data$fuel_economy=as.numeric(Stable_data$fuel_economy)
ggplot(Stable_data,aes(x=hour,colour=hour))+geom_bar()+
  ggtitle("Number of times car brought to stable position(Speed=0,RPM !=0 )
           after some speed has attained in different hours\n")

#finding IDLE rpm
Stable_data$rpm=as.numeric(Stable_data$rpm)
ggplot(Stable_data,aes(x=hour,y=rpm))+geom_point()+
  ggtitle(" IDLE RPM(speed=0 and RPM != 0) 
          expected vs car's IDLE RPM")+
  annotate('Rect',ymin=800,ymax=1000,xmin='7',xmax='16',alpha=0.3,fill='green')+
  annotate('text',y=1100,x='13',label='IDLE RPM the car should have')


#effect of mileage for being to to mileage in different hours
ggplot(Stable_data,aes(x=hour,y=fuel_economy,colour=hour))+geom_point()+
  ggtitle("fuel_economy of the car in different hours when car was
          brought to stable(speed=0,RPM !=0)  \n")+ ylim(1,23)
# % of the time that engine was off
slices=c(29,26,27,24,26,24,39,36,37,46,47,28,26,37,50,45,34,24,25 )
lbls=c(0:18)
pct=round(slices/sum(slices)*100)
lbls=paste(lbls, pct,sep="hr -") # add percents to labels 
lbls=paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="percentage of time of car where 
      speed=0=rpm (key on,engine off)")
switch_on$hour=as.factor(switch_on$hour)
ggplot(switch_on,aes(x=hour,fill=hour))+geom_bar()+
  annotate('rect',xmin=7,xmax=)


# speed vs fuel economy
ggplot(data,aes(x=rpm,y=fuel_economy))+geom_point()+
  annotate('rect',ymin=17,ymax=23,xmax=500,xmin=4000,alpha=0.3,fill='green')


#speed vs fuel economy 
ggplot(data,aes(x=speed,y=fuel_economy))+geom_point()+
  annotate('rect',ymin=17,ymax=23,xmax=0,xmin=60,alpha=0.3,fill='green')



#coolant temp 
gplot(data,aes(x=coolant_temp,y=fuel_economy,colour=rpm_bin))+geom_point()+
  annotate('rect',ymin=17,ymax=23,xmax=100,xmin=200,alpha=0.3,fill='green')




