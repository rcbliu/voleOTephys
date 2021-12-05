#The purpose of this code is to take the csv file output by DLC and count how long the test vole
#spends with his nose inside of a designated area surrounding the cup

#You will have to write the values into a csv yourself, since the videos we used are
#each only a part of a whole trail, I decided it would be easier to just type the values in 
#by hand. 

#after this code runs, it will print your seconds values for each cup in the console.

#Before running this code, you must have completed several things:
# 1. determine the x and y coordinates of the center of each cup (this can be done in jython or fiji/image j)
# 2. determine the diameter of the cup (can be done using jython and the pythagorean theorem or Imagej/Fiji)
# 3. create a folder in which you will be keeping your files, mine is called "VidTrackR"
# 4. put the csv output by DLC into the folder, as well as this R script

# Now you're ready to begin

# First, you need to tell R where your data is. This will be different on each computer. 
# Mine is "/Users/parker/Desktop/VidTrackR/"
# You can access the path of your folder on a mac by right clicking the folder, holding the 
# option key, and clicking "copy [file] as pathname"
# On a PC, right click the file, click the shift key, and then copy as path

# 6. Copy and paste the path between the quotes in the command below

setwd("C:/Users/aborie/Desktop/CSVs/")
#install.packages(tidyverse)
library(tidyverse)


# Next, you want to bring the csv file into R
# 7. Copy and paste the file name into the quotes below

points = read.csv(file = "PPT140B_20191102_idtraj.csv", header=T)
#points1 = read.csv(file = "M2U00018.csv", header = T)
#points2 = read.csv(file = "M2U00019.csv", header = T)
# points3 = read.csv(file = "", header = T)


# 8. Define the parameters below
# Change these to your predetermined numbers
cap <- (.0) #minimum probability that the DLC points are correct
LcenX <- (295) #x coordinate of left cup center
LcenY <- (320) #y coord of left cup center
Ldiam <- (60)#radius of left cup
RcenX <- (1061) #x coord of right cup center
RcenY <- (440) #y coord of right cup center
Rdiam <- (60) #radius of right cup
Lrange = Ldiam * (3) #change "1.5" to whatever proportion of the radius you want. I chose 1.5 arbitrarily
Rrange = Rdiam * (3) 
vidname = "video" #videoname
start = 780 #first high probability point (go into the file and choose the first one where the probability i above your threshold)
#start1 = 1
#start2 = 1
#start3 = 1
volelength = Ldiam*2 #vole length

#only uncomment out the amount of videos you need

#create vectors restricted to 2h for the analysis
NoseX = points$X[start:length(points$X)]
NoseY = points$Y[start:length(points$Y)]


newNoseX = NoseX[1:216000]
newNoseY = NoseY[1:216000]
# # #create vectors of nose data for 2 videos:
# headNoseX = c(points$headNoseX[start:(length(points$headNoseX))], points1$headNoseX[start1:length(points1$headNoseX)])
# headNoseY = c(points$headNoseY[start:(length(points$headNoseY))], points1$headNoseY[start1:length(points1$headNoseY)])
# # 
# #create vectors for 3 videos:
# headNoseX = c(points$headNoseX[start:(length(points$headNoseX))], points1$headNoseX[start1:length(points1$headNoseX)], points2$headNoseX[start2:length(points2$headNoseX)])
# headNoseY = c(points$headNoseY[start:(length(points$headNoseY))], points1$headNoseY[start1:length(points1$headNoseY)], points2$headNoseY[start2:lenght(points2$headNoseY)])

# #create for 4 videos:
# headNoseX = c(points$headNoseX[start:(length(points$headNoseX))], points1$headNoseX[start1:length(points1$headNoseX)], points2$headNoseX[start2:length(points2$headNoseX)], points3$headNoseX[start3:length(points3$headNoseX)])
# headNoseY = c(points$headNoseY[start:(length(points$headNoseY))], points1$headNoseY[start1:length(points1$headNoseY)], points2$headNoseY[start2:lenght(points2$headNoseY)], points3$headNoseY[start3:length(points3$headNoseY)])


#count jumps

totaljump=0
for (i in 2:length(newNoseX)){
  xdist = newNoseX[i]-newNoseX[i-1]
  ydist = newNoseY[i]-newNoseY[i-1]
  if ((sqrt((xdist*xdist)+(ydist*ydist))>volelength*2)==TRUE){
    totaljump=totaljump+1
  }
}

#count the number of frames that the nose is within the specified range
framecountL = 0
framecountR = 0
for (i in 1:length(newNoseX)){
  diffXL = newNoseX[i]-LcenX
  diffYL = newNoseY[i]-LcenY
  distL = sqrt((diffXL*diffXL)+(diffYL*diffYL))
  if (distL < Lrange){
    framecountL = framecountL +1
  }
  diffXR = newNoseX[i]-RcenX
  diffYR = newNoseY[i]-RcenY
  distR = sqrt((diffXR*diffXR)+(diffYR*diffYR))
  if (distR < Rrange){
    framecountR = framecountR + 1
  }
}

#count the number of seconds that the vole is within each range
secL = framecountL/30
secR = framecountR/30

rsecL = round(secL)
rsecR = round(secR)
strsecL = toString(rsecL)
strsecR = toString(rsecR)

ratio = rsecL/rsecR
rratio = round(ratio, digits=3)
#plotting
#install.packages("tidyverse")
#library(tidyverse)


X = data.frame(newNoseX, newNoseY)


pal = colorRampPalette(c("steelblue1", "green", "yellow", "orange", "red"))

ggplot(data = X, mapping = aes(x=newNoseX, y=newNoseY))+
  geom_bin2d(bins=55)+
  xlim(0,1500)+
  ylim(0,1500)+
  xlab("pixels(x)")+
  ylab("pixels(y)")+
  ggtitle(vidname)+
  scale_fill_gradientn(limits=c(50,5000), breaks=seq(0,50000, b=1000), colours = pal(10))+
  annotate("text", x=300, y=600, label= "L Cup Time (s):")+
  annotate("text", x=300, y=675, label = strsecL)+
  annotate("text",x=1000,y=600, label = "R Cup Time (s):")+
  annotate("text", x=1000,y=675, label = strsecR)
 

#print the number of seconds the vole is within each range
print ("The vole spent")
print (secL)
print ("around the left cup")
print ("And the vole spent")
print (secR)
print ("around the right cup")
print ("Go ahead and write these values into your excel file. Thanks!")

