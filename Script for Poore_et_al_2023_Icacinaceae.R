# This code produces the plots for Poore et al. 2023 Fossil Icacinaceae

# load packages
library(ggplot2)
library(ggtext)
library(ggstar)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)
require(ggiraph)
require(ggiraphExtra)
library(ggfortify)
library(pgirmess)
library(see)
library(performance)
require(grDevices)
library(ggpointdensity)
library(data.table)
library(dplyr)
library(raster)
library(dismo)
library(rgdal)
library(maptools)
library(spocc)
library(sp)
library(leaflet)
library(geosphere)
library(rgeos)
library(rgbif)

################################################################
# PART 1                                                       #
# create a scatterplot of fossil 'endocarps' length and width. #
################################################################

# Put the data in your working directory, or use file.choose()
endocarps <- read.csv(file="sizes.csv", header = TRUE) #sizes.csv

#Now, plot length and width to see the relationship.
plot(x=endocarps$width, y=endocarps$length, main="Endocarp dimentions") # a graph using base R

smallEndocarps <- endocarps[endocarps$width < 15, ]

# A graph using ggplot2
p1 <- ggplot(smallEndocarps, aes(x=width, y=length, label=species)) + # this specifies the dataset and variables
  geom_errorbar(aes(ymin = min.length, ymax=  max.length), color="grey60", width=0.5, show.legend = FALSE) +
  geom_errorbar(aes(xmin = min.width, xmax=  max.width), color="grey60", width=0.5, show.legend = FALSE) +
  geom_point() + # include the points in the plot
  geom_label_repel(size = 5.5, label.size=NA, fill = NA) +
  ggtitle("Variation in endocarp size") + # create a title
  theme_classic(base_size = 16) # set a good font size; there are several themes you can try as well.
p1 +
  xlim(0, 20) +
  ylim(0, 20)

#########
# Fig 5 #
#########

p2 <- ggplot(smallEndocarps, aes(x=width, y=length, label=species, col=species)) + # this specifies the dataset and variables
  #geom_point() + # include the points in the plot
  xlab("width (mm)") + ylab("length (mm)") +
  geom_errorbar(aes(ymin = min.length, ymax=  max.length), width=0.5) +
  geom_errorbar(aes(xmin = min.width, xmax=  max.width), width=0.5) +
  geom_label_repel(size = 3, fontface = "italic") +
  ggtitle("Variation in fossil endocarp size") + # create a title
  theme_classic(base_size = 16) +
  theme(legend.position="none") 

p2 + 
  #scale_fill_brewer(palette="Dark2") +
  xlim(0, 20) +
  ylim(0, 20)


################################################################
# PART 2 plot distribution of fossils                          #
################################################################

list.files()
data <- read.csv(file="Occurrence_data")
tail(data)
attach(data)

#age is negative time
age <- data$Mya*-1
data$Age <- age

#endocarps only
data1 <- data[ which(data$Organ=='endocarp'), ]

#high confidence only
data2 <- data1[ which(data1$Confidence=='high'), ]

#Simplify the data to count number of species per site
simple <- dplyr::select(data2, Locality.Formation,Latitude,Longitude,Paleolatitude,Age)

# Count duplicate rows: How  many speices per site?
count.dups <- function(simple){
  DT <- data.table(simple)
  DT[,.N, by = names(DT)]
}
rich <- count.dups(simple)

#Order the points
richness <- rich[order(N),]
head(richness)
tail(richness)

#set up the plot for fossils only
sp2 <- ggplot(richness, aes(x=Age, y=Paleolatitude, colour = N)) +
  geom_point(alpha = 1, size = 3) +
  scale_color_viridis_c("species", option = "plasma") +
  xlim(-90,0) +
  scale_y_continuous(limits = c(-90, 90), 
                     breaks=c(-66.6,-23.5,0,23.5,66.6), 
                     minor_breaks=NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank())

# Create the plot for fossils only
sp2 + ggtitle("Distribution of Phytocreneae through time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Age (million years)") +
  geom_hline(yintercept = 0, color="blue", size=.5) +
  geom_vline(xintercept = -66, color="red", size=.25) +
  geom_hline(yintercept = -23.5, linetype="dashed", color="blue", linewidth=.5) +
  geom_hline(yintercept = 23.5, linetype="dashed", color="blue", linewidth=.5) +
  annotate(geom="text", x=-61, y=-42, label="P. ga",
         color="black", fontface = "italic")

# To add data from modern occurrences
# get occurrences from GBIF for Miquellia, Phytocrene, 
# Pyrenacantha, Sarcostigma, and Stachyanthus. 
# We used Gbif download https://doi.org/10.15468/dl.ebggcf
# But we deleted occurrences of Adelanthus bisetulus 
# and Adelanthus sphalerus because they are mosses
download <- as.data.frame(read.csv(file.choose())) 

#subset the data
Phyt <- subset(download,select = c('scientificName','decimalLatitude', 'decimalLongitude'))
Phyt$scientificName <- as.character(Phyt$scientificName)

#Remove duplicate species occurrences at a site
library(data.table)
count.dups <- function(Phyt){
  
  DT <- data.table(Phyt)
  DT[,.N, by = names(DT)]
}
nodups <- count.dups(Phyt)
nodups

local <- dplyr::select(nodups, decimalLatitude, decimalLongitude)
head(local)

#lump localities to a square degree
local <- round(local, 0)
count.dups <- function(local){
  DT <- data.table(local)
  DT[,.N, by = names(DT)]
}
count.dups(local)

#Now how many species per site?
rich <- count.dups(local)
head(rich)

#All recent observations
rich$Age <- 0
head(rich)

#put the points in order!
rich <- rich[order(N),]
head(rich)

#Match up fossil  and modern dataframes
rich$Paleolatitude <- rich$decimalLatitude
head(rich)

richness2 <- dplyr::select(richness, Paleolatitude, Age, N)
rich2 <- dplyr::select(rich, Paleolatitude, Age, N)

#Now rbind it all together
total <- rbind(richness2, rich2)


#set up the plot
sp4 <- ggplot(total, aes(x=Age, y=Paleolatitude, colour = N)) +
  geom_point(alpha = 1, size = 3) +
  scale_color_viridis_c("species", option = "plasma") +
  xlim(-90,0) +
  scale_y_continuous(limits = c(-90, 90), 
                     breaks=c(-66.6,-23.5,0,23.5,66.6), 
                     minor_breaks=NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) 

#########
# Fig 6 #
#########

sp4 + ggtitle("Latitudinal distribution of fossil and modern Phytocreneae") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Age (million years)") +
  geom_hline(yintercept = 0, color="blue", size=.5) +
  geom_vline(xintercept = -66, color="red", size=.25) +
  geom_hline(yintercept = -23.5, linetype="dashed", color="blue", linewidth=.5) +
  geom_hline(yintercept = 23.5, linetype="dashed", color="blue", linewidth=.5) +
  annotate(geom="text", x=-61, y=-42, label="P. ga",
           color="black", fontface = "italic")


################################################################
# PART 3 A map of the distribution of modern and fossil        #
################################################################

#########
# Fig 1 #
#########

library("tidyverse")
world <- map_data("world")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = Phyt,
    aes(x=decimalLongitude, y=decimalLatitude), col="black") +
  geom_point(aes(x=109.1622, y=18.3756), col="black") + #the Hainan Occurrence Peng Howard 2008 
  geom_point(aes(x=0, y=0), col="white", cex=2) + #cover the 0,0 
  geom_point(  
    data = richness,
  aes(x=Longitude, y=Latitude), pch=21, col="black", fill="white") +
  annotate(geom="text", x=-52, y=-45, label="P. ga",
           color="black", fontface = "italic") +
  geom_star(
    aes(x=-66.983, y=-45.067), col="black", fill="#f0f921", size=2.5) +
  theme_void() +
  #theme(legend.position = "none") +
  #guides(col=guide_legend("Age (Ma)")) + 
  labs(title="Distribution of modern and fossil Phytocreneae")