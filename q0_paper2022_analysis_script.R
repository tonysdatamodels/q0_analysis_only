#exploring the predictive capabilities of the reproductive quotient (Q0) model
#in gastro-intestinal nematode infections of domestic and wild ruminants 

###########################..setup..##############################################

#clear workspace
ls()
rm(list = ls()) #environment
cat("\014") #console
dev.off() #plots

#install packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("kableExtra")

#load packages
library(tidyr)
library(readxl)
library(tidyverse)
library(scales)
library(dplyr)
library(readr)
library(data.table)
library(kableExtra)
library(knitr)

###########################..dual axis graph function..###################################

make.dualaxis.graph <- function (data, x, y){par(mar = c(2, 2, 1, 4) + 0.3) 
  par(bg = "white") 
  plot(a, type = "h", pch = 16, col="blue2", lwd = line2, xaxt = "n", xlab="", ylab = "") #
  labels = c( axis(1, at = c(1:month.length),labels = time.month)) #
  par(new = TRUE) 
  plot(b, pch = 16, col = "black", type = "l", lwd = line1, axes = FALSE, xlab = "", ylab = "") #create second axes
  axis(side = 4, at = pretty(range(b))) 
  mtext(title,side = 3, line = axis1, cex = mtxtsize)}

###########################..section 1 open nematode files..##########################

#choose the files 
setwd("C:\\Users\\40106734\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\2. Data and Analysis\\3. Worm Count Line Graphs\\3.0")
setwd("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\2. Data and Analysis\\3. Worm Count Line Graphs\\3.0")
all.files <- list.files(pattern="*.csv")
all.files

#open all files and make data frames
all.worm.files = list.files(pattern="*.csv")
for (i in 1:length(all.worm.files))
  assign(all.worm.files[i], read.csv(all.worm.files[i]))

###########################..section 2 worm burden dual axis graphs.. ####################

#make graph plots 3 by 3
par(mfrow = c(4, 3))
par(oma = c(1, 1, 1, 1))

#setting up graph parameters
line1 <- 2 #size of line graph
line2 <- 4 #size of bar graph
axis1 <- 0.2 #size of space between title and graph
mtxtsize <- 0.7

#graph 1
title <- "1a. Australia Armidale - Sheep"
a <- australia.armidale.csv.clean.csv %>% pull ('worm_burden')
b <- australia.armidale.csv.clean.csv %>% pull ('mean_q0')
time.month <- australia.armidale.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (australia.armidale.csv.clean.csv)

#graph 2
title <- "2a. Brazil Minas Gerais - Cattle"
a <- brazil.minaisgerais.csv.clean.csv %>% pull ('worm_burden')
b <- brazil.minaisgerais.csv.clean.csv %>% pull ('mean_q0')
time.month <- brazil.minaisgerais.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (brazil.minaisgerais.csv.clean.csv)

#graph 3
title <- "3a. Brazil Rio Grande Do Norte - Sheep"
a <- brazil.riograndedonorte.csv.clean.csv %>% pull ('worm_burden')
b <- brazil.riograndedonorte.csv.clean.csv %>% pull ('mean_q0')
time.month <- brazil.riograndedonorte.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (brazil.riograndedonorte.csv.clean.csv)

#graph 4
title <- "4a. Eastern Ethiopia - Goats"
a <- ethiopia.goatstudy.csv.clean.csv %>% pull ('worm_burden')
b <- ethiopia.goatstudy.csv.clean.csv %>% pull ('mean_q0')
time.month <- ethiopia.goatstudy.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (ethiopia.goatstudy.csv.clean.csv)

#graph 5
title <- "5a. Eastern Ethiopia - Sheep"
a <- ethiopia.sheepstudy.csv.clean.csv %>% pull ('worm_burden')
b <- ethiopia.sheepstudy.csv.clean.csv %>% pull ('mean_q0')
time.month <- ethiopia.sheepstudy.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (ethiopia.sheepstudy.csv.clean.csv)

#graph 6
title <- "6a. Kenya Igango - Tracer Calve"
a <- kenya.igangotracercalve.csv.clean.csv %>% pull ('worm_burden')
b <- kenya.igangotracercalve.csv.clean.csv %>% pull ('mean_q0')
time.month <- kenya.igangotracercalve.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (kenya.igangotracercalve.csv.clean.csv)

#graph 7
title <- "7a. Kenya Igango - Yearling Cattle"
a <- kenya.iganjoyearlingcattle.csv.clean.csv %>% pull ('worm_burden')
b <- kenya.iganjoyearlingcattle.csv.clean.csv %>% pull ('mean_q0')
time.month <- kenya.iganjoyearlingcattle.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (kenya.iganjoyearlingcattle.csv.clean.csv)

#graph 8
title <- "8a. Kenya Kambaa - Yearling Cattle"
a <- kenya.kambaayearlingcattle.csv.clean.csv %>% pull ('worm_burden')
b <- kenya.kambaayearlingcattle.csv.clean.csv %>% pull ('mean_q0')
time.month <- kenya.kambaayearlingcattle.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (kenya.kambaayearlingcattle.csv.clean.csv)

#graph 9
title <- "9a. Kenya Kambaa - Tracer Calve"
a <- kenya.kambaatracercalve.csv.clean.csv %>% pull ('worm_burden')
b <- kenya.kambaatracercalve.csv.clean.csv %>% pull ('mean_q0')
time.month <- kenya.kambaatracercalve.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (kenya.kambaatracercalve.csv.clean.csv)

#graph 10
title <- "10a. Nigeria Borno - Sheep & Goats"
a <- nigeria.borno.csv.clean.csv %>% pull ('worm_burden')
b <- nigeria.borno.csv.clean.csv %>% pull ('mean_q0')
time.month <- nigeria.borno.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (nigeria.borno.csv.clean.csv)

#graph 11
title <- "11a. India Aligarh - Goats"
a <- india.uttarpradesh.csv.clean.csv %>% pull ('worm_burden')
b <- india.uttarpradesh.csv.clean.csv %>% pull ('mean_q0')
time.month <- india.uttarpradesh.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (india.uttarpradesh.csv.clean.csv)

#graph 12
title <- "12a. Pakistan Hyderabad - Sheep"
a <- pakistan.hyderabad.csv.clean.csv %>% pull ('worm_burden')
b <- pakistan.hyderabad.csv.clean.csv %>% pull ('mean_q0')
time.month <- pakistan.hyderabad.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (pakistan.hyderabad.csv.clean.csv)

###########################..section 3 open egg files.. ##############################

getwd()
setwd("C:\\Users\\40106734\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\2. Data and Analysis\\4. Eggs Per Gram Line Graphs\\3.0")
all.files <- list.files(pattern="*.csv")
all.files

#open all files and make data frames
all.egg.files = list.files(pattern="*.csv")
for (i in 1:length(all.egg.files))
  assign(all.egg.files[i], read.csv(all.egg.files[i]))

#make graph plots 3 by 3
par(mfrow = c(4, 3))
par(oma = c(1, 1, 1, 1))

###########################..section 4 egg counts dual axis graphs #########################################

#graph 1.1
title <- "1b. Brazil Minas Gerais - Cattle"
a <- brazil.minasgerais.csv.clean.csv %>% pull ('epg')
b <- brazil.minasgerais.csv.clean.csv %>% pull ('mean_q0')
time.month <- brazil.minasgerais.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (brazil.minasgerais.csv.clean.csv)

#graph 1.2
title <- "2b. Cuba Mantanzas - Sheep"
a <- cuba.matanzas.csv.clean.csv %>% pull ('epg')
b <- cuba.matanzas.csv.clean.csv %>% pull ('mean_q0')
time.month <- cuba.matanzas.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (cuba.matanzas.csv.clean.csv)

#graph 1.3
title <- "3b. Canada Ontario - Sheep"
a <- canada.ontario.csv.clean.csv %>% pull ('epg')
b <- canada.ontario.csv.clean.csv %>% pull ('mean_q0')
time.month <- canada.ontario.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (canada.ontario.csv.clean.csv)

#graph 1.4
title <- "4b. Canada Quebec - Sheep"
a <- canada.quebec.csv.clean.csv %>% pull ('worm_burden')
b <- canada.quebec.csv.clean.csv %>% pull ('mean_q0')
time.month <- canada.quebec.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (canada.quebec.csv.clean.csv)

#graph 1.5
title <- "5b. South Africa Impendle - Goats"
a <- southafrica.impendle.csv.clean.csv %>% pull ('epg')
b <- southafrica.impendle.csv.clean.csv %>% pull ('mean_q0')
time.month <- southafrica.impendle.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (southafrica.impendle.csv.clean.csv)

#graph 1.6
title <- "6b. South Africa Kraipan - Goats"
a <- southafrica.kraipan.csv.clean.csv %>% pull ('epg')
b <- southafrica.kraipan.csv.clean.csv %>% pull ('mean_q0')
time.month <- southafrica.kraipan.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (southafrica.kraipan.csv.clean.csv)

#graph 1.7
title <- "7b. South Africa Rust De Winter - Goats"
a <- southafrica.rustdewinter.csv.clean.csv %>% pull ('epg')
b <- southafrica.rustdewinter.csv.clean.csv %>% pull ('mean_q0')
time.month <- southafrica.rustdewinter.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (southafrica.rustdewinter.csv.clean.csv)

#graph 1.8
title <- "8b. Pakistan Attock - Sheep & Goats"
a <- pakistan.attock.mixed.csv.clean.csv %>% pull ('epg')
b <- pakistan.attock.mixed.csv.clean.csv %>% pull ('mean_q0')
time.month <- pakistan.attock.mixed.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (pakistan.attock.mixed.csv.clean.csv)

#graph 1.9
title <- "9b. Pakistan Chakwal - Sheep & Goats"
a <- pakistan.chakwal.mixed.csv.clean.csv %>% pull ('epg')
b <- pakistan.chakwal.mixed.csv.clean.csv %>% pull ('mean_q0')
time.month <- pakistan.chakwal.mixed.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (pakistan.chakwal.mixed.csv.clean.csv)

#graph 1.10
title <- "10b. Pakistan Islamabad - Sheep & Goats"
a <- pakistan.islamabad.mixed.csv.clean.csv %>% pull ('epg')
b <- pakistan.islamabad.mixed.csv.clean.csv %>% pull ('mean_q0')
time.month <- pakistan.islamabad.mixed.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (pakistan.islamabad.mixed.csv.clean.csv)

#graph 1.11
title <- "11b. Pakistan Jhelum - Sheep & Goats"
a <- pakistan.jhelum.mixed.csv.clean.csv %>% pull ('epg')
b <- pakistan.jhelum.mixed.csv.clean.csv %>% pull ('mean_q0')
time.month <- pakistan.jhelum.mixed.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (pakistan.jhelum.mixed.csv.clean.csv)

#graph 1.12
title <- "12b. Central Switzerland - Goats"
a <- switzerland.central.csv.clean.csv %>% pull ('epg')
b <- switzerland.central.csv.clean.csv %>% pull ('mean_q0')
time.month <- switzerland.central.csv.clean.csv %>% pull ("month")
month.length <- length(time.month)
make.dualaxis.graph (switzerland.central.csv.clean.csv)

###########################..section 5 make worm table.. ##############################

#table 1
worms.q0.table <- read.csv ("C:\\Users\\40106734\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\2. Data and Analysis\\3. Worm Count Line Graphs\\3.0\\worm.qo.table.csv")
worms.q0.table <- read.csv ("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\2. Data and Analysis\\3. Worm Count Line Graphs\\3.0\\worm.qo.table.csv")

names(worms.q0.table)[3] <- 'Climate'
names(worms.q0.table)[5] <- 'Resolution (no.tiles)'
names(worms.q0.table)[6] <- 'Study length (months)'
names(worms.q0.table)[7] <- 'Optimal  \U1D444\U2080 lag (months)'
names(worms.q0.table)[8] <- '\U1D45F' #unicode for italic r
names(worms.q0.table)[9] <- '\U1D45D' #unicode for italic p

kbl(worms.q0.table, booktabs = TRUE, align = "llcccccccc") %>% 
  kable_classic(full_width = F, html_font = "Cambria")%>%
  kable_styling(bootstrap_options = "condensed", latex_options = "scale_down", full_width = F) %>%
  footnote(number = c("no.tiles - number of raster tiles with 0.5\u00B0 * 0.5\u00B0 resolution ",
                      "\U1D45F - Pearson's correlation coefficient",
                      "aterisk*  indicates significanct relationship"))

###########################..section 6 make egg table.. #########################################

#table 2
egg.q0.table <- read.csv ("C:\\Users\\40106734\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\2. Data and Analysis\\4. Eggs Per Gram Line Graphs\\3.0\\egg.q0.table.csv")
egg.q0.table <- read.csv ("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\2. Data and Analysis\\4. Eggs Per Gram Line Graphs\\3.0\\egg.q0.table.csv")

names(egg.q0.table)[3] <- 'Climate'
names(egg.q0.table)[5] <- 'Resolution (no.tiles)'
names(egg.q0.table)[6] <- 'Study length (months)'
names(egg.q0.table)[7] <- 'Optimal  \U1D444\U2080 lag (months)'  #using unicode to type italics into table (q0)
names(egg.q0.table)[8] <- '\U1D45F' #unicode for italic r
names(egg.q0.table)[9] <- '\U1D45D' #unicode for italic p

kbl(egg.q0.table, booktabs = TRUE, align = "llcccccccc") %>% 
  kable_classic(full_width = F, html_font = "Cambria")%>%
  kable_styling(bootstrap_options = "condensed", latex_options = "scale_down", full_width = F) %>%
  footnote(number = c("no.tiles - number of raster tiles with 0.5\u00B0 * 0.5\u00B0 resolution ",
                      "\U1D45F - Pearson's correlation coefficient",
                      "aterisk*  indicates significanct relationship"))

###########################..section 7 setup q0 map.. #########################################

#install packages 
install.packages("ggplot2")
install.packages("rasterVis")
install.packages("rgdal")
install.packages("tibble")
install.packages("RStoolbox")
install.packages("ggtext")
install.packages("mdthemes")
install.packages("ggpubr")
install.packages('patchwork')

#load packages
library(ncdf4)
library(ggplot2) 
library(rasterVis)
library(rgdal)
library(dplyr)
library(RStoolbox)
library(tibble)
library(ggtext)
library(mdthemes)
library(ggpubr)
library(ggthemes)
library(scales)
library(patchwork)

###########################..section 8 calculate mean q0 stats.. #########################################

#chose global q0 output
hcq0 <- raster("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\9. PhD GIS\\Table\\rotate Combined Average 2008 to 2017.tif")
map <- hcq0

#load scripts with necessary supplement code
scripts_2022 <- "C:/Users/tonyb/OneDrive - Queen's University Belfast/3. PhD Year Three/Chapter 3 Evaluating Q0 model performance generalist nematode/6. Q0 Model/Scripts"
setwd(scripts_2022) 
options(digits = 2, scipen=999) #make q0 have 2 decimals and remove scientific notation

#run supplement code
source("create_map_extents.R") #source code
source("crop_q0_by_extent.R") #there might be a be small delay before this code runs 
source("calculate_q0.R")
source("dataframe_q0.R")

#change column name to Q0 and add latitude variables
colnames(q0) <- ("q0")
lat <- (83:-56)
q0 <- q0 %>% add_column(latitude = lat)

library(openxlsx)
write.xlsx(q0, "C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\Analysis\\Outputs\\meanq0.xlsx")

#statistics based on q0 at stated latitudes 
north10_north80 <- slice(q0, 4:74)
north10_south40 <- slice(q0, 74:124)
south40_south56 <- slice(q0, 124:140)

north <- summary (north10_north80$q0)
middle <- summary (north10_south40$q0)
south <- summary(south40_south56$q0)

#combine stats
q0_lat_stats <- rbind(north, middle, south)
write.xlsx(q0_lat_stats, "C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\Analysis\\Outputs\\meanq0_latitude_stats.xlsx")

###########################..section 9 make mean q0 map.. #########################################

hcq0_spdf <- as(hcq0, "SpatialPixelsDataFrame")
hcq0_df <- as.data.frame(hcq0_spdf)
colnames(hcq0_df) <- c("q0", "longitude", "latitude")
coastline_path <- ("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\90. GIS Master Files\\World\\wb_coastlines_10m\\WB_Coastlines_10m")
coastline <- readOGR(dsn=coastline_path, layer="WB_Coastlines_10m") #if error select other "coastline_patch"

#create q0 map with coastal outline 
q0_haemonchus_map <- ggplot() + 
  geom_raster(data=hcq0_df, aes(x=longitude, y=latitude,fill=q0)) +
  geom_path(data=coastline, aes(x=long, y=lat, group=group), 
            color="black", size=0.1) +
  labs(x = "", y = "") +
  ggtitle("A1") +
  scale_fill_gradient(low="white", high ="dark red", name = "") +
  theme(panel.background = element_rect(fill = "gray99", colour = "black"),
        legend.position = c(.15, .10),
        legend.direction="horizontal",
        legend.background = element_rect(fill = alpha("white", 0)),
        panel.grid.major = element_blank(), #removes white lines crossing element react
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1)) + 
  ylim(-56, 83) +
  xlim(-180, 180)

q0_haemonchus_map

###########################..section 10 make mean q0 graph #########################################

#create horizontal line graph (coord_flip)
q0_bar_graph1 <- ggplot (q0, aes(x = latitude, y = q0)) + 
  geom_col(colour = "red", fill = "red", size = 0.1) +
  geom_vline(xintercept=0, linetype = "dashed", size = 1) +
  labs(x = "latitude (°)", y = "") +
  ggtitle("A2") +
  theme(panel.background = element_rect(fill = "gray99", colour = "black"), #creates black outline
        panel.grid.major = element_blank(), #removes white lines crossing element rect
        panel.grid.minor = element_blank(),
        axis.text=element_text(size= 10, color="grey3"), #size of axis text
        axis.title=element_text(size= 13, face="plain"), #size of axis title
        axis.line = element_line(size = 0.5, colour = "black", linetype=1)) + #creates axis lines
  scale_y_continuous(expand = c(0, 0), breaks=seq(10, 60, 20)) +
  scale_x_continuous(breaks=seq(-60 ,90, 30)) +
  coord_flip()  

q0_bar_graph1

###########################..section 11 combine mean map and graph.. ########################################

#combine map and graph
joint.q0map.graph <- q0_haemonchus_map + q0_bar_graph1
joint.q0map.graph

###########################..section 12 calculate difference q0 stats.. ############################

#raster difference map previosly rotated using seperate code
#dimensions also reduced to -58.5S and 84N
diffq0 <- raster("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\9. PhD GIS\\Table\\1988 to 2017 difference final 17.12.21.tif") #mean difference (2017 - 1988)
map <- diffq0
options(digits = 2,scipen=999 )

source("create_map_extents.R") #source code
source("crop_q0_by_extent.R") #there might be a be delay (1-2 mins) before this code runs 
source("calculate_q0.R")
source("dataframe_q0.R") # a dataframe called q0 will be made

colnames(q0) <- ("q0_change")
lat <- (83:-56)
q0 <- q0 %>% add_column(latitude = lat)

library(openxlsx)
write.xlsx(q0, "C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\Analysis\\Outputs\\differenceq0.xlsx")

#statistics
north10_north80 <- slice(q0, 4:74)
north10_south40 <- slice(q0, 74:124)
south40_south56 <- slice(q0, 124:140)

north_change <- summary (north10_north80$q0_change)
middle_change <- summary (north10_south40$q0_change)
south_change <- summary(south40_south56$q0_change)

q0_difference <- rbind(north_change, middle_change, south_change)
q0_difference <- as.data.frame(q0_difference)
write.xlsx(q0_difference, "C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\3. PhD Year Three\\Chapter 3 Evaluating Q0 model performance generalist nematode\\Analysis\\Outputs\\differenceq0_latitude_stats.xlsx", 
           overwrite = T,colNames = T, rowNames = T)

#calculate percentage changes between each decade using data from "q0_difference"
0.86 / 8.82 * 100 #northern hemisphere Q0 % change 10N to 80N degrees 9.8% increase 
2.68 / 32.42 * 100 #mid hemisphere Q0 % change 40S to 10N degrees 8.3% decrease 
0.86 / 7.76 * 100 #southern hemisphere Q0 % change 40S to 56N degrees 11% increase 
options (digits = 2)

###########################..section 14 make difference q0 map.. ############################

#setup q0 change map with coastal outline  
diffhcq0_spdf <- as(diffq0, "SpatialPixelsDataFrame")
diffhcq0_df <- as.data.frame(diffhcq0_spdf)
colnames(diffhcq0_df) <- c("q0_change", "longitude", "latitude")
coastline_path <- "C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\90. GIS Master Files\\World\\wb_coastlines_10m\\WB_Coastlines_10m\\WB_Coastlines_10m.shp"
coastline <- readOGR(dsn=coastline_path, layer="WB_Coastlines_10m")

#make q0 change map with coastal outline
diff_q0_haemonchus_map <- ggplot() + 
  geom_raster(data=diffhcq0_df, aes(x=longitude, y=latitude,fill=q0_change)) +
  geom_path(data=coastline, aes(x=long, y=lat, group=group), color="black", size=0.1) +
  labs(x = "", y = "") +
  ggtitle("B1") +
  scale_fill_gradientn(colours = c("blue","lightblue","white","orange","red","darkred"),
                       values = NULL, limits = c(-30, 40),name = "",) +
  theme(panel.background = element_rect(fill = "gray99", colour = "black"),
        legend.position = c(.15, .10),
        legend.direction="horizontal",
        panel.grid.major = element_blank(), #removes white lines crossing element
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0)),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1)) + 
  ylim(-56, 83) +
  xlim(-180, 180)

diff_q0_haemonchus_map

###########################..section 15 make difference graph.. #########################################

#create horizontal line graph (coord_flip)
q0_bar_graph2 <- ggplot (q0, aes(x = latitude, y = q0_change)) + 
  geom_col(colour = "red", fill = "red", size = 0.1) +
  geom_hline(yintercept=0, size = 0.5) +
  labs(x = "latitude (°)", y = "") +
  ggtitle("B2") +
  theme(panel.background = element_rect(fill = "gray99", colour = "black"), #creates black outline
        panel.grid.major = element_blank(), #removes white lines crossing element rect
        panel.grid.minor = element_blank(),
        axis.text=element_text(size= 10, color="grey3"), #size of axis text
        axis.title=element_text(size= 13, face="plain"), #size of axis title
        axis.line = element_line(size = 0.5, colour = "black", linetype=1)) + #creates axis lines
  scale_y_continuous(expand = c(0, 0), breaks=seq(-8, 4, 4)) +
  scale_x_continuous(breaks=seq(-60 ,90, 30)) + #change 5 to 30 if final number is 5
  coord_flip()

q0_bar_graph2

###########################..section 16 join q0 difference figures.. #########################################

#combine map and graph
joint.q0change.graph <- diff_q0_haemonchus_map  + q0_bar_graph2
joint.q0change.graph

joint.q0map.graph / joint.q0change.graph

###########################..section 17 deer haemonchus q0 graphs.. #########################################

library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

hc_deer <- read.csv("C:/Users/tonyb/OneDrive - Queen's University Belfast/3. PhD Year Three/Chapter 3 Evaluating Q0 model performance generalist nematode/2. Data and Analysis/8. Q0 Deer HC/Analysed/Analysed Master/HC Deer Q0 Analysed 2022.csv")
tc_deer <- read.csv ("C:/Users/tonyb/OneDrive - Queen's University Belfast/3. PhD Year Three/Chapter 3 Evaluating Q0 model performance generalist nematode/2. Data and Analysis/9. Q0 Deer TC/Analysed/Analysed Master/Q0 Deer TC Analysed 2022.csv")

y_title1 <- expression(paste(italic("H. contortus  ") ,"prevalence (%)"))
x_title1 <- expression(paste("Mean ", italic ("Q"[0])))

hcdeerplot <- ggplot(hc_deer, aes(x = Mean_Q0, y = Haemonchus_Prevalence_Percentage)) +
  labs (x = x_title1, y = y_title1) +
  ggtitle("A1") +
  geom_point() + theme(axis.text=element_text(size=12, color="black"),
                       axis.title=element_text(size=14,face="bold"),
                       axis.line = element_line(size = 0.5, colour = "black", linetype=1)) +
  scale_color_manual(values= c('#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
  geom_point(aes(fill = Deer, colour = Deer), size = 6, pch=21, colour = "black") +
  theme(panel.background = element_rect(fill = "gray99", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

hcdeerplot

###########################..section 18 deer teladorsagia q0 graphs.. #########################################

y_title1 <- expression(paste(italic("T. circumcincta  ") ,"prevalence (%)"))

tcdeerplot <- ggplot(tc_deer, aes(x = Mean_Q0, y = Teladorsagia_Prevalence_Percentage)) +
  labs (x = x_title1, y = y_title1) +
  ggtitle("A2") +
  geom_point() + theme(axis.text=element_text(size=12, color="black"),
                       axis.title=element_text(size=14,face="bold"),
                       axis.line = element_line(size = 0.5, colour = "black", linetype=1)) +
  scale_color_manual(values= c('#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
  geom_point(aes(fill = Deer, colour = Deer), size = 6, pch=21, colour = "black") +
  theme(panel.background = element_rect(fill = "gray99", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

tcdeerplot

#combine plots using patchwork
hcdeerplot + tcdeerplot

###########################..section 19 deer q0 stats.. #########################################

#data normal if P-value above 0.05 in shaprio-wilks
shapiro.test(tc_deer$Teladorsagia_Prevalence_Percentage)
shapiro.test(tc_deer$Mean_Q0)

hist(tc_deer$Teladorsagia_Prevalence_Percentage)
hist(tc_deer$Mean_Q0)

hist(hc_deer$Haemonchus_Prevalence_Percentage)
hist(hc_deer$Mean_Q0)

cor.test(hc_deer$Mean_Q0, hc_deer$Haemonchus_Prevalence_Percentage, 
         method = "pearson")

cor.test(hc_deer$Mean_Q0, hc_deer$Haemonchus_Prevalence_Percentage, 
         method = "spearman", exact = FALSE)

cor.test(tc_deer$Mean_Q0, tc_deer$Teladorsagia_Prevalence_Percentage, 
         method = "pearson")

cor.test(tc_deer$Mean_Q0, tc_deer$Teladorsagia_Prevalence_Percentage, 
         method = "spearman", exact = FALSE)