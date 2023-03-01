rm(list=ls())
#set to your own working directory
#getwd() is a good place to start to find out which directory you are currently in
setwd("/This/is/your/file/path") 
install.packages(c("solarPos", "circular","ggplot2", "ggpmisc"))

#import libraries 
library(circular) 
library(solarPos)
library(ggplot2)
library(ggpmisc)

#import and explore 
raw_data <- read.csv("test_data.csv") 
timesheet <-  read.csv("time.csv") 
date <- read.csv("date.csv")
#str(raw_data)
#str(timesheet)
#str(date)
#could be important to convert all to numeric

#functions
# ----------------------------------------------------------------------------------------
#function for finding the mean angle difference (before and after treatment)
#returns 
#mean angle before
#mean angle after
#difference in angles
sum_vec <- function(a_sin_vec=NA, b_sin_vec=NA, a_cos_vec=NA, b_cos_vec=NA) {
  ind = raw_data[row,1]
  #finds the x and y components of each angle and sums these values
  x1<-sum(a_sin_vec)
  y1 <- sum(a_cos_vec)
  x2<-sum(b_sin_vec)
  y2 <- sum(b_cos_vec)
  #from the vectors components, the mean angle can be found 
  angle1 <- deg(atan2(x1,y1))
  angle2 <- deg(atan2(x2,y2))
  #and the difference between agnles established
  ang_dif <- (angle2-angle1)
  #print(angle1)
  #Íprint(angle2)
  #if statments to turn any negative values to positive values (ie. -10=350)
  if (is.na(ang_dif)) {
  }
  else  {
    if (ang_dif < 0) {
      ang_dif <- ang_dif +360
    }
    if ((angle1) < 0) {
      angle1 <- 360 + angle1
    }
    if ((angle2) < 0) {
      angle2 <- 360 + angle2
    }
  }
  #means angles and angle diffrence added to the same vector
  angles <- append(angle1, angle2)
  angles <- append(angles, ang_dif)
  return(angles)
}

#function for finding the corrected mean angle difference (before and after treatment) with regards to the suns azimuth
#returns 
#mean angle difference 
sum_vec_azimuth <- function(angles=NA, a_my_col=NA, b_my_col=NA, a_my_row=NA, b_my_row=NA, date=NA, timesheet=NA) {
  #print(as.numeric(substr(timesheet[a_my_row/10,a_my_col],1,2)))
  #print(as.numeric(substr(timesheet[a_my_row/10,a_my_col],4,5)))
  #print(b_my_row/10)
  #print((timesheet[(b_my_row/10),b_my_col]))
  #time vector
  time_a=julianDay(2022,11,date[a_my_row/10,a_my_col], hour = as.numeric(substr(timesheet[a_my_row/10,a_my_col],1,2)), min = as.numeric(substr(timesheet[a_my_row/10,a_my_col],4,5)), sec = 0, tz= 2)
  #sun position vector
  sp_a=solarPosition(time_a,27.9192,-24.7114)
  #repeated for angle after
  time_b=julianDay(2022,11,date[b_my_row/10,b_my_col], hour = as.numeric(substr(timesheet[b_my_row/10,b_my_col],1,2)), min = as.numeric(substr(timesheet[b_my_row/10,b_my_col],4,5)), sec = 0, tz =2)
  #repeated for angle after
  sp_b=solarPosition(time_b,27.9192,-24.7114)
  #print(angles[1])
  #print(angles[2])
  #print(angles[3])
  #print(sp_a)
  #print(sp_b)
  #print((ang_dif_2))
  angle1_2 <- angles[1]
  sun_dif <- sp_b[2] - sp_a[2] 
  angle2_2 <- angles[2] - sun_dif
  ang_dif_2 <- (angle2_2-angle1_2)
  #correction of angles to positive
  if (is.na(ang_dif_2)) {
  }
  else  {
    if (ang_dif_2 < 0) {
      ang_dif_2 <- ang_dif_2 +360
    }
    if ((angle1_2) < 0) {
      angle1_2 <- 360 + angle1_2
    }
    if ((angle2_2) < 0) {
      angle2_2 <- 360 + angle2_2
    }
  }
  ang_dif_2 <- (angle2_2-angle1_2)
  if (is.na(ang_dif_2)) {
  }
  else  {
    if (ang_dif_2 < 0) {
      ang_dif_2 <- ang_dif_2 +360
    }
  }
  #print((timesheet[a_my_row,a_my_col]))
  #print(ind-1)
  #print(sp_b[2])
  # print(angle1_2)
  #print(angle2_2)
  #print((ang_dif_2))
  return(ang_dif_2)
}

#creates a dataframe of points around a circle
circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=360){
  start <- -(start + 90) + 180
  end <- -(end + 90) + 180
  tt <- seq(start, end, length.out=npoints)
  dataed<- data.frame(
    x = center[1] + diameter / 2 * cos(rad(tt)),
    y = center[2] + diameter / 2 * sin(rad(tt))
  )
  dataed <- rbind(dataed,center)
}

#variables
# ----------------------------------------------------------------------------------------
a_sin_vec <- vector()
b_sin_vec <- vector()
a_cos_vec <- vector()
b_cos_vec <- vector()
diff_vec <- vector()
diff_vec_2 <- vector()
row_name <- vector()
rho_vec <- vector()
rho_azi_vec <- vector()
mean_vec <- vector()
mean_azi_vec <- vector()
sd_real_vec <- vector()
sd_real_azi_vec <- vector()
sd_vec <- vector()
sd_azi_vec <- vector()
ray_vec <- vector()
ray_azi_vec <- vector()
sample_vec <- vector()
sample_azi_vec <- vector()
#main
# ----------------------------------------------------------------------------------------
#for loop through each condition 
for (column in 3:length(raw_data)) {
  ind = raw_data[1,1] #determining first individual
  print(colnames(raw_data)[column])
  #for loop through each run
  for (row in 1:length(raw_data[,1])) {
    #both if statements in place to ensure rolls are associated 1 indivudal, then moves on to next individual
    if (raw_data[row,1] != ind) {
      ind = raw_data[row,1]
      angles <- sum_vec(a_sin_vec, b_sin_vec, a_cos_vec, b_cos_vec) 
      #mean angle difference added to a final vector
      diff_vec <- append(angles[3],diff_vec)
      ang_dif_2<- sum_vec_azimuth(angles, a_my_col, b_my_col, a_my_row, b_my_row, date, timesheet) 
      #corrected mean angle difference added to a final vector
      diff_vec_2 <- append(ang_dif_2,diff_vec_2)
      a_sin_vec <- vector()
      b_sin_vec <- vector()
      a_cos_vec <- vector()
      b_cos_vec <- vector()
      #print(colnames(raw_data)[column])
    }
    if (raw_data[row,1] == ind) {
      #before treatment
      if (raw_data[row,2] == 1) {
        #print(raw_data[row,column])
        #add y component of vector 
        a_cos_vec <- append(a_cos_vec,cos(rad(raw_data[row,column])))
        #add x component of vector
        a_sin_vec <- append(a_sin_vec,sin(rad(raw_data[row,column])))
        #print((raw_data[row,column]))
        #print(cos(rad(raw_data[row,column])))
        a_my_col=column
        a_my_row=row
      }
      #after treatment
      #function to be created to prevent duplication
      else if (raw_data[row,2] == 2) {
        #add y component of vector 
        b_cos_vec <- append(b_cos_vec,cos(rad(raw_data[row,column])))
        #add x component of vector
        b_sin_vec <- append(b_sin_vec,sin(rad(raw_data[row,column])))
        #print(cos(rad(raw_data[row,column])))
        #print("raw_dataed")
        b_my_col=column
        b_my_row=row
      }
    }
  }
  #this section is to add the final row to the vector for each mean angle
  angles <- sum_vec(a_sin_vec, b_sin_vec, a_cos_vec, b_cos_vec) 
  diff_vec <- append(angles[3],diff_vec)
  ang_dif_2<- sum_vec_azimuth(angles, a_my_col, b_my_col, a_my_row, b_my_row, date, timesheet) 
  diff_vec_2 <- append(ang_dif_2,diff_vec_2)
  a_sin_vec <- vector()
  b_sin_vec <- vector()
  a_cos_vec <- vector()
  b_cos_vec <- vector()
  
  #plotting section
  name=paste(as.character(colnames(raw_data)[column]),".pdf")
  pdf(name) 
  #circular variable
  circ_l_30 <- circular(x=diff_vec, type = "angles", units = "degrees", zero= 1.57079633, rotation ="clock")#, modulo = "2pi")#, rotation = "counter")
  azimuth <- circular(x=diff_vec_2, type = "angles", units = "degrees", zero= 1.57079633, rotation ="clock")#, modulo = "2pi")#, rotation = "counter")
  #plot
  circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2){
    start <- -(start + 90) + 180
    end <- -(end + 90) + 180
    tt <- seq(start, end, length.out=npoints)
    dataed<- data.frame(
      x = center[1] + diameter / 2 * cos(rad(tt)),
      y = center[2] + diameter / 2 * sin(rad(tt))
    )
    dataed <- rbind(dataed,center)
    #dataed$x = append(dataed$x, center[1])
    #dataed$y = append(dataed$y, center[2])
    #dataed$x = append(center[1], dataed$x)
    #dataed$y = append(center[2], dataed$y)
  }
  plus <- mean.circular(azimuth,na.rm=TRUE)+180/pi*angular.deviation(azimuth,na.rm = TRUE)[1]
  minus <- mean.circular(azimuth,na.rm=TRUE)-180/pi*angular.deviation(azimuth,na.rm = TRUE)[1]
  ang_dif <- (plus-minus)
  for (part in plus) {
    new_plus <- part
  }
  for (part in minus) {
    new_minus <- part
  }
  plus <- new_plus
  minus <- new_minus
  ang_dif <- (plus-minus)
  #print(plus)
  #print(minus)
  dat <- circleFun(diameter =2, start=minus, end=plus)
  az_dat <- circleFun(diameter =2, start=plus, end=minus)
  plus <- mean.circular(circ_l_30,na.rm=TRUE)+180/pi*angular.deviation(azimuth,na.rm = TRUE)[1]
  minus <- mean.circular(circ_l_30,na.rm=TRUE)-180/pi*angular.deviation(azimuth,na.rm = TRUE)[1]
  ang_dif <- (plus-minus)
  for (part in plus) {
    new_plus <- part
  }
  for (part in minus) {
    new_minus <- part
  }
  plus <- new_plus
  minus <- new_minus
  ang_dif <- (plus-minus)
  dat <- circleFun(diameter =2, start=plus, end=minus)
  
  plot.circular(x = azimuth, stack = TRUE, bins =72, col = "black", bg="gold", pch = 21,cex = 3,sep = 0.035, start.sep = 0.05, axes = FALSE, ylim = c(-1,1.25))#, xlim = c(-1,1), ylim = c(-1,1.5))
  #print(colnames(raw_data)[column], axes = FALSE)#,shrink = 0.7)#,xlim=100, ylim=100)
  #print(diff_vec)
  #print(diff_vec_2)
  
  #various analyses
  print(180/pi*sd.circular(azimuth,na.rm=TRUE))
  print(180/pi*angular.deviation(azimuth,na.rm = TRUE))
  #append(stand_dev,sd.circular(circ_l_30,na.rm=TRUE))
  #print(mean.circular(circ_l_30,na.rm=TRUE))
  #print(mean.circular(azimuth,na.rm=TRUE))
  #print(meandeviation(circ_l_30, na.rm = TRUE))
  #print(rayleigh.test(circ_l_30))
  print(rayleigh.test(azimuth))
  print(rho.circular(azimuth,na.rm=TRUE))
  #print(rayleigh.test(circ_l_30))
  #print(rho.circular(circ_l_30, na.rm=TRUE))
  #add arrow to plot
  #polygon(2*(1i^(seq(0,2,l=100)))*1i^.5 + .1-.3i, col="red")
  # if (180/pi*angular.deviation(circ_l_30,na.rm = TRUE) < 66) {
  #  arrows.circular(mean.circular(circ_l_30,na.rm=TRUE), rho.circular(circ_l_30,na.rm=TRUE), zero = pi/2, length = 0.2, col = "black", lwd = 3)
  # polygon(y=dat$y,x=dat$x, col = "grey", density = 10)
  #}
  #if (180/pi*angular.deviation(azimuth,na.rm = TRUE) < 66) {
  if (180/pi*angular.deviation(azimuth,na.rm = TRUE) < 65) {
    arrows.circular(mean.circular(azimuth,na.rm=TRUE), rho.circular(azimuth,na.rm=TRUE), zero = pi/2, length = 0.2, col = "gold", lwd = 5)
    polygon(y=az_dat$y,x=az_dat$x, col = "gold", density = 10)
  }
  #add corrected mean difference angles
  #points.circular(azimuth, stack = TRUE, col = "black", bg="gold", pch = 21,cex = 3,sep = 0.054, start.sep = 0.05, bins = 72)
  #print(colnames(raw_data)[column])
  #print(180/pi*angular.deviation(circ_l_30,na.rm = TRUE))
  #print(180/pi*angular.deviation(azimuth,na.rm = TRUE))
  row_name <- append(row_name,colnames(raw_data)[column])
  rho_vec <- append(rho_vec, round(rho.circular(circ_l_30,na.rm=TRUE),2)) 
  rho_azi_vec <- append(rho_azi_vec, round(rho.circular(azimuth,na.rm=TRUE),2))
  mean_vec <- append(mean_vec, round(mean.circular(circ_l_30,na.rm=TRUE),2))
  mean_azi_vec <- append(mean_azi_vec, round(mean.circular(azimuth,na.rm=TRUE),2))
  sd_real_vec <- append(sd_real_vec, round(180/pi*sd.circular(circ_l_30,na.rm = TRUE),2))
  sd_real_azi_vec <- append(sd_real_azi_vec, round(180/pi*sd.circular(azimuth,na.rm = TRUE),2))
  sd_vec <- append(sd_vec, round(180/pi*angular.deviation(circ_l_30,na.rm = TRUE),2))
  sd_azi_vec <- append(sd_azi_vec, round(180/pi*angular.deviation(azimuth,na.rm = TRUE),2))
  ray_vec <- append(ray_vec, round(rayleigh.test(circ_l_30)[2]$p.value,2))
  ray_azi_vec <- append(ray_azi_vec, round(rayleigh.test(azimuth)[2]$p.value,2))
  sample_vec <- append(sample_vec, sum(!is.na(circ_l_30)))
  sample_azi_vec <- append(sample_azi_vec, sum(!is.na(azimuth)))
  
  #add title
  title(xlab = colnames(raw_data)[column])#, xlab = NA, xlab = NA)
  #refresh mean difference vectors
  diff_vec <- vector()
  diff_vec_2 <- vector()
  #exporting
  dev.off()
}

#datatable
#time_vec <- c(0, 10, 30)
#azi_dataframe <- data.frame(row_name, c(time_vec),rho_azi_vec, mean_azi_vec, sd_real_azi_vec, sd_azi_vec,ray_azi_vec, sample_azi_vec)
#reg_dataframe <- data.frame(row_name, c(time_vec),rho_azi_vec, mean_azi_vec, sd_real_azi_vec, sd_azi_vec,ray_azi_vec, sample_azi_vec)
azi_dataframe <- data.frame(row_name, rho_azi_vec, mean_azi_vec, sd_real_azi_vec, sd_azi_vec,ray_azi_vec, sample_azi_vec)
reg_dataframe <- data.frame(row_name, rho_azi_vec, mean_azi_vec, sd_real_azi_vec, sd_azi_vec,ray_azi_vec, sample_azi_vec)
write.csv(azi_dataframe, "azi_dataframe.csv", row.names=FALSE)
#tablesgenerator.com for latex format