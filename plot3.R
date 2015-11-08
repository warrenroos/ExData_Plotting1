# plot3.R 


# please see PDF entitled "DS EDA - Proj 1 - Analysis Steps....pdf" 
# please see preprocessing.R script 
#   data loaded into a data.table names data.housePower 
#   data already subsetted 
#   preprocessing steps originally not included in every R script file as redundant and 
#   the same for all 4 plots hence, once the preprocessing.R script is run once, 
#   all 4 plots can be generated through simple plot commands.  

# as question requires preprocessing code to be in each of the 4 plot r scripts, 
# it is included here as ### preprocessing code ### followed by a separate section 
# for ### plot 3 code ### 

####################################################################
####################################################################
##################### preprocessing code ###########################
####################################################################
####################################################################

# pre-porcessing steps 
# -	Downloaded file from url 
#   o	Note due to folder / file length issue, renames file "house_power.txt" 
#   o	No other edits to the file made other than viewing in text editor 
# -	Opened in Text Editor 
#   o	Verified that there were no commas in the file 
#   o	Verified it was semi-colon delineated 
#   o	Verified Rows 
#    ???	2,075,261 
#   o	Verified Data Range 
#    ???	12/16/2006 - 11/26/2010 
# -	Noted from question 
#   o	Desired Date Range 
#    ???	2/1/2007 - 2/2/2007 
# -	In R Studio 
#   o	Did Date Diff on Start Date / End Date 
#    ???	1,441 days 
#   o	Did estimate of data points per day 
#    ???	2,075,261 / 1,441 ~= 1,440 
#   o	Did estimate of how many readings per day per problem 
#    ???	matches 60 * 24 for per minute measurements ~= 1,440 
#   o	Estimated per R calcs 
#    ???	Pre-Subset ~= 67,687 rows 
#    ???	Subset ~= 2,880 rows 
#    ???	Post-Subset ~=  2,006,133 rows 
#   o	Reading in subset (using above estimates as a guide / starting point and trial / error to get exact range 
#    ???	skip = 67,636 rows, with nrows = 2,880 rows 
# -	Used read.table to read into R Studio 
#      o	data.housePower <- read.table(
#            file = "house_power.txt", header = TRUE, sep = ";", na.strings = "?", 
#            skip = 66636, nrows = 2880, 
#            col.names = c("ReadDate", "ReadTime", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3" )
#          )
# -	viewed the resulting data.table 
#      o	double-check the resulting data to be processed 
# -	added columns with Date & Time format respectively to data.table per assignment recommendations 

# set the file first date and the last date - estimate from opening quickly in text editor 
data.date.file.start <- as.Date("12/16/2006", "%m/%d/%Y")
data.date.file.end <- as.Date("11/26/2010", "%m/%d/%Y")

# compute diff in days 
data.date.file.diff <- data.date.file.end[1] - data.date.file.start[1]

# convert to numeric for computations 
data.points.file.days.est <- as.numeric(data.date.file.diff) + 1 

# from opening quickly in text editor - estimated # rows (estimate only) 
data.points.total.est <- 2075261 

# compute points per day 
data.points.perDay.est <- data.points.total.est / data.points.file.days.est 

# set subset start and end dates 
data.date.subset.end <- as.Date("2/2/2007", "%m/%d/%Y")
data.date.subset.start <- as.Date("2/1/2007", "%m/%d/%Y")

# compute subset diff in days 
data.date.post.diff <- data.date.file.end[1] - data.date.subset.end[1]
data.date.pre.diff <- data.date.subset.start[1] - data.date.file.start[1]
data.date.subset.diff <- data.date.subset.end[1] - data.date.subset.start[1]

# convert to numeric - added 1 for subset days (2 days,not 1) 
data.points.subset.days.est <- as.numeric(data.date.subset.diff) + 1 
data.points.pre.days.est <- as.numeric(data.date.pre.diff)
data.points.post.days.est <- as.numeric(data.date.post.diff) 

# compute # points pre-subset, subset, and post subset 
data.points.subset.points.est <- data.points.subset.days.est * data.points.perDay.est
data.points.pre.points.est <- data.points.pre.days.est * data.points.perDay.est
data.points.post.points.est <- data.points.post.days.est * data.points.perDay.est

# read in data from file 
data.housePower <- read.table(
  file = "house_power.txt", header = TRUE, sep = ";", na.strings = "?", 
  skip = 66636, nrows = 2880, 
  col.names = c("ReadDate", "ReadTime", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3" )
)

# view data frame 
View(data.housePower)

# mutate data frame - 1st 2 columns into Date and Time format (from Factor) 
library(dplyr)

# add Date formatted column for date 
data.housePower <- mutate(data.housePower, ReadDate2 = as.Date(ReadDate, "%d/%m/%Y"))

# add Time formatted column for date 
# had to use cbind as mutate from dplyr does not support POSIXlt 
##data.housePower <- mutate(data.housePower, ReadTime2 = strptime(ReadTime, "%H:%M:%S"))
data.housePower <- cbind(data.housePower, ReadTime2 = strptime(data.housePower$ReadTime, "%H:%M:%S"))

# Add Datetime column for time series plots... 
data.housePower <- cbind(data.housePower, ReadDateTime6 = strptime(paste(data.housePower$ReadDate, data.housePower$ReadTime), "%d/%m/%Y %H:%M:%S"))



####################################################################
####################################################################
######################### plot 3 code ###############################
####################################################################
####################################################################
 

png("plot3.png", width = 480, height = 480, units = "px")

plot(
  x = data.housePower$ReadDateTime6, 
  y = data.housePower$Sub_metering_1, 
  type = "n", 
  xlab = "", 
  ylab = "Energy sub metering" 
)

legend(
  "topright", pch = NULL, lty = 1, lwd = 2, 
  col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  , bty = "o", y.intersp = 1
  )

points(
  x = data.housePower$ReadDateTime6, 
  y = data.housePower$Sub_metering_1, 
  type = "l", col = "black" 
)

points(
  x = data.housePower$ReadDateTime6, 
  y = data.housePower$Sub_metering_2, 
  type = "l", col = "red" 
)

points(
  x = data.housePower$ReadDateTime6, 
  y = data.housePower$Sub_metering_3, 
  type = "l", col = "blue" 
)

dev.off()
