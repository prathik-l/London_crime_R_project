#Q1

# The dataset is stored as a CSV file named "london-crime-data.csv"
london_crime <- read.csv("london-crime-data.csv")

# Show the structure of the dataset
str(london_crime)

# Create a new variable 'Date' using paste() function
london_crime$Date <- with(london_crime, paste(day, month, year, sep="/"))
# Q2
# Retain and rename variables as per the table
london_crime <- london_crime[c("borough", "major_category", "minor_category", "value", "Date")]
names(london_crime) <- c("Borough", "MajorCategory", "SubCategory", "Value", "CrimeDate")
#Q3

# Convert the CrimeDate variable to Date type
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, format="%d/%m/%Y")

# Confirm that the variable has been changed
str(london_crime$CrimeDate)
# Summary of crimes by borough
#Q4
crime_summary <- table(london_crime$Borough)

# Plot the summary
barplot(crime_summary, main="Crime Summary by Borough", xlab="Borough", ylab="Number of Crimes", las=2)
# The borough with the highest level of crime will be the one with the tallest bar in the barplot.
# The borough with the lowest level of crime will be the one with the shortest bar in the barplot.
#Q5


# Summary of crimes by MajorCategory
category_summary <- table(london_crime$MajorCategory)

# Plot the summary in a pie chart
pie(category_summary, main="Crime Distribution by MajorCategory")
# The major category with the highest level of crimes will have the largest slice in the pie chart.
# The major category with the lowest level of crimes will have the smallest slice in the pie chart.
#Q6

# Check if any boroughs have not been assigned a region (NA values)
sum(is.na(london_crime$Region)) # If this is more than 0, there are boroughs without a region.

# Replace NA values with a suitable Region if necessary
london_crime$Region[is.na(london_crime$Region)] <- "Suitable Region"

#Q7
# Assuming the Region column has been correctly assigned in Q6
# Aggregate crime data by Region
crime_by_region <- aggregate(Value ~ Region, data=london_crime, FUN=sum)


# Find the region with the highest and lowest number of crimes
max_crime <- crime_by_region[which.max(crime_by_region$Value), ]
min_crime <- crime_by_region[which.min(crime_by_region$Value), ]

# Plot the number of reported crimes by region
plot(crime_by_region$Region, crime_by_region$Value, type='o', col='blue',
     main='Reported Crimes by Region in London', xlab='Region', ylab='Number of Reported Crimes')
# Use text labels to display the crime numbers on the plot
text(crime_by_region$Region, crime_by_region$Value, labels=crime_by_region$Value, pos=3, cex=0.8)

# Indicate which region had the highest and lowest number of crimes
# The region with the highest number of crimes is:
# max_crime$Region with max_crime$Value crimes.
# The region with the lowest number of crimes is:
# min_crime$Region with min_crime$Value crimes.

#Q8
# Extract the subset of data for the region with the highest number of crimes
highest_crime_subset <- london_crime[london_crime$Region == max_crime$Region, ]

# Extract the subset of data for the region with the lowest number of crimes
lowest_crime_subset <- london_crime[london_crime$Region == min_crime$Region, ]

# Comment the major crime categories for both subsets
# For example, you can use the table() function to summarize:
highest_crime_category <- table(highest_crime_subset$MajorCategory)
lowest_crime_category <- table(lowest_crime_subset$MajorCategory)

# Using the resulting tables to add comments about the major crime categories


#Q9
# Set the plotting area to a 1x2 layout
par(mfrow=c(1, 2), mar=c(5, 4, 4, 2) + 0.1)

# Find the maximum height for the y-axis
max_height <- max(c(highest_crime_category, lowest_crime_category))

# Plot for the region with the highest number of crimes
barplot(highest_crime_category, main=paste('Major Crimes in', max_crime$Region), 
        ylim=c(0, max_height), las=2, cex.names=0.8)
# Add this line if you want to display the counts above bars
# text(x = bar_positions, y = highest_crime_category, labels = highest_crime_category, pos = 3)

# Plot for the region with the lowest number of crimes
barplot(lowest_crime_category, main=paste('Major Crimes in', min_crime$Region), 
        ylim=c(0, max_height), las=2, cex.names=0.8)
# Add this line if you want to display the counts above bars
# text(x = bar_positions, y = lowest_crime_category, labels = lowest_crime_category, pos = 3)

# Reset the plotting area to default
par(mfrow=c(1, 1))


#Q10

# Save the modified dataframe
write.csv(london_crime, "london-crime-modified.csv", row.names = FALSE)

# The part about syncing with GitHub cannot be directly done through R.
# I used Git commands in the terminal to add, commit, and push the changes to the remote repository.