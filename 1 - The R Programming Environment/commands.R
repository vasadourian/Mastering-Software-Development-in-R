# question 1
# What is average Arithmetic.Mean for “Bromine PM2.5 LC” in the state of Wisconsin in this dataset?
monitors <- read_csv("/home/vicken/COURSES/Coursera/Software Dev in R/1 - /week 4/data/daily_SPEC_2014.csv.bz2", progress=interactive())
# answer: 0.003960


# question 2
# Calculate the average of each chemical constituent across all states, monitoring sites and all time points.
monitors %>%
	group_by(`Parameter Name`) %>%
	summarise(mean = mean(`Arithmetic Mean`, na.rm=TRUE)) %>%
	arrange(desc(mean))

# A tibble: 92 x 2
#                       Parameter Name       mean
#                                <chr>      <dbl>
#1            Sample Max Baro Pressure 744.632642
#2                Sample Baro Pressure 739.370108
#3            Sample Min Baro Pressure 738.363876
#4      OC CSN Unadjusted PM2.5 LC TOT  67.783826


# question 3
# Which monitoring site has the highest average level of “Sulfate PM2.5 LC” across all time? Indicate the state code, county code, and site number.

monitors %>%
	filter(`Parameter Name`=="Sulfate PM2.5 LC") %>%
	group_by(`State Code`, `County Code`, `Site Num`) %>%
	summarise(mean_sulfate = mean(`Arithmetic Mean`, na.rm=TRUE)) %>%
	arrange(desc(mean_sulfate))
# answer: State 39 County 081 Site 0017


# question 4
# What is the absolute difference in the average levels of “EC PM2.5 LC TOR” between the states California and Arizona, across all time and all monitoring sites?
monitors %>% 
	filter(`State Name` %in% c("California", "Arizona"), `Parameter Name`=="EC PM2.5 LC TOR") %>%
	group_by(`State Name`) %>%
	summarise(mean_ec = mean(`Arithmetic Mean`, na.rm=TRUE))

abs(state_ec_avgs$mean_ec[1]-state_ec_avgs$mean_ec[2])
#[1] 0.01856696

# question 5
#What is the median level of “OC PM2.5 LC TOR” in the western United States, across all time? Define western as any monitoring location that has a Longitude LESS THAN -100. 
monitors %>%
	filter(Longitude < -100, `Parameter Name`=="OC PM2.5 LC TOR") %>%
	group_by(`Parameter Name`) %>%
	summarise(median = median(`Arithmetic Mean`))
#[1] 0.43


# question 6
#Use the readxl package to read the file aqs_sites.xlsx into R (you may need to install the package first). This file contains metadata about each of the monitoring sites in the EPA’s monitoring system. In particular, the "Land Use" and "Location Setting" variables contain information about what kinds of areas the monitors are located in (i.e. “residential” vs. “forest”).

#How many monitoring sites are labelled as both RESIDENTIAL for "Land Use" and SUBURBAN for "Location Setting"?
length(which(aqs$`Location Setting` == "SUBURBAN" & aqs$`Land Use`=="RESIDENTIAL"))
#[1] 3527


# question 7
#What is the median level of “EC PM2.5 LC TOR” amongst monitoring sites that are labelled as both “RESIDENTIAL” and “SUBURBAN” in the eastern U.S., where eastern is defined as Longitude greater than or equal to -100?

# merge the two dataframes aqs and monitors, by column Longitude
total <- merge(monitors, aqs, by="Longitude")

total %>% 
	filter(Longitude>= -100 , `Location Setting` == "SUBURBAN" & `Land Use`=="RESIDENTIAL",`Parameter Name` == "EC PM2.5 LC TOR") %>%
	summarise(median(`Arithmetic Mean`, na.rm=TRUE))
	median(`Arithmetic Mean`, na.rm = TRUE)
#1                                    0.61

# question 8
#Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use", which month of the year has the highest average levels of "Sulfate PM2.5 LC"?

# created a column for the month. Extracted the month from the column `Date Local`
total$`Month Local` <- format(as.Date(total$`Date Local`), "%m")

total %>% filter(`Land Use`=="COMMERCIAL" & `Parameter Name` == "Sulfate PM2.5 LC") %>%
	group_by(`Month Local`) %>%
	summarise(mean=mean(`Arithmetic Mean`, na.rm=TRUE)) %>%
	arrange(desc(mean))
# A tibble: 12 x 2
#   Month Local     mean
#         <chr>    <dbl>
#1           02 2.021325


# question 9

total %>%
     filter(`State Code.x` == "06", `County Code.x` == "065", `Site Num` == "8001",
            `Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
     group_by(`Parameter Name`, `Date Local`) %>%
     select(`State Code.x`, `County Code.x`, `Site Num`, `Date Local`, `Parameter Name`,
            `Arithmetic Mean`) %>%
     summarise(avg = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
     group_by(`Date Local`) %>%
     summarise(Total = sum(avg, na.rm = TRUE)) %>%
     filter(Total > 10)


# question 10

total %>%
    filter(`Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
    group_by(`State Code.x`, `County Code.x`, `Site Num`, `Parameter Name`, `Date Local`) %>%
    select(`State Code.x`, `County Code.x`, `Site Num`, `Date Local`, `Parameter Name`,
           `Arithmetic Mean`) %>%
    summarise(avg = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
    spread(`Parameter Name`, avg) %>%
    group_by(`State Code.x`, `County Code.x`, `Site Num`) %>%
    summarise(correlation = cor(`Sulfate PM2.5 LC`, `Total Nitrate PM2.5 LC`)) %>%
    arrange(desc(correlation))


