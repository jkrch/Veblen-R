library(tidyverse)
library(ggpmisc)

# Read CSV file with working hours
# Source: https://stats.oecd.org/Index.aspx?DataSetCode=ANHRS#
df_hours <- read.csv("oecd_stat_hours_worked.csv", header = TRUE,
                     sep = ",")

# Glimpse of data
df_hours %>% glimpse()

# Show all countries
unique(df_hours$Country)

# Show all years
unique(df_hours$Time)

# Show all employment status
unique(df_hours$Employment.status )

# Specify countries
countries <- c('Belgium', 'Canada', 'France', 'Germany',
               'Italy', 'Netherlands', 'Norway', 'Sweden',
               'United Kingdom', 'United States')

# Specify years
years <- c(2000:2020)

# Select rows for total employment
df_hours <- df_hours[df_hours$EMPSTAT %in% c('TE'),]

# Selct specific columns
df_hours = df_hours[c('Country', 'Time', 'Value')]

# Rename Value column
colnames(df_hours)[3] = 'Hours_worked'

# Select rows for specific countries
df_hours <- df_hours[df_hours$Country %in% countries,]

# Select rows for specific years
df_hours <- df_hours[df_hours$Time %in% years,]

# Show dimension of dataframe
dim(df_hours)

# Change plot size in notebook
options(repr.plot.width=10, repr.plot.height=7)

# Plot time to working hours
ggplot(data=df_hours, aes(x=Time, y=Hours_worked, group=Country)) +
    geom_point(aes(color=Country)) + geom_line(aes(color=Country))

# Read CSV file with earnings inequalities
# Source: https://stats.oecd.org/Index.aspx?DataSetCode=DEC_I#
df_ratio <- read.csv("oecd_stat_decile_ratios.csv", header = TRUE, sep = ",")

# Glimpse of data
df_ratio %>% glimpse()

# Show all unit codes
unique(df_ratio$Unit.Code)

# Show all sexes
unique(df_ratio$SEX)

# Select rows for P9050 in SERIES column
df_ratio <- df_ratio[df_ratio$SERIES %in% c("P9050"),]

# Select rows for MW in SEX column
df_ratio <- df_ratio[df_ratio$SEX %in% c("MW"),]

# Selct specific columns
df_ratio = df_ratio[c('Country', 'Time', 'Value')]

# Rename column
colnames(df_ratio)[3] = 'P90P50'

# Select rows for specific countries
df_ratio <- df_ratio[df_ratio$Country %in% countries,]

# Select rows for specific years
df_ratio <- df_ratio[df_ratio$Time %in% c(2000:2020),]

# Dimension of data set
dim(df_ratio) 

# Delete rows in Hours_worked where P90P50 is missing
df_all <- df_hours[paste(df_hours$Country, df_hours$Time) %in% paste(df_ratio$Country, df_ratio$Time), ]

# Add P90P50
df_all <- cbind(df_all, df_ratio[3])

# Rearange columns
df_all = df_all[, c(1, 2, 4, 3)]

# Dimension of data
dim(df_all)

# Plot inequality earning to working hours
ggplot(data=df_all, aes(x=P90P50, y=Hours_worked)) +
    geom_point(aes(colour=Country))

# Create data set with country means
df_means = aggregate(df_all[, 3:4], list(df_all[, 1]), FUN=mean)
df_means

# Rename Country column
colnames(df_means)[1] = "Country"

# Plot inequality earning to working hours and country means
ggplot(data=df_all, aes(x=P90P50, y=Hours_worked)) +
    geom_point(aes(colour=Country)) +
    geom_point(data=df_means, aes(colour=Country), shape="star",
               show.legend = FALSE)

# Plot inequality earning to working hours and country means and linear 
# regression
ggplot(data=df_all, aes(x=P90P50, y=Hours_worked)) +
    geom_point(aes(colour=Country)) +
    geom_point(data=df_means, aes(colour=Country), shape="star",
               show.legend = FALSE) + 
    geom_smooth(method='lm', se=FALSE, color="black", formula = y ~ x)

# Plot inequality earning to working hours and country means and polynomial 
# regression and R2 score
my.formula = y ~ poly(x, 3)
ggplot(data=df_all, aes(x=P90P50, y=Hours_worked)) +
    geom_point(aes(colour=Country)) +
    geom_point(data=df_means, aes(colour=Country), shape="star",
               show.legend = FALSE) + 
    geom_smooth(method='lm', se=FALSE, color="black", formula = my.formula) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE)

# Get countries and country codes from hours worked dataset
df_countries <- read.csv("oecd_stat_hours_worked.csv", header = TRUE, sep = ",")[1:2]

# Remove all duplicate rows
df_countries <- df_countries[!duplicated(df_countries), ]

# Select rows for specific countries
df_countries <- df_countries[df_countries$Country %in% countries,]

# Drop levels
df_countries <- droplevels(df_countries)

# Remove row names
rownames(df_countries) <- NULL

# Show dataframe
df_countries

# Reduce dataframe to specific countries and years
reduce_df <- function(df){
    
    # Select countries by code
    df <- df[df$LOCATION %in% df_countries$COUNTRY,]

    df
    # Selct specific columns
    df <- df[c('LOCATION', 'TIME', 'Value')]

    # Drop levels
    df <- droplevels(df)

    # Replace country code with country names
    levels(df$LOCATION) = countries
    
    # Rename columns
    colnames(df) = c('Country', 'Time', 'Value')
    
#     # Select years
    df <- df[df$Time %in% years,]
 
    return(df)
}

# Get dataframe with GDP
get_df_gdp <- function(){

    # Get data from https://data.oecd.org/ 
    url <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.GDP.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en"
    df <- read.csv(url, header = TRUE, sep = ",")
    
    # Select USD per capita
    df <- df[df$MEASURE %in% c("USD_CAP"),]
    
    # Reduce dataframe
    df <- reduce_df(df)
    
    # Rename Value column
    colnames(df)[3] = 'GDP'

    return(df)
}

# Get dataframe with unemployment rate
get_df_unemploy <- function(){

    # Get data from https://data.oecd.org/ 
    url <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.HUR.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en"
    df <- read.csv(url, header = TRUE, sep = ",")
    
    # Select total sex
    df <- df[df$SUBJECT %in% c("TOT"),]
    
    # Select annualy
    df <- df[df$FREQUENCY %in% c("A"),]
    
    # Reduce dataframe
    df <- reduce_df(df)
    
    # Rename Value column
    colnames(df)[3] = 'Unemployment_rate'

    return(df)
}

# Get GDP
df_gdp <- get_df_gdp()

# Get unemployment rate
df_unemploy <- get_df_unemploy()

# Create dataframe with all years and countries
df_all <- expand.grid(years, countries)

# Rearrange columns
df_all <- df_all[, c(2, 1)]

# Rename columns
colnames(df_all) = c("Country", "Time")

# Create column with country year comibnations
# df_all$CountryTimn <- paste(df_hours$Country, df_hours$Time)

# # Add all dataframe values
# for (df in list(df_hours)) {
#   df_all <- df[paste(df$Country, df$Time) %in% paste(
#     df_all$Country, df_all$Time), ]
# } 

# Create copy of df_hours
df_all <- cbind(df_hours)

# Iterate over all other dataframes
for (df in list(df_ratio, df_gdp, df_unemploy)) {
    
    # Remove all rows in df_all that not exist in df
    df_all <- df_all[paste(df_all$Country, df_all$Time) %in% paste(
        df$Country, df$Time), ]
    
    # Remove all rows in df that not exist in df_all
    df <- df[paste(df$Country, df$Time) %in% paste(
        df_all$Country, df_all$Time), ]
    
    # Combine df_all with the value column of df
    df_all <- cbind(df_all, df[3])
} 

# Show dataframe
df_all


