library(here)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(ggthemes)

##################################################
# Time series plot of the COVID data             #
##################################################
covid_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
# Turn the data into a longer format
covid19 <- covid_raw %>%
           pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long),
                        names_to = "Date",
                        values_to = "ConfirmedCases")
covid19 <- covid19 %>%
           select(-c(Lat, Long)) %>%
           rename(State = `Province/State`,
                  Country = `Country/Region`) %>%
           mutate(Date = mdy(Date))
covid19_aggregated <- covid19 %>% group_by(Country, Date) %>%
                      summarise(ConfirmedCases = sum(ConfirmedCases, 
                                                     na.rm = TRUE), 
                                .groups = "drop")

covid19_aggregated <- covid19_aggregated %>%
                      arrange(Date) %>%
                      group_by(Country) %>%
                      mutate(NewCases = ConfirmedCases - lag(ConfirmedCases, default = 0)) %>%
                      ungroup()


write.csv(covid19_aggregated, here::here("RCode", "Lecture-4",
                                         "Data", 
                                         "Covid19ConfirmedCases.csv"),
          row.names = FALSE)

######################################################################
# Covid-19 data                                                      #
######################################################################
Cov19Data <- read.csv(here::here("RCode", "Lecture-4",
                                  "Data", 
                                  "Covid19ConfirmedCases.csv"))
glimpse(Cov19Data)

Cov19Data <- Cov19Data %>% mutate(Date = dmy(Date))

glimpse(Cov19Data)

########################################################################
Cov19USA <- Cov19Data %>% filter(Country == "US")
covidUSA_baseplt <- Cov19USA %>% ggplot(aes(Date, NewCases)) +
             geom_line() +
             theme_economist() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text = element_text(face="bold")) +
  labs(y = "Number of new cases")+
             ggtitle("US daily cases of Covid-19")

pdf(here::here("Lecture-4", "PlotsLec4", "covidUSA_baseplt.pdf"), 
    height=4, width=6)
covidUSA_baseplt
dev.off()

Covid19_US_2 <- covidUSA_baseplt +
                scale_x_date(date_breaks = "2 month",
                             date_labels = "%b %y") +
                theme(axis.text.x = element_text(size=8),
                      axis.title.x = element_blank())
pdf(here::here("Lecture-4", "PlotsLec4", "Covid19_US_2.pdf"), 
    height=4, width=6)
Covid19_US_2
dev.off()

Covid19_US_3 <- covidUSA_baseplt +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  theme(axis.text.x = element_text(size=8),
        axis.title.x = element_blank())
pdf(here::here("Lecture-4", "PlotsLec4", "Covid19_US_3.pdf"), 
    height=4, width=6)
Covid19_US_3
dev.off()

Cov19USA_Aus <- Cov19Data %>% filter(Country %in% c("US", "Australia"))

Cov19USA_Ausplt <- Cov19USA_Aus %>% 
  ggplot(aes(Date, NewCases)) +
  geom_line() +
  facet_wrap(~Country, nrow=2) +
  theme_wsj() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text = element_text(face="bold")) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b %y") +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size=20),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        strip.text = element_text(size=15)) +
  labs(y = "Number of new cases",
       title = "Comparison of daily Covid-19 new cases")
       
pdf(here::here("Lecture-4", "PlotsLec4", "Cov19USA_Ausplt.pdf"), 
    height=6, width=8)
Cov19USA_Ausplt
dev.off()

Cov19USA_Ausplt2 <- Cov19USA_Aus %>% 
  ggplot(aes(Date, NewCases)) +
  geom_line() +
  facet_wrap(~Country, nrow=2, scales = "free_y") +
  theme_wsj() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text = element_text(face="bold")) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b %y") +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size=20),
        axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=12),
        strip.text = element_text(size=15)) +
  labs(y = "Number of new cases",
       title = "Comparison of daily Covid-19 new cases")

pdf(here::here("Lecture-4", "PlotsLec4", "Cov19USA_Ausplt2.pdf"), 
    height=6, width=8)
Cov19USA_Ausplt2
dev.off()

# geom_area()

Cov19USA_AusAreaplt <- Cov19USA_Aus %>% 
  ggplot(aes(Date, NewCases)) +
  geom_area(fill = "tan", color="black") +
  facet_wrap(~Country, nrow=2, scales = "free_y") +
  theme_wsj() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text = element_text(face="bold")) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b %y") +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size=20),
        axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=12),
        strip.text = element_text(size=15)) +
  labs(y = "Number of new cases",
       title = "Comparison of daily Covid-19 new cases")

pdf(here::here("Lecture-4", "PlotsLec4", "Cov19USA_AusAreaplt.pdf"), 
    height=6, width=8)
Cov19USA_AusAreaplt
dev.off()
######################################################################
covid19_aus <- covid19_aggregated %>%
               filter(Country == "Australia")



pdf(here::here("Lecture-4", "PlotsLec4", "CovidSeries.pdf"), 
                height=4, width=6)
covid19_aus %>% ggplot(aes(Date, NewCases)) + 
                geom_line(color="blue") +
                theme_classic() +
                ggtitle("Daily new COVID cases in Australia") +
                labs(x = "Date",
                     y = "Number of new cases") +
                theme(text = element_text(size=15,
                                          family = "serif",
                                          face = "bold"))
dev.off()
#########################################
###############################
# Try direct use of ISO 8601  #
###############################
date1 <- 2021-02-11
date1
###############################
# Will quoting help?          #
###############################
date2 <- "2021-02-11"
date2
class(date2)
###############################
# Use as.Date() function      #
###############################
date3 <- as.Date("2021-02-11")
date3
class(date3)
str(date3)

as.Date("2021-08-16") > as.Date("2021-08-01")
as.Date("2021-08-16") + 1
as.Date("2022-08-16")-as.Date("2021-08-16")

# Plotting with Date objects
Date <- c(as.Date("2021-06-01"), 
          as.Date("2021-07-01"), 
          as.Date("2021-08-01"))
Price <- c(50, 200, 100)
class(Date)
data <- data.frame(Date, Price)
pdf(here::here("Lecture-4", "PlotsLec4", "TimeSeries.pdf"), height=4, width=6)
ggplot(data, aes(Date, Price)) + geom_line() + geom_point() + theme_classic()+
  theme(text = element_text(size=20))
dev.off()
#################################
# Example of using as.POSIXct() #
#################################
dtmString <- "2021-02-11 18:30:00"
dtm <- as.POSIXct(dtmString)
class(dtm)
dtm
as.POSIXct("2021-02-11 18:30:00Z")

as.POSIXct("2021-02-11 18:30:00Z", 
           tz = "UTC")
######################################
# Datetime behaves well

# Defining two Datetime strings
dtmString1 <- "2021-02-11 18:00:00Z"
dtmString2 <- "2021-02-11 17:00:00Z"
# Create Datetime objects
dtm1 <- as.POSIXct(dtmString1, tz = "UTC")
dtm2 <- as.POSIXct(dtmString2, tz = "UTC")
# Comparing Datetime
dtm2 < dtm1
# Difference between two Datetimes
dtm1 - dtm2
# Add time (seconds) to Datetime
dtm1 + 3600
dtm1 + 86400

# Hourly series
Time <- c(as.POSIXct("2021-02-11 18:00:00"),
          as.POSIXct("2021-02-11 19:00:00"),
          as.POSIXct("2021-02-11 20:00:00"))
Price <- c(100, 250, 320)
data <- data.frame(Time, Price)
pdf(here::here("Lecture-4", "PlotsLec4", "HourlySeries.pdf"), 
    height=4, width=6)
ggplot(data, aes(Time, Price)) +
      geom_line() +
      geom_point() +
      theme_classic() +
  ggtitle("Example of an hourly timeseries") +
      theme(text = element_text(size=15))
dev.off()
#############################################################
# Key lubridate functions                                   #
#############################################################
# Use ymd() for Year, Month, Day format
ymd("2021-02-11")
# Use dmy() for Day, Month, Year format
dmy("11/2/2021")
# Use parse_date_time() for general formats
parse_date_time(c("Feb 2nd, 2021",
                  "2nd Feb 2021"),
                order = c("mdy", "dmy"))
################################################
# Define a date object
date <- dmy("2nd Feb 2021")
# Extract year
year(date)
# Extract month
month(date)
# Extract day of the year
yday(date)
# Extract day of the week
wday(date)
###################################################
# World Map                                       #
###################################################
library(mapdata)
WorldMap <- map_data("world")
class(WorldMap)
head(WorldMap)
glimpse(WorldMap)
glimpse(Cov19Data)
Cov19Data2 <- Cov19Data %>%
              mutate(Date = dmy(Date)) %>%
              filter(Date == "2021-08-01")
table(Cov19Data2$Country)
WorldMap2 <- WorldMap %>% rename(Country = region)
glimpse(WorldMap2)
table(WorldMap2$Country)
WorldMap3 <- WorldMap2 %>%
             left_join(Cov19Data2, by = "Country")
WorldMap3$ConfirmedCases[is.na(WorldMap3$ConfirmedCases)] <- 0
#################################################
glimpse(WorldMap)
# Plot of World map
pdf(here::here("Lecture-4", "PlotsLec4", "WorldMap.pdf"), 
    height=4.5, width=8)
ggplot(WorldMap, aes(long, lat)) +
  geom_polygon(aes(group = group),
               fill="gray90",
               color = 'blue') +
  theme_void()
dev.off()
####################################################
# Filtering world map
####################################################
table(WorldMap$region)
indoChina <- WorldMap %>% 
  filter(region %in% c("India",
                        "China"))
pdf(here::here("Lecture-4", "PlotsLec4", "IndoChinaMap.pdf"), 
    height=5, width=8)
ggplot(indoChina, aes(long, lat)) +
  geom_polygon(aes(group = group),
               fill = "gray90",
               color = "black") +
  theme_void()
dev.off()
################################################
# USA states map                               #
################################################
usaMap <- map_data("state")
str(usaMap)
pdf(here::here("Lecture-4", "PlotsLec4", "UsaMap.pdf"), 
    height=5, width=8)
ggplot(usaMap, aes(long, lat)) +
  geom_polygon(aes(group = group),
               fill = "gray90",
               color = "black") +
  theme_void()
dev.off()
##############################
states <- unique(usaMap$region)
states
library(dslabs)
data(murders)
murders$region <- str_to_lower(murders$state)
murders
#################################
set.seed(123)
qualVar <- sample(LETTERS[1:5], 49, replace=TRUE)
set.seed(3011)
quantVar <- runif(49, 0, 25)
dataForUSMap <- data.frame(region = states,
                           QualVar = qualVar,
                           QuantVar = quantVar)
usaMapMerged <- usaMap %>%
                left_join(dataForUSMap,
                          by = "region")
usaMapPlt <- 
usaMapMerged %>%
  ggplot(aes(long, lat,
             group = group,
             fill = QualVar)) +
  geom_polygon(color = "black") +
  theme_void() +
  theme(legend.position = "top")

pdf(here::here("Lecture-4", "PlotsLec4", "UsaChoroplethQual.pdf"), 
    height=5, width=8)
usaMapPlt
dev.off()

usaMapPlt2 <- usaMapPlt +
              scale_fill_brewer(palette = "Spectral")
pdf(here::here("Lecture-4", "PlotsLec4", "UsaChoroplethQual2.pdf"), 
    height=5, width=8)
usaMapPlt2
dev.off()
##############################################
usaMapQuantPlt <- 
  usaMapMerged %>%
  ggplot(aes(long, lat,
             group = group,
             fill = QuantVar)) +
  geom_polygon(color = "white") +
  theme_void() +
  theme(legend.position = "top",
        legend.text = element_text(size=12),
        legend.title = element_text(size=15)) +
  guides(fill = guide_legend(override.aes = 
                               list(size = 15)))

pdf(here::here("Lecture-4", "PlotsLec4", "UsaChoroplethQuant.pdf"), 
    height=5, width=8)
usaMapQuantPlt
dev.off()

# Palette of 30 colors
library(RColorBrewer)
my_colors <- brewer.pal(9, "Reds") 
my_colors <- colorRampPalette(my_colors)(30)

usaMapQuantPlt2 <- 
  usaMapMerged %>%
  ggplot(aes(long, lat,
             group = group,
             fill = QuantVar)) +
  geom_polygon(color = "white") +
  theme_void() +
  scale_fill_gradientn(colors = my_colors) +
  theme(legend.position = "top",
        legend.text = element_text(size=12),
        legend.title = element_text(size=15)) +
  guides(fill = guide_legend(override.aes = 
                               list(size = 15)))

pdf(here::here("Lecture-4", "PlotsLec4", "UsaChoroplethQuant2.pdf"), 
    height=5, width=8)
usaMapQuantPlt2
dev.off()
################################################
usaMapMurderMerged <- usaMap %>%
                      left_join(murders,
                                by = "region")
myPalette <- terrain.colors(25)
#myPalette2 <- colorRampPalette(myPalette)(30)

usaMapMurderPlt <- 
  usaMapMurderMerged %>%
  ggplot(aes(long, lat,
             group = group,
             fill = total)) +
  geom_polygon(color = "black") +
  theme_void() +
  scale_fill_gradientn(colors = my_colors) +
  theme(legend.position = "top",
        legend.text = element_text(size=12),
        legend.title = element_text(size=15)) +
  guides(fill = guide_legend(override.aes = 
                               list(size = 15)))
pdf(here::here("Lecture-4", "PlotsLec4", "UsaMapMurderPlt.pdf"), 
    height=5, width=8)
usaMapMurderPlt
dev.off()

