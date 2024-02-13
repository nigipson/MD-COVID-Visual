#Trying to make animated graphic for Maryland Cases


devtools() #helps if your windows computer isn't downloading packages or data sets correctly

############ section 0: loading in libraries and datasets ############

library("tidyverse")
library("mapdata")
library("ggthemes")
library("gganimate")
library('Hmisc')
library("gifski")


county_cases <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
                         stringsAsFactors = F)
#Extract MD cases only
MD <- filter(county_cases, State == "MD") #its good to use filter() because it keeps item as a dataframe

#downloading lat and long county data
county_map <- map_data("county") #this is literally an internal dataframe maintained by R with longitude and latitude data for every county in USA
#it contains all the coordinates for a given region


MD_long_Lat <- filter(county_map, region == "maryland")
#MD_long_Lat2 <- filter(county_map, subregion == "baltimore city")


MD_long_Lat$subregion[MD_long_Lat$subregion == "baltimore city"]



str(MD)
str(MD_long_Lat)

MD$County.Name[2]
MD_long_Lat$subregion[3]



#I want to combine these data sets using county information. However, the way county is labeled for each is off. 
#Let's make sure they're written the same 

MD$County.Name <- tolower(MD$County.Name) #to make text lowercase 
MD$County.Name <- str_remove(MD$County.Name, " county") #to remove text with a space
MD$County.Name <- str_remove(MD$County.Name, "[.]") #to remove punctuation... make sure you enclose in brackets
MD$County.Name <- str_remove(MD$County.Name, "'")
MD$County.Name <- trimws(MD$County.Name)  #to remove white spaces



colnames(MD_long_Lat)[6] <- c("County.Name")


MD$County.Name[2]
MD_long_Lat$County.Name[3]
MD$County.Name[2] == MD_long_Lat$County.Name[3]

MD$County.Name[25]
MD_long_Lat$County.Name[1095]
MD$County.Name[25] == MD_long_Lat$County.Name[1095]

MD$County.Name[17]
MD_long_Lat$County.Name[663]
MD$County.Name[17] == MD_long_Lat$County.Name[663]

#merging the datasets
MD_merge <- merge(MD, MD_long_Lat, by = "County.Name", all = T, sort = F) #This is where we merge the two datasets
MD_merge <- MD_merge[order(MD_merge$order),] #in order to create image of state we need to order lines appropriately


#wide to long format

MD_date <- pivot_longer(MD_merge, cols = starts_with("X"), names_to = "Date", values_to = "Cases")
#we are changing our wide format data frame to a long format dataframe
#We are using the function starts_with(), because all of the dates start with "X." When working with another data from you would have
#to figure out a way to specify... maybe call out the column number in brackets... probably easier when changing long to wide
#names_to() allows you to give a column name to your condensed columne
#values_to allows you to give a name to the column that will now house the values originally under our now merged columnns

MD_date$Date <- str_remove(MD_date$Date, "X")

MD_date$Date <- gsub("[.]", "-", MD_date$Date) #gsub is a cool funtion to replace text... gsub(item to remove, replace with, data$column)

MD_date$Date <- as.Date(MD_date$Date) #convert to date

#MD_date[,7:9] = NULL

MD_final <- MD_date

MD_final$Cases[is.na(MD_final$Cases)] <- 0

MD_final <- MD_final[complete.cases(MD_final),]

MD_final <- MD_final[MD_final$Date > "2020-3-01",] 


#############Things to add to the graph
## needed to create an extra dataset to reference in the ggplot to label our map with county names
## this dataset creates a mean point within the county lat/long to pin the county name label
#it's a VERY simplified data frame 
cnames <- aggregate(cbind(long, lat) ~ County.Name, data = MD_final,
                    FUN = function(x)mean(range(x)))

cnames$County.Name <- capitalize(cnames$County.Name)


cnames[cnames$County.Name == "St marys", 1] <- "St. Mary's"
cnames[cnames$County.Name == "Prince georges", 1] <- "Prince George's"
cnames[cnames$County.Name == "Queen annes", 1] <- "Queen Anne's"


total_cases <- as.character(sum(MD[,length(MD)])) #calculates the total number of cases and converts to character format
total_title <- paste("Total Confirmed Cases in MD:", total_cases)  #storing the full character string


######Making my ggplot

#Base plot
layer1 <- ggplot() 
layer1


#Sizing plot and creating space
layer2 <- layer1 + coord_fixed(1.3) #So for every 1 unit on the X axis it'll be 1.3 units on the y axis... this doesn't work for MD
layer2

#INSTEAD
layer2 <- layer1 + coord_fixed(.95)  #allows us to manipulate the aspect ratio of the coordinates, increasing makes the y axis units longer than the x
layer2


#Add the map! 
layer3 <- layer2 + geom_polygon(data = MD_final, mapping = aes(x = long,
                                                               y = lat,
                                                               group = group,
                                                               fill = Cases),
                                color = "black") #black lines
layer3


#Play with some colors!
layer4 <- layer3 + scale_fill_gradientn(limits = c(0, max(MD_final$Cases)),
                                       colours=c("white", "yellow", "red", "darkred"))

layer4

#Remove background
layer5 <- layer4 + theme_void()

layer5


#adding title, caption and subtitle
layer6 <- layer5 + labs(title = 'Number of COVID-19 Cases on {as.character(frame_time, format = "%m-%d-%y")}',
                        caption = 'Data source: usafacts.org',
                        subtitle = total_title)

layer6


#Adding county labels
layer7 <- layer6 + geom_text(data = cnames, aes(long, lat, label = County.Name, group = NULL), size=3)

layer7

#You can also replace geom_label with geom_text to remove the boxes around county names 


#adjust the font sizes and positions of the text elements on the graph
layer8 <- layer7 + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
                         plot.subtitle = element_text(hjust = 0.5, size = 13), 
                         plot.caption = element_text(hjust = 0.95, size = 13),
                         legend.title=element_text(size=17), 
                         legend.text=element_text(size=15), 
                         legend.position = c(0.3, 0.2))
layer8

#here is where we use gganimate to cycle through the dates!
layer9 <- layer8 + transition_time(as.Date(Date))
layer9




graphic <- ggplot() +
  coord_fixed(.95) +
  geom_polygon(data = MD_final, mapping = aes(x = long,
                                              y = lat,
                                              group = group,
                                              fill = Cases),
               color = "black") +
  scale_fill_gradientn(limits = c(0, max(MD_final$Cases)),
                       colours=c("white", "lightyellow1", "yellow", "darkgoldenrod1", "darkgoldenrod3", "red", "darkred", "black")) +
  theme_void() +
  labs(title = 'Number of COVID-19 Cases on {as.character(frame_time, format = "%m-%d-%y")}',
       caption = 'Data source: usafacts.org',
       subtitle = total_title) +
  geom_text(data = cnames, aes(long, lat, label = County.Name, group = NULL), size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 13), 
        plot.caption = element_text(hjust = 0.95, size = 13),
        legend.title=element_text(size=17), 
        legend.text=element_text(size=15), 
        legend.position = c(0.3, 0.2)) +
  transition_time(as.Date(Date))

install.packages("transformr")
library(transformr)

animate(graphic, duration = 30, fps = 20, width = 900, height = 800, renderer = gifski_renderer())
anim_save("MDCOVID2_updated.gif")

animate(graphic, duration = 8, fps = 20, width = 900, height = 800, renderer = gifski_renderer())
anim_save("MDCOVIDshort2.gif")

