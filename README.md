# The Opportunity Project - 2020 Earth Sprints (September - November 2020)

## Contributors:
Chace Paulson (cp5864a@student.american.edu), Minh-Tuan Nguyen (mnguyen@wcl.american.edu) and Shalini Ramachandra (sr6890a@student.american.edu)

## Advisory team: 
Maria Barouti, Sr Professional Lecturer Mathematics & Statistics (barouti@american.edu)

Zois Boukouvalas, Assistant Professor Mathematics & Statistics (boukouva@american.edu)

Konstantinos Koukoulakis (kkoukoulakis@chem.uoa.gr)

## The Opportunity Project
Brings together government agencies, user advocates, data stewards, product advisors, and students to solve problems facing the community.
The teams of students design, develop and launch products, utilizing federal open data 
 
## Helping the public
The specific problem statement AU participated in was designed by the Environment Protection Agency.
Goal was to create a tool that would help communities to understand the effects of transportation on air quality, health, and emissions.
 
## User Research 
Our team engaged in user research to better understand the how the problem statement affects communities and what they would be looking for in a tech tool.
 
## End User and Concept 
After user research, our team began to work on deciding who our end user would be and what specific concept we wanted for our tool
 
## End User
One of our team members had reached out to the Department of Energy and Environment in DC to get a better understanding of the air quality sensors in DC.
Essentially, air sensors are divided into two types (government and third party).
Only five AirNow sensors, which are run by the EPA, in DC and one slightly outside in Arlington.
We lookinged into purple air sensors to see if they were a viable alternative and if there were additional sensor data we could use that was not open data.
Our contact at DOE DC, provided more detail on the sensors in the area and mentioned a project he was working on to develop a long term data set of fine scale traffic data to approximate vehicle miles traveled and emissions.
 
## The concept 
The goal then - was to create a tool that could automatically pull in data, create a hub with in, and run a platform for visualization and data exploration for the end user
To enabling users to explore the relationship between transportation and air quality locally, at both a granular and citywide level 
 
## AirMotion DC
Gather and clean the data 
 
### TomTom - Citywide 
Two sides to our data collection - citywide levels and location specific data.

For citywide traffic data, we utilized a process of webscraping on the live traffic map for DC at TomTom.

Data points like congestion level, number of jams, length of jams, etc.

To scrape this data, we “inspected” the page and grabbed the fetch key from the network tab.
 
### AirNow 
Taking the 166 date times that come in with the traffic data scrape (i.e., current time to exactly one week ago)
We replace the date shell in the URL frame with these 166 dates in the XXX spots as shown in the middle
Then run a for loop to pull the data from those URLs into a list
Unlist the data and merge it by date with the traffic data 
The six sensor locations the data pulls for is shown on the right and the tidied data in the middle 
 
### OpenDataDC
In order to pull for specific locations - we have to first choose those locations. To do that - we used Open Data DC’s street segment data which contained locations for 13,676 segments across the district. We then randomly sampled these segments to create a data set of 106 locations. This is the max number of free API pulls we are allowed from TomTom per hour.

To make sure we weren’t over representing on location such as downtown, we proportionally sampled the four quadrants based on their relative number of appearances in the data set.

### DC GIS Master Address Repository
To include more useful location information then just lat/long and one address, we webscraped from the DC GIS Master Address Repository for info like ward and quadrant.

Using the XML page link - which appears like the middle picture - we were able to create a shell structure from which to pull this information for the 106 addresses.

We replaced the location in the URL frame, we then used a for loop to pull data, turn into a list, sort, and export.
 
### TomTom - Location Search 
Using these locations, then, we could gather the data for the location specific search.

We used a similar location shell URL frame and replaced the latitude and longitude in the shell then ran a for loop to pull the data, turn it into a list, and sort it into a data frame.

To merge with the air quality data - we found the absolute value of the difference between the latitude and longitude of the traffic data and the six sensors and selected the sensor with the lowest number for each of the four parameters.
 
### Open Weather/Automation 
Lastly we added in hourly citywide weather data from Open Weather. 

In order to automate this process, we utilized cronR package to schedule our R script to run through every hour, on the hour.

Given the structure of the citywide traffic data (i.e., includes one full week of data), it is more robust then location search data as it can pick up missing hours when you log back onto the computer. Location search can only pull for the current time, and, thus, requires the computer to be active to scrape the data.
 
### Product Design 
After data wrangling, then, it was time to begin creating our actual application.

As previously mentioned - AirMotionDC compiles and analyzes real time data on traffic patterns, air pollution, and weather in the District of Columbia.

To explore the relationship between transportation and air quality locally, at both a granular and citywide level. the interface includes several features.
