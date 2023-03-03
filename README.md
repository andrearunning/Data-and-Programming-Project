# Data and Programming for Public Policy II - R Programming
## PPHA 30536
 
# Final Project: Reproducible Research
# Andrea Running

Public opinion of the police became a popular media topic in the United States after the murder of George Floyd in spring of 2020.  Media coverage of an anti-police movement amongst portions of the population became prevalent, and protests were held in many major and minor cities in the United States.  These protests included many blatantly anti-police actions, as police department effectiveness was called into question. My project aimed to see if people are more or less likely to see police stations in major cities as a deterrent when committing shootings in 2019 (before the popularized anti-police movement) versus in 2022 (after the popularized anti-police movement).  

Text analysis fed my interest of the topic.  I wanted to find articles that described the sentiment towards police departments before and after George Floyd’s murder.  To be most efficient, I focused on Chicago.  I was able to find articles scraped from the web to share sentiment.  These articles show a changing sentiment over time which piqued my interest. I used articles that described a policy change in order to determine if the public was seeing the proposed change as positive or negative act- were citizens reacting positively to the change, is the content phrased to show police in a positive light? Or, were citizens demanding change and are the articles an outcry for reform?

To answer my research question, I wanted to visualize the shootings that occurred in three cities (New York, Chicago, and Los Angeles) between two years (2019 and 2022).  An initial problem I ran into was finding datasets that corresponded to one another between the three cities I was researching.  New York City and Chicago data organization separates crimes using firearms from other crimes, which made the datasets easy to download and clean based on the dates needed.  Los Angeles, however, has all historic crime data in one dataset.  I needed to download the large dataset and subset based only on crimes involving firearm discharge.  Once the six shooting datasets were created (two per city), I next needed to visualize the data as a shapefile and plot them along with the police stations and precincts.  This was another issue of corresponding datasets, as Chicago and LA provided datasets with their police station locations and New York City did not.  I was able to find a table on New York PD’s website that provided addresses for each station, which I then put into google maps to find the corresponding coordinates.  I manually inputted the coordinates into the table scraped from NYPD’s webpage.

My static maps help readers visualize the difference in shooting locations compared to police stations. I wished to help readers visualize this further by providing options for comparison in the shiny app.  I created an app initially which displayed only one city with an interactive option to display either police stations or shootings, or police stations and shootings.  This was helpful to see one map clearly but lacked the comparative aspect necessary for my analysis.  The final iteration of the app I found most useful was a two-column user interface to allow for two outputs with different defaults to encourage readers to compare graphs. 

I fit my model to a t test.  I chose to calculate the distance between shooting locations in each year and their closest police station.  Finding these distances proved to be somewhat difficult at first, but when I discovered the st_nearest_feature() command, we were able to create new datasets with distance between shooting incidences and the closest station with just one function for each city and year pair. Our null hypothesis for the t test is that there is no significant change in mean distance between shooting incidences and police stations in 2019 and 2022 for each city.  We used a 95% confidence interval.  When we run the test, we find the p-values for New York and Chicago are insignificant, and we fail to reject the null hypothesis.  In LA, however, our p-value is .01, which is significant.  We can reject the null hypothesis for Los Angeles.   

A future extension of this project would include crime data outside of shootings.  It would be interesting to see if there is a difference in the test outcome if we use petty crimes rather than shootings.  The project could also be improved by taking several years before the George Floyd murder and finding the mean of those, and then doing the same after.
