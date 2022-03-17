# Portfolio-2: Mass shooting in the US (2018 - 2021)

App: https://mishra37.shinyapps.io/portfolio-2/

## Motivation

Coming from India, I was extremely perplexed to see the fragile gun laws in the US and how so many people across the states owned tons of guns leading 
to higher rates of mass shooting. For this reason, I chose the dataset “Mass Shooting in the US (2018-2021)” to analyze which states in the US were safest 
to live in. I also wanted to examine how mass shooting and fatality rates changed over time and also which areas/venues were highly susceptible to mass 
shooting.

## Findings

I was quite surprised to see mass shooting rates significantly increased in Illinois from the year 2020-2021. Although, most people desire to settle in 
California based on the weather and opportunities it offers, but at the same time, it was amongst the top 3 most unsafe state in terms of mass shooting. 
I was also surprised to see that in the year 2019 there we around 8 other states that got overlapped by Wyoming because to the amount of correlation in 
the shooting trends.

## Steps taken to create the app 

While preprocessing the data, I first used `rbind` to join the datasets for years 2018- 2021. Then, I mutated Date, year and month and created a column 
called case_type to group the mass shooting cases based on their venues into categories like Residential, Party and Public areas. I modularized my code 
by creating separate functions to create the three different plots in my shiny app. For the first plot fig1, I used `map_data` to create a map Dataframe 
for US and then used the latitude and longitude from the Dataframe to create the map plot, which was colored by total shooting cases in a state per year. 
For the second plot fig2, I created time series by using area plot for every state and month, depicting total shooting cases, total deaths and total 
injuries by overlapping all of them to draw a direct comparison between the three. For the third plot fig3, I created scatterplot by doing Principal 
Component Analysis on trend features of the time series for each month and state in a particular year. To add interactivity in my visualizations, I used 
`sliderInput` to track changes in mass shooting trends every year from 2018-2021. Along with all the three plots, I also added a dataTable to show a 
breakdown of shooting cases in terms of venues/areas where it took place. I used a reactive expression to minimize duplication of code wherever data was 
shared amongst viisualizations. Apart from that, I also filtered data that was used in rendering map and area plots and dataTable whenever there was a 
click event on the PCA scatter plotly. The logic for this was wrapped within a reactive expression which made use of event_data("plotly_click") to capture 
every click event on a specific point of scatter plot. Also, I used the tooltip feature of ggplotly to show details of a specific state like total cases, 
deaths and injuries while hovering. I used plot-click and hover to show overview + detail interactively through my visualizations.
