library(tidyverse)
library(janitor)
library(dplyr)

#Started with the NYCHA Access Solar Opportunities data set. Followed similar first steps as Affordable Housing data. 

#Cleaned and selected for relevant columns

solar_potential<-read_csv("NYCHA_ACCESSolar_Opportunities.csv")%>%
  clean_names%>%
  select(development, street_address, postcode, borough, no_of_floors, roof_space_sq_ft, estimated_roof_solar_capacity_k_w,adjusted_value_k_w, roof_condition_rating_or_replacement_date, no_of_residential_units)

#Grouped by borough, adding relevant data such as totals and means to get more information about the solar potential across boroughs. 

solar_potential_by_borough<-solar_potential%>%
  group_by(borough)%>%
  summarise(units=n(),
            total_roof_space=sum(roof_space_sq_ft, na.rm=TRUE),
            total_estimated_solar_capacity=sum(estimated_roof_solar_capacity_k_w, na.rm=TRUE), 
            total_residential_units=sum(no_of_residential_units, na.rm=TRUE),
            mean_solar_capacity=mean(estimated_roof_solar_capacity_k_w),
            mean_roof_space=mean(roof_space_sq_ft))

#Uploaded this csv to make a table in DataWrapper that displays the distribution of solar potential by borough.

write_csv(solar_potential_by_borough, "Solar Potential by Borough.csv")

#Repeated step above, but added grouping by post code to get a more detailed breakdown of the data.
  
solar_potential_by_postcode<-solar_potential%>%
  group_by(borough, postcode)%>%
  summarise(units=n(),
            total_roof_space=sum(roof_space_sq_ft, na.rm=TRUE),
            total_estimated_solar_capacity=sum(estimated_roof_solar_capacity_k_w, na.rm=TRUE), 
            total_residential_units=sum(no_of_residential_units, na.rm=TRUE),
            mean_solar_capacity=mean(estimated_roof_solar_capacity_k_w),
            mean_roof_space=mean(roof_space_sq_ft))


#Filtered by borough to produce an individual table for each. Sorted by post code. selected for relevant data and summarised to get information on totals and means of solar capacity. 

manhattan_solar<-solar_potential%>%
  filter(borough=="MANHATTAN")%>%
  select(development,postcode,borough,roof_space_sq_ft, estimated_roof_solar_capacity_k_w, adjusted_value_k_w,no_of_residential_units)%>%
  group_by(postcode)%>%
  summarise(buildings=n(),
            estimated_solar_capacity=sum(estimated_roof_solar_capacity_k_w),
            mean_estimated_solar=mean(estimated_roof_solar_capacity_k_w),
            mean_roof_space=mean(roof_space_sq_ft))

#Wrote csvs for each to use in visuals
write_csv(manhattan_solar, "Manhattan Solar.csv")

brooklyn_solar<-solar_potential%>%
  filter(borough=="BROOKLYN")%>%
  select(development,postcode,borough,roof_space_sq_ft, estimated_roof_solar_capacity_k_w, adjusted_value_k_w,no_of_residential_units)%>%
  group_by(postcode)%>%
  summarise(buildings=n(),
            estimated_solar_capacity=sum(estimated_roof_solar_capacity_k_w),
            mean_estimated_solar=mean(estimated_roof_solar_capacity_k_w),
            mean_roof_space=mean(roof_space_sq_ft))

write_csv(brooklyn_solar, "Brooklyn Solar.csv")

queens_solar<-solar_potential%>%
    filter(borough=="QUEENS")%>%
  select(development,postcode,borough,roof_space_sq_ft, estimated_roof_solar_capacity_k_w, adjusted_value_k_w,no_of_residential_units)%>%
  group_by(postcode)%>%
  summarise(buildings=n(),
            estimated_solar_capacity=sum(estimated_roof_solar_capacity_k_w),
            mean_estimated_solar=mean(estimated_roof_solar_capacity_k_w),
            mean_roof_space=mean(roof_space_sq_ft))

write_csv(queens_solar,"Queens Solar.csv")

bronx_solar<-solar_potential%>%
  filter(borough=="BRONX")%>%
  select(development,postcode,borough,roof_space_sq_ft, estimated_roof_solar_capacity_k_w, adjusted_value_k_w,no_of_residential_units)%>%
  group_by(postcode)%>%
  summarise(buildings=n(),
            estimated_solar_capacity=sum(estimated_roof_solar_capacity_k_w), 
            mean_estimated_solar=mean(estimated_roof_solar_capacity_k_w),
            mean_roof_space=mean(roof_space_sq_ft))

write_csv(bronx_solar, "Bronx Solar.csv")

staten_solar<-solar_potential%>%
  filter(borough=="STATEN ISLAND")%>%
  select(development,postcode,borough,roof_space_sq_ft, estimated_roof_solar_capacity_k_w, adjusted_value_k_w,no_of_residential_units)%>%
  group_by(postcode)%>%
  summarise(buildings=n(),
            estimated_solar_capacity=sum(estimated_roof_solar_capacity_k_w),
            mean_estimated_solar=mean(estimated_roof_solar_capacity_k_w),
            mean_roof_space=mean(roof_space_sq_ft))

#Wanted to compare trends in solar potential by borough/postcode to the dispersion/frequency of affordable housing units. Merged the affordable housing unit tables with the solar potential tables for each borough, joining by value of "postcode". 

manhattan_solar__merged=merge(x=manhattan_solar, y=manhattan_affordable_units, by="postcode", all=FALSE)
brooklyn_solar_merged=merge(x= brooklyn_solar, y=brooklyn_affordable_units, by="postcode", all=FALSE)
queens_solar_merged=merge(x=queens_solar, y=queens_affordable_units, by="postcode", all=FALSE)
bronx_solar_merged=merge(x=bronx_solar, y=bronx_affordable_units, by="postcode", all=FALSE)
staten_solar_merged=merge(x=staten_solar, y=staten_affordable_units, by="postcode", all=FALSE)

write_csv(manhattan_solar__merged, "Manhattan Solar Merged.csv")

write_csv(brooklyn_solar_merged, "Brooklyn Solar Merged.csv")

write_csv(queens_solar_merged, "Queens Solar Merged.csv")

write_csv(bronx_solar_merged, "Bronx Solar Merged.csv")

write_csv(staten_solar_merged, "Staten Solar Merged.csv")

#Used the resulting csvs to produce visuals that helped identify trends in NYCHA solar potential units as they relate to affordable housing units in each borough. 

#See next R script tab for next steps
          


