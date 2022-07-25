library(tidyverse)
library(janitor)
library (dplyr)

#Started with the Affordable Housing Data Set

#Cleaned the data set, and selected the relevant columns I want in my table
affordable_units_clean<- read_csv("Housing_New_York_Units_by_Building.csv") %>%
  clean_names%>%
  select(project_name, bbl,building_id, borough, postcode, community_board, council_district,building_completion_date, reporting_construction_type, extended_affordability_only, prevailing_wage_status, extremely_low_income_units, very_low_income_units, low_income_units, moderate_income_units, other_income_units, counted_homeownership_units, counted_rental_units, all_counted_units, total_units)

#Sorted by borough, selected for the sum of affordable units and total units to see dispersion across boroughs. Also added a column that showed proportion of affordable to non-affordable units in the buildings across the boroughs. 
affordable_units_borough<-affordable_units_clean%>%
  group_by(borough)%>%
  summarize(units=n(),
            affordable_units=sum(all_counted_units,na.rm=TRUE), 
            all_units=sum(total_units,na.rm=TRUE))%>%
  mutate(prop_aff_to_total=affordable_units/all_units)%>%
  subset(select=-units)

#Produced a csv for this table so I could import it to DataWrapper for visuals 

write_csv(affordable_units_borough, "Affordable Units by Borough.csv")

#Sorted by postcode within Boroughs

simple_postcode<-affordable_units_clean%>%
  group_by(borough, postcode)%>%
  summarize(units=n(),
            affordable_units=sum(all_counted_units,na.rm=TRUE), 
            all_units=sum(total_units,na.rm=TRUE))%>%
  subset(select=-units)

#Wanted to look at the boroughs individually, so filtered to produce a table for each borough.Repeated the same process of sorting by postcode in order to find trends by zip. Added further information regarding the distribution of extremely low, very low  and low income units as extra data points that I felt might be relevant. 

manhattan_affordable_units<-affordable_units_clean%>%
  filter(borough=="Manhattan")%>%
  group_by(postcode)%>%
  summarise(units=n(),
           total_affordable_units=sum(all_counted_units, na.rm=TRUE), 
           extremely_lowincome_units=sum(extremely_low_income_units, na.rm=TRUE),
           very_lowincome_units=sum(very_low_income_units, na.rm=TRUE),
           lowincome_units=sum(low_income_units, na.rm=TRUE))%>%
  subset(select=-units)

brooklyn_affordable_units<-affordable_units_clean%>%
  filter(borough=="Brooklyn")%>%
  group_by(postcode)%>%
  summarise(units=n(),
            total_affordable_units=sum(all_counted_units, na.rm=TRUE), 
            extremely_lowincome_units=sum(extremely_low_income_units, na.rm=TRUE),
            very_lowincome_units=sum(very_low_income_units, na.rm=TRUE),
            lowincome_units=sum(low_income_units, na.rm=TRUE))%>%
  subset(select=-units)

bronx_affordable_units<-affordable_units_clean%>%
  filter(borough=="Bronx")%>%
  group_by(postcode)%>%
  summarise(units=n(),
            total_affordable_units=sum(all_counted_units, na.rm=TRUE), 
            extremely_lowincome_units=sum(extremely_low_income_units, na.rm=TRUE),
            very_lowincome_units=sum(very_low_income_units, na.rm=TRUE),
            lowincome_units=sum(low_income_units, na.rm=TRUE))%>%
  subset(select=-units)

staten_affordable_units<-affordable_units_clean%>%
  filter(borough=="Staten Island")%>%
  group_by(postcode)%>%
  summarise(units=n(),
            total_affordable_units=sum(all_counted_units, na.rm=TRUE), 
            extremely_lowincome_units=sum(extremely_low_income_units, na.rm=TRUE),
            very_lowincome_units=sum(very_low_income_units, na.rm=TRUE),
            lowincome_units=sum(low_income_units, na.rm=TRUE))%>%
  subset(select=-units)

queens_affordable_units<-affordable_units_clean%>%
  filter(borough=="Queens")%>%
  group_by(postcode)%>%
  summarise(units=n(),
            total_affordable_units=sum(all_counted_units, na.rm=TRUE), 
            extremely_lowincome_units=sum(extremely_low_income_units, na.rm=TRUE),
            very_lowincome_units=sum(very_low_income_units, na.rm=TRUE),
            lowincome_units=sum(low_income_units, na.rm=TRUE))%>%
  subset(select=-units)

#This was the level I wanted the affordable housing data to be manipulated at so I could use the tables in my future analyses. 

#See next R script tab for next steps. 
