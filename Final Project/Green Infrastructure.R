library(tidyverse)
library(janitor)
library(dplyr)

#Working with Green Infrastructure data set. Repeated same steps as Solar Potential with some differences to account for the different data sets. 

#Cleaned and selected for relevant columns. Renamed the "zip" column in this table to "postcode" for the purpose of joining with Affordable Housing Data in later steps (and for consistency across data sets)

green_final<-read_csv("NarrowedGreenInfra_geocoded.csv")%>%
  clean_names%>%
  select(asset_id, project_na,asset_type, status, borough, asset_area,status_gro,accuracy_type,zip)%>%
  rename(postcode=zip)

#Grouped by borough, adding total and mean of the area of the green assets. 
  
green_infra_by_borough<-green_final%>%
  group_by(borough)%>%
  summarise(assets=n(),
            mean_area=mean(asset_area),
            total_area=sum(asset_area))

write_csv(green_infra_by_borough, "Green Infrastructure by Borough.csv")

#Grouped by post code within borough. 

green_infra_by_postcode<-green_final%>%
  group_by(borough, postcode)%>%
  summarise(assets=n(),
            mean_area=mean(asset_area))

write_csv(green_infra_by_postcode, "Green Infrasructure by Borough Postcode.csv")

#Filtered by borough to produce an individual table for each. Sorted by post code.
#At first, summarised only for the total number of assets per postcode in each borough-for simplicity purposes in future visuals. 

manhattan_green_infra_ziponly<-green_final%>%
  filter(borough=="Manhattan")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode)%>%
  summarise(assets=n())

#Repeated the above step, but added information on the mean area of each green asset for a more detailed visual of the green assets. 

manhattan_green_infra<-green_final%>%
  filter(borough=="Manhattan")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode, asset_type)%>%
  summarise(assets=n(),
            mean_area=mean(asset_area))

#Repeated the two above steps for each borough. 

brooklyn_green_infra_ziponly<-green_final%>%
  filter(borough=="Brooklyn")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode)%>%
  summarise(assets=n())

brooklyn_green_infra<-green_final%>%
  filter(borough=="Brooklyn")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode, asset_type)%>%
  summarise(assets=n(),
            mean_area=mean(asset_area))

bronx_green_infra_ziponly<-green_final%>%
  filter(borough=="Bronx")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode)%>%
  summarise(assets=n())

bronx_green_infra<-green_final%>%
  filter(borough=="Bronx")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode, asset_type)%>%
  summarise(assets=n(),
            mean_area=mean(asset_area))

queens_green_infra_ziponly<-green_final%>%
  filter(borough=="Queens")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode)%>%
  summarise(assets=n())

queens_green_infra<-green_final%>%
  filter(borough=="Queens")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode, asset_type)%>%
  summarise(assets=n(),
            mean_area=mean(asset_area))

staten_green_infra_ziponly<-green_final%>%
  filter(borough=="Staten Island")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode)%>%
  summarise(assets=n())

staten_green_infra<-green_final%>%
  filter(borough=="Staten Island")%>%
  select(asset_type, status, asset_area, accuracy_type, postcode)%>%
  group_by(postcode, asset_type)%>%
  summarise(assets=n(),
            mean_area=mean(asset_area))

#Wanted to visualize the affordable unit data relative to the green infrastructure data. In order to have both variables on a more similar scale (but keep them proportional across boroughs) added a mutate column that divided the affordable housing unit sums by 100. This made the final visual of this data much easier to interpret. 

#Created a "mutated" version for each borough that scaled the affordable units down. 

manhattan_units_mutated<-manhattan_affordable_units%>%
  mutate(units_mutated=total_affordable_units/100)
brooklyn_units_mutated<-brooklyn_affordable_units%>%
  mutate(units_mutated=total_affordable_units/100)
bronx_units_mutated<-bronx_affordable_units%>%
  mutate(units_mutated=total_affordable_units/100)
queens_units_mutated<-queens_affordable_units%>%
  mutate(units_mutated=total_affordable_units/100)
staten_units_mutated<-staten_affordable_units%>%
  mutate(units_mutated=total_affordable_units/100)

#Merged the new scaled borough tables withe affordable housing data, joined by "postcode" to produce a digestible visual of the relationship. 

manhattan_green_infra_merged=merge(x=manhattan_green_infra_ziponly, y=manhattan_units_mutated, by="postcode", all=FALSE)

brooklyn_green_infra_merged=merge(x=brooklyn_green_infra_ziponly, y=brooklyn_units_mutated, by="postcode", all=FALSE)

bronx_green_infra_merged=merge(x=bronx_green_infra_ziponly, y=bronx_units_mutated, by="postcode", all=FALSE)

queens_green_infra_merged=merge(x=queens_green_infra_ziponly, y=queens_units_mutated, by="postcode", all=FALSE)

staten_green_infra_merged=merge(x=staten_green_infra_ziponly, y=staten_units_mutated, by="postcode", all=FALSE)

write_csv(manhattan_green_infra_merged, "Manhattan Green Infrastructure Merged.csv")
write_csv(brooklyn_green_infra_merged, "Brooklyn Green Infrastructure Merged.csv")
write_csv(bronx_green_infra_merged, "Bronx Green Infrastructure Merged.csv")
write_csv(queens_green_infra_merged, "Queens Green Infrastructure Merged.csv")
write_csv(staten_green_infra_merged, "Staten Green Infrastructure Merged.csv")

