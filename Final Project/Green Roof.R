library(tidyverse)
library(janitor)
library(dplyr)

#Cleaned and selected for relevant columns. Renamed the "zip" column in this table to "postcode" for the purpose of joining with Affordable Housing Data in later steps (and for consistency across data sets)

green_clean<-read_csv("GreenRoofData2016_geocoded.csv") %>%
  clean_names%>%
  select(zip, gr_area,bldg_area,prop_gr,heightroof,cnstrct_yr,address,borough,ownertype)%>%
  rename(postcode=zip)

#Grouped by borough, including data on the mean area of each green roof footprint and the total green roof area of each borough. 

roofs_by_borough<-green_clean%>%
  group_by(borough)%>%
  summarise(roof_units=n(), 
            mean_green_area=mean(gr_area), 
            total_green_area=sum(gr_area))

write_csv(roofs_by_borough, "Green Roofs by Borough.csv")

#Grouped by post code, including data on the mean, max and mins of different data points (ie green area, building area) to get a sense of the green roof dispersion across postcodes. 

roofs_by_zip<-green_clean%>%
  group_by(postcode) %>%
  summarize(roof_units=n(), 
            mean_green_area=mean(gr_area),
            max_grean_area= max(gr_area), 
            min_green_area= min(gr_area),
            mean_bldg_area=mean(bldg_area),
            max_bldg_area=max(bldg_area),
            min_bldg_area=min(bldg_area),
            mean_green_prop=mean(prop_gr), 
            max_green_prop=max(prop_gr),
            min_green_prop=min(prop_gr))

#Optional: the next two codes are for additional information about building size, and proportion of green roof to building size grouped by postcode. Didn't end up delving too much into this data but still helpful to have as additional resources. 

roofs_by_bldg_size<-green_clean%>%
  group_by(postcode) %>%
  summarize(roofs=n(),
            mean_bldg_area=mean(bldg_area, na.rm=TRUE),
            max_bldg_area=max(bldg_area, na.rm=TRUE),
            min_bldg_area=min(bldg_area, na.rm=TRUE))

roofs_by_prop<-green_clean%>%
  group_by(postcode)%>%
  summarize(roofs=n(),
            mean_green_prop=mean(prop_gr, na.rm=TRUE), 
            max_green_prop=max(prop_gr, na.rm=TRUE),
            min_green_prop=min(prop_gr, na.rm=TRUE))

#Filtered by borough to produce an individual table for each. Sorted by post code. Added information on the size of the green areas across the different post codes to get a sense of the dispersion (using the mean function). 

manhattan_green_roof<-green_clean%>%
  filter(borough=="MN")%>%
  select(postcode, gr_area, bldg_area, prop_gr, postcode)%>%
  group_by(postcode)%>%
  summarise(roofs=n(),
            green_area=mean(gr_area))

brooklyn_green_roof<-green_clean%>%
  filter(borough=="BK")%>%
  select(postcode, gr_area, bldg_area, prop_gr, postcode)%>%
  group_by(postcode)%>%
  summarise(roofs=n(),
            green_area=mean(gr_area))

queens_green_roof<-green_clean%>%
  filter(borough=="QN")%>%
  select(postcode, gr_area, bldg_area, prop_gr, postcode)%>%
  group_by(postcode)%>%
  summarise(roofs=n(),
            green_area=mean(gr_area))

bronx_green_roof<-green_clean%>%
  filter(borough=="BX")%>%
  select(postcode, gr_area, bldg_area, prop_gr, postcode)%>%
  group_by(postcode)%>%
  summarise(roofs=n(),
            green_area=mean(gr_area))

staten_green_roof<-green_clean%>%
  filter(borough=="SI")%>%
  select(postcode, gr_area, bldg_area, prop_gr, postcode)%>%
  group_by(postcode)%>%
  summarise(roofs=n(),
            green_area=mean(gr_area))

#Merged with affordable housing units. Once again used the mutated affordable housing unit data to make it more scalable when contrasted to the number of green roof footprints

manhattan_green_roof_merged=merge(x=manhattan_green_roof, y=manhattan_units_mutated, by="postcode", all=FALSE)
brooklyn_green_roof_merged=merge(x=brooklyn_green_roof, y=brooklyn_units_mutated, by="postcode", all=FALSE)
queens_green_roof_merged=merge(x=queens_green_roof, y=queens_units_mutated, by="postcode", all=FALSE)
bronx_green_roof_merged=merge(x=bronx_green_roof, y=bronx_units_mutated, by="postcode", all=FALSE)
staten_green_roof_merged=merge(x=staten_green_roof, y=staten_units_mutated, by="postcode", all=FALSE)

write_csv(manhattan_green_roof_merged, "Manhattan Green Roof Merged.csv")
write_csv(brooklyn_green_roof_merged, "Brooklyn Green Roof Merged.csv")
write_csv(bronx_green_roof_merged, "Bronx Green Roof Merged.csv")
write_csv(queens_green_roof_merged, "Queens Green Roof Merged.csv")
write_csv(staten_green_roof_merged, "Staten Island Green Roof Merged.csv")
