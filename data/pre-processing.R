## Schools and colleges ##

# Source: Department for Education
# Publisher URL: https://get-information-schools.service.gov.uk/
# Licence: Open Government Licence

library(tidyverse) ; library(sf)

df <- read_csv("results.csv") %>%
  filter(`EstablishmentTypeGroup (name)` != "Children's Centres") %>% 
  unite(headteacher, HeadFirstName, HeadLastName, sep = " ") %>% 
  select(URN,
         name = EstablishmentName,
         type = `EstablishmentTypeGroup (name)`,
         nursery_provision = `NurseryProvision (name)`,
         sixth_form = `OfficialSixthForm (name)`,
         gender = `Gender (name)`,
         lowest_admission_age = StatutoryLowAge,
         highest_admission_age = StatutoryHighAge,
         census_date = CensusDate,
         number_of_pupils = NumberOfPupils,
         percent_fsm = PercentageFSM,
         ofsted_rating = `OfstedRating (name)`,
         headteacher,
         website = SchoolWebsite,
         Easting, Northing) %>% 
  st_as_sf(crs = 27700, coords = c("Easting", "Northing")) %>% 
  st_transform(4326) %>% 
  mutate(lon = map_dbl(geometry, ~st_coordinates(.x)[[1]]),
         lat = map_dbl(geometry, ~st_coordinates(.x)[[2]]),
         nursery_provision = case_when(
           nursery_provision == "Has Nursery Classes" ~ "Yes",
           TRUE ~ "No"
         ),
         sixth_form = case_when(
           sixth_form == "Has a sixth form" ~ "Yes",
           TRUE ~ "No"
         ),
         lowest_admission_age = as.integer(lowest_admission_age),
         highest_admission_age = as.integer(highest_admission_age),
         phase = case_when(
           highest_admission_age <= 11 ~ "Primary",
           lowest_admission_age >= 11 & highest_admission_age <= 19 ~ "Secondary",
           lowest_admission_age >= 16 ~ "Further Education",
           TRUE ~ "All through"
         ),
         number_of_pupils = as.integer(number_of_pupils),
         census_date = as.Date(census_date, format = "%d-%m-%Y"),
         ofsted_report = case_when(
           is.na(ofsted_rating) ~ "",
           TRUE ~ str_c("http://reports.ofsted.gov.uk/inspection-reports/find-inspection-report/provider/ELS/", URN),
         ),
         compare_performance = str_c("https://www.compare-school-performance.service.gov.uk/school/", URN),
         website = str_replace(website, "\\/$", "")) %>% 
  st_set_geometry(NULL) %>% 
  select(URN, name, type, phase, nursery_provision, sixth_form, gender, lowest_admission_age,
         highest_admission_age, census_date, number_of_pupils, percent_fsm, ofsted_rating, ofsted_report,
         compare_performance, headteacher, website, lon, lat)

write_csv(df, "schools.csv")
