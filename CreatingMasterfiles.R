library(tidyverse)
library(readxl)

## Files pulled from following locations:
## https://nces.ed.gov/ccd/files.asp - Public and District data
## https://nces.ed.gov/ccd/elsi/tableGenerator.aspx - Private data

schools <- read_csv("ccd_sch_052_2223_l_1a_083023.csv")
schools2 <- read_csv("ccd_sch_033_2223_l_1a_083023.csv")
schools3 <- read_csv("ccd_sch_029_2223_w_1a_083023.csv")
schools4 <- read_csv("ccd_sch_129_2223_w_1a_083023.csv")
schools5 <- read_csv("ccd_sch_059_2223_l_1a_083023.csv")
locale <- read_xlsx("EDGE_GEOCODE_PUBLICSCH_2223.xlsx")

## Creating the master that you can join for everything else

Masterschool <- schools5 %>%
  select(SCHOOL_YEAR,SCH_NAME,LEAID,ST_SCHID,NCESSCH,TEACHERS)

## This has charter, low and high grade level, and name of district
Masterschool2 <- schools3 %>%
  select(NCESSCH,LEA_NAME,CHARTER_TEXT,GSLO,GSHI, LEVEL)

## This has FRL information
Masterschool3 <- schools2 %>%
  filter(LUNCH_PROGRAM %in% c("Free lunch qualified","Reduced-price lunch qualified")) %>%
  group_by(NCESSCH,DATA_GROUP) %>%
  summarise(STUDENT_COUNT=sum(STUDENT_COUNT)) %>%
  ungroup() %>%
  mutate(DATA_GROUP="Free and Reduced-price lunch")

Masterschool3 <- Masterschool3 %>%
  bind_rows(.,schools2 %>% 
              filter(DATA_GROUP=="Direct Certification") %>%
              select(NCESSCH,DATA_GROUP,STUDENT_COUNT)) %>%
  pivot_wider(names_from="DATA_GROUP",values_from="STUDENT_COUNT", values_fill=0)

## This has grade level information. We will make two spreadsheets -
## a general spreadsheet for total race/ethnicity and grade level and another
## that separates race by grade level

Masterschool4 <- schools %>%
  filter((GRADE=="No Category Codes" | RACE_ETHNICITY=="No Category Codes") &
           TOTAL_INDICATOR!="Education Unit Total") %>%
  group_by(NCESSCH,GRADE,RACE_ETHNICITY) %>%
  summarise(STUDENT_COUNT=sum(STUDENT_COUNT)) %>%
  ungroup() %>%
  mutate(GROUP=case_when(GRADE!="No Category Codes" ~ GRADE,
                         RACE_ETHNICITY!="No Category Codes" ~ RACE_ETHNICITY,
                         GRADE=="No Category Codes" & RACE_ETHNICITY=="No Category Codes" ~ "Total students",
                         .default="check")) %>%
  select(NCESSCH,GROUP,STUDENT_COUNT) %>%
  pivot_wider(names_from="GROUP",values_from="STUDENT_COUNT", values_fill=0) %>%
  relocate(NCESSCH,`Total students`, `Pre-Kindergarten`,Kindergarten, `Grade 1`,
           `Grade 2`,`Grade 3`,`Grade 4`,`Grade 5`,`Grade 6`,
           `Grade 7`,`Grade 8`,`Grade 9`,`Grade 10`,`Grade 11`,
           `Grade 12`,`American Indian or Alaska Native`,
           Asian, `Black or African American`,`Hispanic/Latino`,
           `Native Hawaiian or Other Pacific Islander`,`Not Specified`,
           `Two or more races`,White) %>%
  select(NCESSCH:White)

Gradespecific <- schools %>%
  filter(GRADE!="No Category Codes") %>%
  group_by(NCESSCH,GRADE,RACE_ETHNICITY) %>%
  summarise(STUDENT_COUNT=sum(STUDENT_COUNT)) %>%
  ungroup() %>%
  mutate(GROUP=case_when(RACE_ETHNICITY=="No Category Codes" ~ "Total students",
                         .default=RACE_ETHNICITY)) %>%
  select(NCESSCH,GRADE,GROUP,STUDENT_COUNT) %>%
  pivot_wider(names_from="GROUP",values_from="STUDENT_COUNT", values_fill=0) %>%
  filter(GRADE!="Not Specified")

## Locale work

Masterschool5 <- locale %>%
  select(NCESSCH,STREET,CITY,STATE,ZIP,LOCALE,LAT,LON)

## Now to join everything together to make 2 public school lists

Public <- Masterschool %>%
  left_join(Masterschool2) %>%
  left_join(Masterschool3) %>%
  left_join(Masterschool4) %>%
  left_join(Masterschool5) %>%
  rename(`School Year`=SCHOOL_YEAR,`School Name`=SCH_NAME,`District ID`=LEAID,
         `State Assessment ID`=ST_SCHID,`NCES School ID`=NCESSCH,`Total Teachers`=TEACHERS,
         `District Name`=LEA_NAME,`Charter Status`=CHARTER_TEXT,`Lowest Grade`=GSLO,
         `Highest Grade`=GSHI,`School Level`=LEVEL, Street=STREET, City=CITY, State=STATE,`Zip Code`=ZIP,
         locale=LOCALE,lat=LAT,lon=LON) %>%
  relocate(`School Year`,`School Name`,`NCES School ID`,`District Name`,`District ID`,`State Assessment ID`,
           Street,City,State,`Zip Code`,locale,lat,lon,
           `Lowest Grade`,`Highest Grade`,`Total Teachers`,`Charter Status`,
           `School Level`,`Free and Reduced-price lunch`,
           `Direct Certification`,`Total students`,`Pre-Kindergarten`,Kindergarten,
           `Grade 1`,`Grade 2`,`Grade 3`,`Grade 4`,`Grade 5`,`Grade 6`,`Grade 7`,
           `Grade 8`,`Grade 9`,`Grade 10`,`Grade 11`,`Grade 12`,
           `American Indian or Alaska Native`,Asian,`Black or African American`,
           `Hispanic/Latino`,`Native Hawaiian or Other Pacific Islander`,
           `Not Specified`,`Two or more races`,White)
           
PublicGrade <- Masterschool %>%
  left_join(Masterschool2) %>%
  left_join(Masterschool3) %>%
  right_join(Gradespecific) %>%
  rename(`School Year`=SCHOOL_YEAR,`School Name`=SCH_NAME,`District ID`=LEAID,
         `State Assessment ID`=ST_SCHID,`NCES School ID`=NCESSCH,`Total Teachers`=TEACHERS,
         `District Name`=LEA_NAME,`Charter Status`=CHARTER_TEXT,`Lowest Grade`=GSLO,
         `Highest Grade`=GSHI,`School Level`=LEVEL, Grade=GRADE) %>%
  select(`School Year`:`NCES School ID`,`District Name`,Grade:White) %>%
  relocate(`Total students`, .after=`Grade`) 


rm(schools,schools2,schools3,schools4,schools5,locale,Masterschool,
   Masterschool2,Masterschool3,Masterschool4,Masterschool5,Gradespecific)

## District work
districts <- read_csv("ccd_lea_052_2223_l_1a_083023.csv")
districts2 <- read_csv("ccd_lea_059_2223_l_1a_083023.csv")
districts3 <- read_csv("ccd_lea_029_2223_w_1a_083023.csv")
districtlocale <- read_xlsx("EDGE_GEOCODE_PUBLICLEA_2223.xlsx")

## Charter text and basic information on districts
Masterdistrict <- districts3 %>%
  select(SCHOOL_YEAR,LEA_NAME,LEAID,ST_LEAID,CHARTER_LEA,OPERATIONAL_SCHOOLS)

## Staff information for districts
Masterdistrict2 <- districts2 %>%
  filter(STAFF=="Teachers") %>%
  select(LEAID,STAFF_COUNT)

## Student numbers for districts
Masterdistrict3 <- districts %>%
  filter((GRADE=="No Category Codes" | RACE_ETHNICITY=="No Category Codes") &
           TOTAL_INDICATOR!="Education Unit Total") %>%
  group_by(LEAID,GRADE,RACE_ETHNICITY) %>%
  summarise(STUDENT_COUNT=sum(STUDENT_COUNT)) %>%
  ungroup() %>%
  mutate(GROUP=case_when(GRADE!="No Category Codes" ~ GRADE,
                         RACE_ETHNICITY!="No Category Codes" ~ RACE_ETHNICITY,
                         GRADE=="No Category Codes" & RACE_ETHNICITY=="No Category Codes" ~ "Total students",
                         .default="check")) %>%
  select(LEAID,GROUP,STUDENT_COUNT) %>%
  pivot_wider(names_from="GROUP",values_from="STUDENT_COUNT", values_fill=0) %>%
  relocate(LEAID,`Total students`, `Pre-Kindergarten`,Kindergarten, `Grade 1`,
           `Grade 2`,`Grade 3`,`Grade 4`,`Grade 5`,`Grade 6`,
           `Grade 7`,`Grade 8`,`Grade 9`,`Grade 10`,`Grade 11`,
           `Grade 12`,`American Indian or Alaska Native`,
           Asian, `Black or African American`,`Hispanic/Latino`,
           `Native Hawaiian or Other Pacific Islander`,`Not Specified`,
           `Two or more races`,White) %>%
  select(LEAID:White)

## Locale work
Masterdistrict4 <- districtlocale %>%
  select(LEAID,STREET,CITY,STATE,ZIP,LOCALE,LAT,LON)

## Now to join everything together to make a district file

District <- Masterdistrict %>%
  left_join(Masterdistrict2) %>%
  left_join(Masterdistrict3) %>%
  left_join(Masterdistrict4) %>%
  rename(`School Year`=SCHOOL_YEAR,`District ID`=LEAID,
         `State Assessment ID`=ST_LEAID,`Total Teachers`=STAFF_COUNT,
         `District Name`=LEA_NAME,`Charter Status`=CHARTER_LEA,
         `Number Schools`=OPERATIONAL_SCHOOLS,
         Street=STREET, City=CITY, State=STATE,`Zip Code`=ZIP,
         locale=LOCALE,lat=LAT,lon=LON) %>%
  relocate(`School Year`,`District Name`,`District ID`,`State Assessment ID`,
           Street,City,State,`Zip Code`,locale,lat,lon,
           `Total Teachers`,`Charter Status`,
           `Total students`,`Pre-Kindergarten`,Kindergarten,
           `Grade 1`,`Grade 2`,`Grade 3`,`Grade 4`,`Grade 5`,`Grade 6`,`Grade 7`,
           `Grade 8`,`Grade 9`,`Grade 10`,`Grade 11`,`Grade 12`,
           `American Indian or Alaska Native`,Asian,`Black or African American`,
           `Hispanic/Latino`,`Native Hawaiian or Other Pacific Islander`,
           `Not Specified`,`Two or more races`,White)

DistrictFRL <- Public |> 
  mutate(`Free and Reduced-price lunch` = case_when(is.na(`Free and Reduced-price lunch`) ~ `Direct Certification`,
                                                    .default=`Free and Reduced-price lunch`)) |>
  group_by(`District ID`) |> 
  summarise(`Free and Reduced-price lunch`=round(sum(`Free and Reduced-price lunch`,na.rm=TRUE)/sum(`Total students`,na.rm=TRUE)*100,1))

District <- District |> 
  left_join(DistrictFRL, by=c("District ID"="District ID"))

rm(districts,districts2,districts3,districtlocale,Masterdistrict,
   Masterdistrict2,Masterdistrict3,Masterdistrict4)


## Private information
Private <- read_csv("Private.csv") %>%
  mutate(`School Year`="2019-2020") %>%
  rename(`School Name`=`Private School Name`,
         `NCES School ID`=`School ID - NCES Assigned [Private School] Latest available year`,
         Street=`Physical Address [Private School] 2019-20`,
         City=`City [Private School] 2019-20`,
         `Zip Code`=`ZIP [Private School] 2019-20`,
         `Lowest Grade`=`Lowest Grade Taught [Private School] 2019-20`,
         `Highest Grade`=`Highest Grade Taught [Private School] 2019-20`,
         locale=`Locale [Private School] 2019-20`,
         `Total Teachers`= `Full-Time Equivalent (FTE) Teachers [Private School] 2019-20`,
         `Total students`= `Total Students (Ungraded & PK-12) [Private School] 2019-20`,
         `Pre-Kindergarten`=`Prekindergarten Students [Private School] 2019-20`,
         Kindergarten=`Kindergarten Students [Private School] 2019-20`,
         `Grade 1`=`Grade 1 Students [Private School] 2019-20`,
         `Grade 2`=`Grade 2 Students [Private School] 2019-20`,
         `Grade 3`=`Grade 3 Students [Private School] 2019-20`,
         `Grade 4`=`Grade 4 Students [Private School] 2019-20`,
         `Grade 5`=`Grade 5 Students [Private School] 2019-20`,
         `Grade 6`=`Grade 6 Students [Private School] 2019-20`,
         `Grade 7`=`Grade 7 Students [Private School] 2019-20`,
         `Grade 8`=`Grade 8 Students [Private School] 2019-20`,
         `Grade 9`=`Grade 9 Students [Private School] 2019-20`,
         `Grade 10`=`Grade 10 Students [Private School] 2019-20`,
         `Grade 11`=`Grade 11 Students [Private School] 2019-20`,
         `Grade 12`=`Grade 12 Students [Private School] 2019-20`,
         `American Indian or Alaska Native`=`American Indian/Alaska Native Students [Private School] 2019-20`,
         Asian=`Asian or Asian/Pacific Islander Students [Private School] 2019-20`,
         `Black or African American`=`Black or African American Students [Private School] 2019-20`,
         `Hispanic/Latino`=`Hispanic Students [Private School] 2019-20`,
         `Native Hawaiian or Other Pacific Islander`=`Nat. Hawaiian or Other Pacific Isl. Students [Private School] 2019-20`,
         `Two or more races`=`Two or More Races Students [Private School] 2019-20`,
         White=`White Students [Private School] 2019-20`)


## save everything
save(Public,PublicGrade,District,Private,file="24Masterfiles.RData")
