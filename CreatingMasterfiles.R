library(tidyverse)
library(readxl)

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
