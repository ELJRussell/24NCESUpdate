library(tidyverse)
library(readxl)

schools <- read_csv("ccd_sch_052_2223_l_1a_083023.csv") %>% filter(NCESSCH=="360008105944")
schools2 <- read_csv("ccd_sch_033_2223_l_1a_083023.csv") %>% filter(NCESSCH=="360008105944")
schools3 <- read_csv("ccd_sch_029_2223_w_1a_083023.csv") %>% filter(NCESSCH=="360008105944")
schools4 <- read_csv("ccd_sch_129_2223_w_1a_083023.csv") %>% filter(NCESSCH=="360008105944")
schools5 <- read_csv("ccd_sch_059_2223_l_1a_083023.csv") %>% filter(NCESSCH=="360008105944")
locale <- read_xlsx("EDGE_GEOCODE_PUBLICSCH_2223.xlsx") %>% filter(NCESSCH=="360008105944")
