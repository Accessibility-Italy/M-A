
# 1) PUBLIC TRANSPORT - PRIMARY SCHOOL


library(dplyr)
library(readr)
library(sf)
library(spdep)
library(ggplot2)


primary_PT <- read.csv(
"~/Library/Mobile Documents/com~apple~CloudDocs/ver1_0_IT_LAU_public_transport_primary_school_facilities_travel_time_to_nearest_facility_average.csv"
)


primary_PT$Minutes[primary_PT$Minutes == 99999] <- 90


primary_PT <- primary_PT %>%
mutate
(Minutes = na_if(Minutes, -1))

summary_primary_PT <- data.frame(
Statistic = c(
"Min.",
"Q1",
"Median",
"Q3",
"Max.",
"Mean",
"Missing"),
Value = c(
min(primary_PT$Minutes, na.rm = TRUE),
quantile(primary_PT$Minutes, 0.25, na.rm = TRUE),
median(primary_PT$Minutes, na.rm = TRUE),
quantile(primary_PT$Minutes, 0.75, na.rm = TRUE),
max(primary_PT$Minutes, na.rm = TRUE),
mean(primary_PT$Minutes, na.rm = TRUE),
sum(is.na(primary_PT$Minutes))
)
)

print(summary_primary_PT)


comuni <- st_read(
"~/Library/Mobile Documents/com~apple~CloudDocs/Com01012023/Comuni1.shp"
)

comuni <- st_make_valid(comuni)

province <- comuni %>%
group_by(COD_PROV) %>%
summarise(
geometry = st_union(geometry),
.groups = "drop"
)




comuni_primary <- comuni %>%
mutate(
PRO_COM_T = as.character(PRO_COM_T)
) %>%
left_join(
primary_PT %>%
mutate(
LAU_CODE = gsub("IT_", "", LAU_CODE)
),
by = c("PRO_COM_T" = "LAU_CODE"))

unique(comuni_primary$COD_REG)

head(comuni_primary)

comuni_lisa <- comuni_primary %>% filter(!is.na(Minutes))



nb <- poly2nb(comuni_lisa,queen = TRUE)


lw <- nb2listw(nb,
style = "W",
zero.policy = TRUE)


moran_test <- moran.test(
comuni_lisa$Minutes,
lw,
zero.policy = TRUE
)

print(moran_test)



lisa <- localmoran(
comuni_lisa$Minutes,
lw,
zero.policy = TRUE
)


comuni_lisa <- comuni_lisa %>%
mutate(
Ii = lisa[,1],
Z_Ii = lisa[,4],
P_Ii = lisa[,5]
)


mean_minutes <- mean(
comuni_lisa$Minutes,
na.rm = TRUE
)


lag_minutes <- lag.listw(
lw,
comuni_lisa$Minutes,
zero.policy = TRUE
)



comuni_lisa <- comuni_lisa %>%
mutate(
Quadrant = case_when(
      
Minutes > mean_minutes &
lag_minutes > mean_minutes &
P_Ii < 0.05 ~ "High-High",
      
Minutes < mean_minutes &
lag_minutes < mean_minutes &
P_Ii < 0.05 ~ "Low-Low",
      
Minutes > mean_minutes &
lag_minutes < mean_minutes &
P_Ii < 0.05 ~ "High-Low",
      
Minutes < mean_minutes &
lag_minutes > mean_minutes &
P_Ii < 0.05 ~ "Low-High",
      
TRUE ~ "Not significant"
))


table(comuni_lisa$Quadrant)



comuni_map <- comuni_primary %>%
left_join(
comuni_lisa %>%
st_drop_geometry() %>%
select(PRO_COM_T, Quadrant),
by = "PRO_COM_T"
) %>%
mutate(
Quadrant = ifelse(
is.na(Quadrant),
"NA",
Quadrant
)
)



comuni_map$Quadrant <- factor(
comuni_map$Quadrant,
levels = c(
"High-High",
"Low-Low",
"High-Low",
"Low-High",
"Not significant",
"NA"
)
)



ggplot(comuni_map) +
geom_sf(
aes(fill = Quadrant),
colour = NA
) +
geom_sf(
data = province,
fill = NA,
colour = "black",
linewidth = 0.01
) +
theme_void() +
scale_fill_manual(
values = c(
"High-High" = "darkgreen",
"Low-Low" = "lightgreen",
"High-Low" = "red",
"Low-High" = "lightblue",
"Not significant" = "grey80",
"NA" = "white"
)
) +
labs(
title = "Public transport accessibility to primary schools",
fill = "Quadrant"
)

# 2) PUBLIC TRANSPORT - HEALTHCARE FACILITIES


# Libraries

library(dplyr)
library(readr)
library(sf)
library(spdep)
library(ggplot2)



healthcare_PT <- read.csv(
"~/Library/Mobile Documents/com~apple~CloudDocs/ver1_0_IT_LAU_public_transport_healthcare_facilities_travel_time_to_nearest_facility_average.csv"
)



healthcare_PT$Minutes[healthcare_PT$Minutes == 99999] <- 90


healthcare_PT <- healthcare_PT %>%
mutate(
Minutes = na_if(Minutes, -1)
)



summary_healthcare_PT <- data.frame(
Statistic = c(
"Min.",
"Q1",
"Median",
"Q3",
"Max.",
"Mean",
"Missing"
),
Value = c(
min(healthcare_PT$Minutes, na.rm = TRUE),
quantile(healthcare_PT$Minutes, 0.25, na.rm = TRUE),
median(healthcare_PT$Minutes, na.rm = TRUE),
quantile(healthcare_PT$Minutes, 0.75, na.rm = TRUE),
max(healthcare_PT$Minutes, na.rm = TRUE),
mean(healthcare_PT$Minutes, na.rm = TRUE),
sum(is.na(healthcare_PT$Minutes))
)
)

print(summary_healthcare_PT)


comuni <- st_read(
"~/Library/Mobile Documents/com~apple~CloudDocs/Com01012023/Comuni1.shp"
)



comuni <- st_make_valid(comuni)

province <- comuni %>%
group_by(COD_PROV) %>%
summarise(
geometry = st_union(geometry),
.groups = "drop"
)



comuni_healthcare <- comuni %>%
mutate(
PRO_COM_T = as.character(PRO_COM_T)
) %>%
left_join(
healthcare_PT %>%
mutate(
LAU_CODE = gsub("IT_", "", LAU_CODE)
),
by = c("PRO_COM_T" = "LAU_CODE")
)


comuni_lisa <- comuni_healthcare %>%
filter(!is.na(Minutes))


nb <- poly2nb(
comuni_lisa,
queen = TRUE
)


lw <- nb2listw(
nb,
style = "W",
zero.policy = TRUE
)


moran_test <- moran.test(
comuni_lisa$Minutes,
lw,
zero.policy = TRUE
)

print(moran_test)


lisa <- localmoran(
comuni_lisa$Minutes,
lw,
zero.policy = TRUE
)


comuni_lisa <- comuni_lisa %>%
mutate(
Ii = lisa[,1],
Z_Ii = lisa[,4],
P_Ii = lisa[,5]
)


mean_minutes <- mean(
comuni_lisa$Minutes,
na.rm = TRUE
)


lag_minutes <- lag.listw(
lw,
comuni_lisa$Minutes,
zero.policy = TRUE
)



comuni_lisa <- comuni_lisa %>%
mutate(
Quadrant = case_when(
      
Minutes > mean_minutes &
lag_minutes > mean_minutes &
P_Ii < 0.05 ~ "High-High",
      
Minutes < mean_minutes &
lag_minutes < mean_minutes &
P_Ii < 0.05 ~ "Low-Low",
      
Minutes > mean_minutes &
lag_minutes < mean_minutes &
P_Ii < 0.05 ~ "High-Low",
      
Minutes < mean_minutes &
lag_minutes > mean_minutes &
P_Ii < 0.05 ~ "Low-High",
      
TRUE ~ "Not significant"
)
)


table(comuni_lisa$Quadrant)


comuni_map <- comuni_healthcare %>%
left_join(
comuni_lisa %>%
st_drop_geometry() %>%
select(PRO_COM_T, Quadrant),
by = "PRO_COM_T"
) %>%
mutate(
Quadrant = ifelse(
is.na(Quadrant),
"NA",
Quadrant
)
)



comuni_map$Quadrant <- factor(
comuni_map$Quadrant,
levels = c(
"High-High",
"Low-Low",
"High-Low",
"Low-High",
"Not significant",
"NA"
)
)


ggplot(comuni_map) +
geom_sf(
aes(fill = Quadrant),
colour = NA
) +
geom_sf(
data = province,
fill = NA,
colour = "black",
linewidth = 0.01
) +
theme_void() +
scale_fill_manual(
values = c(
"High-High" = "darkgreen",
"Low-Low" = "lightgreen",
"High-Low" = "red",
"Low-High" = "lightblue",
"Not significant" = "grey80",
"NA" = "white"
)
) +
labs(
title = "Public transport accessibility to healthcare facilities",
fill = "Quadrant"
)


# 3) DRIVING - PRIMARY SCHOOLS




library(dplyr)
library(readr)
library(sf)
library(spdep)
library(ggplot2)


primary_DR <- read.csv(
"~/Library/Mobile Documents/com~apple~CloudDocs/ver1_0_IT_LAU_driving_primary_school_facilities_travel_time_to_nearest_facility_average.csv"
)


primary_DR$Minutes[primary_DR$Minutes == 99999] <- 90


primary_DR <- primary_DR %>%
mutate(
Minutes = na_if(Minutes, -1))


summary_primary_DR <- data.frame(
Statistic = c(
"Min.",
"Q1",
"Median",
"Q3",
"Max.",
"Mean",
"Missing"
),
Value = c(
min(primary_DR$Minutes, na.rm = TRUE),
quantile(primary_DR$Minutes, 0.25, na.rm = TRUE),
median(primary_DR$Minutes, na.rm = TRUE),
quantile(primary_DR$Minutes, 0.75, na.rm = TRUE),
max(primary_DR$Minutes, na.rm = TRUE),
mean(primary_DR$Minutes, na.rm = TRUE),
sum(is.na(primary_DR$Minutes))
)
)

print(summary_primary_DR)


comuni <- st_read(
"~/Library/Mobile Documents/com~apple~CloudDocs/Com01012023/Comuni1.shp"
)


comuni <- st_make_valid(comuni)
province <- comuni %>%
group_by(COD_PROV) %>%
summarise(
geometry = st_union(geometry),
.groups = "drop"
)


comuni_primary_DR <- comuni %>%
mutate(
PRO_COM_T = as.character(PRO_COM_T)
) %>%
left_join(
primary_DR %>%
mutate(
LAU_CODE = gsub("IT_", "", LAU_CODE)
),
by = c("PRO_COM_T" = "LAU_CODE")
)


comuni_lisa <- comuni_primary_DR %>%
filter(!is.na(Minutes))



nb <- poly2nb(
comuni_lisa,
queen = TRUE
)


lw <- nb2listw(
nb,
style = "W",
zero.policy = TRUE
)


moran_test <- moran.test(
comuni_lisa$Minutes,
lw,
zero.policy = TRUE
)

print(moran_test)


lisa <- localmoran(
comuni_lisa$Minutes,
lw,
zero.policy = TRUE
)


comuni_lisa <- comuni_lisa %>%
mutate(
Ii = lisa[,1],
Z_Ii = lisa[,4],
P_Ii = lisa[,5]
)


mean_minutes <- mean(
comuni_lisa$Minutes,
na.rm = TRUE
)


lag_minutes <- lag.listw(
lw,
comuni_lisa$Minutes,
zero.policy = TRUE
)



comuni_lisa <- comuni_lisa %>%
mutate(
Quadrant = case_when(
      
Minutes > mean_minutes &
lag_minutes > mean_minutes &
P_Ii < 0.05 ~ "High-High",
      
Minutes < mean_minutes &
lag_minutes < mean_minutes &
P_Ii < 0.05 ~ "Low-Low",
      
Minutes > mean_minutes &
lag_minutes < mean_minutes &
P_Ii < 0.05 ~ "High-Low",
      
Minutes < mean_minutes &
lag_minutes > mean_minutes &
P_Ii < 0.05 ~ "Low-High",
      
TRUE ~ "Not significant"
)
)


table(comuni_lisa$Quadrant)


comuni_map <- comuni_primary_DR %>%
left_join(
comuni_lisa %>%
st_drop_geometry() %>%
select(PRO_COM_T, Quadrant),
by = "PRO_COM_T"
) %>%
mutate(
Quadrant = ifelse(
is.na(Quadrant),
"NA",
Quadrant
)
)



comuni_map$Quadrant <- factor(
comuni_map$Quadrant,
levels = c(
"High-High",
"Low-Low",
"High-Low",
"Low-High",
"Not significant",
"NA"
)
)


ggplot(comuni_map) +
geom_sf(
aes(fill = Quadrant),
colour = NA
) +
geom_sf(
data = province,
fill = NA,
colour = "black",
linewidth = 0.01
) +
theme_void() +
scale_fill_manual(
values = c(
"High-High" = "darkgreen",
"Low-Low" = "lightgreen",
"High-Low" = "red",
"Low-High" = "lightblue",
"Not significant" = "grey80",
"NA" = "white"
)
) +
labs(
title = "Driving accessibility to primary schools",
fill = "Quadrant"
)

# 4) DRIVING - HEALTHCARE FACILITIES



library(dplyr)
library(readr)
library(sf)
library(spdep)
library(ggplot2)


healthcare_DR <- read.csv(
"~/Library/Mobile Documents/com~apple~CloudDocs/ver1_0_IT_LAU_driving_healthcare_facilities_travel_time_to_nearest_facility_average.csv"
)


healthcare_DR$Minutes[healthcare_DR$Minutes == 99999] <- 90


healthcare_DR <- healthcare_DR %>%
mutate(
Minutes = na_if(Minutes, -1)
)


summary_healthcare_DR <- data.frame(
Statistic = c(
"Min.",
"Q1",
"Median",
"Q3",
"Max.",
"Mean",
"Missing"
),
Value = c(
min(healthcare_DR$Minutes, na.rm = TRUE),
quantile(healthcare_DR$Minutes, 0.25, na.rm = TRUE),
median(healthcare_DR$Minutes, na.rm = TRUE),
quantile(healthcare_DR$Minutes, 0.75, na.rm = TRUE),
max(healthcare_DR$Minutes, na.rm = TRUE),
mean(healthcare_DR$Minutes, na.rm = TRUE),
sum(is.na(healthcare_DR$Minutes))
)
)

print(summary_healthcare_DR)


comuni <- st_read(
"~/Library/Mobile Documents/com~apple~CloudDocs/Com01012023/Comuni1.shp"
)


comuni <- st_make_valid(comuni)
province <- comuni %>%
group_by(COD_PROV) %>%
summarise(
geometry = st_union(geometry),
.groups = "drop"
)



comuni_healthcare_DR <- comuni %>%
mutate(
PRO_COM_T = as.character(PRO_COM_T)
) %>%
left_join(
healthcare_DR %>%
mutate(
LAU_CODE = gsub("IT_", "", LAU_CODE)
),
by = c("PRO_COM_T" = "LAU_CODE")
)



comuni_lisa <- comuni_healthcare_DR %>%
filter(!is.na(Minutes))


nb <- poly2nb(
comuni_lisa,
queen = TRUE
)


lw <- nb2listw(
nb,
style = "W",
zero.policy = TRUE
)


moran_test <- moran.test(
comuni_lisa$Minutes,
lw,
zero.policy = TRUE
)

print(moran_test)


lisa <- localmoran(
comuni_lisa$Minutes,
lw,
zero.policy = TRUE
)


comuni_lisa <- comuni_lisa %>%
mutate(
Ii = lisa[,1],
Z_Ii = lisa[,4],
P_Ii = lisa[,5]
)


mean_minutes <- mean(
comuni_lisa$Minutes,
na.rm = TRUE
)


lag_minutes <- lag.listw(
lw,
comuni_lisa$Minutes,
zero.policy = TRUE
)



comuni_lisa <- comuni_lisa %>%
mutate(
Quadrant = case_when(
      
Minutes > mean_minutes &
lag_minutes > mean_minutes &
P_Ii < 0.05 ~ "High-High",
      
Minutes < mean_minutes &
lag_minutes < mean_minutes &
P_Ii < 0.05 ~ "Low-Low",
      
Minutes > mean_minutes &
lag_minutes < mean_minutes &
P_Ii < 0.05 ~ "High-Low",
      
Minutes < mean_minutes &
lag_minutes > mean_minutes &
P_Ii < 0.05 ~ "Low-High",
      
TRUE ~ "Not significant"
)
)


table(comuni_lisa$Quadrant)


comuni_map <- comuni_healthcare_DR %>%
left_join(
comuni_lisa %>%
st_drop_geometry() %>%
select(PRO_COM_T, Quadrant),
by = "PRO_COM_T"
) %>%
mutate(
Quadrant = ifelse(
is.na(Quadrant),
"NA",
Quadrant
)
)

  
  



comuni_map$Quadrant <- factor(
comuni_map$Quadrant,
levels = c(
"High-High",
"Low-Low",
"High-Low",
"Low-High",
"Not significant",
"NA"
)
)


ggplot(comuni_map) +
geom_sf(
aes(fill = Quadrant),
colour = NA
) +
geom_sf(
data = province,
fill = NA,
colour = "black",
linewidth = 0.01
) +
theme_void() +
scale_fill_manual(
values = c(
"High-High" = "darkgreen",
"Low-Low" = "lightgreen",
"High-Low" = "red",
"Low-High" = "lightblue",
"Not significant" = "grey80",
"NA" = "white"
)
) +
labs(
title = "Driving accessibility to healthcare facilities",
fill = "Quadrant"
)

