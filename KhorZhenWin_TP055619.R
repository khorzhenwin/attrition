# Khor Zhen Win
# TP055619

# --------------------------Installing Packages----------------------------------

# Install the package to be used
# Load the package

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

install.packages("esquisse")
library(esquisse)

# --------------------------Importing Data---------------------------------------

# Storing the csv file into a local tibble
employee_attrition = read.csv(file="D:\\Program Files\\R\\data\\employee_attrition.csv")
employee_attrition



# -----------------------Exploratory Data Analysis-------------------------------

# Checking for any empty values in any columns
apply(employee_attrition, 2, function(x) any(is.na(x)))


# Checking for duplicates with the main identifier
employee_attrition %>% group_by(EmployeeID) %>% summarise(n = n(),) %>% arrange(desc(n))



# -----------------------Data Manipulation---------------------------------

# Manipulating Duplicates
unique_employee_attrition = employee_attrition %>%
  arrange(desc(STATUS_YEAR)) %>%
  group_by(EmployeeID) %>%
  distinct(EmployeeID, .keep_all = TRUE)

unique_employee_attrition


# Rename Column Headers
names(unique_employee_attrition) = c(
  "Employee_ID",
  "Record_Date",
  "Birth_Date",
  "Hired_Date",
  "Termination_Date",
  "Age",
  "Service_Length",
  "City",
  "Department",
  "Job_Title",
  "Store_Code",
  "Gender",
  "Gender_Full",
  "Termination_Reason",
  "Termination_Type",
  "Status_Year",
  "Status",
  "Business_Unit"
)


# ---------------------------Data Transformation---------------------------------

# Extracting termination year and adding it into a new column
unique_employee_attrition$Termination_Year = 
  format(as.Date(unique_employee_attrition$Termination_Date, "%m/%d/%Y"), "%Y")

# Extracting termination month and adding it into a new column
unique_employee_attrition$Termination_Month = 
  format(as.Date(unique_employee_attrition$Termination_Date, "%m/%d/%Y"), "%m")

# Extracting hired year and adding it into a new column
unique_employee_attrition$Hired_Year = 
  format(as.Date(unique_employee_attrition$Hired_Date, "%m/%d/%Y"), "%Y")

# Extracting hired month and adding it into a new column
unique_employee_attrition$Hired_Month = 
  format(as.Date(unique_employee_attrition$Hired_Date, "%m/%d/%Y"), "%m")

# Classifying age into ranges
min(unique_employee_attrition$Age)
max(unique_employee_attrition$Age)

unique_employee_attrition =
  mutate(unique_employee_attrition, Age_Group = 
           case_when(Age >= 18  & Age <= 24 ~ '18-24',
                    Age >= 25  & Age <= 34 ~ '25-34',
                    Age >= 35  & Age <= 44 ~ '35-44',
                    Age >= 45  & Age <= 54 ~ '45-54',
                    Age >= 55  ~ '55 Above'))


sample(unique_employee_attrition)

# Remove active employees and store into another data frame

terminated_employees = subset(unique_employee_attrition,
                              Status != "ACTIVE")

active_employees = subset(unique_employee_attrition,
                              Status == "ACTIVE")


# ---------------------------Data Visualization---------------------------------

###################################
# Question 1 - Termination Period #
###################################

#Analysis1
# Termination Reason distribution by Month
ggplot(terminated_employees,aes(x=Termination_Month, fill=Termination_Reason)) + 
  geom_bar() +
  labs(title = "Distribution of Termination Reason in Terminated Employees"
       , x = "Month", y = "No. Of Employees")

#Analysis2
# Termination Reason distribution by Year
ggplot(terminated_employees,aes(x=Termination_Year, fill=Termination_Reason)) + 
  geom_bar() +
  labs(title = "Distribution of Termination Reason in Terminated Employees"
       , x = "Year", y = "No. Of Employees")

#Analysis3
# Distribution of hired employees
unique_employee_attrition %>%
  group_by(Hired_Year) %>%
  summarise(n = n(), ) %>%
  ggplot(aes(x=Hired_Year, y=n, group=1)) + 
    geom_line(color="red") +
    geom_point() +
    labs(title = "Employees Hired per Year"
         , x = "Year", y = "No. Of Employees")







###############################
# Question 2 - Discrimination #
###############################

# Termination Reason  distribution by Gender
ggplot(terminated_employees,aes(x=Gender_Full, fill=Termination_Reason)) + 
  geom_bar() +
  facet_grid(~Termination_Reason)+
  labs(title = "Distribution of Termination Reason in Terminated Employees by Gender"
       , x = "Gender", y = "No. Of Employees")

#Analysis1
# Pie Chart for Termination by Gender
terminated_employees %>%
  group_by(Gender_Full, Termination_Reason) %>%
  summarise(n = n(), ) %>% 
  ggplot(aes(x=0, y=n, fill=Gender_Full)) +
    geom_col(position="fill") +
    facet_wrap(~Termination_Reason) +
    coord_polar(theta ="y", start=0) +
    theme_void() +
    labs(title = "Gender Distribution of Terminated Employees") +
    guides(fill = guide_legend(title = "Gender")) 


#Analysis2
# Termination Reason  distribution by Age Group
ggplot(terminated_employees,aes(x=Age_Group, fill=Termination_Reason)) + 
  geom_bar() +
  labs(title = "Distribution of Termination Reason in Terminated Employees by Gender"
     , x = "Age Group", y = "No. Of Employees")








#########################
# Question 3 - Location #
#########################

#Analysis1
# Geographical distribution of Cities
library(sf) #spatial data
library(tmaptools) #access OSM geocode
library(leaflet) #plotting interactive map
library(leaflegend) #legend for the map


cityname = 
  unique_employee_attrition %>%
  mutate(City = paste(City,", Canada", sep="")) %>%
  group_by(City) %>%
  rename(query=City) %>%
  summarise(count = n(), ) %>%
  arrange(desc(count))

information = geocode_OSM(cityname$query,as.sf = TRUE)
information = left_join(information, cityname, by="query")

symbols = makeSizeIcons(
          values = information$count,
          shape = 'circle',
          color = 'purple',
          fillColor = 'purple',
          opacity = .5,
          baseSize = 5
        )

leaflet(information) %>%
  addTiles() %>%
  addMarkers( icon = symbols,
              popup = information$query) %>%
  addLegendSize( 
    values = information$count,
    color ='purple',
    opacity = 0.5,
    shape = 'circle',
    orientation = 'horizontal',
    breaks =5,
    baseSize = 5 #baseSize to change size of legend and icons
  )


#Analysis2
# Termination Type by City
library(forcats)

ggplot(unique_employee_attrition, aes(y = fct_infreq(City), fill=Termination_Type)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Termination Type by City"
       , x = "No. Of Employees.", y = "City")


#Analysis3
# Termination Type by Store Code
unique_employee_attrition %>%
  mutate(Store_Code = paste("A",Store_Code , sep="")) %>%
  group_by(Store_Code, Termination_Type) %>%
  summarise(count = n(), ) %>%
  ggplot(aes(y = reorder(Store_Code, -count), x=count, fill=Termination_Type)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Distribution of Termination Type by Store Code"
         , x = "No. Of Employees.", y = "Store Code")


store_city = 
unique_employee_attrition %>%
  mutate(Store_Code = paste("A",Store_Code , sep="")) %>%
  group_by(Store_Code, City) %>%
  summarise(count = n(), ) %>%
  arrange(desc(count))







###########################
# Question 4 - Department #
###########################

# Termination By Business Unit
ggplot(unique_employee_attrition, aes(x = Business_Unit, fill=Termination_Type)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Termination Type by Business Unit"
       , y = "No. Of Employees.", x = "Business_Unit") +
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    position = position_stack(vjust = 0.5))


# Termination by Department
ggplot(terminated_employees, aes(y = fct_infreq(Department), fill=Termination_Reason)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Termination Type by Department"
       , x = "No. Of Employees.", y = "Department") 


# Terminated Department Age
ggplot(terminated_employees, aes(y = fct_infreq(Department), fill=Age_Group)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Age Group of Terminated Employees by Department"
       , x = "No. Of Employees.", y = "Department") 


# Active Departments Remaining
ggplot(active_employees, aes(y = fct_infreq(Department), fill=Gender_Full)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Active Employees by Department"
       , x = "No. Of Employees.", y = "Department") 







##################################
# Question 5 - Length of Service #
##################################

install.packages("plotly")
library(plotly)

involuntary_employees = subset(unique_employee_attrition,
                          Termination_Type == "Involuntary")

# Length of Service by Involuntary Terminations
ggplot(involuntary_employees, aes(x = Service_Length)) +
  geom_histogram(bins=7, col="red") +
  geom_freqpoly(bins=7, color = "blue", size = 0.7) +
  labs(title = "Distribution of length of service for laid off employees"
       , x = "Length of Service (Years)", y = "No. of Employees")


# Length of Service By Department in Terminated Employees
ggplotly(
  ggplot(terminated_employees, aes(x = Service_Length, fill = Department)) +
  geom_histogram(bins=7, color="black") +
  labs(title = "Distribution of length of service for terminated employees by department"
       , x = "Length of Service (Years)", y = "No. of Employees")
  )







##################
# Extra Features #
##################

# Saving The Data Frames as new CSV files

write.csv(unique_employee_attrition, 
          "D:\\Program Files\\R\\data\\NewData\\unique_employee_attrition.csv", 
          row.names = FALSE)

write.csv(terminated_employees, 
          "D:\\Program Files\\R\\data\\NewData\\terminated_employees.csv", 
          row.names = FALSE)

write.csv(active_employees, 
          "D:\\Program Files\\R\\data\\NewData\\active_employees.csv", 
          row.names = FALSE)

write.csv(involuntary_employees, 
          "D:\\Program Files\\R\\data\\NewData\\involuntary_employees.csv", 
          row.names = FALSE)



# Pie Chart

unique_employee_attrition %>%
  group_by(Gender_Full) %>%
  summarise(n = n(), ) %>% 
  ggplot(aes(x=0, y=n, fill=Gender_Full)) +
  geom_col(position="fill") +
  coord_polar(theta ="y", start=0) +
  theme_void() +
  labs(title = "Gender Distribution of All Employees") +
  guides(fill = guide_legend(title = "Gender")) 

