# Project1 - Ferah Yilmaz, Nguyen Le, Miles Furnish 

vaccinations <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
demographics <- read_csv("C:/Users/ferah/OneDrive/Desktop/demographics.csv")
data <- read_csv("C:/Users/ferah/OneDrive/Desktop/data.csv")

library(tidyverse)

# Vaccinations
vaccinations <- vaccinations %>% filter(is.na(Province_State)) 
vaccinations <- vaccinations %>% select (-c(UID, iso2, iso3, code3, FIPS, Admin2, Province_State, Lat, Long_, Combined_Key ))
vaccinations <- vaccinations %>% pivot_longer(-c(Country_Region, Population), names_to = "datetime", values_to = "shots")
vaccinations <- vaccinations %>% filter(!is.na(shots)) %>% filter (shots > 0)
vaccinations <- vaccinations %>% mutate(vacRate = shots / Population)
vaccinations <- vaccinations %>% group_by(Country_Region) %>% mutate(daysSinceStart = 1:n())
vaccinations <- vaccinations %>% select (-datetime)



# Hospital Data
data <- data %>% group_by(Country) %>% top_n(1, Year)
data <- data %>% select (-Year)
data <- data %>% filter(!is.na(`Hospital beds (per 10 000 population)`))


# Demographics
demographics <- demographics %>% select(-`Series Name`) %>% pivot_wider(names_from = "Series Code", values_from = YR2015)

demographics <- demographics %>% mutate(SP.POP.80UP=as.numeric(SP.POP.80UP.FE)+as.numeric(SP.POP.80UP.MA)) 
demographics <- demographics %>% mutate(SP.POP.1564.IN=as.numeric(SP.POP.1564.MA.IN)+as.numeric(SP.POP.1564.FE.IN)) 
demographics <- demographics %>% mutate(SP.POP.0014.IN=as.numeric(SP.POP.0014.MA.IN)+as.numeric(SP.POP.0014.FE.IN))
demographics <- demographics %>% mutate(SP.DYN.AMRT=as.numeric(SP.DYN.AMRT.MA)+as.numeric(SP.DYN.AMRT.FE)) 
demographics <- demographics %>% mutate(SP.POP.TOTL.IN=as.numeric(SP.POP.TOTL.FE.IN)+as.numeric(SP.POP.TOTL.MA.IN)) 
demographics <- demographics %>% mutate(SP.POP.65UP.IN=as.numeric(SP.POP.65UP.FE.IN)+as.numeric(SP.POP.65UP.MA.IN))
demographics <- demographics %>% select(-contains(".FE")) %>% select(-contains(".MA"))
demographics <- demographics %>% select (-c(`Country Code`, SP.POP.TOTL, SP.POP.80UP, SP.POP.1564.IN, SP.POP.0014.IN, SP.POP.0014.IN, SP.DYN.AMRT, SP.POP.TOTL.IN, SP.POP.65UP.IN ))

# Merge All Tables 
mydata <- vaccinations %>% full_join(data, by= c('Country_Region' = 'Country'))
mydata <- mydata %>% full_join(demographics, by= c('Country_Region' = 'Country Name'))
mydata <- mydata %>% select(Country = Country_Region, vacRate, shots, Population, daysSinceStart, `Hospital beds (per 10 000 population)`, SP.DYN.LE00.IN, SP.URB.TOTL)
names(mydata)[6] <- 'beds'

# Plotting the data
ggplot(data=mydata %>% group_by(Country) %>% top_n(1, daysSinceStart)) + geom_point(aes(y=vacRate, x=daysSinceStart)) + xlim(350, 500)

# Create Linear models and coefficients
model1 <- lm(vacRate~Population+shots+beds, data=mydata)
model1_cf <- coef(model1)

