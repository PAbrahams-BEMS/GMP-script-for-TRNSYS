#Packages installation
my_packages<-c("gridExtra","lubridate","ggplot2","ggpubr","here","stringr")
lapply(my_packages,require,character.only=TRUE);rm(my_packages)

#Path for files import
path<-"C:/YourPathTo/wizard_settings/TMY/ssp245"
setwd(path)

#Files names list
myfiles<-list.files(here(path))
meteodata<-list()

for(i in 1:length(myfiles)){
  meteodata[[i]]<-read.csv(myfiles[i],header=T,sep = " ")}

for(i in 1:length(meteodata)){
  names(meteodata)[i]<-substr(myfiles[i],1,(nchar(myfiles[i])-11))}

#Column name change from "date" by "TIME"
for(i in 1:length(meteodata)){
  colnames(meteodata[[i]])[1]<-"TIME"}

#ConversionDateTRNSYS import
conversionDateTRNSYS <- read.csv("YourPathTo/conversionDateTRNSYS.csv",header=T,sep=";")
conversionDateTRNSYS<-conversionDateTRNSYS[-c(8762:8904),]


#merge conversionDateTRNSYS with meoto files inside meteodata

meteo_arlon2040<-merge(meteodata$`Arlon2021-2040`[,-c(3:14)],conversionDateTRNSYS,by="TIME")  
meteo_charleroi2040<-merge(meteodata$`Charleroi2021-2040`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_liege2040<-merge(meteodata$`Liege2021-2040`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_mons2040<-merge(meteodata$`Mons2021-2040`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_namur2040<-merge(meteodata$`Namur2021-2040`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_arlon2060<-merge(meteodata$`Arlon2041-2060`[,-c(3:14)],conversionDateTRNSYS,by="TIME")  
meteo_charleroi2060<-merge(meteodata$`Charleroi2041-2060`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_liege2060<-merge(meteodata$`Liege2041-2060`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_mons2060<-merge(meteodata$`Mons2041-2060`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_namur2060<-merge(meteodata$`Namur2041-2060`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_arlon2100<-merge(meteodata$`Arlon2081-2100`[,-c(3:14)],conversionDateTRNSYS,by="TIME")  
meteo_charleroi2100<-merge(meteodata$`Charleroi2081-2100`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_liege2100<-merge(meteodata$`Liege2081-2100`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_mons2100<-merge(meteodata$`Mons2081-2100`[,-c(3:14)],conversionDateTRNSYS,by="TIME")
meteo_namur2100<-merge(meteodata$`Namur2081-2100`[,-c(3:14)],conversionDateTRNSYS,by="TIME")


#Creation of list with meteodata files merged with conversionDateTRNSYS
meteo_city <- list(arlon2040 = meteo_arlon2040,arlon2060 = meteo_arlon2060,arlon2100 = meteo_arlon2100,
                    charleroi2040= meteo_charleroi2040, charleroi2060= meteo_charleroi2060,charleroi2100 = meteo_charleroi2100 ,
                    liege2040 = meteo_liege2040,liege2060 = meteo_liege2060, liege2100 = meteo_liege2100,
                    mons2040 = meteo_mons2040,mons2060 = meteo_mons2060,mons2100 = meteo_mons2100,
                    namur2040 = meteo_namur2040,namur2060 = meteo_namur2060,namur2100 = meteo_namur2100)


# Creation of dataframe from list meteo_city for monthly mean temperature calculation
meteo_monthly <- lapply(meteo_city, function(df) {
  # Calculatio of monthly mean temperature by day of year
  monthly_avg <- aggregate(df$dry.bulb.temperature..C., 
                           by = list(MonthOfYear = df$Month_NUM), 
                           FUN = mean, na.rm = TRUE)
   colnames(monthly_avg)[2] <- "MeanMonthlyTemp"
  
  return(monthly_avg)
})

# Creation of dataframes from list meteo_monthly for parameters calculation
dfwizard <- lapply(meteo_monthly, function(df) {
  min_val <- min(df$MeanMonthlyTemp, na.rm=TRUE)
  max_val <- max(df$MeanMonthlyTemp, na.rm=TRUE)
  mean_val <- mean(df$MeanMonthlyTemp, na.rm=TRUE)
  ampl_val <- (max_val - min_val)/2
  list(min = min_val, max = max_val, mean = mean_val, ampl = ampl_val)
})
min_day_each_city  <- sapply(meteo_city, function(df) {
  min_temp <- min(df$dry.bulb.temperature..C., na.rm = TRUE)
  min_day <- df$Day_365[which.min(df$dry.bulb.temperature..C.)]
  return(min_day)
})

min_tempt_city  <- sapply(meteo_city, function(df) {
  min_temp_yr <- min(df$dry.bulb.temperature..C., na.rm = TRUE)
  return(min_temp_yr)
})
max_tempt_city  <- sapply(meteo_city, function(df) {
  max_temp_yr <- max(df$dry.bulb.temperature..C., na.rm = TRUE)
  return(max_temp_yr)
})
mean_tempt_city  <- sapply(meteo_city, function(df) {
  mean_temp <- mean(df$dry.bulb.temperature..C., na.rm = TRUE)
  return(mean_temp)
})
# Matrix wuth parameters
wizards <- matrix(NA, nrow=5, ncol=length(dfwizard))
colnames(wizards) <- names(dfwizard)
rownames(wizards) <- c("Min", "Max", "Mean", "Ampl","Tshift")

# Fill the matrix with values
for(i in seq_along(dfwizard)){
  wizards["Min", i] <- min_tempt_city[i]
  wizards["Max", i] <- max_tempt_city[i]
  wizards["Mean", i] <- mean_tempt_city[i]
  wizards["Ampl", i] <- dfwizard[[i]]$ampl
  wizards["Tshift", i]<- min_day_each_city[i]
}
print(wizards)


#Print parameters results in .xls file
library(openxlsx)
# Chemin complet où tu veux sauver le fichier
save_path <-"C:/YourPathTo/wizard_settings/TMY/wizardsTMY.xlsx"
write.xlsx(wizards, file = save_path, rowNames = TRUE)

