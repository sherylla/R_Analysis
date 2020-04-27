###MechaCarChallenge.RScript
library(tidyverse)
mecha_car <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)
lm(mpg ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + AWD,data=mecha_car) #generate multiple linear regression model
summary(lm(formula = mpg ~ `vehicle length` + `vehicle weight` + `spoiler angle` + 
             `ground clearance` + AWD, data = mecha_car))
suspension_coil <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F)
summarize_sc <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Maximum_PSI=max(PSI),Minimum_PSI=min(PSI),Median_PSI=median(PSI),SD_PSI=sd(PSI),Var_PSI=var(PSI),VehicleID=n())
plt <- ggplot(suspension_coil,aes(x=log10(PSI)))
plt + geom_density()
sample_sc_table <- suspension_coil %>% sample_n(50)
plt <- ggplot(sample_sc_table,aes(x=log10(PSI)))
plt + geom_density()
t.test(log10(sample_sc_table$PSI),mu=mean(log10(suspension_coil$PSI)))
