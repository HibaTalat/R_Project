#directory
getwd()
#install.packages(c("dplyr","ggplot2","naniar"))
#install.packages(c("dplyr","ggplot2","naniar"))
#import the data
healthdata <- read.csv("Monthly_provisional_counts_of_deaths_by_age_group__sex__and_race_ethnicity_for_select_causes_of_death.csv", header =TRUE, sep = ",")
View(healthdata)
str(healthdata)
head(healthdata)
df <- healthdata[,c(1:15,17,18,19,20,38,39)]
str(df)
head(df)
tail(df)
View(df)
colnames(df)
colnames(df) <- gsub("\\.","",colnames(df))
#Changing the names of the cloumns 
names(df)[names(df) == "AllCause"] <- "AllCauses"
names(df)[names(df) == "SepticemiaA40A41"] <- "Septicemia"
names(df)[names(df) == "MalignantneoplasmsC00C97"] <- "MalignantNeoplasms"
names(df)[names(df) == "DiabetesmellitusE10E14"] <- "DiabetesMellitus"
names(df)[names(df) == "AlzheimerdiseaseG30"   ] <- "AlzheimerDisease"
names(df)[names(df) == "InfluenzaandpneumoniaJ09J18"] <- "Influenza&Pneumonia"
names(df)[names(df) == "ChroniclowerrespiratorydiseasesJ40J47" ] <- "ChronicLowerRespiratoryDiseases"
names(df)[names(df) == "OtherdiseasesofrespiratorysystemJ00J06J30J39J67J70J98" ] <- "OtherDiseasesofRespiratorySystem"
names(df)[names(df) =="NephritisnephroticsyndromeandnephrosisN00N07N17N19N25N27"] <- "NephritisNephroticSyndromeAndNephrosis"
names(df)[names(df) =="DiseasesofheartI00I09I11I13I20I51"] <- "HeartDiseases"
names(df)[names(df) =="CerebrovasculardiseasesI60I69" ] <- "CerebrovascularDiseases"
names(df)[names(df) =="COVID19U071UnderlyingCauseofDeath"] <- "COVID19UnderlyingCauseofDeath"
names(df)[names(df) =="COVID19U071MultipleCauseofDeath"] <- "COVID19MultipleCausesofDeath"
colnames(df)

View(healthdata)
View(df)
# Check na values and replace with 0
sum(is.na(df))
vis_dat(df)
vis_miss(df)
df[is.na(df)] = 0
sum(is.na(df))
vis_dat(df)
vis_miss(df)

#vis_dat() helps explore the data class structure and missingness:
#vis_miss() function provides a custom plot for missing data
vis_dat(df)
vis_miss(df)
#Count the number of variables in df
#> install.packages("dplyr")

count(df, df$Sex)
count(df,df$AgeGroup)
count(df,df$RaceEthnicity)

#Visuals 
#install.packages("ggplot2")
#piechart
#Find some of the diseases causes death
sept<- sum(df$Septicemia)
mal <-sum(df$MalignantNeoplasms)
dia<-sum(df$DiabetesMellitus)
alz <- sum(df$AlzheimerDisease)
inf <- sum(df$`Influenza&Pneumonia`)
chr <- sum(df$ChronicLowerRespiratoryDiseases)
oth <- sum(df$OtherDiseasesofRespiratorySystem)
nep <- sum(df$NephritisNephroticSyndromeAndNephrosis)
heart <- sum(df$HeartDiseases)
cere <- sum(df$CerebrovascularDiseases)
comul <- sum(df$COVID19MultipleCausesofDeath)
cov <- sum(df$COVID19UnderlyingCauseofDeath)

pc <- c(sept,mal,dia,alz,inf,chr,oth,nep,heart,cere,comul,cov)
lab <- c("Septicemia","MalignantNeoplasms","DiabetesMellitus","AlzheimerDisease","Influenza&Pneumonia","ChronicLowerRespiratoryDiseases","OtherDiseasesofRespiratorySystem","NephritisNephroticSyndromeAndNephrosis","HeartDiseases"
        ,"CerebrovascularDiseases","COVID19MultipleCausesofDeath","COVID19UnderlyingCauseofDeath")
pc
pie(pc,labels = lab, main ="Death per disease")

#histogram

hd_hist<-ggplot(data= df, aes(x=HeartDiseases)) + geom_histogram(binwidth = 300,color="green", fill="white")+labs(title="Deaths by HeartDiseases", x="No. of deaths")
hd_hist
mal_hist<-ggplot(df, aes(x=MalignantNeoplasms)) + geom_histogram(binwidth=300,color="blue", fill="white")+labs(title="Deaths by MalignantNeoplasms", x="No. of deaths")
mal_hist

#Finding Sex,Age group and Ethnicity at highest HeartDisease 
hd_sex<-tapply(df$HeartDiseases,df$Sex, max)
hd_race<-tapply(df$HeartDiseases, df$RaceEthnicity, max)
hd_age<-tapply(df$HeartDiseases,df$AgeGroup, max)
View(sort(hd_sex,decreasing = TRUE))
View(sort(hd_race,decreasing = TRUE))
View(sort(hd_age, decreasing = TRUE))
##Finding Sex,Age group and Ethnicity at highest MalignantNeoplasms
mn_sex<-tapply(df$MalignantNeoplasms,df$Sex, max)
mn_race<-tapply(df$MalignantNeoplasms, df$RaceEthnicity, max)
mn_age<-tapply(df$MalignantNeoplasms,df$AgeGroup, max)
View(sort(mn_sex,decreasing = TRUE))
View(sort(mn_race,decreasing = TRUE))
View(sort(mn_age, decreasing = TRUE))



