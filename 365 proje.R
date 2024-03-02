library(readxl)
data<-read_excel('anket.xlsx')
#tic:time in campus,tt:time of transportation, toh:thoughts on hitchiking, 
#tohf:thoughts on hitchhiking's future, iop:influence on people,
#tor:thoughts on reasons to be declined
colnames(data)<-c('timestamp','gender','age','occupation','tic','hitcdecreased','transportation',
                  'tot','toh','tohf','securityconcernslevel','iop','whichoneareyou','frequencyofhitch',
                  'changeinhitch','tor','securityconcerns','changeinpersonalpref','behaviorofhitchhiker',
                  'hitchlineissues','healthreasons','reasons','frequencyoftakinghitch','changeinhitch2',
                  'reasonstotake','personalsecurity','appereanceofhitchhiker','presenceofothers','weather')

library(dplyr)

data<-mutate(data, hitcdecreased = ifelse(hitcdecreased == "5-Kesinlikle kat??l??yorum", "5", hitcdecreased))
data<-mutate(data, hitcdecreased = ifelse(hitcdecreased == "1-Kesinlikle kat??lm??yorum", "1", hitcdecreased))

data <- data %>%
  mutate(hitcdecreased = case_when(
    is.na(hitcdecreased) ~ NA_integer_,  # Keep NAs as NAs
    hitcdecreased %in% c('5', '4.0', '3.0', '2.0', '1.0') ~ as.integer(hitcdecreased),
    TRUE ~ as.integer(hitcdecreased)  # Keep other values as integers
  ))

#for hitchikers
#SECUR??TY CONCERNS MEAN

data <- data %>%
  mutate(securityconcerns = case_when(
    is.na(securityconcerns) ~ NA_integer_,  # Keep existing NAs as NAs
    securityconcerns == '1' ~ 1L,
    securityconcerns == '2' ~ 2L,
    securityconcerns == '3' ~ 3L,  
    securityconcerns == '4' ~ 4L,  
    securityconcerns == '5' ~ 5L,
    TRUE ~ as.integer(securityconcerns)  # Keep other values as integers
  ))

security_concerns<-hitchhiker$securityconcerns
mean(data$securityconcerns, na.rm=TRUE)

#changeinpersonalpref mean
data <- data %>%
  mutate(changeinpersonalpref = case_when(
    is.na(changeinpersonalpref) ~ NA_integer_,  # Keep existing NAs as NAs
    changeinpersonalpref %in% c('1', '2', '3', '4', '5') ~ as.integer(changeinpersonalpref),
    TRUE ~ as.integer(changeinpersonalpref)  # Keep other values as integers
  ))
personal_preferences_of_the_driver<-hitchhiker$changeinpersonalpref
mean(data$changeinpersonalpref, na.rm=TRUE)

#behaviorofhithchiker mean
data <- data %>%
  mutate(behaviorofhitchhiker = case_when(
    is.na(behaviorofhitchhiker) ~ NA_integer_,  # Keep existing NAs as NAs
    behaviorofhitchhiker == '1' ~ 1L,
    behaviorofhitchhiker == '2' ~ 2L,
    behaviorofhitchhiker == '3' ~ 3L,  
    behaviorofhitchhiker == '4' ~ 4L,  
    behaviorofhitchhiker == '5' ~ 5L,
    TRUE ~ NA_integer_  # Set any unexpected values to NA
  ))

behavior_of_the_hitchhikers<-hitchhiker$behaviorofhitchhiker
mean(data$behaviorofhitchhiker, na.rm=TRUE)

#hitchlineissues
data <- data %>%
  mutate(hitchlineissues = case_when(
    is.na(hitchlineissues) ~ NA_integer_,  # Keep existing NAs as NAs
    hitchlineissues == '1' ~ 1L,
    hitchlineissues == '2' ~ 2L,
    hitchlineissues == '3' ~ 3L,  
    hitchlineissues == '4' ~ 4L,  
    hitchlineissues == '5' ~ 5L,
    TRUE ~ NA_integer_  # Set any unexpected values to NA
  ))

disregarding_hitchhiking_queue<-hitchhiker$hitchlineissues
mean(data$hitchlineissues,na.rm=TRUE)

#healthreaons
data <- data %>%
  mutate(healthreasons = case_when(
    is.na(healthreasons) ~ NA_integer_,  # Keep existing NAs as NAs
    healthreasons == '1' ~ 1L,
    healthreasons == '2' ~ 2L,
    healthreasons == '3' ~ 3L,  
    healthreasons == '4' ~ 4L,  
    healthreasons == '5' ~ 5L,
    TRUE ~ NA_integer_  # Set any unexpected values to NA
  ))
health_reasons<-hitchhiker$healthreasons
mean(data$healthreasons,na.rm=TRUE)

#FOR OTOSTOP ALAN
#for perosnlasecurity
data <- data %>%
  mutate(personalsecurity = case_when(
    is.na(personalsecurity) ~ NA_integer_,  # Keep existing NAs as NAs
    personalsecurity == '1' ~ 1L,
    personalsecurity == '2' ~ 2L,
    personalsecurity == '3' ~ 3L,  
    personalsecurity == '4' ~ 4L,  
    personalsecurity == '5' ~ 5L,
    TRUE ~ NA_integer_  # Set any unexpected values to NA
  ))
personal_security<-otostopalan$personalsecurity
mean(data$personalsecurity, na.rm=TRUE)

#apperanceofhitchiker
data <- data %>%
  mutate(appereanceofhitchhiker = case_when(
    is.na(appereanceofhitchhiker) ~ NA_integer_,  # Keep existing NAs as NAs
    appereanceofhitchhiker  == '1' ~ 1L,
    appereanceofhitchhiker  == '2' ~ 2L,
    appereanceofhitchhiker  == '3' ~ 3L,  
    appereanceofhitchhiker  == '4' ~ 4L,  
    appereanceofhitchhiker  == '5' ~ 5L,
    TRUE ~ NA_integer_  # Set any unexpected values to NA
  ))
appearance_of_the_hitchhiker<-otostopalan$appereanceofhitchhiker
mean(data$appereanceofhitchhiker,na.rm=TRUE)

#presenceofothers
data <- data %>%
  mutate(presenceofothers = case_when(
    is.na(presenceofothers) ~ NA_integer_,  # Keep existing NAs as NAs
    presenceofothers  == '1' ~ 1L,
    presenceofothers  == '2' ~ 2L,
    presenceofothers  == '3' ~ 3L,  
    presenceofothers  == '4' ~ 4L,  
    presenceofothers  == '5' ~ 5L,
    TRUE ~ NA_integer_  # Set any unexpected values to NA
  ))
presence_of_others_in_the_car<-otostopalan$presenceofothers

mean(data$presenceofothers,na.rm=TRUE)

#weather
data <- data %>%
  mutate(weather = case_when(
    is.na(weather) ~ NA_integer_,  # Keep existing NAs as NAs
    weather  == '1' ~ 1L,
    weather  == '2' ~ 2L,
    weather  == '3' ~ 3L,  
    weather  == '4' ~ 4L,  
    weather  == '5' ~ 5L,
    TRUE ~ NA_integer_  # Set any unexpected values to NA
  ))
weather<-otostopalan$weather
mean(data$weather,na.rm=TRUE)

#BOXPLOTS OF OTOSTOP ALAN
df<-data.frame(personal_security,appearance_of_the_hitchhiker,presence_of_others_in_the_car,weather)
boxplot(df, col = c("red", "blue", "green", "purple", "orange"), main = "Boxplot of 5 Variables",outline = TRUE, outpch = 16, outcol = "black")
means<-c(3.24,3.62,3.42,3.22)
text(x = 1:5, y = means, labels = round(means, 2), pos = 3, col = "black")
#BOXPLOT OF OTOSTOP ??EKEN
df2<-data.frame(security_concerns,personal_preferences_of_the_driver,behavior_of_the_hitchhikers,disregarding_hitchhiking_queue,health_reasons)
boxplot(df2, col = c("red", "blue", "green", "purple", "orange"), main = "Boxplot of 5 Variables",outline = TRUE, outpch = 16, outcol = "black")
means2<-c(3.1,3.45,3.75,2.80625,2.84375)
text(x = 1:5, y = means2, labels = round(means2, 2), pos = 3, col = "black")


# Perform one-way ANOVA


# Assuming you have the data loaded into a data frame named df2
# If not, replace df2 with your actual data frame name

# Create the boxplot
boxplot(df, col = c("red", "blue", "green", "purple", "orange"), main = "Boxplot of 5 Variables", outline = TRUE, outpch = 16, outcol = "black")

# Calculate means
means2 <- c(3.1, 3.45, 3.75, 2.80625, 2.84375)

# Add mean points with 'x' symbol
points(x = 1:4, y = means, col = "black", pch = "x", cex = 1.5)

# Add mean values as text labels
text(x = 1:4, y = means + 0.1, labels = round(means, 2), pos = 3, col = "black")


ktdf2<-kruskal.test(df)
summary(ktdf2)
aov()

#is there significant difference btw the means of people's thought on hicthiking'decline according to their time in campus 
lessthan1year <- subset(data, tic == "1 y??ldan az")
a<-lessthan1year$hitcdecreased
onetothreeyears<-subset(data,tic =='1-3 y??l')
b<-onetothreeyears$hitcdecreased
fourtosixyears<-subset(data,tic=='4-6 y??l')
c<-fourtosixyears$hitcdecreased
morethan7years<-subset(data,tic=='7 y??ldan fazla')
d<-morethan7years$hitcdecreased 

kruskal_test_result <- kruskal.test(list(a,b,c,d))
kruskal_test_result

mean(a)
mean(b)
mean(c)
mean(d)

library(ggplot2)
ggplot(data, aes(x = tic, fill = factor(hitcdecreased))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Bar Plot of Time in Campus vs. Thoughts on Hitchhiking's Decline",
       x = "Time in Campus",
       y = "Count") +
  scale_fill_discrete(name = "Thoughts on Decline",
                      labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))

anova_result <- aov(data$hitcdecreased ~ data$tic, data = data)
summary(anova_result)

#is there significant difference  btw means of females hitchhiker's security concerns and males hitchhiker's security concerns
hitchhiker<-subset(data,data$whichoneareyou =='Hitchhiker')
femalehitch<-subset(hitchhiker,hitchhiker$gender=='Kad??n')
malehitch<-subset(hitchhiker,hitchhiker$gender=='Erkek')
mean(femalehitch$securityconcernslevel)
mean(malehitch$securityconcernslevel)
t.test(malehitch$securityconcernslevel,femalehitch$securityconcernslevel)


female<-subset(data,data$gender=='Kad??n')
male<-subset(data,data$gender=='Erkek')
t.test(female$securityconcernslevel,male$securityconcernslevel)

female2<- subset(female, changeinhitch < 0 & securityconcernslevel > 2)
nrow(female2)
female3<-subset(female, changeinhitch2 < 0 & securityconcernslevel > 2)
nrow(female3)
nrow(female)

males2<-subset(male, changeinhitch < 0 & securityconcernslevel > 2)
nrow(males2)
males3<-subset(male, changeinhitch2 < 0 & securityconcernslevel > 2)
nrow(males3)
nrow(male)



piechart1<-25/102
pichart2<-11/103

# Data
if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales")
}
library(scales)

# Data
hitchhiking_data <- c(NotDecreased = 77, Decreased = 25)

# Colors for the pie chart
colors <- c("blue", "red")

# Create a pie chart with percentages
pie(hitchhiking_data, labels = percent(hitchhiking_data / sum(hitchhiking_data)), 
    col = colors, main = "Hitchhiking Frequency")

hitchhiking_data <- c(NotDecreased = 92, Decreased = 11)

# Calculate percentages
percentages <- hitchhiking_data / sum(hitchhiking_data) * 100

# Colors for the pie chart
colors <- c("blue", "red")

# Create a pie chart with percentages as floating-point numbers
pie(hitchhiking_data, labels = sprintf("%.1f%%", percentages), 
    col = colors, main = "Hitchhiking Frequency of Males")

# Add a legend
legend("topright", legend = c('Other males','Decreased and have security concerns'), fill = colors, title = "Frequency")



# Data
hitchhiking_data <- c(NotDecreased = 77, Decreased = 25)

# Calculate percentages
percentages <- hitchhiking_data / sum(hitchhiking_data) * 100

# Colors for the pie chart
colors <- c("blue", "red")

# Create a pie chart with percentages as floating-point numbers
pie(hitchhiking_data, labels = sprintf("%.1f%%", percentages), 
    col = colors, main = "Hitchhiking Frequency of Females")

# Add a legend
legend("topright", legend = c('Other females', 'Decreased and have security concerns'), fill = colors, title = "Frequency")




subset_femalehitch <- subset(femalehitch, changeinhitch < 0 & securityconcernslevel > 3)
l <- nrow(subset_femalehitch)
subset_femaledriver<-subset(frmale)
l2<-length(femaledriver$changeinhitch2{})
l2<-nrow(femalehitch$chan)
l/l2

lm<-length(malehitch$changeinhitch[malehitch$changeinhitch<0])
lm2<-nrow(malehitch)
lm/lm2

#difference btw means of hithkiker's hitch freq and otostop alan's hitch freq
otostopalan<-subset(data,data$whichoneareyou=='Otostop alan')
t.test(hitchhiker$changeinhitch,otostopalan$changeinhitch2)
mean(hitchhiker$changeinhitch)
mean(otostopalan$changeinhitch2)

#hitchhikers' reason to give up hitchhiking and otostop alan's reasons to take hitchhikers
oar<-otostopalan$reasonstotake


#otostobu artt??lar??n te??vik nedenleri ve hitchhiker'lar??n azaltanlar??n nedeni
oac<-subset(otostopalan,otostopalan$changeinhitch2>0)
oac$reasonstotake

nrow(oac)/nrow(otostopalan)

ha<-subset(hitchhiker,hitchhiker$changeinhitch<0)
ha$reasons

nrow(ha)/nrow(hitchhiker)

summary(data)

data<-subset(data, select=-timestamp)  






time_on_campus <- data[[5]] # Replace 5 with the actual column index
thoughts_on_hitchhiking <- as.numeric(sub(".*?(\\d+).*", "\\1", data[[6]]))
analysis_data <- data.frame(time_on_campus, thoughts_on_hitchhiking)

# Removing NA values
analysis_data <- na.omit(analysis_data)

# Performing ANOVA
anova_result <- aov(thoughts_on_hitchhiking ~ time_on_campus, data=analysis_data)
summary(anova_result)
# Replace 6 with the actual column index
