############################################################################################
############################################################################################
# Clear workspace 
rm(list = ls())
############################################################################################

#######################################################################################################
#######################################################################################################
##################################LOADING PACKAGES#####################################################

################################################################################
#Do tryCatch here

tryCatch(require(pacman),finally=utils:::install.packages(pkgs='pacman',repos='http://cran.r-project.org'));
require(pacman)

##if the above doesn't work, use this code##
##tryCatch
#detach("package:pacman", unload = TRUE)
#install.packages("pacman", dependencies = TRUE)
# ## install.packages("pacman")
pacman::p_load(xlsx, dplyr, reshape2, tidyr, ggplot2, RCurl)

#Loading libraries

##for importing xlsx files
library(xlsx)

##for creating new variables, recoding variables, filtering/selecting variables
library(dplyr)

##for melting and casting datasets
library(reshape2)

##tidy datasets
library(tidyr)

library(ggplot2)

##Grabbing URLs
library(RCurl)


##Set working directory
#Change this to your wd of choice
#If you don't know your wd, type getwd()

setwd("C:\\Users\\bufto\\Dropbox (Personal)\\Spring 2018\\SIOP 2018")
#setwd("D:\\SIOP\\2018\\Master Tutorial on Data Wrangling")

##Base r for .csv file import

data <- read.csv("SIOP Data Wrangling Master Tutorial Data Set.csv")
#data <- read.csv("SIOP Data Wrangling Master Tutorial Data Set REGEX.csv")

#Or grab right off of GitHub
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/RobStilson/SIOP_2018_Master_Tutorial/master/SIOP%20Data%20Wrangling%20Master%20Tutorial%20Data%20Set%20REGEX.csv")
data <- read.csv(text = x)

##Load libraries for importing different file types -- .xlsx

library(xlsx)
data2 <- read.xlsx("SIOP Data Wrangling Master Tutorial Data Set.xlsx", sheetName = "SIOP Data Wrangling Master Tuto")
data2

##Load libraries for importing different file types -- .sav

library(foreign)
data3 <- read.spss("SIOP Data Wrangling Master Tutorial Data Set.sav", to.data.frame=TRUE)
data3

##Load libraries for importing different file types -- .dat
library(foreign)
data4 <- read.table("SIOP Data Wrangling Master Tutorial Data Set.dat",sep= "\t", header = TRUE)
data4

head(data,5)

str(data)



names(data)

#Replacing spaces with underscores
names(data) <- gsub(x = names(data),
                    pattern = " ",
                    replacement = "_")

#Replace ":" with "_" in variable names
names(data) <- gsub(x = names(data),
                    pattern = "\\:",
                    replacement = "_")


#Replacing em-dash
names(data) <- gsub(x = names(data),
                    pattern = '.[\u2013].', #"-" aka "em-dash" From: https://stat.ethz.ch/pipermail/r-help/2017-April/438139.html
                    replacement = ".")

#removing double quotes and curly double quotes
names(data) <- gsub(x = names(data),
                    pattern = "[\u201C\u201D\u201E\u201F\u2033\u2036]",
                    replacement = "")

#Check columns to make sure code worked
colnames(data)

##Remove text before last period for MC items

names(data)[9:59] <- gsub(x = names(data)[9:59], 
                    pattern = ".*\\.", 
                    replacement = "")

colnames(data)


##brute force variable name change, Method #1

names(data) <- gsub(x = names(data),
                    pattern = "On.a.scale.of.zero.to.ten..how.likely.is.it.that.you.would.recommend.Company.to.friends.as.a.great.place.to.work.",
                   replacement = "NPS_Score")



##brute force variable name change, Method #2

data <- rename(data, 
                    OEQ_1 = 
                       `What.2.3.things.do.you.value.most.about.working.at.Company.`,
                     OEQ_2 = 
                       `What.2.3.things.should.Company.begin.to.do.`,
                     OEQ_3 = 
                       `What.2.3.things.should.Company.stop.doing.`,
                     OEQ_4 = 
                       `Please.provide.suggestions.for.ongoing.improvement.to.the.performance.feedback.process.`)



##Check to see if it worked

names(data)

data <- data %>% 
    select(-Respondent.IP, -Spaces, -Colons, -EmDash,-Quotes)
names(data)


names(data)

data_melt <- melt(data, id.vars = c("Number", 
                 "First.Name",
                 "Last.Name",
                 "Email.Address",
                 "Department",
                 "Division",
                 "Completed.On"))

head(data_melt, 10)

##separating the variable column on the '_' into Area and Item

data_melt <- separate(data_melt, variable, into = c("Area", "Item"), sep ="_", remove = FALSE)

##Check to make sure you made your new columns

names(data_melt)

##Also check the head of the data to make sure the observations look okay

head(data_melt, 10)

unique(data_melt$variable)

## create empty Alias column

data_melt$Alias <- NA
names(data_melt)

data_melt <- data_melt %>%
  mutate(Alias =
ifelse(variable == "ECS_Q1", "I am proud to work at Company." , 
ifelse(variable == "ECS_Q2", "I feel appreciated for the work I do." , 
ifelse(variable == "ECS_Q3", "I have what I need to do my job" , 
ifelse(variable == "ECS_Q4", "My job is good." ,       
ifelse(variable == "ECS_Q5", "I like my work." , 
ifelse(variable == "ECS_Q6", "Works is fun.",        
ifelse(variable == "WEC_Q1", "I feel free to say what I want." , 
ifelse(variable == "WEC_Q2", "Ideas are cool." , 
ifelse(variable == "WEC_Q3", "We go to happy hours." , 
ifelse(variable == "WEC_Q4","I know why we do things." ,
ifelse(variable == "WEC_Q5", "Things are fair." , 
ifelse(variable == "CB_Q1","I know how much I make." , 
ifelse(variable == "CB_Q2", "I get paid enough." , 
ifelse(variable == "CB_Q3", "I am happy with my pay" , 
ifelse(variable == "CB_Q4", "I get my more money if I work hard" , 
ifelse(variable == "PM_Q1", "My boss knows stuff." , 
ifelse(variable == "PM_Q2", "My boss helps me." , 
ifelse(variable == "PM_Q3", "My boss is helpful." , 
ifelse(variable == "PM_Q4", "My boss does his/her job." , 
ifelse(variable == "PM_Q5", "My other boss is helpful." , 
ifelse(variable == "PM_Q6", "My other buss helps me." ,     
ifelse(variable == "PM_Q7", "My other boss does his/her job." , 
ifelse(variable == "CDT_Q1", "Company taught me how to do my job well." , 
ifelse(variable == "CDT_Q2", "Company gives me opportunities." , 
ifelse(variable == "CDT_Q3", "I can grow at Company." , 
ifelse(variable == "CDT_Q4", "I want to grow at Company." , 
ifelse(variable == "CDT_Q5","Everyone has a fair shot at growth at Company." , 
ifelse(variable == "CDT_Q6", "Company has clear promotion standards." ,   
ifelse(variable == "COMM_Q1", "I know when Company changes stuff." , 
ifelse(variable == "COMM_Q2", "I get info about how to do my job well." , 
ifelse(variable == "COMM_Q3", "Corp and the people have transparency." , 
ifelse(variable == "COMM_Q4", "I know why Company changes stuff." ,        
ifelse(variable == "TE_Q1", "At Company we work effectively as a team." , 
ifelse(variable == "TE_Q2", "At Company, I can use my strengths to succeed" , 
ifelse(variable == "TE_Q3", "We have good teamwork." , 
ifelse(variable == "TE_Q4", "I have shared goals with my co-workers." , 
ifelse(variable == "TE_Q5", "My team lets me know when I mess up." , 
ifelse(variable == "JS_Q1", "I don't have too much work." , 
ifelse(variable == "JS_Q2", "I have a good work-life balance." , 
ifelse(variable == "JS_Q3", "My work environment is fun." , 
NA)))))))))))))))))))))))))))))))))))))))))

###split because ifelse nesting can only handle 50 statements at a time    
data_melt <- data_melt %>%
  mutate(Alias =      
ifelse(variable == "CI_Q1", "Our customers rock" , 
ifelse(variable == "CI_Q2", "I like working hard for our customers." , 
ifelse(variable == "CI_Q3", "Customer service provides meaning to my work." , 
ifelse(variable == "CI_Q4", "Company cares about our customers." , 
ifelse(variable == "STRAT_Q1", "I am confidence the company is going in a good direction." , 
ifelse(variable == "STRAT_Q2", "Company has a good plan for the future." , 
ifelse(variable == "STRAT_Q3", "I know how I contribute to Company's goals" , 
ifelse(variable == "NPS_Score", "On a scale of zero to ten how likely is it that you would recommend Company to friends as a great place to work" , 
ifelse(variable == "OEQ_1", "What do you like about working at Company?", 
ifelse(variable == "OEQ_2", "What do you not like about working at Company", 
ifelse(variable == "OEQ_3", "What could Company improve?", 
ifelse(variable == "OEQ_4", "Any other info for Company?", 
       Alias)))))))))))))

head(data_melt, 10)

##See full data
unique(data_melt$Alias)

data_melt

##Creating Alias_Area

data_melt$Alias_Area <- NA
unique(data_melt$Area)

##Creating Alias_Area

data_melt <- data_melt %>%
  mutate(Alias_Area = 
ifelse(Area == "ECS","Commitment and Satisfaction" , 
ifelse(Area == "WEC", "Workplace Environment and Culture" , 
ifelse(Area == "CB", "Compensation and Benefits" , 
ifelse(Area == "PM", "Performance Management" , 
ifelse(Area == "CDT", "Career Development and Training" , 
ifelse(Area == "COMM", "Communications" , 
ifelse(Area == "TE", "Team Effectiveness" , 
ifelse(Area == "JS" , "Job Stress",
ifelse(Area == "CI", "Company Image" , 
ifelse(Area == "STRAT", "Strategy" ,     
ifelse(Area == "NPS" , "NPS",
ifelse(Area == "OEQ" , "OEQ" ,
        NA)))))))))))))

##Check out the new observations within Alias_Area

unique(data_melt$Alias_Area)

data_melt <- data_melt %>% 
  mutate(response = ifelse(value == "Strongly Disagree", 1,
                 ifelse(value == "Disagree", 2,
                 ifelse(value == "Somewhat Disagree", 3,
                 ifelse(value == "Somewhat Agree", 4,
                 ifelse(value == "Agree", 5,
                 ifelse(value == "Strongly Agree", 6,
                    value)))))))
unique(data_melt$response)    

str(data_melt)

data_melt_num <- data_melt %>%
    filter(Area != "OEQ" &
          Area != "NPS")

unique(data_melt_num$Area)

##convert 'response' to numeric variable type in data_melt_num

data_melt_num$response <- as.numeric(data_melt_num$response)
class(data_melt_num$response)
head(data_melt,5)

##Grouping into 'Area' and taking the mean

Topic_Area_Means <- data_melt_num %>%
group_by(Area) %>%
summarize(Area_Mean = round(mean(response,na.rm = TRUE),2))
Topic_Area_Means

##Create a pretty theme

windowsFonts(Calibri=windowsFont("Calibri"))

Hor_Bar_LightBlue_Theme <-theme(
text = element_text(family = "Calibri"),
axis.title.x = element_blank(),
axis.title.y = element_blank(),
axis.line = element_blank(),
#panel.border = element_blank(),
#panel.grid=element_blank(),
axis.ticks = element_blank(),
plot.title=element_text(size=20, face="bold",hjust=0.6),
plot.subtitle=element_text(size=15,face=c("bold","italic"),hjust=0.6),
axis.text.x=element_text(size=15),#element_blank(),
axis.text.y=element_text(size=20),
legend.position = "none",
panel.spacing=unit(2,"cm"),
    panel.background = element_rect(fill = "white",colour = NA), # or element_blank()
    panel.grid.minor = element_line(color = "gray90", size = 0.20),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.20),
    panel.grid.major.y = element_blank()#removes horizontal lines
    # plot.background = element_rect(fill = "transparent",colour = NA)
)

##Horizontal Bar Chart

hbar_Overall_Area <- ggplot(Topic_Area_Means, aes(Area, y=Area_Mean, fill=Area)) +
  geom_bar(stat='identity') + #Light Blue
  geom_text(aes(x=Area, y=Area_Mean, label=sprintf("%0.2f", round(Area_Mean, digits = 2))),hjust = -0.1, color="#4D4D4D", size = 6, fontface = "bold") + 
  Hor_Bar_LightBlue_Theme + 
  coord_flip(ylim = c(1,6)) + scale_y_continuous(breaks=seq(1,6,1), position = "right") #Add this to make 1,2,3,4,5,6 appear on axis 
#print it!
print(hbar_Overall_Area)


