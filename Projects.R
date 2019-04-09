library(dplyr)
library(tidyverse)
library(ggthemes)
library(anytime)
library(lubridate)
library(rworldmap)
raw_data <- read.csv("Kickstarter001.csv", header = TRUE, sep = ",")

# There are 3784 data totally and there are 3680 projects are completed.
live_data <- raw_data %>% filter(raw_data$state == "live")

# To clean the "catagory" column
raw_data$category <- raw_data$category %>%
  str_extract("slug\":\".+\",\"") %>%
  str_replace_all("\",\"", "") %>%
  str_replace_all("slug\":\"", "") %>%
  str_replace("/.+", "")



# To clean the "location" column
raw_data$location <- raw_data$location %>%
  str_extract("name\":\".+\",\"") %>%
  str_replace("\",\".+", "") %>%
  str_replace_all("name\":\"", "")

# To get rid of creator column
raw_data$creator <- NULL

# To get rid of photo column
raw_data$photo <- NULL

# To get rid of slug column
raw_data$slug <- NULL

# To get rid of url column
raw_data$urls <- NULL

# To add a preparation_duration column
raw_data$preparation_duration <- raw_data$launched_at - raw_data$created_at
raw_data$preparation_duration_r <- seconds_to_period(raw_data$preparation_duration)
# To add a launch_duration column
raw_data$launch_duration <- raw_data$deadline - raw_data$launched_at
raw_data$launch_duration_r <- seconds_to_period(raw_data$launch_duration)
raw_data$launch_duration_r <- day(raw_data$launch_duration_r)
# To convert epoch seconds to readable time
raw_data$created_at_readable <- anytime(raw_data$created_at)
raw_data$deadline_readable <- anytime(raw_data$deadline)
raw_data$launched_at_readable <- anytime(raw_data$launched_at)

# To get rid of url column
raw_data$preparation_duration_r <- NULL

clean_data <- raw_data
##-----------------------------------------------
# Data exploration

#1. Summarise the number of projects for each status
status_prjects <- clean_data %>%
  group_by(clean_data$state) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot the number of projects for each status
ggplot(status_prjects, aes(reorder(status_prjects$`clean_data$state`, -count), count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Status") + xlab("Project Status") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#1.1 

#2. Summarise the number of projects for each catagory
catagory_projects <- clean_data %>%
  group_by(clean_data$category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot the popularity of each category, which is dertermined by the number of projects
ggplot(catagory_projects, aes(reorder(catagory_projects$`clean_data$category`, -count), count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Category") + xlab("Project Category") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")


#3. What types of projects are being funded?
pledged_category <- clean_data %>%
  group_by(clean_data$category) %>%
  summarise(total = sum(usd_pledged)) %>%
  arrange(desc(total))

# Plot the amount pledged by each category
ggplot(pledged_category, aes(reorder(pledged_category$`clean_data$category`, -total), total/1000000, fill=total)) + geom_bar(stat="identity") + 
  ggtitle("Total Amount Pledged by Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD millions)") + 
  geom_text(aes(label=paste0("$", round(total/1000000,1))), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#4. How much is pledged per backer for each category?
pledged_avg_category <- clean_data %>%
  group_by(clean_data$category) %>%
  summarise(pledged = sum(usd_pledged), backers=sum(backers_count)) %>%
  mutate(avg = pledged/backers) %>%
  arrange(desc(avg))

# Plot the amount pledged per backer for each category
ggplot(pledged_avg_category, aes(reorder(pledged_avg_category$`clean_data$category`, -avg), avg, fill=avg)) + geom_bar(stat="identity") + 
  ggtitle("Average Amount Pledged per Backer") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + 
  geom_text(aes(label=paste0("$", round(avg,2))), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#5. Get the 10 highest goal successful projects
top_ten_success <- clean_data[clean_data$state == "successful",] %>%
  select("blurb", "category", "goal", "state") %>%
  arrange(desc(goal))

#6. Get the average project goal.
goal_avg <- clean_data %>%
  group_by(category) %>%
  summarise(goals = sum(goal), projects = n()) %>%
  mutate(avg = goals/projects) %>%
  arrange(desc(avg))
  
# Plot the average project goal.
ggplot(goal_avg, aes(reorder(goal_avg$category, -avg), avg, fill=avg)) + geom_bar(stat="identity") + 
  ggtitle("Average Project Goal") + xlab("Project Category") + ylab("Project Goal (USD)") + 
  geom_text(aes(label=paste0("$", round(avg,0))), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#7. percentage for projects in each category
perc_projects <- clean_data %>%
  filter(state %in% c("successful", "failed")) %>%
  group_by(category, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(state), pct)

# Plot the percentage for each category
ggplot(perc_projects, aes(perc_projects$category, pct, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Project Category") + 
  xlab("Project Category") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) + 
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="white", size=5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold")) + coord_flip()

#8. Does project length affect success rate?
perc_length <- clean_data %>%
  filter(state %in% c("successful", "failed"), launch_duration_r < 61) %>%
  group_by(launch_duration_r, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count))

ggplot(perc_length[perc_length$state=="successful",], aes(launch_duration_r, pct)) + 
  geom_point(colour="royalblue4", size=2.5) + ggtitle("Success Rate vs. Project Length") + 
  xlab("Project Length (Days)") + ylab("Success Rate (%)") + 
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60)) + geom_vline(xintercept=30, colour="red") + 
  theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"))

#9. Where it is coming form?
countries_freq <- clean_data %>%
  group_by(country) %>%
  summarize(count=n())

countries.match <- joinCountryData2Map(countries_freq, joinCode="ISO2", nameJoinColumn="country")

mapCountryData(countries.match, nameColumnToPlot="count", 
               mapTitle="Number of Projects by Country", catMethod="logFixedWidth", 
               colourPalette="heat")
