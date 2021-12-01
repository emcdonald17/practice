library(dplyr)
library(janitor)
data <- read.csv("shots_data.csv")

#Add a column that will show which zone a shot is in based off the given
#x and y coordinates
shots = data %>% mutate(shottype = case_when(
  abs(x)>22.0 & abs(y)<=7.8 ~ "C3", 
  (x**2 + y**2 <= 23.75**2) ~ "2PT",
  (x**2 + y**2 > 23.75**2) ~ "NC3")
  )

#using the janitor package, the tabyl function can return distributions for each variable
tabyl(shots, shottype, team) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=1)


#Calculated eFG in two different ways
#1. Hard Code: Uses 3 pointers made in each specific Zone
##eFG for C3A and C3B
corner3pointA = shots %>% filter(team == "Team A", shottype=="C3") %>% count(fgmade)
(corner3pointAefg = (corner3pointA[2,2]+(0.5*corner3pointA[2,2])) / (corner3pointA[2,2] + corner3pointA[1,2]))
corner3pointB = shots %>% filter(team == "Team B", shottype=="C3") %>% count(fgmade)
(corner3pointBefg = (corner3pointB[2,2]+(0.5*corner3pointB[2,2])) / (corner3pointB[2,2] + corner3pointB[1,2]))
##eFG for 2PTA and 2PTB
point2A = shots %>% filter(team == "Team A", shottype == "2PT") %>% count(fgmade)
point2Aefg = (point2A[2,2]) / (point2A[2,2] + point2A[1,2])
point2B = shots %>% filter(team == "Team B", shottype == "2PT") %>% count(fgmade)
point2Befg = (point2B[2,2]) / (point2B[2,2] + point2B[1,2])
##eFG for NC3A and NC3B
NC3A = shots %>% filter(team == "Team A", shottype=="NC3") %>% count(fgmade)
NC3Aefg = (NC3A[2,2]+(0.5*NC3A[2,2])) / (NC3A[2,2] + NC3A[1,2])
NC3B = shots %>% filter(team == "Team B", shottype=="NC3") %>% count(fgmade)
NC3Befg = (NC3B[2,2]+(0.5*NC3B[2,2])) / (NC3B[2,2] + NC3B[1,2])
##Create a dataframe to display the values
efg = data.frame(team = c("Team A", "Team B"), C3eFG = c(corner3pointAefg, corner3pointBefg), TwoPTeFG = c(point2Aefg, point2Befg), NC3eFG = c(NC3Aefg, NC3Befg))
efg

#2. Easier way
##Returns total fg made for each team in each zone
shot1 = shots %>% select(team, fgmade, shottype) %>% group_by(team, shottype) %>% summarise(totalfgmade = sum(fgmade))
##Returns total fg attempted for each team in each zone
shot1totals = shots %>% group_by(team, shottype) %>% count(shottype)
##Merge the two, so you can see total fg made and attempted
shotsmerged = inner_join(shot1, shot1totals)
shotsmerged = shotsmerged %>% mutate(average = totalfgmade / n)
##Returns a data frame with the number of 3 pointers made for each team in each zone
shotsmerged = shotsmerged %>% mutate(point3made = ifelse(shottype == "2PT", 0, totalfgmade))
shotsmerged = shotsmerged %>% mutate(eFG = (totalfgmade + 0.5*point3made)/n )
shotsmerged




