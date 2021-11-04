##Strawberries data wrangling
#setup
Sys.setenv("LANGUAGE" = "EN")
Sys.setlocale("LC_ALL", "C")
library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(ggiraph)
library(plotly)
library(shiny)

#read data
strb <- read.csv("strawberry_test.csv")

#dim(strb) # 10215 21

#NA data, columns with only 1 value 
#colSums(is.na(strb)) 

#create a function to drop these columns
drop_no_info_cols <- function(df){
  cnames <- colnames(df)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))} # unique values of each column
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

# we see columns with only one value: Geo.Level(STATE); watershed_code(0); 
#Commodity(STRAWBERRIES)

strb <- drop_no_info_cols(strb)
#colnames(strb)

#look at the type of chemical
# View(strb)
strb %<>% separate(col = Data.Item, 
                   into = c("Strawberries", "items", "discription", "units"), 
                   sep = ",", 
                   fill = "right")

# unique(strb$Strawberries)
# unique(strb$items)
# unique(strb$discription)
# unique(strb$units) 

#distinct(strb, Domain)

# copy column of Domain.Category
strb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category)

#separate the columns into readable variables
strb %<>% separate(col = Chemicals,
                   into = c("chemical.type", "chemical"),
                   sep = ":",
                   fill = "right")

#take out extraneous symbols
strb$chemical <- str_extract(str_trim(strb$chemical) ,"[^(].*[^)]")

#distinct(strb, chemical.type)


strb %<>% separate(col = chemical, 
                   into = c("chemical", "che_num"), 
                   sep = "=", 
                   fill = "right")



## Add the pesticides csv file
pest <- read.csv("pesticides.csv")
# dim(pest)
names(pest)[1] <- "Pesticide"

pest <- pest[!apply(pest == "",1, all), ] # delete empty rows


# include empty relationship
l_pest <- pest %>% gather(Toxicity, Level, Carcinogen:Bee.Toxins, -Pesticide)

# clarify a new column, human or bee
l_pest <- mutate(l_pest, humanORbee = ifelse(Toxicity == "Bee.Toxins", "Bee", "Human"))

# without empty relationship
rela_pest <- l_pest[!(l_pest$Level == ""), ]

# turn name into capital letters
rela_pest$Pesticide <- toupper(rela_pest$Pesticide)

names(rela_pest)[1] <- "chemical"


##Combine pest and strawb

strb$chemical <- str_trim(strb$chemical)
strb$discription <- str_trim(strb$discription)
# count(strb, chemical.type)
df <- merge(x= strb, y = rela_pest, by = "chemical")
# count(df, chemical.type)

df %<>% select(chemical, chemical.type, humanORbee, Level, State, Year, discription, Value, Year,  Toxicity)

df %<>% filter(!Value == " (D)" & !Value == " (Z)")

df %<>% filter(discription == "MEASURED IN LB / ACRE / APPLICATION")

#make the dataframe useful
df <- drop_no_info_cols(df)

df$Value <- as.numeric(df$Value)
df$Year <- as.factor(df$Year)
df$chemical.type <- as.factor(df$chemical.type)
# df %>% count(chemical)
# 
# df %>% count(chemical.type)

df %>%count(State)
#summary(df$Value)
# set flag:
# 1: 0.0010 ~ 0.1420
# 2: 0.1420 ~ 0.8004
# 3: 0.8004 ~ 1.0900 
# 4: 1.0900 ~ 3.0900 
df$Value_q <- as.factor(ifelse((df$Value <= .1420), "0.0010 ~ 0.1420", 
                               ifelse((df$Value > .1420 & df$Value <= .8004), "0.1420 ~ 0.8004", 
                                      ifelse((df$Value > .8004 & df$Value <= 1.0900), "0.8004 ~ 1.0900", "1.0900 ~ 3.0900" ))))

mu <- df %>% group_by(chemical.type) %>% summarise(mean = mean(Value))
mubee <- df %>% filter(humanORbee=="Bee") %>% summarise(mean = mean(Value)) 
#Extract the observation in California
df_Cal <- filter(df, State == "CALIFORNIA")

#######
#Plotting

#added two functions to the following 2 ggplot functions
#scale_x_discrete to only display every 4 years
#theme(axis.text.x to make it diagonal)
chem_year_valueq <- ggplot(df) + geom_bar(aes(x = Year, fill = Value_q), alpha = .8, position = "fill") + facet_grid(~ chemical.type) +  scale_x_discrete(breaks=seq(1990, 2019, 4)) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + ylab("Relative Frequency") + 
  scale_fill_discrete(name = "Value Quartile")+
  labs(title = "fig.1 Relative Frequency of Chemical use per year")

chem_state_valueq <- ggplot(df) + geom_bar(aes(x = State, fill = Value_q), alpha = .8, position = "fill") + facet_grid(~ chemical.type) + theme(legend.position  = "bottom") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab("Relative Frequency") + scale_fill_discrete(name = "Value Quartile") 


density <- ggplot() + geom_density(data = df, aes(x = Value, fill = chemical.type, color = chemical.type), alpha = .5) + geom_vline(data = mu, aes(xintercept = mean, color = chemical.type), linetype = "dashed", lwd = .8) + scale_fill_discrete(name = "Chemical Type") + scale_color_discrete(name = "Chemical Type")

violin <- ggplot(df, aes( x = chemical.type, y = Value, fill = chemical.type)) + geom_violin(alpha = .5) + geom_boxplot(width=0.1) + stat_summary(fun=mean, geom="point", shape=1, size=2) + theme(legend.position  = "none") + xlab("Chemical type") +
  labs(title = "fig.2 Distribution of Chemical use and corresponding strawberry production")

#interactive plot 
gg_point <- ggplot(df) + geom_point_interactive(aes(x = Year, y = Value, color = State, tooltip = chemical, data_id = chemical))  +  scale_x_discrete(breaks=seq(1990, 2019, 4))

plot <- girafe(ggobj = gg_point)

#plot


#Add plots by toxicity category, by state, and in California
toxic1 <- ggplot(df) + geom_freqpoly(aes(x = Value, color = Level), alpha= 100) + 
  geom_vline(data = mu, aes(xintercept = mean, color = chemical.type), linetype = "dashed", lwd = .8) + 
  coord_cartesian(ylim = c(0, 30))+
  facet_wrap(~Toxicity)+
  labs(title = "fig.5 Chemical use by Toxicity level")


# ggplot(filter(df, humanORbee=="Bee")) + 
toxic2 <- ggplot(df) + 
  geom_freqpoly(aes(x = Value, color = State), alpha= 100) + 
  geom_vline(data = mu, aes(xintercept = mean, color = chemical.type), linetype = "dashed", lwd = .8) + 
  scale_color_manual(values = c("CALIFORNIA"="red"))+
  coord_cartesian(ylim = c(0, 30))+
  facet_wrap(~humanORbee) +
  labs(title = "fig.6 Chemical use in California compared to other states")


#Plot for only CALIFORNIA
toxic3 <- ggplot(df_Cal) + geom_freqpoly(aes(x = Value, color = Level), alpha= 100) + 
  geom_vline(data = mu, aes(xintercept = mean, color = chemical.type), linetype = "dashed", lwd = .8) + 
  coord_cartesian( ylim = c(0, 30))+
  facet_wrap(~Toxicity)+
  labs(title = "fig.7 Chemical use by Toxicity level in California")

#interactive plot of the means 
fungi <- df  %>% group_by(State, chemical.type, Year)%>% summarise(Value = mean(Value)) %>% filter(chemical.type == "CHEMICAL, FUNGICIDE")

p_f <- ggplot(fungi, aes(x = Year, y = Value, group = State))+
  geom_point(aes(color = State))+
  geom_line(aes(color = State)) + 
  scale_x_discrete(breaks=seq(1990, 2019, 4))+
  labs(title = "fig.3 Strawberry production per acre per fungicide application over time by state")

#ggplotly(p_f)


insec <- df  %>% group_by(State, chemical.type, Year)%>% summarise(Value = mean(Value)) %>% filter(chemical.type == "CHEMICAL, INSECTICIDE")

p_i <- ggplot(insec, aes(x = Year, y = Value, group = State))+
  geom_point(aes(color = State))+
  geom_line(aes(color = State)) +
  scale_x_discrete(breaks=seq(1990, 2019, 4)) +
  labs(title = "fig.4 Strawberry production per acre per fungicide application over time by state")
#ggplotly(p_i)


