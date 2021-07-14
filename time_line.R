library(ggplot2)
library(openintro)
data("birds")
library(patchwork)

######################################
## Change levels
############
factor_phase_flt<-c("Parked", "Taxi", "Take-off run", "Climb", "En Route", "Descent", "Approach", "Landing Roll")
birds$phase_of_flt<-factor(birds$phase_of_flt, levels=factor_phase_flt)

factor_struck<-c("0", "1", "2-10", "11-100", "Over 100")
birds$birds_struck<-factor(birds$birds_struck, levels=factor_struck)


######################################
## Missing values
############
table(birds$birds_struck)
sum(is.na(birds$birds_struck)) # amount of na values for birds struck: 39
sum(birds$birds_struck=="0", na.rm=T)  # 33

#get rows where birds struck is missing
df <- birds[is.na(birds$birds_struck), c("remarks", "birds_seen", "effect", "birds_struck")]

no_strike <- grepl("NOT A STRIKE|NO STRIKE|NOT A STRKE", df$remarks)
sum(no_strike)  # 19

df[no_strike,]
# set birds_struck to 0 for "no strike"
birds[is.na(birds$birds_struck),][no_strike,"birds_struck"] <- "0"

sum(is.na(birds$birds_struck))  # now 20
birds[is.na(birds$birds_struck),] 
sum(birds$birds_struck=="0", na.rm=T)  # now 52


######################################
## Time line
############

str(birds$date)
birds$date<-gsub(pattern="\\s?\\d{1,2}:\\d{2}:\\d{2}", replacement="", x=birds$date)
birds$month<-format(as.Date(as.character(birds$date), format = "%m/%d/%Y"), "%m")

# Data from 1995 (as test)
collisions.1995<-birds[grepl("1995", birds[["date"]]),]
date_collisions <- ggplot(collisions.1995) +
  aes(x = as.Date(as.character(date), format = "%m/%d/%Y"), y = birds_struck) +
  geom_point()
date_collisions + scale_x_date(date_breaks = "30 days", date_labels = "%d %b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# monthly grouping over all years:
month_plot<-ggplot(birds[!is.na(birds$birds_struck),]) +
  aes(x = month, group=birds_struck, fill=birds_struck) +
  geom_bar()
# -> more collisions during autumn/fall

# Or weekly:
birds$day<-format(as.Date(as.character(birds$date), format = "%m/%d/%Y"), "%a")
week_plot<-ggplot(birds[!is.na(birds$birds_struck) & as.integer(birds$birds_struck) > 3,]) +
  aes(x = day, group=birds_struck, fill=birds_struck) +
  geom_bar(position="fill")
# -> bird collisions not dependent on weekdays 

# Time of Day
tod_plot<-ggplot(birds) +
  aes(x = month, fill=time_of_day) +
  geom_bar(position="fill")
# -> possible explanation: It gets dark later in the summer (in the US)
# -> less strucks during the night in the summer
?strptime()

# Week of the Year
birds$calendar_week<-format(as.Date(as.character(birds$date), format = "%m/%d/%Y"), "%W")
woy_plot<-ggplot(birds) +
  aes(x = calendar_week, group=birds_struck, fill=birds_struck) +
  geom_bar()

month_plot / woy_plot
week_plot / tod_plot

month_plot_filt<-ggplot(birds[!is.na(birds$birds_struck) & as.integer(birds$birds_struck) > 3,]) +
  aes(x = month, group=birds_struck, fill=birds_struck) +
  geom_bar()

month_plot / month_plot_filt

birds$first_half_year<-factor(as.factor(as.numeric(birds$month) <= 6), levels=c(TRUE, FALSE))
ggplot(birds[!is.na(birds$birds_struck) & as.integer(birds$birds_struck) > 3,]) +
  aes(x = as.factor(first_half_year), group=birds_struck, fill=birds_struck) +
  geom_bar()
# way more collisions in second half of the year for birds -> mehr Zugvögel?
# But they would have to come back in spring which we can't see in the diagram


# Maybe look at species with 11-100 or over 100
ggplot(birds[!is.na(birds$birds_struck) & as.integer(birds$birds_struck) > 3,]) +
  aes(y = species, group=birds_struck, fill=birds_struck) +
  geom_bar()

# TODO:
## 1. Anzahl der Kollisionen auf Map plotten Sommer/Winter für eine bestimmte Spezies
library(maps)
library(mapdata)

usa <- map_data('usa')
us.state <- map_data("state")
df.state <- as.data.frame(cbind(state.abb, state.name)); df.state
df.state$state.name <- tolower(df.state$state.name)

birds$struck_num<-unclass(birds$birds_struck)

birds.with.statenames<-merge(x = birds, y = df.state, by.x = "state", by.y = "state.abb")
filtered.has.species<-birds.with.statenames[!is.na(birds.with.statenames$species),]
is.hawk<-grepl("HAWK", filtered.has.species$species)
filtered.hawk <- filtered.has.species[is.hawk,]

filtered.hawk.summer<-filtered.hawk[as.numeric(filtered.hawk$month) >= 4 & as.numeric(filtered.hawk$month) < 10,]
filtered.hawk.winter<-filtered.hawk[as.numeric(filtered.hawk$month) <= 3 | as.numeric(filtered.hawk$month) >= 10,]

num.collisions.per.state.summer<-aggregate(filtered.hawk.summer$struck_num, by=list(region=filtered.hawk.summer$state.name), FUN=sum)
num.collisions.per.state.winter<-aggregate(filtered.hawk.winter$struck_num, by=list(region=filtered.hawk.winter$state.name), FUN=sum)

dfs.merged.summer<-merge(us.state, num.collisions.per.state.summer, by = "region")
dfs.merged.winter<-merge(us.state, num.collisions.per.state.winter, by = "region")

plot_summer<-ggplot(data=dfs.merged.summer, aes(x=long, y=lat, fill=x, group=group)) + 
  geom_polygon(color = "white") + 
  scale_fill_continuous(low="grey", high="red") +
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('Hawk collisions during Summer') + 
  coord_fixed(1.3)

plot_winter<-ggplot(data=dfs.merged.winter, aes(x=long, y=lat, fill=x, group=group)) + 
  geom_polygon(color = "white") + 
  scale_fill_continuous(low="grey", high="blue") +
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('Hawk collisions during Winter') + 
  coord_fixed(1.3)

plot_summer / plot_winter

# 2. Grouping per year -> Anzahl der Kollisionen
birds$year<-format(as.Date(as.character(birds$date), format = "%m/%d/%Y"), "%Y")
birds.filtered<-birds[!is.na(birds$struck_num),]

require(plyr)
birds.filtered$struck_num <- mapvalues(birds.filtered$struck_num, 
                               from=c(1,2,3,4,5), 
                               to=c(0,1,5,50,100))

num.collisions.per.month<-aggregate(birds.filtered$struck_num, by=list(month=birds.filtered$month), FUN=sum)

library(dplyr)
strucks.per.year.month<-birds.filtered %>%
  group_by(year, month) %>% 
  summarize_at("struck_num",sum,na.rm=TRUE)

ggplot(strucks.per.year.month, aes(x = month, y = struck_num, colour=year, group = year)) + geom_line() + geom_point()

