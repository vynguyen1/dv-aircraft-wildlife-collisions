library(ggplot2)
library(openintro)
data("birds")

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
## State - Wildlife relationship
############

# 20 most frequent species
ggplot(as.data.frame(sort(table(birds$species), decreasing = TRUE)[1:20]), aes(y=reorder(Var1, Freq), x=Freq)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

table(birds$species, birds$state)

# Number of Gulls collisions by state
gulls_collisions <- birds[birds$species == 'GULLS',]
ggplot(gulls_collisions, aes(..count.., state)) + geom_bar(aes(fill = state)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Species in California collisions
california_collisions<-birds[birds$state == 'CA',]
ggplot(california_collisions, aes(..count.., species)) + 
  geom_bar(aes(fill = species))

# Number of collisions per state
ggplot(data = birds[birds$birds_struck != "0" | !is.na(birds$birds_struck),], aes(..count.., state, fill = as.factor(ac_mass))) + 
  geom_bar()
  #scale_fill_continuous(low="grey", high="red")


# Number of collisions per state in a map
library(maps)
library(mapdata)

usa <- map_data('usa')
us.state <- map_data("state")
df.state <- as.data.frame(cbind(state.abb, state.name)); df.state
df.state$state.name <- tolower(df.state$state.name)

birds$struck_num<-unclass(birds$birds_struck)

birds.with.statenames<-merge(x = birds, y = df.state, by.x = "state", by.y = "state.abb")
filtered.birds<-birds.with.statenames[!is.na(birds.with.statenames$birds_struck),]

require(plyr)
birds.filtered$struck_num <- mapvalues(birds.filtered$struck_num, 
                                     from=c(1,2,3,4,5), 
                                     to=c(0,1,5,50,100))

num.collisions.per.state<-aggregate(filtered.birds$struck_num, by=list(region=filtered.birds$state.name), FUN=sum)
dfs.merged<-merge(us.state, num.collisions.per.state, by = "region")
length(unique(us.state$region))  # 49
length(num.collisions.per.state$region) # 50

`%nin%` = Negate(`%in%`)
unique(us.state$region)[unique(us.state$region) %nin% num.collisions.per.state$region]   # District Of Columbia
unique(num.collisions.per.state$region)[unique(num.collisions.per.state$region) %nin% us.state$region]  # Alaska, Hawaii

ggplot(data=dfs.merged[with(dfs.merged, order(-group, order)), ], aes(x=long, y=lat, fill=x, group=group)) + 
  geom_polygon(color = "white") + 
  scale_fill_continuous(low="grey", high="red") +
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)

  
ggplot(data=us.state, aes(x=long, y=lat, fill=region, group=group)) + 
  geom_polygon(color = "white") + 
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)


## Kategorisierung Bird/Non-Bird (Grep wo species bird beinhaltet & dann nach height; alles kleiner als z.B. 7 ist nicht Vogel)

is.bird<-grepl("BIRD|DUCK|PIGEON|HAWK|GULL|DOVE|PARROT|SWAN|GEESE|GOOSE|TURKEY|NIGHTJAR|GREBE|CARDINAL|MARTIN|BLUE JAY|CRANE|WHIP-POOR-WILL|TOWHEE|WARBLER|CORMORANT|HARRIER|BUNTING|PHEASANT|SANDPIPER|PELICAN|OSPREY|ANHINGA|WREN|MAGPIE|MANNIKIN|FINCH|ORIOLE|SNIPE|WIGEON|STARLING|AVOCET|GROUSE|BOBWHITE|CROW|TERN|VULTURE|ROBIN|OWL|FALCON|KILLDEER|GRACKLE|MYNA|PLOVER|MALLARD|SWALLOW|EGRET|KESTREL|LARK|SPARROW|HERON|EAGLE|CHICKADEE|MERGANSER", birds.with.statenames$species) | grepl("BIRD", birds.with.statenames$remarks) | birds.with.statenames$height >= 7
birds.with.statenames$is.bird<-is.bird

# Species that are not bird
ggplot(birds.with.statenames[!birds.with.statenames$is.bird & !is.na(birds.with.statenames$is.bird),], aes(..count.., species)) + geom_bar(aes(fill = species)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

filtered.isbird<-birds.with.statenames[!is.na(birds.with.statenames$is.bird),]
is.bird.state<-aggregate(filtered.isbird$is.bird, by=list(region=filtered.isbird$state.name), FUN=sum)
num.coll.per.state<-as.data.frame(table(filtered.isbird$state.name))
is.bird.state$x <- is.bird.state$x / num.coll.per.state$Freq
dfs.merged.2<-merge(us.state, is.bird.state, by = "region")

# The redder the more birds
ggplot(data=dfs.merged.2[with(dfs.merged.2, order(-group, order)), ], aes(x=long, y=lat, fill=x, group=group)) + 
  geom_polygon(color = "white") + 
  scale_fill_continuous(low="blue", high="red") +
guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)

## Birds/Non-Birds in zwei Karten nebeneinander
# https://ggplot2.tidyverse.org/reference/geom_map.html

## Look at different species (what's closer to equator)
# Eine Spezies raussuchen: mappen
is.vulture<-grepl("VULTURE", filtered.isbird$species)
filtered.isbird$is.vulture <- is.vulture
is.vulture<-aggregate(filtered.isbird$is.vulture, by=list(region=filtered.isbird$state.name), FUN=sum)
sum(is.vulture$x) # 172
is.vulture$x <- is.vulture$x / num.coll.per.state$Freq
dfs.merged.3<-merge(us.state, is.vulture, by = "region")

ggplot(data=dfs.merged.3[with(dfs.merged.3, order(-group, order)), ], aes(x=long, y=lat, fill=x, group=group)) + 
  geom_polygon(color = "white") + 
  scale_fill_continuous(low="grey", high="red") +
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)

# Proportionale Kollisionen?

non.bird.species<-as.data.frame(table(birds.with.statenames[birds.with.statenames$is.bird & !is.na(birds.with.statenames$is.bird),]$species))
ggplot(non.bird.species[order(non.bird.species$Freq, decreasing = TRUE),], aes(label = Var1, size=Freq)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 10)


data("love_words_small")
set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 40) +
  theme_minimal()
