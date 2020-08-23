
library(dplyr)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(corrplot)
library(ggthemes)

Spotify.df <- read.csv("FinalDataSet.csv")
summary(Spotify.df)
dim(Spotify.df)

str(Spotify.df)
head(Spotify.df)


str(Spotify.df)
glimpse(Spotify.df)
summary(Spotify.df)

#Eliminating Unwanted Variables
Spotify.df <- Spotify.df[,-c(3,16,17)]
colnames(Spotify.df)

#Convert target to factor
Spotify.df$top100 <- as.factor(Spotify.df$top100)
str(Spotify.df)

#Converting Duration from ms to Seconds 
Spotify.df$duration_ms <- round(Spotify.df$duration_ms / 1000)
colnames(Spotify.df)[14] <- "duration"
colnames(Spotify.df)


#Only Numerical Columns of Data Frame
SpotifyNum.df <- Spotify.df[,-c(1,2,15,16)]

#No of Appearances in Top 100 in the Decade
Hits <- filter(Spotify.df, top100 == '1' )
View(Hits)
colnames(Hits)

HitsNum <- Hits[,-c(1,2,15,16)]

#Distribution of Features across all songs
SpotifyNum.df %>% 
  tidyr::gather() %>% 
  ggplot(aes(x=value)) + geom_histogram() + 
  facet_wrap(~key, scales='free', ncol=3) + 
  theme_economist()


#Distribution of Features across Hit Songs
HitsNum %>% 
  tidyr::gather() %>% 
  ggplot(aes(x=value)) + geom_histogram() + 
  facet_wrap(~key, scales='free', ncol=3) + 
  theme_economist()


colnames(Spotify.df)


#Distribution of Genre
ggplot(Spotify.df, aes(x=genre)) +  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ labs(title = "GENRE DISTRIBUTION", x = "GENRE", y = "COUNT OF GENRE") + 
  theme(plot.title = element_text(size=15,hjust=.5,face = "bold"), axis.title = element_text(size=13,face = "bold"), axis.text.x =  element_text(size=12,face = "bold"),axis.text.y =  element_text(size=12,face = "bold"))

#Distribution of Genre across Hits
ggplot(Hits, aes(x=genre)) +  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ labs(title = "GENRE DISTRIBUTION ACROSS HITS", x = "GENRE", y = "COUNT OF GENRE")+ 
  theme(plot.title = element_text(size=15,hjust=.5,face = "bold"), axis.title = element_text(size=13,face = "bold") ,axis.text.x =  element_text(size=12,face = "bold"),axis.text.y =  element_text(size=12,face = "bold"))

#Distribution of Hits
ggplot(Spotify.df, aes(top100)) + geom_bar(stat="count", width=0.7) + 
  labs(title = "HITS v/s NON-HITS", x = "TOP 100 (0 = NO , 1 = YES)", y = "COUNT OF SONGS") + 
  theme(plot.title = element_text(size=15,hjust=.5,face = "bold"), axis.title = element_text(size=13,face = "bold") ,axis.text.x =  element_text(size=12,face = "bold"),axis.text.y =  element_text(size=12,face = "bold"))


#Distributions of Hits by Genre
ggplot(Spotify.df, aes(fill = top100, x=genre)) +  geom_bar(stat="count", width=0.7, position = "dodge") + labs(title = "TOP 100 BY GENRE", x = "GENRE", y = "COUNT OF SONGS IN GENRE") +
  theme(plot.title = element_text(size=15,hjust=.5,face = "bold"), axis.title = element_text(size=13,face = "bold") ,axis.text.x =  element_text(size=12,face = "bold"),axis.text.y =  element_text(size=12,face = "bold"))


#Correlation Plot
colnames(Spotify.df)
VarCor <- cor(SpotifyNum.df)
corrplot(VarCor, method = "number", type = "full", tl.srt = 90)

VarCor1 <- cor(HitsNum)
corrplot(VarCor1, method = "number", type = "full", tl.srt = 90)



# Bar Plot Top Artists Of the Decade By No. Of Appearances in the Top 100
top_artists <- Hits %>%
  group_by(artist)  %>%
  summarise(n_appearance = n()) %>%
  filter(n_appearance > 15) %>%
  arrange(desc(n_appearance))

top_artists$artist <- factor(top_artists$artist, levels = top_artists$artist[order(top_artists$n_appearance)])

ggplot(top_artists, aes(x = artist, y = n_appearance)) +
  geom_bar(stat = "identity",  fill = "darkgoldenrod2", width = 0.6 ) + 
  labs(title = "TOP ARTISTS OF THE DECADE", x = "ARTISTS", y = "NUMBER OF APPEARANCES ON THE TOP 100") +
  theme(plot.title = element_text(size=15,hjust=.5,face = "bold"), axis.title = element_text(size=13,face = "bold") ,axis.text.x =  element_text(size=12,face = "bold"),axis.text.y =  element_text(size=12,face = "bold"))  +
  geom_text(aes(label=n_appearance), hjust = 2, size = 3, color = 'white') +
  coord_flip()



#Correlation Density Plot of Energy, Valence and Danceability for Hits

Density <- ggplot(Hits) +
  geom_density(aes(energy, fill ="Energy", alpha = 0.1)) + 
  geom_density(aes(valence, fill ="Valence", alpha = 0.1)) + 
  geom_density(aes(danceability, fill ="Danceability", alpha = 0.1)) + 
  scale_x_continuous(name = "Energy, Valence and Danceability") +
  scale_y_continuous(name = "Density") +
  ggtitle("DENSITY PLOT OF ENERGY, VALENCE AND DANCEABILITY") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12, face = "bold")) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="GnBu")

Density

#Loudness Density Plot for Hits

loudness_density <- ggplot(Hits) +
  geom_density(aes(loudness, fill ="Loudness")) + 
  scale_x_continuous(name = "Loudness") +
  scale_y_continuous(name = "Density") +
  ggtitle("DENSITY PLOT OF LOUDNESS FOR HITS") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12, face = "bold")) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Paired")

loudness_density

#ScatterPlot for Acousticness v/s Danceability 

ggDance <- ggplot(Spotify.df , aes(x = danceability , y = acousticness)) + geom_point(aes(color = top100)) + labs(title = "ACOUSTICNESS v/s DANCEABILITY" , y = "Acousticness" , x = "Danceability")
ggDance + theme(plot.title = element_text(size = 20 , face = "bold" , family = "Calibri" , color = "gray18" , hjust = 0.5 , lineheight = 1.2), axis.title = element_text(size = 13, face = "bold"), axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"))



#ScatterPlot for Acousticness v/s Loudness
ggLoud <- ggplot(Spotify.df , aes(x = loudness , y = acousticness)) + geom_point(aes(color = top100)) + labs(title = "ACOUSTICNESS v/s LOUDNESS" , y = "Acousticness" , x = "Loudness")
ggLoud + theme(plot.title = element_text(size = 20 , face = "bold" , family = "Calibri" , color = "gray18" , hjust = 0.5 , lineheight = 1.2), axis.title = element_text(size = 13, face = "bold"), axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"))

