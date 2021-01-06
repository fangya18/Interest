---
title: "Spotify Song2018"
author: "fangya"
date: "Updated: `r Sys.Date()`"
output: 
 html_document:
    number_sections: true
    fig_caption: true
    toc: true
    fig_width: 7
    fig_height: 4.5
    theme: cosmo
    highlight: tango
    code_folding: hide
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
library(treemap)
library(fmsb)
library(reshape)
library(MASS)
library(vcd)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
music <- read.csv("/Users/fangya/Desktop/Fangya/1.Data Science/program/top2018.csv", header=TRUE)
```
#       Introduction

The Top Spotify Tracks of 2018 dataset contains 100 of the most popular songs. In this notebook, we will analyze the data structure and try to identify what are the secret ingredients(Tempo, Keys, Name) for popular songs.
\
I normally listen to Chinese pop music or Classical music, thanks to this dataset, I listened to 20+ Western Hit Songs in 2 days. My personal favorite is **Happier** by *Marshmello*.

**Note**: *I used majority of my code for [Spotify Song Analysis 2017](https://www.kaggle.com/fangya/spotify-2017-popular-song-analysis), meanwhile I have added some personal opinions to the popular songs/artists for the 2018 analysis.* 

##    Data preparation {.tabset .tabset-fade .tabset-pills}

We will rescale some variables, such as Danceability, Energy , Speechiness, Liveness, Valence, and Accoustiness for visualization purposes. In addition, We will combine some variables (Key and Mode) and categorize tempo for them to make sense musically. 

### Data Rescaling

We will rescale Danceability, Energy , Speechiness, Liveness, Valence, and Accoustiness by multiply 100.

*  **Danceability**: describes how suitable a track is for dancing,0.0 is least danceable and 1.0 is most danceable.


* **Energy**: is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity.

*  **Speechiness**: detects the presence of spoken words in a track. Values above 0.66 describe tracks that are probably made entirely of spoken words. 

*  **Acousticness**: A confidence measure from 0.0 to 1.0 of whether the track is acoustic

*  **Liveness**: Higher liveness values represent an increased probability that the track was performed live.


*  **Valence**: A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), 
```{r warning=FALSE}
music$danceability<- music$danceability*100
music$energy<- music$energy*100
music$speechiness<- music$speechiness*100
music$acousticness<- music$acousticness*100
music$instrumentalness<- music$instrumentalness*100
music$liveness<- music$liveness*100
music$valence<- music$valence*100
```


### Data Preparation for Key Signatures and Tempo

We will categorize the tempo by classical music standard. We will show the tonality of each song by combining the variables key and mode, and then group the tonality of the songs by the key signatures.

* **Key** :The key the track is integers map to pitches using standard Pitch Class notation. E.g. 0 = C, 1 = C sharp/D flat, 2 = D, and so on.
* **Mode** : Major is represented by 1 and minor is 0.

**Key Characterstics and Mood**

* C Major :Innocently Happy
* C minor :Innocently Sad, Love-Sick
* C sharp minor : Despair, Wailing, Weeping
* C sharp Major: fullness, sonorousness, euphony
* D Major: Triumphant, Victorious War-Cries
* D minor: Serious, Pious, Ruminating
* D sharp minor: Deep Distress, Existential Angst
* D sharp Major: Cruel, Hard, Yet Full of Devotion
* E Major: Quarrelsome, Boisterous, Incomplete Pleasure
* E minor: Effeminate, Amorous, Restless
* F Major: Complaisance and calm
* E sharp minor: Obscure, Plaintive, Funereal
* F sharp Major : Conquering Difficulties, Sighs of Relief
* F sharp minor: Gloomy, Passionate Resentment
* G Major: Serious, Magnificent, Fantasy
* G minor: Discontent, Uneasiness
* G sharp Major : Death, Eternity, Judgement
* G sharp minor : Grumbling, Moaning, Wailing
* A Major : Joyful, Pastoral, Declaration of Love
* A minor : Tender, Plaintive, Pious
* A sharp major: Joyful, Quaint, Cheerful
* B flat minor : Terrible, the Night, Mocking
* B Major: Harsh, Strong, Wild, Rage
* B minor: Solitary, Melancholic, Patience


```{r warning=FALSE}
music$tone <- ifelse(music$mode==0, "minor", "major")

music$scale <-ifelse (music$key==0, "C",
                      ifelse(music$key==1, "C#",
                             ifelse(music$key==2, "D",
                               ifelse(music$key==3,"D#" ,
                                      ifelse(music$key==4, "E",
                                             ifelse(music$key==5, "E#",
                                                    ifelse(music$key==6,"F",
                                                           ifelse(music$key==7, "F#",
                                                                  ifelse(music$key==8, "G",
                                                                         ifelse(music$key==9,"G#",
                                                                                ifelse(music$key==10,"A",
                                                                                       "A#"))))))))))) 


music$keys <- paste(music$scale, music$tone, sep= " ")

music$keysign <- ifelse (music$keys %in% c("C major","A minor" ), "Original",
                         ifelse(music$keys %in% c("G major","E minor","D# minor" ), "F sharp",
                          ifelse(music$keys %in% c("D major","B minor" ), "F,C Sharp",
                            ifelse(music$keys %in% c("A major","F# minor" ), "F,C,G Sharp",    
                            ifelse(music$keys %in% c("E major","C# minor" ), "F,A,G,D Sharp",
                            ifelse(music$keys %in% c("B major","G# minor" ), "F,C,G,D,A Sharp",
                            ifelse(music$keys %in% c("F# major","G# minor" ), "F,A,C,G,D,A,E Sharp",
                             ifelse(music$keys %in% c("C# major","A# minor" ), "F,A,C,G,D,A,E,B Sharp",
                             ifelse(music$keys %in% c("F major","D minor","E# major" ), "B Flat",
                              ifelse(music$keys %in% c("G minor", "A# major" ), "B,E Flat",
                               ifelse(music$keys %in% c("C minor","D# major"), "B,E, A Flat",
                               ifelse(music$keys %in% c("F minor","G# major","E# minor" ), "B,A,D,E Flat",
                              "Unknown"))))))))))))


music$keylabel <- ifelse(music$keys== "C major",  "C major :Innocently Happy", 
                  ifelse (music$keys=="C minor", "C minor :Innocently Sad, Love-Sick",
                  ifelse(music$keys=="C# minor" , "C sharp minor : Despair, Wailing, Weeping", 
                   ifelse(music$keys=="C# major","C sharp major: Fullness, Sonorousness, Euphony",
                  ifelse(music$keys=="D major", "D major: Triumphant, Victorious War-Cries",
                  ifelse(music$keys=="D minor", "D minor: Serious, Pious, Ruminating",
                   ifelse(music$keys=="D# minor", "D sharp minor: Deep Distress, Existential Angst",
                   ifelse(music$keys=="D# major", "Cruel, Hard, Yet Full of Devotion",
                   ifelse(music$keys=="E major", "E major: Quarrelsome, Boisterous, Incomplete Pleasure",
                    ifelse(music$keys=="E minor", "E minor: Effeminate, Amorous, Restless",
                    ifelse(music$keys %in% c("E# major" ,"F major"), "F major: Complaisance and calm",
                    ifelse(music$keys %in% c("F minor", "E# minor"),  "F minor: Obscure, Plaintive, Funereal",
                    ifelse(music$keys=="F# major", "F sharp major : Conquering Difficulties, Sighs of Relief",
                    ifelse(music$keys=="F# minor", "F sharp minor: Gloomy, Passionate Resentment",
                    ifelse(music$keys=="G major", "G major: Serious, Magnificent, Fantasy",
                    ifelse(music$keys=="G minor", "G minor: Discontent, Uneasiness",
                    ifelse(music$keys=="G# major", "G sharp major : Death, Eternity, Judgement",
                    ifelse(music$keys=="G# minor", "G sharp minor: Grumbling, Moaning, Wailing",
                    ifelse(music$keys=="A major", "A major : Joyful, Pastoral, Declaration of Love",
                    ifelse(music$keys=="A minor", "A minor : Tender, Plaintive, Pious",
                    ifelse (music$keys=="A# major", "A sharp major: Joyful, Quaint, Cheerful",
                    ifelse(music$keys=="A# minor", "A sharp minor: Terrible, the Night, Mocking",
                           "Unknown")  )))))))      )))       )))))))))))

# tempo classification
music$tempoc[music$tempo >= 66 & music$tempo <76] <- "Adagio" 
music$tempoc[music$tempo >=  76 & music$tempo <108] <- "Andante" 
music$tempoc[music$tempo >= 108 & music$tempo <120] <- "Moderato" 
music$tempoc[music$tempo >= 120 & music$tempo <156 ] <- "Allegro" 
music$tempoc[music$tempo >= 156 & music$tempo <176] <- "Vivace" 
music$tempoc[music$tempo >= 176 ] <- "Presto" 


music$tlabel[music$tempo >= 66 & music$tempo <76] <-" 66- 76"
music$tlabel[music$tempo >=  76 & music$tempo <108] <- "76-108" 
music$tlabel[music$tempo >= 108 & music$tempo <120] <- "108- 120" 
music$tlabel[music$tempo >= 120 & music$tempo <156 ] <- "120 -156" 
music$tlabel[music$tempo >= 156 & music$tempo <176] <- "156-176" 
music$tlabel[music$tempo >= 176 ] <- "> 176" 

```


#  Spotify 2018 Top 100 Song Tracks Analysis 


## The Most Popular Songs

The top5 songs demonstrated a high level of danceability, valence, and energy. This shows audiences generally like happy, positive, and envigorated feelings. 
\
We would like to show the most popular artists on the billboard. *XXXTENTACION* and *Post Malone* has 6 songs each. 
\
*Ed Sheeran* and *Drake* made to the popular artists list for two consecutive years. As we can see from the average scores they tend to focus more on the energy, danceability, and valences of a song. 
\

Personally, I think it is quite interesting because in Chinese music lyrics is a very important index for a popular song.


```{r warnings=FALSE}
# Top 5 Songs
m5 <- music[c(1:5),]
m5<- m5[, c(2,4,5,9,10,12,13)] 
m5<- as.data.frame(m5)
m5.long <- melt(m5, id.vars="name")

mp1<- ggplot(data=m5.long, aes(x=variable, y=value))+geom_bar(aes(y=value, fill=name),stat="identity", alpha=0.8 , position="dodge")+ ylab("Value")+ xlab("Variables of a song")+coord_flip()+ggtitle("Top 5 songs in Spotify 2018 ")
mp1


# Top artists
a1 <- group_by(music, artists )
a2 <- dplyr::summarise(a1,  count=n())
a2 <- arrange(a2, desc(count))
a3 <- filter(a2, count>1)

# Graph the artists have more than 2 songs
ap1 <- ggplot(a3, aes(x=reorder(artists,count),y=count))+
  geom_bar(aes(y=count,fill=artists), stat="identity")+
  labs(x="Artists", y="Number of Songs",
       title="2018 Popular Artists On Billboard")+ theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) 
ap1

# the differences between 1,2,3,4 song artisits
a4<- merge (music, a2, x.by=artists)
a5 <- group_by(a4, count)
a6 <- summarise(a5, 
                   adance= mean(danceability), aenergy=mean(energy),  aspeech=mean(speechiness),aacous= mean(acousticness) , alive=mean(liveness) ,avalence=mean(valence))

# reshape it to the long format
a66<- as.data.frame(a6)
a66.long <- melt(a66, id.vars="count")
a66.long <- a66.long[with(a66.long, order(variable)),]

#circle bar plot
mdata1 <- a66.long
mdata1$id=seq(1, nrow(mdata1))
mlabel_data1=mdata1
mnumber_of_bar1=nrow(mlabel_data1)
angle1m= 90 - 360 * (mlabel_data1$id-0.5) /mnumber_of_bar1 
mlabel_data1$hjust<-ifelse( angle1m < -90, 1, 0)
mlabel_data1$angle<-ifelse(angle1m < -90, angle1m+180, angle1m)

mp <- ggplot(mdata1, aes(x=as.factor(id), y=value, fill=variable))+geom_bar(stat="identity", alpha=0.8) + ylim(-50,120)+theme_minimal()+theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,6), "cm")  ) +
  coord_polar()+
  geom_text(data=mlabel_data1, aes(x=id, y=value+10, label=count, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= mlabel_data1$angle, inherit.aes = FALSE ) + ggtitle("d") 
 
mp

```



## Individual Player analysis {.tabset .tabset-fade .tabset-pills}

We will see the radarchart of the most popular artists. 


### Marshmello

I like the song Happier! It is the farovite of the 20+ songs I listened today. Based on my taste , it is prefect hit song. The hook is catchy, lyrics are positive but with a little sorrowness, melody flows smoothly, and verses are easy to sing. 

The MV story is warm and complete.
**"I want to you to be happier!"**

<img src="https://static.billboard.com/files/media/Marshmello-Bastille-Happier-2018-billboard-1548-compressed.jpg" alt="Starbucks?" width="300" align="center">

```{r }
tsong13o <- filter(music, artists %in% c("Marshmello"))
tsong23o<-  tsong13o[, c(2,4,5,9,10,12,13)] 

# radar chart
rownames(tsong23o)=tsong23o$name
tsong33o <- tsong23o[, c(2,3,4,5,6,7)]
data3=rbind(rep(100,6) , rep(0,6) , tsong33o)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4))
 radarchart( data3  , axistype=1 , 
    #custom polygon
    pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
    #custom labels
    vlcex=1 , title="Marshmello Top Songs"
    )
legend(x=1.3, y=1.0, legend = rownames(data3[-c(1,2),]), bty = "n", pch=10 , col=colors_in , text.col = "black", cex=0.6, pt.cex=1.5)


```


### XXX TENTACION

Because of this list, I went to listen to XXX TENTACION.  My first impression is the tempo systematic. The drum stands out in **SAD! , Moonlight, Jocelyn Flores**. The piano plays *B quarter* note in **Change**. When the drum gives beat, it is very easy to dance to, as we can see the dancibility is very strong. 

<img src="http://m.gettywallpapers.com/wp-content/uploads/2020/01/XXXTentacion-Wallpaper-HD.png" alt="Starbucks?" width="300" align="center">


```{r }
tsong1x <- filter(music, artists %in% c("XXXTENTACION"))
tsong2x<-  tsong1x[, c(2,4,5,9,10,12,13)] 


# radar chart
rownames(tsong2x)=tsong2x$name
tsong3x <- tsong2x[, c(2,3,4,5,6,7)]
data=rbind(rep(100,6) , rep(0,6) , tsong3x)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.5,0.4,0.8,0.9),rgb(0.1,0.3,0.4,0.9),rgb(0.8,0.2,0.6,0.9) )

colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) , rgb(0.5,0.4,0.8,0.4),rgb(0.1,0.3,0.4,0.4),rgb(0.8,0.2,0.6,0.4))
radarchart( data  , axistype=1 , 
    #custom polygon
    pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
    #custom labels
    vlcex=1 , title="XXX TENTACION Top Songs"
    )
legend(x=1.3, y=1.0, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.7, pt.cex=1.5)
 
```

###  Post Malone

In my point of view, Post Malone's songs have relatively connected melody lines compare to other rappers. His music flows more and the lyrics are not too dense. His voice 

```{r }
tsong1m <- filter(music, artists %in% c("Post Malone"))
tsong2m<-  tsong1m[, c(2,4,5,9,10,12,13)] 


# radar chart
rownames(tsong2m)=tsong2m$name
tsong3m <- tsong2m[, c(2,3,4,5,6,7)]
data=rbind(rep(100,6) , rep(0,6) , tsong3m)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.5,0.4,0.8,0.9),rgb(0.3,0.3,0.6,0.9),rgb(0.4,0.3,0.8,0.6) )

colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) , rgb(0.5,0.4,0.8,0.4),rgb(0.3,0.3,0.6,0.4),rgb(0.4,0.3,0.8,0.4))
radarchart( data  , axistype=1 , 
    #custom polygon
    pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
    #custom labels
    vlcex=1 , title="Post Malone Top Songs"
    )
legend(x=1.3, y=1.0, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.7, pt.cex=1.5)
 
```

<img src="https://upload.wikimedia.org/wikipedia/commons/c/c3/Post_Malone_Stavernfestivalen_2018_%28202940%29.jpg" alt="Starbucks?" width="300" align="center">


### Drake (Kris Wu's Favorite)

Drake made it from 2017 to 2018. I feel his style is a little lay back, not too tense. The acoustic level is very low maybe because of the autotune.
\
*P.S Kris Wu likes Drake!吴亦凡喜欢Drake.*

```{r }
tsong13 <- filter(music, artists %in% c("Drake"))
tsong23<-  tsong13[, c(2,4,5,9,10,12,13)] 

# radar chart
rownames(tsong23)=tsong23$name
tsong33 <- tsong23[, c(2,3,4,5,6,7)]
data3=rbind(rep(100,6) , rep(0,6) , tsong33)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4))
 radarchart( data3  , axistype=1 , 
    #custom polygon
    pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
    #custom labels
    vlcex=1 , title="Drake Top Songs"
    )
legend(x=1.3, y=1.0, legend = rownames(data3[-c(1,2),]), bty = "n", pch=10 , col=colors_in , text.col = "black", cex=0.6, pt.cex=1.5)


```

<img src="https://upload.wikimedia.org/wikipedia/commons/1/15/Drake_at_The_Carter_Effect_2017_%2836818935200%29_%28cropped%29.jpg" alt="Starbucks?" width="300" align="center">

###  Ed Sheeran 

Ed sheeran made to the list again in 2018, in which, Shape of You and Perfect was in the list 2017. His songs demonstrated a substantial diversity abilities, and the tension is presented in various aspects.

```{r }
tsong1 <- filter(music, artists %in% c("Ed Sheeran"))
tsong2<-  tsong1[, c(2,4,5,9,10,12,13)] 


# radar chart
rownames(tsong2)=tsong2$name
tsong3 <- tsong2[, c(2,3,4,5,6,7)]
data=rbind(rep(100,6) , rep(0,6) , tsong3)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.5,0.4,0.8,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) , rgb(0.5,0.4,0.8,0.4))
radarchart( data  , axistype=1 , 
    #custom polygon
    pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
    #custom labels
    vlcex=1 , title="Ed Sheeran Top Songs"
    )
legend(x=1.3, y=1.0, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.7, pt.cex=1.5)
 
```


<img src="https://upload.wikimedia.org/wikipedia/commons/8/82/Ed_Sheeran_4%2C_2013.jpg" alt="Starbucks?" width="300" align="center">




# Music Theory 101

In order for our work to make sense musically, we need to go over 2 basic components of music very quick: Key and Rhythm. 

## Tonality VS Key signature


<img src="https://guitar.ricmedia.com/wp-content/uploads/key-signature-chart/key-signature-chart.jpg" alt="Starbucks?" width="400" align="center">

<img src="http://www.piano-keyboard-guide.com/wp-content/uploads/2015/05/key_signatures_chart.gif" alt="Beer?" width="400" align="center">

Key signature refers to what notes are sharp and flat. 
I understand tonality as the color of a music piece, two pieces of music can have same key signature, but totally different tonality. For example, First chapter of *Mozart G major violin* is happy and bubbly whereas First Chapter *Mendelssohn E minor violin concerto* shares a color of myseterious and sorrowness. But their key signature are both in *F Sharp*.


## Tempo Classifation

Tempo

* **Adagio** – slowly with great expression[10] (66–76 bpm)
* **Andante** – at a walking pace (76–108 bpm)
* **Moderato** – at a moderate speed (108–120 bpm)
* **Allegro** – fast, quickly, and bright (120–156 bpm) 
* **Vivace** – lively and fast (156–176 bpm)
* **Presto** – very, very fast (168–200 bpm)

# Basic Music Theory Analysis {.tabset .tabset-fade .tabset-pills}

We will take a look at what tone the top 100 songs use most frequently, and what is the most often used key signituares. and temp 

## Tonality of Top 100 songs

In general , Major chords revoke a happy feeling, whereas Minor chords represent sadness. I m very surprised Csharp major is used so often. In addition, Top 3 categories all share a grief, sad emotions.

```{r warning=FALSE}
tone1 <- group_by(music, keylabel )
tone2 <- dplyr::summarise(tone1,  count=n())
tone2 <- arrange(tone2, desc(count))


# Tonality treemap
treemap(tone2, index="keylabel", vSize="count", type="index", 
        palette="Pastel2", title="Top 100 Songs Key charactersics and Emotion", fontsize.title=12)


```

##  Major vs Minor

We can see more songs use Major chords indeed.

```{r warning=FALSE}

ctone1 <- group_by(music, keys )
ctone2 <- dplyr::summarise(ctone1,  count=n())
ctone2 <- arrange(ctone2, desc(count))


# Tonality treemap
treemap(ctone2, index="keys", vSize="count", type="index", 
        palette="Pastel2", title="Top 100 Songs Key charactersics", fontsize.title=12)



# major vs Minor
major <- group_by(music, tone )
major2 <- dplyr::summarise(major,  count=n())

# Major treemap
treemap(major2, index="tone", vSize="count", type="index", 
        palette="Pastel1", title="Top 100 Songs Major", fontsize.title=12)

```


# Key signature analysis {.tabset .tabset-fade .tabset-pills}

## Emotion with Key signature

From the graph, we can see more than 15 songs used *C# Major* and *A# minor*, which means F,A,C,G,D,A,B sharp. Im quite surprised the popular songs used such complicated key signatures.

```{r}

keystone1 <- group_by(music, keysign, keys )
keystone2 <- dplyr::summarise(keystone1,  count=n())
keystone2 <- arrange(keystone2, desc(count))

keysp1<- ggplot(data=keystone2, aes(x=reorder(keysign,count), y=count))+geom_bar(aes(y=count, fill=keys),stat="identity", alpha=0.8 )+ ylab("Value")+ xlab("Variables to a song")+coord_flip()+ggtitle("Key signature and Emotion")
keysp1


```


## What key signiture uses the most


```{r}
# Key signature count
keys1 <- group_by(music, keysign )
keys2 <- dplyr::summarise(keys1,  count=n())
keys2 <- arrange(keys2, desc(count))

# key signiature treemap
treemap(keys2, index="keysign", vSize="count", type="index", 
        palette="Pastel2", title="Top 100 Key Signature", fontsize.title=12)

```



# Rythme analysis  {.tabset .tabset-fade .tabset-pills}

## Tempo Classification

Now we tak a look at what is the most popular Tempo type amont the top 100 songs, We will use the categorized tempo type to do the analysis. We can see about half of the songs has tempo between 76 to 108. So Andante is the most used tempo. Adagio is the least used tempo. I guess slow songs is not that popular then.

```{r}
tempo1 <- group_by(music, tempoc ,tlabel )
tempo2 <- dplyr::summarise(tempo1,  count=n())
tempo2 <- arrange(tempo2, desc(count))

tempop1<- ggplot(data=tempo2, aes(x=reorder(tempoc,count), y=count))+geom_bar(aes(y=count),stat="identity", alpha=0.8,fill="skyblue" )+ ylab("Count")+ xlab("Tempo Type")+ggtitle("What is the most popular Tempo type? ")+
  geom_text(aes(label=tlabel), vjust=1, color="maroon", size=3.5)+ theme_minimal()
tempop1



```


## general Tempo range

```{r}

music$id1=seq(1, nrow(music))

plot1 <- ggplot(music, aes(x=reorder(id1,tempo),y=tempo)) +geom_bar(stat = "identity", col = "pink", fill = "pink")+theme_minimal()
plot1

qqnorm(music$tempo)
qqline(music$tempo, col="red")

```


# Categorical analysis on Key and Tempo{.tabset .tabset-fade .tabset-pills}



## Heatmap Valence respect to Key and Tempo

The songs showed the most valence with the Moderato tempo in C# major.

```{r}
vorig <- group_by(music, tempoc , keys)
vorig1 <- summarise(vorig,  count=n() ,rate= mean(valence))

 ggplot(vorig1, aes(x=tempoc, y=keys, fill = rate)) + 
    geom_tile(colour = "white")  +
    scale_fill_gradient(low="skyblue", high="Pink") +
    labs(x="Tempo", y=NULL, title="Heatmap of Valence" ,fill="Valence")

```


## Heatmap Danceability respect to Key and Tempo

We can see from the Adagio and G minor give the most dance ability. We can try to dance with Bach Sonata No.1 in G minor, BWV 1001- Adagio at home.

```{r}
orig <- group_by(music, tempoc , keys)
orig1 <- summarise(orig,  count=n() ,rate= mean(danceability))

 ggplot(orig1, aes(x=tempoc, y=keys, fill = rate)) + 
    geom_tile(colour = "white")  +
    scale_fill_gradient(low="lightgreen", high="violetred") +
    labs(x="Tempo", y=NULL, title="Heatmap of Danceability" ,fill="Danceability")

```

## Heatmap Energy respect to Key and Tempo

we can see Vivace and A# major demonstrated high energy. I think higher speed will represent higher energy. However, we notice with Vivace and C# major, the energy is quite low. I think that's because C# has grief depressive feeling

```{r}
eorig <- group_by(music, tempoc , keys)
eorig1 <- summarise(eorig,  count=n() ,rate= mean(energy))

 ggplot(eorig1, aes(x=tempoc, y=keys, fill = rate)) + 
    geom_tile(colour = "white")  +
    scale_fill_gradient(low="yellow", high="red") +
    labs(x="Tempo", y=NULL, title="Heatmap of Engery" ,fill="Engery")
 
 

```


## Heatmap Speechiness respect to Key and Tempo


```{r}

 sorig <- group_by(music, tempoc , keys)
sorig1 <- summarise(eorig,  count=n() ,rate= mean(speechiness))

 ggplot(sorig1, aes(x=tempoc, y=keys, fill = rate)) + 
    geom_tile(colour = "white")  +
    scale_fill_gradient(low="yellow", high="green") +
    labs(x="Tempo", y=NULL, title="Heatmap of Speechiness" ,fill="Speechiness")
 

```

# Mosaic Plot Keys vs Rythmn

Now we want to inverstigate if the key has relationship with rythmn . From the mosaic graph, there is no evidence to show Minor Key or Major Key has association with the Rythmn.

```{r mosaic plot}
ktstat <- group_by(music, tone, tempoc)
ktstat3 <- summarise(ktstat,  count=n())

v.lm <- loglm(count ~  tempoc+ tone, data=ktstat3)
v.m1<-mosaic(v.lm , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))


```

# Does the name of a song matter?


```{r echo=FALSE, message=FALSE , warning=FALSE}
names <- music[, c(2)]
set.seed(888)
wordcloud(names, max.words=10 ,random.order=FALSE,rot.per=0.35,colors=brewer.pal(4, "Dark2"), main="Title")

```

**PostCredit Scene- My Favorite Song in 2018**


I came across a song, **绯 Fei** by Mubo and Roi 2 weeks ago, i really like it. It is so easy to dance to! I forgot what song I liked the most in 2018, but this song is from 2018, so I decided to list it here.

<img src="https://y.gtimg.cn/music/photo_new/T002R300x300M000004K36Kg0ByKFv_1.jpg?max_age=2592000" alt="Starbucks?" width="300" align="center">

MuBo <3

#  References
[1] Tempo Classification, https://en.wikipedia.org/wiki/Tempo 

[2] Key Signature Chart,https://www.piano-keyboard-guide.com/key-signatures.html

