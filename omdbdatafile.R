install.packages("devtools")  # instaling Devtools - it makes it easy to install R packages that are not on CRAN
devtools::install_github("hrbrmstr/omdbapi") # instaling omdb API package trugh using devtools
library(omdbapi) # loading the omdb api - this package is an R Package to access the Open Movie Database 
library(stringr)# loading a package that help with string manipulation such as regex expresions easier
library(dplyr) # loading dplyr package it give us more functions that correspond to the most common data manipulation tasks
install.packages("stringr") #installing String librarry a set of functions to help in working with string data 
library(stringr) #loading the package
install.packages("ggmap") #installing ggmap, its A collection of functions to visualize spatial data and models on top of static maps from various online sources 
library(ggmap) #loading ggmap we want it for the long and alt of places in the world
install.packages("ggplot2") #ggplot2 is a plotting system for R, based on the grammar of graphics
library(ggplot2) # loading ggplot
install.packages("corrplot") # The corrplot package is a graphical display of a correlation matrix
library(corrplot) #loading corrplot
install.packages("ggmap") # collection of functions to visualize spatial data and models on top of static maps from various online sources
library(ggmap) # loading ggmap

# read csv file of movie names it contain the top imdb rated 500 movies that we scraped from the web
MovieNamesToCheck <- read.csv("moviesNames.csv" ,header=FALSE, sep=",")  
numOfRecords = count(MovieNamesToCheck)[[1]] # save the data number of records
# This is the data pulling from the web procedure, it may take few minutes (about 10 minutes)
# this loop get all the initial data about the movies we want to exploer 
#afterward using the imdb movie ID numbers we geting the full information about the movie 
# for that we making another request that pulls the full movie data trugh the api
MoviesDataset = list()
for(i in 1:numOfRecords){
  temp <-  head(search_by_title(MovieNamesToCheck$V1[i]),1) # temp contain the initial data about the movie by name (we pick the first movie if there few with same name)
  moreDataTemp <- find_by_id(temp[3]) # using the ID name to pull the full data trugh the api
  MoviesDataset[[i]] <- moreDataTemp # the data stored in this list at the n'th position
}

# in this step we decided to move the data to data frame format so it will be easier to work with farther on
MoviesdataFrame <- data.frame(MoviesDataset[[1]])
for(i in 2:numOfRecords)
{
  MoviesdataFrame = rbind(MoviesdataFrame,MoviesDataset[[i]]) # moving all the data to data frame and binding it all together
}


Awerdss = list() 
MoviesdataFrame$Awards_oscarWon <- 0  
MoviesdataFrame$Awards_oscarNominated <-0
MoviesdataFrame$Awards_OtherWon <-0
MoviesdataFrame$Awards_OtherNominated <-0
MoviesdataFrame$Awards_ggWon <-0
MoviesdataFrame$Awards_BAFTAwon <-0
# in this section we pars the data from the awerds column to understande better the awards to score countrys 
for(i in 1:numOfRecords )
{
  MoviesdataFrame$Awards[i]
  Awerdss[[i]] <- list()
  #extract number from pettern "Won N Oscars" where N is a number
  Awerdss[[i]][1] = str_extract(str_extract(MoviesdataFrame$Awards[i],"Won \\(?[0-9]+\\)? Oscars"),"[0-9]+") 
  #extract number from pettern "Nominated for N Oscars" where N is a number
  Awerdss[[i]][2] = str_extract(str_extract(MoviesdataFrame$Awards[i],"Nominated for \\(?[0-9]+\\)? Oscars"),"[0-9]+")
  #extract pettern number from pettern "N wins" where N is a number
  Awerdss[[i]][3] = str_extract(str_extract(MoviesdataFrame$Awards[i],"\\(?[0-9]+\\)? wins"),"[0-9]+")
  #extract pettern number from pettern "N nominations" where N is a number
  Awerdss[[i]][4] = str_extract(str_extract(MoviesdataFrame$Awards[i],"\\(?[0-9]+\\)? nominations"),"[0-9]+")
  #extract pettern number from pettern "Won N Golden Globe" where N is a number
  Awerdss[[i]][5] = str_extract(str_extract(MoviesdataFrame$Awards[i],"Won \\(?[0-9]+\\)? Golden Globe"),"[0-9]+")
  #extract pettern number from pettern "Won N BAFTA" where N is a number
  Awerdss[[i]][6] = str_extract(str_extract(MoviesdataFrame$Awards[i],"Won \\(?[0-9]+\\)? BAFTA"),"[0-9]+")
  length(Awerdss[[i]])
  for(x in 1:length(Awerdss[[i]]))
  {
    if(!is.na(Awerdss[[i]][x]))
    {Awerdss[[i]][x] = strtoi(Awerdss[[i]][x], base = 0L)}
    else
    {Awerdss[[i]][x] = 0}
  }
  MoviesdataFrame$Awards_oscarWon[i] = Awerdss[[i]][1]  
  MoviesdataFrame$Awards_oscarNominated[i] <-Awerdss[[i]][2]
  MoviesdataFrame$Awards_OtherWon[i] <-Awerdss[[i]][3]
  MoviesdataFrame$Awards_OtherNominated[i] <-Awerdss[[i]][4]
  MoviesdataFrame$Awards_ggWon[i] <-Awerdss[[i]][5]
  MoviesdataFrame$Awards_BAFTAwon[i] <-Awerdss[[i]][6]
}

# in this section we parse the country names, connect to an api that give use the 
#longtitud and latitude of the country by name 
countrys <- list()
countryToPosMap <- vector(mode = "list")
for(i in 1:numOfRecords)
{
countrys[i] <- strsplit(MoviesdataFrame$Country[i],", ")
  for(name in countrys[[i]])
  {
    if(is.null(countryToPosMap[[name]]))
    { 
      countryToPosMap[[name]] = geocode(name)
    }
  }
}

#in this section we prepere the data score of movies by two diffrent score types
#we check if the value is na and replace by mean value 
# the first vector is meta score movie rate the seconed one is the imdb score both from our db
vector1 = vector() # contains the metascor scors
for (i in 1:length( MoviesdataFrame$Metascore))
{
  if(MoviesdataFrame$Metascore[[i]] == "N/A"|  MoviesdataFrame$Metascore[[i]] == "NA")
  { vector1 <- c(vector1, 50)}
  else
  {vector1 <- c(vector1, strtoi(MoviesdataFrame$Metascore[[i]], base = 0L))}
}
vector2 = vector() # contain the imdb scors
for (i in 1:length( MoviesdataFrame$imdbRating))
{
  if(is.na(MoviesdataFrame$imdbRating[[i]])| MoviesdataFrame$imdbRating[[i]] == "N/A" |  MoviesdataFrame$imdbRating[[i]] == "NA")
  { vector2 <- c(vector2, 50)}
  else
  {vector2 <- c(vector2, as.numeric(MoviesdataFrame$imdbRating[[i]], base = 0L)*10)}
}

#this is the box plot to show the scors distrebutions for 2 score systems for the movies
boxplot(vector1, vector2 ,names = c("Meta Score","IMDB Score"),xlab="Score  types of movies ",ylab="distrebution of scors",main = "Plot of movie Scors distrebutions")

# the vector of all the countrys in the data from whice the data came from
countrysVec = vector()
for(i in 1:numOfRecords)
{
  temp = strsplit(MoviesdataFrame$Country[i],", ")
  for(x in 1:length(temp))
  {countrysVec <- c(countrysVec,temp[[x]])}
}

# this bar plot show how the movies distrebutes by the country they were made in
barplot(table(countrysVec),
        horiz=TRUE,
        col=rainbow(length(table(countrysVec))),
        cex.names=0.4,
        xlab="ammount of movies",
        las=2,
        main = "top movies by Country")



#in this plot we want to see if there is a connection between the movie length and the number of voters that responed to this movie
plot(strtoi(MoviesdataFrame$imdbVotes),strtoi(str_extract(MoviesdataFrame$Runtime,"[0-9]+")),main = "movie length vs number of movie responeses " ,
                           xlab = "number of Votes on that movie",ylab = "movie length in minuts")
#in this plot we want to see if there is a connection between the movies year to its length
plot(strtoi(MoviesdataFrame$Year),strtoi(str_extract(MoviesdataFrame$Runtime,"[0-9]+")),main = "movie length vs number of movie responeses " ,
     xlab = "movies year of production ",ylab = "movie length in minuts")

#in this section we prepering the data for the corrorlation so we need to get all the gener to yet to length tuples
Ganer_Year_length_Vec = vector(mode="list")
for(i in 1:numOfRecords)
{
  temp = strsplit(MoviesdataFrame$Genre[i],", ")
  for(y in 1:length(temp[[1]]))
  {
    key <- paste(temp[[1]][y],MoviesdataFrame$Year[i], sep = "_")
    
    if(!is.null(Ganer_Year_length_Vec[[key]]) )
    {
      oldVal <- Ganer_Year_length_Vec[[key]]
      if(!is.na(str_extract(MoviesdataFrame$Runtime[i],"[0-9]+")))
     { newVal <- c(strtoi(str_extract(MoviesdataFrame$Runtime[i],"[0-9]+"))+oldVal[[1]],oldVal[[2]]+1)
     
     }
      else
        
      {  print("val na")
        newVal <- c(120+oldVal[[1]],oldVal[[2]]+1)
        }
     }
    else
    {
      if(!is.na(str_extract(MoviesdataFrame$Runtime[i],"[0-9]+"))){
      newVal <- c(strtoi(str_extract(MoviesdataFrame$Runtime[i],"[0-9]+")),1)
      }
      else
      {
        print("st null")
        newVal <- c(120,1)
      }
    }
  Ganer_Year_length_Vec[[key]] <- newVal
  }
}
# here we create the unique years and unique geners vectoris 
uniqueYears = unique(MoviesdataFrame$Year)
geners <-vector()
for(i in 1:length(MoviesdataFrame$Genre )){
  for(x in strsplit(MoviesdataFrame$Genre[i],", ")){
geners <- c(geners,x)}
}
uniqueGeners = unique(geners)
generVectors = vector(mode="list")
for(i in uniqueGeners)

{
  for(j in uniqueYears)
  {
    key <- paste(i,j,sep="_")
    
    if(!is.null(Ganer_Year_length_Vec[[key]])  )
    {
      generVectors[[i]] <- c(generVectors[[i]], Ganer_Year_length_Vec[[key]][1] / Ganer_Year_length_Vec[[key]][2])
    }
    else
    {
      generVectors[[i]] <- c(generVectors[[i]],0)
    }
  }
}

#here we working with the corrolation data calculating the distance between vectors by the score we give each gener of movies as vector of years and in each one we look on the avarage movie length in that year
corrMacorrMatrix<-data.frame()
corrMacorrMatrix<-rbind.data.frame(generVectors)
corrMacorrMatrix <- cor(corrMacorrMatrix)
corrplot(corrMacorrMatrix, method = "square",main = "COrroleation matrix of Movie geners by movie length by years" ,col = colorRampPalette(c("red","yellow","green"))(100))

#in this section we give each country the score of awardsfor all the movies from that country 
# the formula we used is 10 pints for oscar win 
#7 points for golden globus win
#5 points for BAFT award win
#3 points for othere awards win
#2points for oscar nomination and one point for othere awards nominations
countrys <- list()
countryToAwerdsScore <- vector(mode = "list")
for(i in 1:numOfRecords)
{
  countrys[i] <- strsplit(MoviesdataFrame$Country[i],", ")
  for(name in countrys[[i]])
  {
    if(!is.null(countryToAwerdsScore[[name]]))
    { 
      oldVal <- countryToAwerdsScore[[name]] 
      newVal <- c(
        oldVal[1] + (MoviesdataFrame$Awards_oscarWon[[i]] * 10) ,
        oldVal[2] + (MoviesdataFrame$Awards_ggWon[[i]] * 7) ,
        oldVal[3] + (MoviesdataFrame$Awards_BAFTAwon[[i]]  * 5) ,
        oldVal[4] + (MoviesdataFrame$Awards_OtherWon[[i]]  * 3 ),
        oldVal[5] + (MoviesdataFrame$Awards_oscarNominated[[i]]  * 2),
        oldVal[6] + (MoviesdataFrame$Awards_OtherNominated[[i]]  * 1) )
       }
    else
    {
      newVal <- c(
         MoviesdataFrame$Awards_oscarWon[[i]]  * 10 ,
         MoviesdataFrame$Awards_ggWon[[i]]  * 7 ,
         MoviesdataFrame$Awards_BAFTAwon[[i]] * 5 ,
         MoviesdataFrame$Awards_OtherWon[[i]]  * 3,
         MoviesdataFrame$Awards_oscarNominated[[i]]  * 2,
         MoviesdataFrame$Awards_OtherNominated[[i]]  * 1 )
    }
    countryToAwerdsScore[[name]] <- newVal
  }
}

# here we add the long and lat for each country name we got the information from another api that was mantioned before in line 85
countryScorsDataFrame <- data.frame()
for(countryname in names(countryToAwerdsScore))
{
countryScorsDataFrame <- rbind( countryScorsDataFrame, 
                    c(
  "Score" = sum(c(countryToAwerdsScore[[countryname]])),
  "long" = countryToPosMap[[countryname]][1],
  "lat" = countryToPosMap[[countryname]][2]) )
}

#here we create a plot that show on map by size and color the countrys of the world given the scor on movies in our list 
map <- get_map(location = c(lon =0, lat = 0), zoom = 1.1,
               maptype = "satellite", scale = 2)
# plotting the map with some points on it
ggmap(map) +
  geom_point( data = countryScorsDataFrame, 
             aes(x = long.lon, y = lat.lat, color=Score, alpha = 0.1), size = 1+countryScorsDataFrame$Score/mean(countryScorsDataFrame$Score)) +  
  guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  scale_color_gradient2(midpoint=mean(countryScorsDataFrame$Score), low="pink", mid="green", high="Yellow" )





