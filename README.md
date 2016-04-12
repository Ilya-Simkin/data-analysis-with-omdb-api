# data-analysis-with-omdb-api

###An home work where we analyze data from omdb data base 

###introdaction
* this report will explain the work that was done by use to analyze the data that can be recived from OMDB API.
we used a list of top 500 movies that we find on imdb.com to test the api.
the list is under this directory named moviesNames.csv

###the authors of this work are : 
* Ilya Simkin, id : 305828188
* Or May-Paz, id : 301804134

###part one : installing and Loading packages

```{r load_packages, message=FALSE, results='hide'}
# if necessary comment and dont install packages.
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
```
###part two : Loading data 
this section shows how we load the csv with the movie names after that we connecting the api of OMDB to request first the baisic data about the movies and with that we request the full data that the api can give us about those movies 
* we should note that we chose to work with the first result for movies with ambiguse names
```{r load_data}
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
```
###part three : first look on the data before manipulations done on it
in this section we will explain what data we could recive from the api :
but first here is an example of the raw data and explain all the features we have :

![alt text](https://github.com/Ilya-Simkin/data-analysis-with-omdb-api/blob/master/1.PNG "first part of data")
 * the data you can see here is the first part of our data frame : 
 1. name - is the name of the movie 
 2. year - the year when the movie came out
 3. rated - is the restriction of age for the given movie 
 4. relesed - is the exact date when the movie came out
 5. Gener - is a list of geners to which the movie belonges
 6. Director - the directr of the movie
 7. Writer - the writer of the move 
 
![alt text](https://github.com/Ilya-Simkin/data-analysis-with-omdb-api/blob/master/2.PNG "scond part of data")
* the data you can see here is the second part of our data frame in this part we can see the features : 
8. Actors - list of all the actors played in the movie
9. Plot - a short description of the main events of the movie
10. Language - the first production langauges 
11. Country - where the movie was created (not where it filmed but by what country team\s) may be more then one

![alt text](https://github.com/Ilya-Simkin/data-analysis-with-omdb-api/blob/master/3.PNG "thired part of data")
* the data you can see here is the thired part of our data frame in this part we can see the features : 
12. Awards - a text field explaining the awards the movie won we will pars this fild farther on
13. Poster - is a link to imdb data base that contains the movie poster 
14. metaScore - one of the raiting score given to that movie
15. IMDB score - another raiting score given to that movie by imdb voters 
16. imdbid - id of the movie in imdb.com
17. type - the type  movie or tv series

###Part four : data manipulation 
* in this part of the report we will explain part of the data manipulations we had to do :
##### the awards field parsing :
* one of the first thing we did is to pars the Award raw text field into few separet fields so the will be easier to work with :
```{r pars award}
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
```
##### the Longtetude and Altetude cordinates from another API:
* for one of our later plots we needed to get the coordinates of each country we have in the country field:
```{r get coordinates}
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
```
##### Score preperations by ratings given in data  :
* in this section we prepere the data score of movies by two diffrent score types
* we check if the value is na and replace by mean value 
* the first vector is meta score movie rate the seconed one is the imdb score both from our db
```{r scores play}
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
```
##### Corrolation of ganers 
* in this section we prepering the data for the corrorlation so we need to get all the gener to year to length tuples 
* after that we create a vector of all the geners with all the years and the avarage movie length in that year
* by that vectors we will plot the corrolation between diffrent geners trugh the history and will look geners of movies that act in same manners 
* eventually we will get a corrolation matrix
```{r corrolation }
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
```
#####  the country scors by awards system 
* after parsing the data we decided to create a unique awards score system :
* each movie get a score by its awards and then the country the movie was made at will get the total awards score for our data set
* we give each country the score of awardsfor all the movies from that country 
   the formula we used is 10 pints for oscar win 
   7 points for golden globus win
   5 points for BAFT award win
   3 points for othere awards win
   2 points for oscar nomination 
   one point for othere awards nominations
* afterwerd we manipulated the data to fit our needs and added the longtitude and latitude for each country to score table 
```{r country scors by awards  }
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
```

###part five : the visualisation time !
* here we will demonstrate the visualizations we made and things we wanted to check by them :


