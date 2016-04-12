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


