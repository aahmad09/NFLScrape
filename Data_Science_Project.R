# Must install the devtools package using the below commented out code
install.packages("devtools")
#install.packages("Rtools")
#av <- available.packages(filters=list())
#av[av[, "Rtools"] == pkg, ]
# Then can install using the devtools package from either of the following:
#devtools::install_github(repo = "maksimhorowitz/nflscrapR")
# or the following (these are the exact same packages):
nfl <- devtools::install_github(repo = "ryurko/nflscrapR")
Sys.which("make")
install.packages("jsonlite", type = "source")
View(nflscrapR)

#nfldata <- read_csv("C:\Users\tlisa\OneDrive\Desktop\BAT\nfldata.csv")
#?read.csv
getwd()
nfldata <- read.csv("nfldata.csv", stringsAsFactors = F)
View(nfldata)
glimpse(nfldata)
nfldata2014 <- nfldata %>% 
  filter(Season == "2014/2015")
View(nfldata2014)
