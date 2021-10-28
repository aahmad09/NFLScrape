#nfldata <- read_csv("C:\Users\tlisa\OneDrive\Desktop\BAT\nfldata.csv")
#?read.csv
getwd()
nfldata <- read.csv("nfldata.csv", stringsAsFactors = F)
View(nfldata)
glimpse(nfldata)
nfldata2014 <- nfldata %>% 
  filter(Season == "2014/2015")
View(nfldata2014)
#>>>>>>> 68093e7ac22f7dedde18ce6787d734e020259f54
