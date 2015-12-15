#gathering data from web
library(XML)
library(astsa)

url = "http://www.census.gov/econ/currentdata/dbsearch?program=RESSALES&startYear=1963&endYear=2015&
      categories=SOLD&dataType=TOTAL&geoLevel=WE&notAdjusted=1&errorData=1&submit=GET+DATA"
tables = readHTMLTable(url, header = T)[[1]]
rownames(tables) = as.character(tables[['Year']])
tables = t(tables[,-1])
names = character()
for (j in 1:ncol(tables)){ 
  for (i in 1:nrow(tables)){
    names = c(names, paste(dimnames(tables)[[1]][i], dimnames(tables)[[2]][j], sep = " "))
  }
}
vec = as.vector(unlist(tables))
names(vec) = names
vec = vec[!is.na(as.numeric(vec))]
west = ts(as.numeric(vec), frequency = 12, start = 1973, end = c(2015, 10), class = 'ts') 
west_pre = ts(west[time(west)<2009], frequency = 12, start = 1973, end = c(2008, 12), class = 'ts')
west_post = ts(west[time(west)>=2009], frequency = 12, start = 2009, end = c(2015, 10), class = 'ts')

save(west, west_pre, west_post, file = '~/Github/Stat153FinalProject_WestCoastHousing/west_homes_ts.RData')
