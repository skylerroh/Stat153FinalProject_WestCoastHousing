library(XML)
library(astsa)

url = "http://www.census.gov/econ/currentdata/dbsearch?program=RESSALES&startYear=1963&endYear=2015&categories=SOLD&dataType=TOTAL&geoLevel=WE&notAdjusted=1&errorData=1&submit=GET+DATA"
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
west = ts(vec, frequency = 12, start = c(1973, 1)) 
west
plot(west)
