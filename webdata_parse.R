library(XML)
theurl <- "http://www.census.gov/econ/currentdata/dbsearch?program=RESSALES&startYear=1963&endYear=2015&categories=SOLD&dataType=TOTAL&geoLevel=WE&notAdjusted=1&errorData=1&submit=GET+DATA"
tables <- readHTMLTable(theurl, header = F)[[1]]
