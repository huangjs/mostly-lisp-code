r <- read.csv(file="~/programming/R/survey-result-csv.csv")

head(sort(table(strsplit(paste(tolower(as.matrix(r$What.is.the.job.title.for.your.current.position., mode="character")), collapse=" "), " ")), decreasing=TRUE), 20)

