# Joseph Naylor
# MIS500 R Portfolio Project

# Analyzing MLB Temperature vs Offensive Stats

library(RODBC)
library(ggplot2)

# Load data from SQL database
server <- 'localhost\\RETROSHEET' 
database <- 'RETROSHEET'
conn_string <- paste('driver={SQL Server};server=',server,';database=',database,';trusted_connection=true',sep = "")  
dbhandle <- odbcDriverConnect(conn_string)

# Query database to gather data
gamelog <- sqlQuery(dbhandle, 'SELECT * FROM Game_Log_Combined')
tbl = table(gamelog$total_hits,gamelog$TempClassification)

# Create and print ANOVA table for hits
a <- aov(total_hits~TempClassification,data=gamelog)
summary(a)

# Create and print ANOVA table for home runs
a <- aov(total_home_runs~TempClassification,data=gamelog)
summary(a)

# Create and print ANOVA table for runs
a <- aov(total_runs~TempClassification,data=gamelog)
summary(a)

# Generate visualizations
hist <- qplot(gamelog$temperature, geom="blank",main="Game Temperature Distribution for\nOutdoor MLB Games Between 2000 and 2018",xlab="Temperature °F")
hist + geom_histogram(color="black",fill="darkgrey",breaks=seq(20,120,5))

## Hits
hitsmean <- sqlQuery(dbhandle,'SELECT AVG(CAST(total_hits as decimal)) AS total_hits, TempClassification FROM Game_Log_Combined GROUP BY TempClassification')

hitsmean$TempClassification <- factor(hitsmean$TempClassification,levels = c("50 or Less", "50s", "60s", "70s", "80s", "90s", "100 or More"))
d <- ggplot(hitsmean, aes(TempClassification)) 
d <- d + stat_summary_bin(aes(y = total_hits), geom = "bar")
d <- d + coord_cartesian(xlim =c(1, 7), ylim = c(16,20))
d <- d + ggtitle("Average Hits Per Game by Temperature") + xlab("Temperature °F") + ylab("Average Hits Per Game")
d + geom_text(aes(label = format(total_hits, digits = 5), y = total_hits + .2), size =3)

## Home Runs
hitsmean <- sqlQuery(dbhandle,'SELECT AVG(CAST(total_home_runs as decimal)) AS total_home_runs, TempClassification FROM Game_Log_Combined GROUP BY TempClassification')

hitsmean$TempClassification <- factor(hitsmean$TempClassification,levels = c("50 or Less", "50s", "60s", "70s", "80s", "90s", "100 or More"))
d <- ggplot(hitsmean, aes(TempClassification)) 
d <- d + stat_summary_bin(aes(y = total_home_runs), geom = "bar")
d <- d + coord_cartesian(xlim =c(1, 7), ylim = c(1.5,2.75))
d <- d + ggtitle("Average Home Runs Per Game by Temperature") + xlab("Temperature °F") + ylab("Average HR Per Game")
d + geom_text(aes(label = format(total_home_runs, digits = 5), y = total_home_runs + .05), size =3)

## Runs
hitsmean <- sqlQuery(dbhandle,'SELECT AVG(CAST(total_runs as decimal)) AS total_runs, TempClassification FROM Game_Log_Combined GROUP BY TempClassification')

hitsmean$TempClassification <- factor(hitsmean$TempClassification,levels = c("50 or Less", "50s", "60s", "70s", "80s", "90s", "100 or More"))
d <- ggplot(hitsmean, aes(TempClassification)) 
d <- d + stat_summary_bin(aes(y = total_runs), geom = "bar")
d <- d + coord_cartesian(xlim =c(1, 7), ylim = c(8,10.5))
d <- d + ggtitle("Average Runs Per Game by Temperature") + xlab("Temperature °F") + ylab("Average Runs Per Game")
d + geom_text(aes(label = format(total_runs, digits = 5), y = total_runs + .15), size =3)

## Stacked bar chart showing months
hitsmean <- sqlQuery(dbhandle,'SELECT SUM(CAST(total_hits as decimal)) AS total_hits, TempClassification, month FROM Game_Log_Combined GROUP BY TempClassification, month')
options(scipen=5000)
hitsmean$TempClassification <- factor(hitsmean$TempClassification,levels = c("50 or Less", "50s", "60s", "70s", "80s", "90s", "100 or More"))
hitsmean$month <- factor(hitsmean$month, levels = c("March","April","May","June","July","August","September","October"))
g <- ggplot(data = hitsmean, aes(x = TempClassification, y = total_hits, fill = month)) 
g <- g + ggtitle("Distribution of Hits by Temperature") + xlab("Temperature °F") + ylab("Hits")
g <- g + geom_bar(stat = "identity") 
g

