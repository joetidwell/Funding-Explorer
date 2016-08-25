# Revenue per WADA pre Robin Hood
# revenue per WADA post Robin Hood
# actual revenue per student
# actual revenue per student including Federal Aid 

library(data.table)
library(reshape2)
library(plur)
library(tidyr)

options(stringsAsFactors = FALSE)


# school data from TEA
# http://tea.texas.gov/Reports_and_Data/

dat.wada <- data.table(read.csv("2005-2015_ADA_WADA.csv"))
dat.41 <- data.table(read.csv("1994-2014_Chapter41.csv"))
dat.finance <- data.table(read.csv("2000-2015_Summarized_Financial_Data.csv"))  
setnames(dat.wada,gsub("X","",gsub("X\\.","X",names(dat.wada))))
setnames(dat.41, gsub("X","",names(dat.41)))
setnames(dat.41,"County.District.Number","CDN")
setnames(dat.finance,"DISTRICT.NUMBER","CDN")


dat.41[,Chapter.41.District:=NULL]
dat.41 <- dat.41[!is.na(CDN)]
dat.41 <- data.table::melt(dat.41, 
                           id.var="CDN", 
                           variable.name="year", 
                           value.name="Chap41",
                           variable.factor=FALSE)

dat.wada[, DISTRICT.NAME:=NULL]
dat.wada <- data.table::melt(dat.wada,
                             id.var="CDN")
dat.wada[,c("year","tmp","variable"):=tstrsplit(variable,"\\.")]
dat.wada[,tmp:=NULL]
dat.wada <- spread(dat.wada, variable, value) 

setnames(dat.finance, "DISTRICT.NAME", "District Name")
setnames(dat.finance, "YEAR", "year")
dat.finance <- dat.finance[,.SD,.SDcols=c("CDN","District Name","year",
                                          "GEN.FUNDS.TOTAL.OPERATING.REVENUE",
                                          "GEN.FUNDS.LOCAL.TAX",
                                          "GEN.FUNDS.STATE.REVENUE",
                                          "GEN.FUNDS.FEDERAL.REVENUE")]

dat.41[,year:=as.integer(year)]
dat.wada[,year:=as.integer(year)]

setkeyv(dat.41, c("CDN","year"))
setkeyv(dat.wada, c("CDN","year"))
setkeyv(dat.finance, c("CDN","year"))

mydata <- dat.41[dat.wada[dat.finance]]
mydata <- mydata[!(is.na(ADA) | is.na(WADA))]


# Inflation data from the world bank
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
dat.CPI <- data.table(read.csv("CPI.csv"))
dat.CPI <- data.table::melt(dat.CPI,
                            variable.name="year", 
                            value.name="CPI",
                            variable.factor=FALSE)
dat.CPI <- dat.CPI[,.(year,CPI)]
dat.CPI[,year:=as.integer(gsub("X","",year))]
setkey(dat.CPI,year)
setkey(mydata,year)
mydata <- mydata[dat.CPI][!is.na(CDN)]
mydata[,CPI:=CPI/100]

mydata[,pop.growth:=ADA/lag(ADA)-1,by=CDN]
mydata[`District Name`=="BOERNE ISD"]
mydata[`District Name`=="ALAMO HEIGHTS ISD"]

mydata[,growth:=pop.growth+CPI]


load("~/git/District-Explorer/data/districts.RData")
tmp <- districts@data
setnames(tmp, "DISTRICT NUMBER", "CDN")
tmp <- tmp[,.(CDN,fgsd,gsa)]
setkey(tmp,CDN)
setkey(mydata,CDN)

mydata <- tmp[mydata]

setnames(mydata,
         names(mydata)[9:12],
         c("Total Revenue", "Local Taxes", "State Revenue", "Federal Revenue"))

mydata <- mydata[ADA!=0]
mydata[is.na(Chap41), Chap41:=0]
mydata[CDN==234903, gsa:=FALSE]

save(mydata, file="mydata.RData")


library(ggplot2)
theme_set(theme_classic())
quantile(mydata$blah,c(.99))

mydata[ADA==0]
ggplot(mydata, aes(x=year, y=`Total Revenue`/ADA)) +
  geom_point()

library(plotly)
d <- diamonds[sample(nrow(diamonds), 1000), ]
# note how size is automatically scaled and added as hover text

pdata <- mydata[fgsd==TRUE,.(`District Name`,
                            year,
                            `Revenue per WADA`=(`Total Revenue`-Chap41)/WADA)]
pdata <- pdata[`Revenue per WADA`<quantile(`Revenue per WADA`,.99)]

mydata[gsa==TRUE]


setkey(pdata, year)
ggplot(pdata[], aes(x=year, y=`Revenue per WADA`, group=`District Name`)) +
  geom_line(color="lightgrey") +
  geom_line(data=pdata[`District Name`=="BOERNE ISD"], color="firebrick", size=2)


pdata <- mydata[gsa==TRUE,.(`District Name`,
                            CDN,
                            year,
                            `Revenue per WADA`=(`State Revenue`-`Chap41`)/ADA)]
                            # `Revenue per WADA`=(`Total Revenue`)/WADA)]
# pdata <- pdata[`Revenue per WADA`<quantile(`Revenue per WADA`,.99)]
pdata[,color:="A"]
pdata[`District Name`=="BOERNE ISD",color:="B"]
pdata[`District Name`=="EDGEWOOD ISD",color:="C"]
pdata[,hjust := ifelse(`Revenue per WADA`< 0, "left", "right")]


ggplot(pdata[year==2014], aes(x=reorder(CDN, `Revenue per WADA`), 
                              y=`Revenue per WADA`,
                              label=`District Name`,
                              hjust=hjust)) +
  geom_bar(aes(fill=color), position="dodge", stat="identity") +
  coord_flip() +
  scale_fill_manual(values=c("grey","firebrick","steelblue")) +
  # scale_x_discrete(labels=pdata[year==2014][order(`Revenue per WADA`)]$`District Name`) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="") +
  guides(fill=FALSE) +
  geom_text(aes(y=0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

   +
  geom_line(data=pdata[`District Name`=="BOERNE ISD"], color="firebrick", size=2)



mydata[`District Name`=="BOERNE ISD"]
