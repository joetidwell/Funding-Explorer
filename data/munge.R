# Revenue per WADA pre Robin Hood
# revenue per WADA post Robin Hood
# actual revenue per student
# actual revenue per student including Federal Aid 

library(data.table)
library(reshape2)
library(plyr)
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


# CPI data
# http://data.bls.gov
dat.CPI <- data.table(read.csv("annualCPI.csv"))
setnames(dat.CPI, c("year","CPI"))
dat.CPI <- dat.CPI[!is.na(year)]

setkey(dat.CPI,year)
setkey(mydata,year)
mydata <- mydata[dat.CPI][!is.na(CDN)]

mydata[,pop_plus_inf:=(ADA/dplyr::lag(ADA)-1) + (CPI/dplyr::lag(CPI)-1),by=.(CDN)]
mydata[is.na(pop_plus_inf),pop_plus_inf:=0]
mydata[,pop_plus_inf:=pop_plus_inf+1]
mydata[,factor2016:=cumprod(pop_plus_inf),by=.(CDN)]
mydata[,factor2016:=1/(factor2016/max(factor2016)),by=.(CDN)]

setkey(mydata, year)
mydata[,inflation:=(CPI/dplyr::lag(CPI)-1),by=.(CDN)]
mydata[is.na(inflation),inflation:=0]
mydata[,inflation:=inflation+1]
mydata[,inf.factor:=cumprod(inflation),by=.(CDN)]
mydata[,inf.factor:=1/(inf.factor/max(inf.factor)),by=.(CDN)]

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

# Correct Duplicate ISD Names
setkey(mydata, `District Name`)
mydata[year==2014][,.(.N, CDN),by=`District Name`][N>1]

mydata[CDN==15905, `District Name`:="EDGEWOOD ISD (BEXAR)"]
mydata[CDN==234903,`District Name`:="EDGEWOOD ISD (VAN ZANDT)"]
mydata[CDN==187901, `District Name`:="BIG SANDY ISD (POLK)"]
mydata[CDN==230901, `District Name`:="BIG SANDY ISD (UPSHUR)"]
mydata[CDN==145902, `District Name`:="CENTERVILLE ISD (LEON)"]
mydata[CDN==228904, `District Name`:="CENTERVILLE ISD (TRINITY)"]
mydata[CDN==212909, `District Name`:="CHAPEL HILL ISD (SMITH)"]
mydata[CDN==202815, `District Name`:="CHAPEL HILL ISD (TARRANT)"]
mydata[CDN==58902, `District Name`:="DAWSON ISD (DAWSON)"]
mydata[CDN==175904, `District Name`:="DAWSON ISD (NAVARRO)"]
mydata[CDN==188903, `District Name`:="HIGHLAND PARK ISD (POTTER)"]
mydata[CDN==57911, `District Name`:="HIGHLAND PARK ISD (DALLAS)"]
mydata[CDN==109905, `District Name`:="HUBBARD ISD (HILL)"]
mydata[CDN==19913, `District Name`:="HUBBARD ISD (BOWIE)"]
mydata[CDN==39905, `District Name`:="MIDWAY ISD (CLAY)"]
mydata[CDN==161903, `District Name`:="MIDWAY ISD (MCLENNAN)"]
mydata[CDN==244905, `District Name`:="NORTHSIDE ISD (WILBARGER)"]
mydata[CDN==15915, `District Name`:="NORTHSIDE ISD (BEXAR)"]
mydata[CDN==49903, `District Name`:="VALLEY VIEW ISD (COOKE)"]
mydata[CDN==108916, `District Name`:="NORTHSIDE ISD (HIDALGO)"]
mydata[CDN==221912, `District Name`:="WYLIE ISD (TAYLOR)"]
mydata[CDN==43914, `District Name`:="WYLIE ISD (COLLIN)"]


# fix northside
mydata[CDN==244905, gsa:=FALSE]

save(mydata, file="mydata.RData")
load("mydata.RData")

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


mydata[,form1:=(`Total Revenue`)/ADA]

setkey(mydata,form1)
mydata[year==2014][1:4]$`District Name`

mydata[,form1:=factor2016*(`Total Revenue`-Chap41)/ADA]
mydata[,form2:=factor2016*`Total Revenue`/ADA]
mydata[,is41:=Chap41>0]

mydata[,form3:=factor2016*(`State Revenue`-Chap41)/`Total Revenue`]
mydata[,form4:=factor2016*``/ADA]

mydata[Chap41==max(Chap41)]

mydata[`Total Revenue`<100000]

ggplot(mydata, aes(x=year, y=form2, group=is41, color=is41)) +
  geom_smooth() +
  scale_color_manual(name="",
                     values=c("firebrick","steelblue"),
                     labels=c("Non - Chapter 41 Districts","Chapter 41 Districts")) +
  labs(y="Total Revenue / ADA",
       title="Revenue per Student") +
  theme(legend.position = c(0.2, 0.95),
       legend.background = element_rect(fill="transparent")) +
  expand_limits(x = 2004) +
  scale_x_continuous(expand = c(0, 0))




ggplot(mydata, aes(x=year, y=form1, group=is41, color=is41)) +
  geom_smooth() +
  scale_color_manual(name="",
                     values=c("firebrick","steelblue"),
                     labels=c("Non - Chapter 41 Districts","Chapter 41 Districts")) +
  labs(y="(Total Revenue - Chapter 41) / ADA",
       title="Revenue per Student\nAdjusted for Chapter 41") +
  theme(legend.position = c(0.2, 0.95),
       legend.background = element_rect(fill="transparent")) +
  expand_limits(x = 2004) +
  scale_x_continuous(expand = c(0, 0))

ggplot(mydata, aes(x=year, y=form4, group=is41, color=is41)) +
  geom_smooth() +
  scale_color_manual(name="",
                     values=c("firebrick","steelblue"),
                     labels=c("Non - Chapter 41 Districts","Chapter 41 Districts")) +
  labs(y="Total Revenue / ADA",
       title="Revenue per Student") +
  theme(legend.position = c(0.2, 0.95),
       legend.background = element_rect(fill="transparent")) +
  expand_limits(x = 2004) +
  scale_x_continuous(expand = c(0, 0))


ggplot(mydata, aes(x=year, y=form3, group=is41, color=is41)) +
  geom_smooth() +
  scale_color_manual(name="",
                     values=c("firebrick","steelblue"),
                     labels=c("Non - Chapter 41 Districts","Chapter 41 Districts")) +
  labs(y="(Total Revenue - Chapter 41) / ADA",
       title="Revenue per Student\nAdjusted for Chapter 41") +
  theme(legend.position = c(0.2, 0.95),
       legend.background = element_rect(fill="transparent")) +
  expand_limits(x = 2004) +
  scale_x_continuous(expand = c(0, 0))


ggplot(mydata[is41==TRUE,chap41,by=.(year)], aes(x=year, y=form4)) +
  geom_smooth() 
