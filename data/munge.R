####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Inits
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(reshape2)
library(plyr)
library(tidyr)
options(stringsAsFactors = FALSE)


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Finance Data
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat.wada <- data.table(read.csv("2005-2015_ADA_WADA.csv"))
dat.finance <- data.table(read.csv("2000-2015_Summarized_Financial_Data.csv"))  
setnames(dat.wada,gsub("X","",gsub("X\\.","X",names(dat.wada))))
setnames(dat.finance,"DISTRICT.NUMBER","CDN")

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
                                          "GEN.FUNDS.FEDERAL.REVENUE",
                                          "ALL.FUNDS.EQUITY.TRANSFERS")]

dat.wada[,year:=as.integer(year)]
setkeyv(dat.wada, c("CDN","year"))
setkeyv(dat.finance, c("CDN","year"))

mydata <- dat.wada[dat.finance]
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


setnames(mydata,
         names(mydata)[6:10],
         c("Total Revenue", "Local Taxes", "State Revenue", "Federal Revenue", "Chap41"))

mydata <- mydata[!(ADA==0)]

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
mydata[CDN==220815, `District Name`:="CHAPEL HILL ACADEMY"]
mydata[CDN==225906, `District Name`:="CHAPEL HILL ISD (TITUS)"]
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
mydata[CDN==108916, `District Name`:="VALLEY VIEW ISD (HIDALGO)"]
mydata[CDN==221912, `District Name`:="WYLIE ISD (TAYLOR)"]
mydata[CDN==43914, `District Name`:="WYLIE ISD (COLLIN)"]

mydata[`District Name`=="HAYS CONS ISD", `District Name`:="HAYS CISD"]
mydata[`District Name`=="SAN MARCOS CONS ISD", `District Name`:="SAN MARCOS CISD"]
mydata[`District Name`=="CROCKETT CO CONS CSD", `District Name`:="CROCKETT COUNTY CONSOLIDATED CSD"]

mydata[year==2014][,.(.N, CDN),by=`District Name`][N>1]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Austin Metro Area
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

austin <- data.table(read.csv("austin.csv"))
austin[,`District.Name`:=toupper(`District.Name`)]

austin$District.Name[!(austin$District.Name %in% mydata$`District Name`)]
mydata[`District Name` %in% austin$District.Name, austin:=TRUE]


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Houston Metro Area
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

houston <- data.table(read.csv("greater_houston.csv"))
houston[,`District.Name`:=toupper(`District.Name`)]

houston$District.Name[!(houston$District.Name %in% mydata$`District Name`)]

houston[District.Name=="GOOSE CREEK ISD", District.Name:="GOOSE CREEK CISD"]
houston[District.Name=="LAPORTE ISD", District.Name:="LA PORTE ISD"]
houston[District.Name=="LAMAR CONS ISD", District.Name:="LAMAR CISD"]

houston$District.Name[!(houston$District.Name %in% mydata$`District Name`)]

mydata[`District Name` %in% houston$District.Name, houston:=TRUE]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Dallas - Fort Worth
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


dfw <- data.table(read.csv("DFW.csv"))
dfw[,`District.Name`:=toupper(`District.Name`)]

dfw$District.Name[!(dfw$District.Name %in% mydata$`District Name`)]

dfw[District.Name=="CARROLLTON-FARMERS BRANCH ISD   ", District.Name:="CARROLLTON-FARMERS BRANCH ISD"]
dfw[District.Name=="EAGLE MOUNTAIN-SAGINAW ISD", District.Name:="EAGLE MT-SAGINAW ISD"]
dfw[District.Name=="HIGHLAND PARK ISD", District.Name:="HIGHLAND PARK ISD (DALLAS)"]

dfw$District.Name[!(dfw$District.Name %in% mydata$`District Name`)]

mydata[`District Name` %in% dfw$District.Name, dfw:=TRUE]


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### San Antonio Area
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gsa <- data.table(read.csv("san_antonio.csv"))
gsa[,`District.Name`:=toupper(`District.Name`)]

gsa$District.Name[!(gsa$District.Name %in% mydata$`District Name`)]

gsa$District.Name[!(gsa$District.Name %in% mydata$`District Name`)]

gsa[District.Name=="SCHERTZ−CIBOLO−U CITY ISD", District.Name:="SCHERTZ-CIBOLO-U CITY ISD"]

gsa$District.Name[!(gsa$District.Name %in% mydata$`District Name`)]

mydata[`District Name` %in% gsa$District.Name, gsa:=TRUE]



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Equity Center
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

eq <- as.data.table(read.csv("equity.csv"))
eq[District.Name=="HUTTO ISD \\ IDALOU ISD", District.Name:="HUTTO ISD"]
eq <- rbind(eq, data.table(District.Name="IDALOU ISD"))
eq[!(District.Name %in% mydata$`District Name`) & grepl("\\(",District.Name),
   District.Name := sapply(strsplit(District.Name," "), function(x) {
      ln <- length(x)
      paste(paste(x[-(ln-1)], collapse=" "),x[(ln-1)])
    })
]

eq$District.Name[!(eq$District.Name %in% mydata$`District Name`)]

eq[District.Name=="COMO-PICKTON ISD", District.Name:="COMO-PICKTON CISD"]
eq[District.Name=="CROSBYTON ISD", District.Name:="CROSBYTON CISD"]
eq[District.Name=="EAGLE MT.-SAGINAW ISD", District.Name:="EAGLE MT-SAGINAW ISD"]
eq[District.Name=="EDGEWOOD (VAN ISD ZANDT)", District.Name:="EDGEWOOD ISD (VAN ZANDT)"]
eq[District.Name=="ERA CON ISD", District.Name:="ERA ISD"]
eq[District.Name=="FORT DAVIS ISD", District.Name:="FT DAVIS ISD"]
eq[District.Name=="GOLD-BURG ISD", District.Name:="GOLD BURG ISD"]
eq[District.Name=="GOOSE CREEK ISD", District.Name:="GOOSE CREEK CISD"]
eq[District.Name=="KNOX CITY-O'BRIEN ISD", District.Name:="KNOX CITY-O'BRIEN CISD"]
eq[District.Name=="LA POYNOR ISD", District.Name:="LAPOYNOR ISD"]
eq[District.Name=="LEVERETT'S CHAPEL ISD", District.Name:="LEVERETTS CHAPEL ISD"]
eq[District.Name=="TERLINGUA COMMON SD", District.Name:="TERLINGUA CSD"]
eq[District.Name=="THREE WAY ISD (ERATH)", District.Name:="THREE WAY ISD"]
eq[District.Name=="ZAPATA ISD", District.Name:="ZAPATA COUNTY ISD"]

eq$District.Name[!(eq$District.Name %in% mydata$`District Name`)]

mydata[`District Name` %in% eq$District.Name, equity:=TRUE]


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Fast Growth Districts
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fgsd <- as.data.table(read.csv("fast_growth.csv"))
fgsd[,`District.Name`:=toupper(`District.Name`)]


fgsd$District.Name[!(fgsd$District.Name %in% mydata$`District Name`)]

fgsd[District.Name=="GOOSE CREEK ISD", District.Name:="GOOSE CREEK CISD"]
fgsd[District.Name=="HAYS CONSOLIDATED ISD", District.Name:="HAYS CISD"]
fgsd[District.Name=="NORTHSIDE ISD", District.Name:="NORTHSIDE ISD (BEXAR)"]
fgsd[District.Name=="SCHERTZ-CIBOLO-UNIVERSAL CITY ISD", District.Name:="SCHERTZ-CIBOLO-U CITY ISD"]

fgsd$District.Name[!(fgsd$District.Name %in% mydata$`District Name`)]

mydata[`District Name` %in% fgsd$District.Name, fgsd:=TRUE]



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Texas School Coalition
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tsc <- as.data.table(read.csv("tsc.csv"))
tsc[,`District.Name`:=toupper(`District.Name`)]

tsc$District.Name[!(tsc$District.Name %in% mydata$`District Name`)]

tsc[District.Name=="CROCKETT COUNTY CCSD", District.Name:="CROCKETT COUNTY CONSOLIDATED CSD"]
tsc[District.Name=="DAWSON ISD (DAWSON COUNTY)", District.Name:="DAWSON ISD (DAWSON)"]
tsc[District.Name=="GLASSCOCK CO. ISD", District.Name:="GLASSCOCK COUNTY ISD"]
tsc[District.Name=="GOOSE CREEK ISD", District.Name:="GOOSE CREEK CISD"]
tsc[District.Name=="HIGHLAND PARK ISD (AMARILLO)", District.Name:="HIGHLAND PARK ISD (POTTER)"]
tsc[District.Name=="IRAAN SHEFFIELD ISD", District.Name:="IRAAN-SHEFFIELD ISD"]

tsc$District.Name[!(tsc$District.Name %in% mydata$`District Name`)]

mydata[`District Name` %in% tsc$District.Name, TSC:=TRUE]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Chapter 41
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mydata



save(mydata, file="mydata.RData")
