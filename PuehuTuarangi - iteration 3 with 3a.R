# Packages to load

library(sf)
library(dplyr)
library(spData) 
library(tmap)
library(shiny)
library(shinyjs)
library(grid)
library(cartogram)
library(FunnelPlotR)
library(ggplot2)
library(gridExtra)
library(egg)
library(viridis)
library(knitr)


setwd('C:/Users/munch/Documents/Uni/Year 6/Elective')
rm(list = ls())
Maps  <- list(NULL)
Plots <- list(NULL)
Year   <- 2013:2022
windowsFonts("Calibri" = windowsFont("Calibri"))


### Set up the object for reporting data by Area Census Unit and Year. 

# Import Faster Cancer Treatment data for Te Tai Tokerau
TTdata <- read.csv("./Dataset3aNoSkinNoDups.csv", header=TRUE)
names(TTdata)

# Add a column for DiagnosisYear
TTdata$DiagnosisYear <- sapply(TTdata$DateDiagnosed,function(x){
  Date  <- as.POSIXct(x, format = "%d/%m/%Y")
  format(Date,format="%Y")
})

# Add a column testing for Surgery as first treatment
TTdata$FirstTreatmentSurgery <- TTdata$FirstTreatmentType == "Surgery"

# Relocate working columns to the front
TTdata   <- TTdata %>% relocate(DiagnosisYear,
                                DomicileDesc,
                                AreaCensusUnit,
                                FirstTreatmentSurgery,
                                TumourStream,
                                DeprivationIndex)

# Arrange  rows by year and by Area Census Unit
TTdata   <- TTdata %>% arrange(DiagnosisYear,
                               AreaCensusUnit)                               

# Data integrity check: which years are represented in the diagnosis data?
sort(unique(TTdata$DiagnosisYear),na.last=T)
# Interestingly, there are no diagnosis dates for 2011!  Also there are only two entries for 2010 and 2012.  Hence Year is set to 2013:2022.
TTdata[1:5,1:5]


# Find unique combinations of Area Census Unit and Domicile Descriptions.
ACunit       <- (function(){   
       Code  <- TTdata[,c(2,3)]
       List  <- Code[!duplicated(t(apply(Code, 1, sort))),]
       List[order(List[,1]),]
})()

#Code above seems broken - not removing duplicates. Solved in line below
ACunit       <- ACunit[!duplicated(ACunit$AreaCensusUnit),]

#Arranging by Area Census Unit
ACunit       <- ACunit %>% arrange(AreaCensusUnit)                               

# Removing Wellsford and South from the series
ACunit       <- subset(ACunit,AreaCensusUnit < 505300)


# Data integrity check: are any Domicile Descriptions matched to more than one Area Census Unit?
ACunit[which(duplicated(ACunit$DomicileDesc)),]
# No duplicates returned. Bream Head discrepancy fixed in the CSV on my version. 

# Summary tables for each AreaCensusUnit.  Showing total diagnoses and total FirstTreatmentSurgery in each DiagnosisYear.

TTlist  <- (function(){
  List  <- split(TTdata,TTdata$AreaCensusUnit)
  Summ  <- function(i = List[[33]]) {
    i %>%
    group_by(DiagnosisYear) %>%
    summarise(TotalSurgery = sum(FirstTreatmentSurgery),
              Diagnoses = length(FirstTreatmentSurgery))
  } 
  lapply(List,Summ)
})()
  
# Surgery in each Area Census Unit
ACunit$Surgery <- sapply(ACunit$AreaCensusUnit,function(i){
  Index <- which(names(TTlist)==i)
  sum(TTlist[[Index]]$TotalSurgery)
})

# Total cases in each Area Census Unit
ACunit$Cases  <- sapply(ACunit$AreaCensusUnit,function(i){
  Index <- which(names(TTlist)==i)
  sum(TTlist[[Index]]$Diagnoses)
})

# Surgery Performance in each Area Census Unit
ACunit$SurgPerformance  <- ACunit$Surgery / ACunit$Cases

#Arranging by Performance
ACunit       <- ACunit %>% arrange(SurgPerformance)


## LagFSA.TreatDecn data manipulation - calculating performance of CAUs based on proportion of Lag times Under 31 days in relation to cases.

# Removing data south of Northland and missing data points, placing in copied dataframe TTdata1 for manipulation.
# This removes 2670 and 1991 data points
TTdata1 <- subset(TTdata,AreaCensusUnit < 505300)
TTdata1 <- subset(TTdata1,LagFSA.TreatDecn!="#VALUE!")


#Subsetting all Lag times under 1 day and Under 2 years - this removes 15 and 849 data points respectively
TTdata1 <- subset(TTdata1,LagFSA.TreatDecn > -1)
TTdata1 <- subset(TTdata1,LagFSA.TreatDecn < 730)


#Defining Lag time as numeric value
TTdata1$LagFSA.TreatDecn <- as.numeric(TTdata1$LagFSA.TreatDecn)


#Adding new column LagFTDUnder31 for if Lag Time between FSA and treatment decision is Under 31 day target
#This is an arbitrary indicator currently - need to check guidelines
TTdata1$LagFTDUnder31 <- TTdata1$LagFSA.TreatDecn < 32


#Summarising by lag time between FSA and treatment decision
ACunit1 <- TTdata1 %>%
  group_by(AreaCensusUnit) %>%
  summarise(Median.LagFSA.TreatDecn = median(LagFSA.TreatDecn),
            LagUnder31 = sum(LagFTDUnder31))

ACunit1$Median.LagFSA.TreatDecn <- as.integer(ACunit1$Median.LagFSA.TreatDecn)


### Adding populations to CAU data


# CAU Master sheet found at https://figure.nz/table/uQ6OOVwPunralaft
CAU13 <- read.csv("./Census_Population_by_sex_by_Area_Unit_2001_2006_2013.csv")
names (CAU13)

# Stats NZ 2013 census sheet found at https://datafinder.stats.govt.nz/layer/25743-area-unit-2013/
AreaCodes <- read.csv("./area-unit-2013.csv")
names (AreaCodes)


# Subsetting for 2013 census, Census usually resident population count
CAU13 <- subset(CAU13,Census.Year > 2012)
CAU13 <- subset(CAU13,Measure == "Census usually resident population count")
CAU13 <- subset(CAU13,select = -c(Area.Unit.Code,Census.Year,Measure,Sex,Value.Unit,Value.Label,Null.Reason))
CAU13 <- subset(CAU13,Value > 0)


# Subsetting Area Codes to return just area codes and names
AreaCodes <- subset(AreaCodes,select = -c(WKT,AREA_SQ_KM,LAND_AREA_SQ_KM,Shape_Length))
AreaCodes <- AreaCodes %>% arrange(AU2013_V1_00)
AreaCodes <- subset(AreaCodes,AU2013_V1_00 < 505299)


# Creating new Data Frame matching area codes to populations - was missing from original data sheet

AreaCodes   <- rename(AreaCodes,Area.Unit = AU2013_V1_00_NAME)

Populations <- merge(CAU13,AreaCodes,by="Area.Unit")
Populations <- rename(Populations,Usual.Resident = Value, 
                      CAU2013.Code = AU2013_V1_00)
Populations <- Populations %>% relocate(CAU2013.Code,
                                        Area.Unit,
                                        Usual.Resident)
Populations   <- rename(Populations,AreaCensusUnit = CAU2013.Code)

# Merging to FinalTable new dataframe containing population data
FinalTable <- merge(ACunit,Populations,by = "AreaCensusUnit")
FinalTable <- merge(FinalTable,ACunit1,by = "AreaCensusUnit")
FinalTable <- subset(FinalTable, select = -c(Area.Unit))

# Adding Maori count column
TTdata$Māori <- TTdata$M.nM == "Māori"


ACunit2 <- TTdata %>%
  group_by(AreaCensusUnit) %>%
  summarise(MāoriCount = sum(Māori))

FinalTable <- merge(FinalTable,ACunit2,by = "AreaCensusUnit")

FinalTable$MāoriRatio <- FinalTable$MāoriCount / FinalTable$Cases


#Lag Time Performance Column

FinalTable$LagPerformance <- FinalTable$LagUnder31 / FinalTable$Cases

#Cancer incidence per CAU
FinalTable$CaseRatio <- FinalTable$Cases / FinalTable$Usual.Resident

FinalTable <- FinalTable %>% relocate(AreaCensusUnit, DomicileDesc,
                                      Cases, Usual.Resident, CaseRatio, MāoriCount, MāoriRatio,
                                      Surgery, SurgPerformance,
                                      Median.LagFSA.TreatDecn, LagUnder31, LagPerformance)

FinalTable <- rename(FinalTable, CAU = AreaCensusUnit, Domicile = DomicileDesc,
                     Residents = Usual.Resident, MedianLagFSA.DTT = Median.LagFSA.TreatDecn,
                     LagCaseRatio = LagPerformance, SurgCaseRatio = SurgPerformance)



kable(FinalTable, digit = 4)


## Funnel plots


# Surgery Funnel plot

options(ggrepel.max.overlaps = Inf)


Plots$SurgFunPlot <- (function(x = FinalTable) {
  
  x$Domicile     <- factor(x$Domicile)
  x$Surgery      <- as.numeric(x$Surgery)
  
  Anchor         <- glm(Surgery ~ Cases , 
                        family="poisson", 
                        data=x)
  
  Predicted      <- predict(Anchor, newdata = x, type="response")
  
  
  # Draw plot, returning just the plot object
  funnel_plot(denominator=Predicted, numerator=x$Surgery,
              limit=95 , group = x$Domicile, label = "outlier",
              draw_unadjusted = FALSE, draw_adjusted = TRUE, title = "Surgery by CAU Funnel Plot", 
              x_label = "Expected Cases")
  
})()

Plots$SurgFunPlot


#Funnel plot for Lags Under 31 count

Plots$LagFunPlot <- (function(x=FinalTable) {
  
  x$Domicile        <- factor(x$Domicile)
  x$LagUnder31      <- as.numeric(x$LagUnder31)
  
  Anchor         <- glm(LagUnder31 ~ Cases, 
                        family="poisson", 
                        data=x)
  
  Predicted      <- predict(Anchor, newdata = x, type="response")
  
  
  # Draw plot, returning just the plot object
  funnel_plot(denominator=Predicted, numerator=x$LagUnder31,
              limit=95 , group = x$Domicile, label = "outlier",
              draw_unadjusted = TRUE, title = "Lag Time Under 31 days by CAU", 
              x_label = "Expected Cases")
})()

Plots$LagFunPlot



# National Census Area Map
# Source is https://datafinder.stats.govt.nz/layer/25743-area-unit-2013/ 
CAmap <- read_sf("./area-unit-2013.csv")

# Census Area map for Te Tai Tokerau 
TTmap  <- subset(CAmap,AU2013_V1_00 < 505300)

TTmap$MapIndex         <- match(TTmap$AU2013_V1_00,FinalTable$CAU)
TTmap$SurgCaseRatio    <- FinalTable$SurgCaseRatio[TTmap$MapIndex] 
TTmap$LagCaseRatio     <- FinalTable$LagCaseRatio [TTmap$MapIndex]
TTmap$Cases            <- FinalTable$Cases[TTmap$MapIndex] 
TTmap$Populations      <- FinalTable$Residents[TTmap$MapIndex]

tmap_options(fontfamily = "Calibri", bg.color = "black", legend.text.color = "white", 
             legend.title.color = "white", title.color = "white")

Maps$tt1  <- tm_shape(TTmap) + tm_polygons() + tm_layout(title = "Northland CAUs", title.position = c("right","top"))

Maps$Cases_carto = cartogram_cont(TTmap, "Cases", itermax = 5)
Maps$ca1        <- tm_shape(Maps$Cases_carto) + 
                   tm_polygons("Cases") + tm_layout(title = "Cases by CAU", title.position = c("right","top"))

Maps$Populations_carto = cartogram_cont(TTmap, "Populations", itermax = 5)
Maps$ca2 <- tm_shape(Maps$Populations_carto) +
            tm_polygons("Populations") + tm_layout(title = "Residents by CAU", title.position = c("right","top"))

tmap_arrange(Maps$tt1,Maps$ca1,Maps$ca2)

# Palettes https://r-graph-gallery.com/38-rcolorbrewers-palettes.html 
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html 
Maps$pe1 <- tm_shape(TTmap) + 
  tm_polygons("SurgCaseRatio", palette = "cividis", n = 6, style = "jenks", border.col = "black") +
  tm_layout(title = "Surgical Rate by CAU", title.position = c("right","top"))
  

Maps$pe2 <- tm_shape(Maps$Cases_carto) +
  tm_polygons("SurgCaseRatio", palette = "cividis", n = 6, style = "jenks", border.col = "black") +
  tm_layout(title = "Surgical Rate on Cases Cartogram", title.position = c("right","top")) + 
  tm_shape(Maps$Cases_carto %>%
             filter(AU2013_V1_00_NAME %in% c("Kerikeri","Awanui","Port-Limeburners"))) +
  tm_borders(col = "red") +
  tm_text("AU2013_V1_00_NAME", col = "white", xmod = 2.5, ymod = 1)

Maps$pe3 <- tm_shape(TTmap) +
  tm_polygons("LagCaseRatio", palette = "cividis", n = 6, style = "jenks", border.col = "black") +
  tm_layout(title = "Lag Times Rate by CAU", title.position = c("right","top"))


Maps$pe4 <- tm_shape(Maps$Cases_carto) +
  tm_polygons("LagCaseRatio", palette = "cividis", n = 6, style = "jenks", border.col = "black") +
  tm_layout(title = "Lag Times on Cases Cartogram", title.position = c("right","top")) +
  tm_shape(Maps$Cases_carto %>%
             filter(AU2013_V1_00_NAME %in% c("Kerikeri", "Awanui", "Port-Limeburners"))) +
  tm_borders(col = "red") +
  tm_text("AU2013_V1_00_NAME", col = "white", xmod = 2.5, ymod = 1)


tmap_arrange(Maps$pe1,Maps$pe2)

tmap_arrange(Maps$pe3,Maps$pe4)

#Individual Maps
Maps$pe1
Maps$pe2
Maps$pe3
Maps$pe4


##Appendix

# 1: Kable

kable(FinalTable, dfigit = 4)

# 2: Map highlighting whangarei central
Maps$pe5 <- tm_shape(Maps$Cases_carto) +
  tm_polygons("SurgCaseRatio", palette = "cividis", n = 6, style = "jenks", border.col = "black") +
  tm_layout(title = "Surgical Rate on Cases Cartogram", title.position = c("right","top")) + 
  tm_shape(Maps$Cases_carto %>%
             filter(AU2013_V1_00 %in% c("502001", "502002", "502003", "502004", "502005", "502101", 
                                        "502201", "502202", "502300", "502400", "502500", "502600", "502700", 
                                        "502800", "502900", "503000", "503100", "503200", "503300", "503400", 
                                        "503500", "503600", "503700", "503800", "503900", "504000", "504100"))) +
  tm_borders(col = "red")

Maps$pe6 <- tm_shape(Maps$Cases_carto) +
  tm_polygons("LagCaseRatio", palette = "cividis", n = 6, style = "jenks", border.col = "black") +
  tm_layout(title = "Lag Times on Cases Cartogram", title.position = c("right","top")) +
  tm_shape(Maps$Cases_carto %>%
             filter(AU2013_V1_00 %in% c("502001", "502002", "502003", "502004", "502005", "502101", 
                                        "502201", "502202", "502300", "502400", "502500", "502600", "502700", 
                                        "502800", "502900", "503000", "503100", "503200", "503300", "503400", 
                                        "503500", "503600", "503700", "503800", "503900", "504000", "504100"))) +
  tm_borders(col = "red")

tmap_arrange(Maps$pe5,Maps$pe6)

# 3: Population prediction linear regression

lmCases <-  lm(Cases ~ Residents, data = FinalTable)

summary(lmCases)

ggplot(FinalTable, aes(Residents,Cases)) +
  geom_point() +
  geom_smooth(method = 'lm')


#To add text:
tm_shape(Maps$Cases_carto %>%
           filter(AU2013_V1_00_NAME %in% c("Kerikeri", "Whangarei Central", "Awanui"))) +
  tm_text("AU2013_V1_00_NAME", col = "white", xmod = -1, ymod = 1)




### NOT FOR FINAL REPORT ###
#### TEST 

FinalTable$LagOver31 <- FinalTable$Cases - FinalTable$LagUnder31

Plots$LagFunPlot1 <- (function(x=FinalTable) {
  
  x$Domicile        <- factor(x$Domicile)
  x$LagOver31       <- as.numeric(x$LagOver31)
  
  Anchor         <- glm(LagOver31 ~ Cases, 
                        family="poisson", 
                        data=x)
  
  Predicted      <- predict(Anchor, newdata = x, type="response")
  
  
  # Draw plot, returning just the plot object
  funnel_plot(denominator=Predicted, numerator=x$LagOver31,
              limit=95 , group = x$Domicile, label = "outlier",
              draw_unadjusted = TRUE, title = "Lag Time Over 31 days by CAU", 
              x_label = "Expected Cases")
})()

Plots$LagFunPlot1