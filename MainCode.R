#LIBRARIES
library(XML)
library(rvest)
library(stringr)
library(RSelenium)
library(xlsx)

#VECTOR INITIALIZATION
hrefs=c()
YearBuilt=c()
LotSize=c()
FloorSize=c()
CurrPrice=c()
PriceSqft=c()
MLSNumber=c()
address=c()
ParcelNumber=c()
stories=c()
ZillowHomeID=c()
LastRemodelYear=c()
StructureType=c()
RoomNumber=c()
ZipCode=c()
HouseType=c()
BedsNumber=c()
BathsNumber=c()
skls=c()
pHist=c()
tHist=c()
neighbourHood=c()
pages=c()

#BASE URL
url = "https://www.zillow.com/homes/recently_sold/Phoenix-AZ/40326_rid/76001-80000_price/288-303_mp/globalrelevanceex_sort/33.932536,-111.655427,33.278305,-112.593384_rect/9_zm/"

#FUNCTIONS
#yearBuilt
Year_Built=function(facts)
{
  yb<-grep("Built in",facts)
  if(!length(yb)==0)
  {YearBuilt=c(YearBuilt,sub("(.*) ","",facts[yb]))
  }else
  {YearBuilt=c(YearBuilt,"NA")}
  return(YearBuilt)
}
##LotSize
Lot_Size=function(facts)
{
  ls<-grep("Lot",facts)
  if(!length(ls)==0)
  {LotSize=c(LotSize,sub("(.*): ","",facts[ls]))
  }else
  {LotSize=c(LotSize,"NA")}
  return(LotSize)
}
##Price/sqft
Price_Sqft=function(facts)
{
  pps<-grep("[Pp]rice/sqft",facts)
  if(!length(pps)==0)
  {PriceSqft=c(PriceSqft,sub("(.*): ","",facts[pps]))
  }else
  {PriceSqft=c(PriceSqft,"NA")}
  return(PriceSqft)
}
##MLS
MLS_Number=function(facts)
{mls<-grep("MLS #",facts)
if(!length(mls)==0)
{MLSNumber=c(MLSNumber,sub("(.*): ","",facts[mls]))
}else
{MLSNumber=c(MLSNumber,"NA")}
return(MLSNumber)}
##Neighbour
Neighborhood=function(neighbour)
{nbr<-grep("Neighborhood:",neighbour)
if(!length(nbr)==0)
{neighbourHood=c(neighbourHood,sub("(.*): ","",neighbour[nbr]))
}else
{neighbourHood=c(neighbourHood,"NA")}
return(neighbourHood)}
#Address
Address=function(meta)
{
  
  if(!length(meta)==0){
    ans<-html_attr(meta,"content")
    address=c(address,ans)
  }else
  {address=c(address,"NA")
  return(address)}
}
#FloorSize
Floor_Size=function(others)
{
  fs<-grep("Floor size",others)
  if(!length(fs)==0)
  {FloorSize=c(FloorSize,sub("(.*): ","",others[fs]))
  }else
  {FloorSize=c(FloorSize,"NA")}
  return(FloorSize)}
#ParcelNumber
Parcel_Number=function(others)
{
  pn<-grep("Parcel #",others)
  if(!length(pn)==0)
  {ParcelNumber=c(ParcelNumber,sub("(.*): ","",others[pn]))
  }else
  {ParcelNumber=c(ParcelNumber,"NA")}
  return(ParcelNumber)}
#Stories
Stories=function(others)
{
  s<-grep("^Stories",others)
  if(!length(s)==0)
  {stories=c(stories,sub("(.*): ","",others[s]))
  }else
  {stories=c(stories,"NA")}
  return(stories)}
#ZillowHomeId
Zillow_Home_ID=function(others)
{
  zhi<-grep("Zillow Home ID",others)
  if(!length(zhi)==0)
  {ZillowHomeID=c(ZillowHomeID,sub("(.*): ","",others[zhi]))
  }else
  {ZillowHomeID=c(ZillowHomeID,"NA")}
  return(ZillowHomeID)}
#LastRemodelYear
Last_Remodel_Year=function(others)
{
  lry<-grep("Last remodel",others)
  if(!length(lry)==0)
  {LastRemodelYear=c(LastRemodelYear,sub("(.*): ","",others[lry]))
  }else
  {LastRemodelYear=c(LastRemodelYear,"NA")}
  return(LastRemodelYear)}
#StructureType
Structure_Type=function(others)
{
  st<-grep("Structure type",others)
  if(!length(st)==0)
  {StructureType=c(StructureType,sub("(.*): ","",others[st]))
  }else
  {StructureType=c(StructureType,"NA")}
  return(StructureType)}
#RoomNumber
Room_Number=function(others)
{
  rn<-grep("Room count",others)
  if(!length(rn)==0)
  {RoomNumber=c(RoomNumber,sub("(.*): ","",others[rn]))
  }else
  {RoomNumber=c(RoomNumber,"NA")}
  return(RoomNumber)}
#ZipCode
Zip_Code=function(meta)
{
  if(!length(meta)==0){
    zipAdd<-html_attr(meta,"content")
    zc=grep("(.*) [0-9]{5}$",zipAdd)
    if(zc==1){
      ZipCode=c(ZipCode,sub("(.*) ","",zipAdd))
    }else{
      ZipCode=c(ZipCode,"NA")
    }
  }else{
    ZipCode=c(ZipCode,"NA")
  }
  return(ZipCode)
}
#HouseType
House_Type=function(facts)
{
  ht=facts[grep("([Ss]ingle [Ff]amily)|([Mm]ulti [Ff]amily)|([Cc]ondo) ",facts)]
  if(!length(ht)==0)
  {HouseType=c(HouseType,ht)
  }else
  {HouseType=c(HouseType,"NA")}
  return(HouseType)
}
#BedsNumber
Beds_Number=function(heads)
{
  bn=heads[grep("(.*) beds",heads)]
  if(!length(bn)==0)
  {BedsNumber=c(BedsNumber,sub(" (.*)","",bn))
  }else
  {BedsNumber=c(BedsNumber,"NA")}
  return(BedsNumber)
}
#BathNumber
Baths_Number=function(heads)
{
  btn=heads[grep("(.*) baths",heads)]
  if(!length(btn)==0)
  {BathsNumber=c(BathsNumber,sub(" (.*)","",btn))
  }else
  {BathsNumber=c(BathsNumber,"NA")}
  return(BathsNumber)
}
#Schools
SchoolNames=function(schools)
{
  if(!length(schools)==0)
  { skools=""
  lk=str_replace_all(schools,"\n","")
  pk=str_replace_all(lk," \\s+","_")
  dk=str_replace_all(pk,"(^_)|(_$)","")
  for(a in 1:length(schools))
  {
    skools=paste(skools,dk[a],sep=";") 
  }
  
  skls=c(skls,skools)
  }else
  {skls=c(skls,"NA")}
  return(str_replace_all(skls,"^;",""))
}
historyTables<-function(tableType,tableNode,priceNode)
{
  
  if(length(tableNode)==0){
    print("came here")
    return("NA")
  }else{
    remDr$setImplicitWaitTimeout(20000)
    tab <-remDr$findElements(using = 'class','zsg-table')
    if(length(tab)==0){
      return("NA")
    }
    if(tableType=="price"){
      a<-tab[[1]]$getElementAttribute("outerHTML")
    }else{
      if(length(tab)<2){
        a<-tab[[1]]$getElementAttribute("outerHTML")
      }else{
        a<-tab[[2]]$getElementAttribute("outerHTML")  
      }
      
    }
    
    table1 <- readHTMLTable(a[[1]], header=TRUE, as.data.frame=TRUE)[[1]]
    acc<-table1
    ta<-table1[-length(table1)]
    
    ans<-""
    for(i in 1:nrow(ta)){
      for(j in 1:ncol(ta)){
        
        if(j==1){
          ans<-paste0(ans,toString(ta[i,j]))
        }else{
          ans<-paste(ans,toString(ta[i,j]),sep = "_")
          
        }
      }
      ans<-paste0(ans,";")
    }
    return(ans)
  }
}
CurrentPrice<-function(currPriceNode)
{
  if(length(currPriceNode)==0){
    return("NA")
  }else{
    if(html_text(currPriceNode)=="     "){
      return("NA")
    }else{
      return(html_text(currPriceNode))    
    }
    
  }
}

#TAKE INDIVIDUAL URLs OF ALL HOUSES ON THE BASE URL
for (i in 1:20)
{
  k=readLines(paste0(url,i,"_p"))
  htmlpage <- htmlParse(k, asText = TRUE)
  hrefs <- c(hrefs,xpathSApply(htmlpage, "//*[@id='search-results']/ul[@class='photo-cards']/li/article/div[@class='zsg-photo-card-content zsg-aspect-ratio-content']/a", function(u) xmlAttrs(u)["href"]))
  
  pages=c(pages,htmlpage)
}
# creating the selenium server
rD <- rsDriver() # search for and download Selenium Server java binary.  Only need to run once.
remDr <- rD$client
#individual house urls
url="https://www.zillow.com"
individualUrl=paste0(url,hrefs)

#CALL FUNCTIONS FOR ALL DATA INTO RESPECTIVE VECTORS
for(j in 1:length(individualUrl))
{ 
  indPage = read_html(individualUrl[j])
  
  #extract facts
  factNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div/section[@class='zsg-content-section ']/div[@class='hdp-facts zsg-content-component z-moreless']/div[@class='fact-group-container zsg-content-component top-facts']/ul[@class='zsg-list_square zsg-lg-1-3 zsg-md-1-2 zsg-sm-1-1']/li")
  
  if(!length(factNodes)==0){
    priceNode=html_node(indPage,xpath='//div[@id="hdp-price-history"]')
    taxNode=html_node(indPage,xpath='//div[@id="hdp-tax-history"]')
    meta=html_node(indPage,xpath = '//head/meta[@property="og:zillow_fb:address"]')
    otherNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div/section[@class='zsg-content-section ']/div[@class='hdp-facts zsg-content-component z-moreless']/div[@class='fact-group-container zsg-content-component z-moreless-content hide']/ul[@class='zsg-list_square zsg-lg-1-3 zsg-md-1-2 zsg-sm-1-1']/li")
    headNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div/div[@class='zsg-lg-2-3 zsg-sm-1-1 hdp-header-description']/header[@class='zsg-content-header addr']/h3/span")
    schoolNodes=html_nodes(indPage,xpath="//*[@id='nearbySchools']/div[1]/div[@class='zsg-content-item']/ul[@class='nearby-schools-list']/li[@class='nearby-school assigned-school  clearfix']/div[@class='nearby-schools-info']")
    neighbourNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div/section[@id='hdp-neighborhood']/h2")
    currPriceNode=html_node(indPage,xpath='//*[@id="home-value-wrapper"]/div[@class="estimates"]/div[@class="main-row status-icon-row recently-sold-row home-summary-row"]/span[2]')
    priceNode=html_node(indPage,xpath='//div[@id="hdp-price-history"]')
  }else{
    priceNode=html_node(indPage,xpath='//div[@id="hdp-price-history"]')
    taxNode=html_node(indPage,xpath='//div[@id="hdp-tax-history"]')
    factNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/section[@class='zsg-content-section ']/div[@class='hdp-facts zsg-content-component z-moreless']/div[@class='fact-group-container zsg-content-component top-facts']/ul[@class='zsg-list_square zsg-lg-1-3 zsg-md-1-2 zsg-sm-1-1']/li")
    meta=html_node(indPage,xpath = '//head/meta[@property="og:zillow_fb:address"]')
    otherNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/section[@class='zsg-content-section ']/div[@class='hdp-facts zsg-content-component z-moreless']/div[@class='fact-group-container zsg-content-component z-moreless-content hide']/ul[@class='zsg-list_square zsg-lg-1-3 zsg-md-1-2 zsg-sm-1-1']/li")
    headNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/div[@class='zsg-lg-2-3 zsg-sm-1-1 hdp-header-description']/header[@class='zsg-content-header addr']/h3/span")
    schoolNodes=html_nodes(indPage,xpath="//*[@id='nearbySchools']/div[1]/div[@class='zsg-content-item']/ul[@class='nearby-schools-list']/li[@class='nearby-school assigned-school  clearfix']/div[@class='nearby-schools-info']")
    neighbourNodes=html_nodes(indPage,xpath="//*[@id='hdp-content']/div[@role='main']/section[@id='hdp-neighborhood']/h2")
    currPriceNode=html_node(indPage,xpath='//*[@id="home-value-wrapper"]/div[@class="estimates"]/div[@class="main-row  home-summary-row"]/span[1]')
    priceNode=html_node(indPage,xpath='//div[@id="hdp-price-history"]')
  }
  
   
  remDr$navigate(individualUrl[j])  
   
  
  #html from nodes
  facts = html_text(factNodes)
  others = html_text(otherNodes)
  heads = html_text(headNodes)
  schools = html_text(schoolNodes)
  neighbour = html_text(neighbourNodes)
  
  #function calls
  CurrPrice=c(CurrPrice,CurrentPrice(currPriceNode))
  YearBuilt=Year_Built(facts)
  LotSize=Lot_Size(facts)
  PriceSqft=Price_Sqft(facts)
  FloorSize=Floor_Size(others)
  MLSNumber=MLS_Number(facts)
  address=Address(meta)
  ParcelNumber=Parcel_Number(others)
  stories=Stories(others)
  ZillowHomeID=Zillow_Home_ID(others)
  LastRemodelYear=Last_Remodel_Year(others)
  neighbourHood=Neighborhood(neighbour)
  StructureType=Structure_Type(others)
  RoomNumber=Room_Number(others)
  ZipCode=Zip_Code(meta)
  HouseType=House_Type(facts)
  BedsNumber=Beds_Number(heads)
  BathsNumber=Baths_Number(heads)
  skls=SchoolNames(schools)
  pHist = c(pHist,historyTables("price",priceNode,priceNode))
  tHist = c(tHist,historyTables("tax",taxNode,priceNode))
  
  #writing the webpages
  write_html(indPage,file =sprintf("C:\\HTMLPages\\%s.html",ZillowHomeID[length(ZillowHomeID)]))
 }

#CREATE A DATAFRAME USING DATA VECTORS AND WRITE INTO AN EXCEL FILE
DF=data.frame("Zillow_Home_ID"=ZillowHomeID,"Current_Price"=CurrPrice,"Price_Each_SQFT"=PriceSqft,"Price_History"=pHist,"Tax_History"=tHist,"URL"=individualUrl,"Address"=address,"NEIGHBOURHOOD"=neighbourHood,"Zip_Code"=ZipCode,"School"=skls,"Lot_Size"=LotSize,"Floor_Size"=FloorSize,"MLS_Number"=MLSNumber,"Parcel_Number"=ParcelNumber,"stories"=stories,"Built_Year"=YearBuilt,"Last_Year_Remodel"=LastRemodelYear,"House_Type"=HouseType,"Structure_Type"=StructureType,"Baths_Number"=BathsNumber,"Beds_Number"=BedsNumber,"Room_Num"=RoomNumber)
write.xlsx(DF,"C:\\Users\\parth raste\\Desktop\\R Project\\rs4.xlsx")
