#FiltersElement
library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)

source("showMe.R")


#grap all page references to svg elements
url="http://www.w3.org/TR/SVG/eltindex.html"
getHTMLLinks(url, relative = TRUE)->tmp.links
unique(gsub("#.*$","", tmp.links))->pages


# for(page in pages2){
#   cat("2: ",page,"\n")
# }
# 
# pageUrl<-"http://www.w3.org/TR/SVG/filters.html"
clean<-function(x){
  gsub("")
}
scrape4ElementSummary<-function(pageUrl){
  #url<-"http://www.w3.org/TR/SVG/filters.html"
  #url<-"http://www.w3.org/TR/SVG/Overview.html"
  #showMe(pageUrl)
  script<-getURL(pageUrl)
  doc <- htmlParse(script)
  #getNodeSet(doc, "//h3/span[@class='element-name']")->ns.eln
  getNodeSet(doc, "//div[@class='element-summary']")->ns.els
  
  cleanVals1<-function(vals){
    vals<-gsub(" — .*$",":",vals) #truncate ending
    vals<-gsub("in’", "in1", vals) #be aware of this kludge
    vals<-gsub("[‘’,]","",vals) 
    vals<-gsub("\n"," ",vals)
    vals<-gsub(" +"," ",vals)
    vals<-gsub("^ ","",vals)
    vals<-gsub(" $","",vals)
  }
  
  node2Vals<-function(node){
    if(length(node)>0){
      vals<-xmlApply(node, xmlValue )
      vals<-unlist(vals, use.names=F)
      vals<-cleanVals1(vals)
    } else {
      vals<-NULL
    }
    vals  
  }
  
  elementSummaryNode2Table<-function(node){
    ele.name<-xmlValue(node[['div']])
    ele.name<-gsub("[‘’]","",ele.name)
    node.dl<-node[['dl']] 
    #building dt1
    getNodeSet(node.dl, 'dd')->tmp.ns
    val<-xmlValue(tmp.ns[[1]])
    dt1<-data.table(element=ele.name, type='category', value=val)
    
    #building dt2
    # this lools for anything under the 2nd dd which is below a ul
    # but need to consider either href or svg-term
    node.ul<-tmp.ns[[2]][['ul']]
    val1<-node2Vals(node.ul)
    
    node2<-tmp.ns[[2]]
    ns2<-getNodeSet(node2, '*[@href]')
    val2<-sapply(ns2, xmlValue)
    val2<-unlist(val2)
    val3<-union(val1,val2)
    if(is.null(val3)){
      val3<-xmlValue(node2)
    }
    if(is.null(val3)){
      val3<-"NULL"
    }
    val3<-cleanVals1(val3)
    dt2<-data.table(element=ele.name, type='content.model', value=val3)
    
    
    node.ul<-tmp.ns[[3]][['ul']]
    val<-node2Vals(node.ul)
    dt3<-data.table(element=ele.name, type='attr', value=val)
    
    dt<-rbindlist(list(dt1,dt2,dt3))
    
  }
  
  page.dt<-rbindlist(lapply( ns.els, elementSummaryNode2Table))
  
}

#scrape4ElementSummary(pageUrl)->test.page.dt

elemementSummary.list<-lapply(pages, scrape4ElementSummary)
rbindlist(elemementSummary.list)->elementSummary.dt

write.table(elementSummary.dt,file="dataTableLink/elementSummary.csv",
            sep=",",
            row.names=FALSE,
            quote=FALSE)


#ns.els

# i=5
# node<-ns.els[[i]]
# dt<-elementSummaryNode2Table(node)
#data.table(element=ele.name, type='category', value=category)

# node in ns.els has 2 children: div and dl 
# the element name being describe is given by  xmlValue(node[['div']])
# the category, content model and attributes reside below node[['dl']]
# node[['dl']] has 8 children
# "dt" "dd" "dt" "dd" "dt" "dd" "dt" "dd" 
# we are interested only in the values which reside beneath the dd's
# so setting node2<-node[['dl']] 
# getNodeSet(node2, "dd')->tmp.ns will return thes dd's
# the category is given by 
#  category=xmlValue(tmp.ns[[1]])
# the content model is given by 
#  contentModle=xmlValue(tmp.ns[[2]][['ul']])
# tmp.ns[[3]] has one child tmp.ns[[3]][['ul]]
# tmp.ns[[3]][['ul]] has 8 children all with name li
# these children correspond to 
# core attributes 
# presentation attributes 
# filter primitive attributes 
# ‘class’
# ‘style’
# ‘in’
# ‘type’
# ‘values’

# getNodeSet(doc, "//div[@class='element-summary-name']")->ns.els.name
# getNodeSet(doc, "//div[@class='element-summary']")->ns.els.content
# getNodeSet(doc, "//div[@class='element-summary']")->ns.els.attributes
# 
# 
# #getNodeSet(ns.els[[5]], "/dd")->tmp
# 
# 
# getNodeSet(doc, "//div[@class='element-summary']/dl/dd")->ns.els.stuff
# 
# readHTMLList(ns.els[[5]])