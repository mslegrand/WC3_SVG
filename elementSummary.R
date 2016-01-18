#FiltersElement
library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)

source("showMe.R")

#http://www.w3.org/TR/SVG11/
#http://www.w3.org/TR/SVG11/eltindex.html
#grap all page references to svg elements
url="http://www.w3.org/TR/SVG/eltindex.html"
getHTMLLinks(url, relative = TRUE)->tmp.links
unique(gsub("#.*$","", tmp.links))->pages


# for(page in pages2){
#   cat("2: ",page,"\n")
# }
# 
# pageUrl<-"http://www.w3.org/TR/SVG/filters.html"

capitalizeIt<-function(name){
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", name, perl=TRUE)
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
    val<-str_split(val,",")[[1]]
    val<-capitalizeIt(str_trim( val))
    val<-gsub("None","Uncategorized Element", val)
    dt1<-data.table(element=ele.name, variable='category', value=val)
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
    dt2<-data.table(element=ele.name, variable='content.model', value=val3)
    
    
    node.ul<-tmp.ns[[3]][['ul']]
    val<-node2Vals(node.ul)
    dt3<-data.table(element=ele.name, variable='attr', value=val)
    
    dt<-rbindlist(list(dt1,dt2,dt3))
    
  }
  
  page.dt<-rbindlist(lapply( ns.els, elementSummaryNode2Table))
  
}

#scrape4ElementSummary(pageUrl)->test.page.dt

elementSummary.list<-lapply(pages, scrape4ElementSummary)
rbindlist(elementSummary.list)->es.DT

#------------------------ATTENTION!!!!-----------------------------------------
#  kludge to clean es.DT 
#------------------------BEGIN CLEAN!!!!-----------------------------------------

es.DT$value->values # extract last column

#restrict our attention to content.mode
grep('content.model', es.DT$variable)->content.indx

grep('altGlyphDef',es.DT$element)->rpl.indx
indx<-intersect(content.indx,rpl.indx)
if(length(indx)!=3){stop("altGlyphDef issues")}
values[indx]<-c('glyphRef', 'altGlyphItem', '')

values<-gsub('elements?$','elements:',values)

es.DT[,value:=values]
es.DT<-es.DT[value!=""] #remove row with empty value

#------------------------END CLEAN!!!!-----------------------------------------


write.table(es.DT,file="dataTableLink/elementSummary.tsv",
            sep="\t",
            row.names=FALSE,
            quote=FALSE)


