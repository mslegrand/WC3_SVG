library(XML)
library(RCurl)
#library(stringi)

cleanQuotes<-function(x){
  gsug("[‘’]","",x)
}

cleanValues<-function(values){
  gsub('\\"',"",values)->values
  gsub("\\n","",values)->values
  gsub("[<>]","", values)->values
  gsub("^ +","",values)->values
  gsub(" +$","",values)->values
  gsub("[[:space:]]+"," ",values)->values
  values
}

#returns dataframe with animatable data types
getAnimatableDataTypes<-function(){
  url<-"http://www.w3.org/TR/SVG/animate.html#Animatable"
  rawPMI<-readHTMLTable(url)
  animatableDataTypes<-data.frame(rawPMI[[10]])  
}


#returns dataframe with attr, elems,  animatiable status
getElementAttrAniTable<-function(){
  url<-"http://www.w3.org/TR/SVG/attindex.html"
  rawPMI2<-readHTMLTable(url)
  df1<-data.frame(rawPMI2[[1]])
  #df2<-data.frame(rawPMI2[[2]])
  df1
}



getAEVL.df<-function(){
  # returns an dataframe with attr, elements and page-links to values
  # this is derived from the same table as getElementAttrAniTable
  # the difference is that his include the page-links, 
  # while getElementAttrAniTable includes animatable flag
  
  getElementAttrLinkTable<-function(){
    url="http://www.w3.org/TR/SVG/attindex.html"    
    script<-getURL(url)
    doc <- htmlParse(script)
    getNodeSet(doc, "//tr")->ns.tr
    
    extractAttr<-function(n){
      kids<-xmlChildren(n)
      rtv<-list()
      if(length(kids)==3 ){
        #     alink<-xmlAttrs(kids[[1]])
        #     attr<- xmlValue(kids[[1]])
        rtv<-xmlToList(n)
      }
      unlist(rtv)
    } 
    sapply(ns.tr, extractAttr)->links  
    indx<-sapply(links, length)
    goodIndx<-indx>=6 & indx%%2==0
    initialNames<-c("td.a.span.text" ,   "td.a.span..attrs.class", "td.a..attrs.href",    "td.a.span.text" ,   "td.a.span..attrs.class", "td.a..attrs.href"  )
    startsGood<-function(x){all(names(x)[1:6]==initialNames)}
    links<-links[goodIndx]
    goodIndx<-sapply(links, startsGood)
    links<-links[goodIndx]
    
    # links[[1]]
    # names(links[[1]])
    
    doRow<-function(row){
      txt<-row[names(row)=="td.a.span.text"]
      ref<-row[names(row)=="td.a..attrs.href"]
      
      attribName<-txt[1]
      attribRef<-ref[1]
      elements<-txt[-1]
      # elementRefs<-ref[-1]
      df<-data.frame(attr=attribName, element=elements, link=attribRef, stringsAsFactors = F)
      row.names(df)<-NULL
      df
    }
    
    #links[[1]]->row
    links<-lapply(links, doRow)
    linkInfo<-do.call(rbind, links)
    strsplit(linkInfo$link, "#")->tmp
    do.call(rbind, tmp)->tmp2
    linkInfo$page<-tmp2[,1]
    linkInfo$loc<-tmp2[,2]
    
    linkInfo  
  }
  
  elemAttrLinkTable<-getElementAttrLinkTable()
  pages<-unique(elemAttrLinkTable$page)
  #next we get each page and open it, and extract for each elem-attr pair on that page
  # the corresponding values to create an element-attribute-values tabel
  #
  
  # input page (and elemAttrLinkTable),
  # returns dataframe with attr, element, value and links to value description on that
  # page
  getElAttrValLinkFromPage<-function(page){
    # given a page
    elAtt<-elemAttrLinkTable[elemAttrLinkTable$page==page,c(1,2,5)]
    url<-paste("http://www.w3.org/TR/SVG/",page, sep="")  
    script<-getURL(url)
    doc <- htmlParse(script)
    #ids<-paste( "//dt[@id='", elAtt$loc, "']", sep="")
    ids<-elAtt$loc
    getValLinks<-function(id){
      pid=paste( "//dt[@id='", id, "']", sep="")
      ns<-getNodeSet(doc,pid)
      if(length(ns)>0){
        val<-xmlValue(ns[[1]])
        sib<-getSibling(ns[[1]])
        links<-getHTMLLinks(sib)
      } else {
        pid=paste( "//a[@id='", id, "']", sep="")
        ns<-getNodeSet(doc,pid)
        if(length(ns)>0){
          parent<-xmlParent(ns[[1]])
          kids<-xmlChildren(parent)
          val<-sapply(kids,xmlValue)
          val<paste(val,collapse="")
          links<-c("**")        
        } else {
          pid=paste( "//p[@id='", id, "']", sep="")
          ns<-getNodeSet(doc,pid)
          if(length(ns)>0){
            val<-paste(xmlValue(ns[[1]]),collapse=" ")
            links<-getHTMLLinks(ns[[1]]) 
            cat(class(val),"\n")
            print(length(val))
            cat(class(links),"\n")
            cat("val=",val,"\n")
            cat("links=",links,"\n")
          } else {
            val<-"???"
            links<-"???"
          }
        }
      }
      rtv<-c(val,links) 
    }
    
    val_links<-sapply(ids, getValLinks)
    cat("\n\nclass(val_links)=",class(val_links),"\n")
    #   if(page=="interact.html"){
    #     print(val_links)
    #     print("class(val_links)=",class(val_links),"\n" )
    #   }
    if(class(val_links)=="matrix"){
      val_links<-list(val_links[,1], val_links[,2])
    }
    txts<-sapply(val_links, function(x){x[1]} )
    txts<-strsplit(txts,"=")
    
    sapply(txts, function(x){x[2]})->values
    values<-cleanValues(values)
    cat(page,"\nlenght(values)=",length(values),"\nnrow(elAtt)=", nrow(elAtt),"\n")
    if(nrow(elAtt)!=length(values)){
      print(elAtt)
      print(values)
      print(paste(values,collapse=")>-<"))
      stop("bad news")
    }
    elAtt$Values<-values
    
    firstLinks<-sapply(val_links, function(x)x[2])
    elAtt$firstLinks<-firstLinks
    elAtt
  }
  
  # for(i in 1:length(pages)){
  #   cat("page[",i,"]\n")
  #   page<-pages[i]
  #   elAttrLnk<-getElAttrValLinkFromPage(page)
  # }
  # page<-pages[1]
  
  
  #elAttrLnk<-getElAttrValLinkFromPage(page)
  attr_ele_loc_val_1stLnk<-lapply(pages, getElAttrValLinkFromPage)
  AELV.df<-do.call(rbind, attr_ele_loc_val_1stLnk)->attr_ele_loc_val_1stLnk
  #getNodeSet(doc, "//dt[@id='FontFaceElementBboxAttribute']")->bbox
  AELV.df  
}

AELV.df<-getAEVL.df()

AELV.df$Values->vals
unique(vals)->uvals
table(vals)->valFreq
grep("\\|", names(valFreq))->indx.with.vbar
vals.with.vbar<- valFreq[indx.with.vbar]
setdiff(1:length(valFreq),indx.with.vbar)->indx.without.vbar
vals.without.vbar<-valFreq[indx.without.vbar]




#links[[2]]->tmp
#if we unlist, should see character vector of length >=6
# 1st string is named "td.a.span.text", and contains the attribute
  # 2nd char is named td.a.span..attrs.class" and contains the string "attr-name"
# 3rd is named "td.a..attrs.href" and contains the link
  # 4th is named "td.a.span.text" and contains an element
# 5th is "td.a.span..attrs.class" and contains the string "element-name
  #6 is "td.a..attrs.href" and contains the link to the element

# etc:
# 
# ‘accumulate’  ::  ‘animate’, ‘animateColor’, ‘animateMotion’, ‘animateTransform’
# becomes
# 
# td.a.span.text                 td.a.span..attrs.class 
# "‘accumulate’"                            "attr-name" 
# td.a..attrs.href                         td.a.span.text 
# "animate.html#AccumulateAttribute"                            "‘animate’" 
# td.a.span..attrs.class                       td.a..attrs.href 
# "element-name"          "animate.html#AnimateElement" 
# td.text                         td.a.span.text 
# ", "                       "‘animateColor’" 
# td.a.span..attrs.class                       td.a..attrs.href 
# "element-name"     "animate.html#AnimateColorElement" 
# td.text                         td.a.span.text 
# ", "                      "‘animateMotion’" 
# td.a.span..attrs.class                       td.a..attrs.href 
# "element-name"    "animate.html#AnimateMotionElement" 
# td.text                         td.a.span.text 
# ", "                   "‘animateTransform’" 
# td.a.span..attrs.class                       td.a..attrs.href 
# "element-name" "animate.html#AnimateTransformElement" 
# # 
# 
# td.a.span.text is flag for text
# td.a..attrs.href are the links for the text
# The td.a.span..attrs.class  should be c( "attr-name", "element-name", ...)
# 
# 

# sapply(ns.td, xmlValue)->tmp
# head(kidNames)
# 
# which(kidNames=="")
# 
# grep("span", unlist(kidNames))
# page<-"linking.html"
# elAtt<-elemAttrLinkTable[elemAttrLinkTable$page==page,c(1,2,5)]
# url<-paste("http://www.w3.org/TR/SVG/",page, sep="")  
# script<-getURL(url)
# doc <- htmlParse(script)
# 
# id<-"//dt[@id='XLinkTypeAttribute'] " 
# ns.dt<-getNodeSet(doc,id)
# length(ns.dt)
# id<-"//a[@id='XLinkTypeAttribute']" 
# ns.dt<-getNodeSet(doc,id)
# length(ns.dt)
# ns

# getValLinks(id)
