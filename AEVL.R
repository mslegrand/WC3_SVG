library(XML)
library(RCurl)


cleanElements<-function(x){
  gsub("[‘’,]","",x)
}

cleanValues<-function(x){
  #gsub("[<>]","",x)
  x<-gsub("\\s+"," ",x)
  x
}

getAEAL<-function(){
  url="http://www.w3.org/TR/SVG/attindex.html"    
  script<-getURL(url)
  doc <- htmlParse(script)
  getNodeSet(doc, "//tr")->ns.tr
  
  extractAttrRow<-function(node){
    xmlDoc(node)->tmpDoc
    #kids<-xmlChildren()
    kids<-getNodeSet(tmpDoc,"//td")
    rtv<-NULL
    df<-data.frame()
    if(length(kids)==3 ){
      kids.strings<-getChildrenStrings(node)
      attr<-cleanElements(kids.strings[1])
      elements<-kids.strings[2]
      elements<-cleanElements(elements)
      elements<-unlist(strsplit(elements," "))
      anim<-kids.strings[3]!=""
      link<-getHTMLLinks(kids[[1]])
      strsplit(link,"#")->eleLinkInfo
      matrix(unlist(eleLinkInfo),2,)->eleLinkInfo.m
      eleLinkInfo.m[1,]->page
      eleLinkInfo.m[2,]->loc
#       cat("attr=",mode(attr),"\n")
#       cat("elements=",mode(elements),"\n")
#       cat("anim=",mode(anim),"\n")
#       cat("link=",mode(link),"\n")
#       cat("page=",mode(page),"\n")
#       cat("loc=",mode(loc),"\n")
      
      df<-data.frame(attr=attr, element=elements, anim=anim,
                link=link, page=page, loc=loc, row.names=NULL,
                stringsAsFactors=F
      )
    #  cat("df=",class(df),"\n") 
    #  cat("rownames=",rownames(df),"\n")
    }
    rownames(df)<-NULL
    #cat("rownames=",row.names(df),"\n")
    return(df)
  } 
  
  lapply(ns.tr, extractAttrRow)->rows 
  rows
  #remove empty row
  #rbind
}

getAEAL()->res
lapply(res, function(x){rownames(x)<-NULL; x})->res
AVEL.df<-do.call(rbind, res)


# xx_valLnk<-function(ns){
#  txt<-xmlValue(ns[[1]])
#  val<-gsub("^.+=", "", txt)
#  link<-getHTMLLinks(ns[[1]])[1]
#  c(val,link)
# }


dt_valLnk<-function(ns){
  #   val<-xmlValue(ns[[1]])
  #   sib<-getSibling(ns[[1]])
  #   links<-getHTMLLinks(sib)
  #   c(val=val, link=links)  
  xmlDoc(ns[[1]])->tmpDoc
  kidV<-getNodeSet(tmpDoc,'//*[@class="attr-value"]')
  if(length(kidV)!=0){
    val<-xmlValue(kidV[[1]])
    link<-getHTMLLinks(kidV[[1]])[1]
    val<-cleanValues(val)
    return(c(val=val, link=link))
  }
  kidV<-getNodeSet(tmpDoc,'//a[@href]')
  if(length(kidV)!=0){
    val<-xmlValue(kidV[[1]])
    link<- xmlGetAttr(kidV[[1]],"href")
    val<-cleanValues(val)
    #cat(val=val,link=link,"\n")
    return(c(val=val, link=link))    
  }
  txt<-xmlValue(ns[[1]])
  val<-gsub("^.+=", "", txt)
  link<-getHTMLLinks(ns[[1]])[1]
  #c(val,link)
  val<-cleanValues(val)
  c(val=val, link=link)
}

a_valLnk<-function(ns){
  parent<-xmlParent(ns[[1]])
  kids<-xmlChildren(parent)
  val<-sapply(kids,xmlValue)
  val<-paste(val,collapse="")
  links<-c("**")          
  c(val=val, link=links)  
}

p_valLnk<-function(ns){ # this may be too simply???
  val<-NA
  links<-NA
  values<-xmlValue(ns[[1]])
  if(length(values>0)){
    val<-paste(xmlValue(ns[[1]]),collapse=" ")
  }
  lnks.v<-getHTMLLinks(ns[[1]])
  lnks<-ifelse(length(lnks.v)>0, lnks.v[1], NA)
  if(length(lnks)>2){
   browser()
  }
  #links<-getHTMLLinks(ns[[1]]) 
  c(val=val, link=lnks)  
}

doValLink<-function(doc,id){
  idDes<-paste("//*[@id='",id,"']",sep="")
  nodeSet<-getNodeSet(doc,idDes)
  valLnk<-data.frame()
  if(length(nodeSet)>0){
    ntag<-xmlName(nodeSet[[1]])
#     if(ntag!="dt"){
#       cat("tnag=",ntag," id=",id,"\n")
#     }
    
    valLnk<-switch(ntag,
                   "dt"= dt_valLnk(nodeSet),
                   "a" = a_valLnk(nodeSet),
                   "p" = p_valLnk(nodeSet),
                   "h2"= dt_valLnk(nodeSet),
                    c(NA,NA)
    )  
  }
  if(length(valLnk)>2){
    cat("length(valLnk)=",length(valLnk)," tnag=",ntag," id=",id,"\n")
  }
  valLnk
}


do.Page<-function(AVEL.df,page){
  page.df<-AVEL.df[AVEL.df$page==page,]
  ids<-page.df$loc
  url<-paste("http://www.w3.org/TR/SVG/",page, sep="") 
  script<-getURL(url)
  doc <- htmlParse(script)  
  res1<-lapply(ids, function(id){ doValLink(doc,id) } )
  #res1<-lapply(res1, function(x){data.frame(x)})
  res2<-do.call(rbind, res1)
  page.df$val<-res2[,1]
  page.df$lnk<-res2[,2]
  page.df
}

#do.Page(AVEL.df, "fonts.html")->tmp.1.df
#do.Page(AVEL.df, pages[17])->tmp.df


do.all.Pages<-function(AVEL.df){
  pages<-unique(AVEL.df$page)
  res11<-lapply(1:length(pages), 
                function(i){   page<-pages[i];
                               #cat("i=",i," page=",page,"\n");
                               df<-do.Page(AVEL.df, page); 
                               #cat("dim(df)=",dim(df),"\n");
                               df<-as.matrix(df);
                               df})
#   res22 <- as.data.frame(data.table::rbindlist(res11))
#   head(res22)
  res22<-as.data.frame(do.call(rbind,res11),stringsAsFactors = F)
  res22
}

pages<-unique(AVEL.df$page)

# dff<-data.frame()
# for(page in pages[4]){
#  #cat("1page",page,"\n")
#   df<-do.Page(AVEL.df,page)
#   #cat("names for",page,":",names(df),"\n")
#   #dff<-rbind(dff,df)
#   #cat("2page",page,"\n")
# }

AVEL2.df<-do.all.Pages(AVEL.df)
row.names(AVEL2.df)<-NULL
treatValueAs<-(AVEL2.df$lnk)
#cat(class(treatValueAs),"\n")
strsplit(treatValueAs,"#")->treatValueAs
lapply(treatValueAs, function(x){ifelse(length(x)>1,gsub("DataType","",x[[2]]), NA)})->treatValueAs
treatValueAs<-unlist(treatValueAs)
AVEL2.df$treatValueAs<-treatValueAs
grep("\\|",AVEL2.df$val)->choiceIndx
AVEL2.df[choiceIndx,"treatValueAs"]<-"Choice"


which(is.na(AVEL2.df$treatValueAs))->NA.indx
unique(AVEL2.df$val[NA.indx])->missingTypes
table(AVEL2.df$val[NA.indx])->missingTypeTable
sort(missingTypeTable, decreasing = T)->missingTypeTable
missingType.df<-data.frame(val=names(missingTypeTable), treatValueAs=NA)
missingType.List<-lapply(names(missingTypeTable), function(x){intersect(NA.indx, which(x==AVEL2.df$val)) })
names(missingType.List)<-names(missingTypeTable)
missingType.info.df<-lapply(missingType.List, function(x){AVEL2.df[x[[1]],]} )
missingType.info.df<-do.call(rbind, missingType.info.df)
missingType.info.df$page<-NULL
missingType.info.df$loc<-NULL
rownames(missingType.info.df)<-NULL
missingType.info.df$Example<-NA

n<-1
setMissing<-function(attr, type, example){
  n<-which(missingType.info.df$attr==attr)
  missingType.info.df[n, 7]<<-type
  missingType.info.df[n, 8]<<-example
  n<<-n+1
}

#problems from here down
#1
setMissing('xml:lang', "string", 'xml:lang=""en-GB"')
setMissing('id', "string", 'id="string_wo_colon"')
setMissing('class', 'wsp-list', '(just called "list" in documentationclass="info attr-def"')
setMissing('style', 'cln-scln-list', '(named list?) style="fill: red; stroke: blue; stroke-width: 3"')
#5
setMissing('requiredExtensions', 'wsp-list', 'list of IRI references: http://example.com/requiredExtension1.svg http://example.com/requiredExtension2.svg')
setMissing('requiredFeatures','wsp-list', 'list of feature strings: http://www.w3.org/TR/SVG11/feature#CoreAttribute')
setMissing("systemLanguage", 'cmm-list', 'comma-separated list of language names:systemLanguage="mi, en"')
setMissing("xlink:arcrole",  'string', ' http://www.example.org/D<c3><bc>rst')
#9
setMissing("xlink:role",  'string', ' http://www.example.org/D<c3><bc>rst')
setMissing("xlink:title",  'string', ' http://www.example.org/D<c3><bc>rst')
setMissing("xlink:type",  'string', ' http://www.example.org/D<c3><bc>rst')
setMissing("xlink:actuate",  'string', 'xlink:actuate = "onLoad"')

setMissing("transform", "transform", "TODO!!!!!!!!")
setMissing("result", "string", '<feGaussianBlur in="SourceAlpha" stdDeviation="4" result="blur" />')
setMissing("horiz-adv-x", "number", "??")
setMissing("keySplines", "cmm-scln-list", 'keySplines="0,0.5,0.5,1; 0.5,0,1,0.5; 0,0.5,0.5,1; 0,0.5,0.5,1" ')
#‘keyPoints’ takes a semicolon-separated list of floating point values between 0 and 1
setMissing("keyPoints", "scln-list", 'in doc: <list-of-numbers>, a semicolon-separated list of floating point values between 0 and 1 : keyPoints="0; 0.5; 1"')
# appears that space-semicolon seperated list will also work
# calcMode="spline" keySplines="0 0 1 1; 0 0 1 1" 
setMissing("keyTimes", "cmm-scln-list", 'keySplines="0,0.5,0.5,1; 0.5,0,1,0.5; 0,0.5,0.5,1; 0,0.5,0.5,1" ')

setMissing("bbox", "cmm-4list",
'comma-separated list of exactly four numbers specifying, in order, the lower left x, lower left y, upper right x, and upper right y of the bounding box for the complete font'
)
setMissing("viewBox", "cmm-4list", '(<min-x>, <min-y>, <width> and <height>): viewBox="0 0 1500 1000"' )

setMissing("begin", "scln-list", 'beginValueList')

setMissing('end', "scln-list", "'end-value-list'")
setMissing('preserveAspectRatio','special-string', 'preserveAspectRatio="[defer] <align> [<meetOrSlice>]"')
setMissing('g1', "cmm-list", 'equence (comma-separated) of glyph names')
setMissing("attributeName","string",'ex: attributeName="bar"')
setMissing("contentStyleType","string", 'Identifies the default style sheet language: ex: contentStyleType = "text/css"')
setMissing("d", "path-data", "ToDO!!!!! path-data")
setMissing("path", "path-data", "ToDO!!!!! path-data")

setMissing("panose-1", "ws-list{10}", "The Panose-1 number, consisting of ten decimal integers, separated by whitespace" )
setMissing("kernelUnitLength" , "number-optional-number", "TODO !!!!!!!!!!!!!!")

setMissing("lang", "cmm-list", "comma-separated list of language names" )
setMissing("xlink:actuate", "choice","for <a> must be 'onRequest', for rest is 'onLoad'")

setMissing("media", "cmm-list", "comma-seperated list of media-descriptors")

setMissing("xlink:href", "iri", "iri")
setMissing("viewTarget", "string", 'viewTarget = "XML_Name [XML_NAME]*"')
setMissing("to","string", "in doc is <value>")
setMissing("baseProfile", "string", 'Describes the minimum SVG language profile that the author believes is necessary to correctly render the content: ex "none"')

setMissing("target", "choice", 'target = "_replace" | "_self" | "_parent" | "_top" | "_blank" | "<XML-Name>"')
# missingType.info.df[1, 9]<-"String"
# missingType.info.df[2,10]<-'xml:lang=""en-GB"'
# 
# missingType.info.df[3, 9]<-'String'
# missingType.info.df[3, 10]<-'id="string_wo_colon"'
# 
# missingType.info.df[4, 9]<-'list-of-strings'
# missingType.info.df[4, 10]<-'class="info attr-def"'
# 
# missingType.info.df[5,10]<-'style="fill: red; stroke: blue; stroke-width: 3"'
# missingType.info.df[5,9]<-'named-list'
# 



# for(i in 1:length(pages)){
#   
# #for(i in 15){
#    pages[i]->page
#   
#   ids<-AVEL.df[AVEL.df$page==page,]$loc
#   
#   id<-ids[1]
#   
#   url<-paste("http://www.w3.org/TR/SVG/",page, sep="") 
#   script<-getURL(url)
#   doc <- htmlParse(script)
#   
#   idDes<-paste("//*[@id='",id,"']",sep="")
#   
#   
#   
#   nodeSet<-getNodeSet(doc,idDes)
#   ntag<-xmlName(nodeSet[[1]])
#   cat("page=",page,"; ntag=",ntag, "; idDes=",idDes," \n")
#   #valLnk<-xx_valLnk(nodeSet)
#   if(length(nodeSet)>0){
#     ntag<-xmlName(nodeSet[[1]])
#     valLnk<-switch(ntag,
#                    "dt"= dt_valLnk(nodeSet),
#                    "a" = a_valLnk(nodeSet),
#                    "p" = p_valLnk(nodeSet),
#                    "h2"= dt_valLnk(nodeSet),
#                    c(NULL,NULL)
#     )  
#   }
#   cat("valLnk=",valLnk,"\n" ) 
# }
# 

