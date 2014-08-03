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
      attr<-kids.strings[1]
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
valType<-(AVEL2.df$lnk)
#cat(class(valType),"\n")
strsplit(valType,"#")->valType
lapply(valType, function(x){ifelse(length(x)>1,gsub("DataType","",x[[2]]), NA)})->valType
valType<-unlist(valType)
AVEL2.df$valType<-valType
grep("\\|",AVEL2.df$val)->choiceIndx
AVEL2.df[choiceIndx,"valType"]<-"Choice"


which(is.na(AVEL2.df$valType))->NA.indx
unique(AVEL2.df$val[NA.indx])->missingTypes
table(AVEL2.df$val[NA.indx])->missingTypeTable
sort(missingTypeTable, decreasing = T)->missingTypeTable
missingType.df<-data.frame(val=names(missingTypeTable), valType=NA)
missingType.List<-lapply(names(missingTypeTable), function(x){intersect(NA.indx, which(x==AVEL2.df$val)) })
names(missingType.List)<-names(missingTypeTable)
missingType.info.df<-lapply(missingType.List, function(x){AVEL2.df[x[[1]],]} )
missingType.info.df<-do.call(rbind, missingType.info.df)
rownames(missingType.info.df)<-NULL
missingType.info.df$Example<-NA


missingType.info.df[1, 9]<-"Choice"
missingType.info.df[1, 10]<-'xml:space = "default"'

missingType.info.df[2, 9]<-"String"
missingType.info.df[2,10]<-'xml:lang=""en-GB"'
missingType.info.df[3, 9]<-'String'
missingType.info.df[3, 10]<-'id="string_wo_colon"'
missingType.info.df[4, 9]<-'list-of-strings'
missingType.info.df[4, 10]<-'class="info attr-def"'
missingType.info.df[5,10]<-'style="fill: red; stroke: blue; stroke-width: 3"'
missingType.info.df[5,9]<-'named-list'




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
