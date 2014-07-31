cleanElements<-function(x){
  gsub("[‘’,]","",x)
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
      link<-getHTMLLinks(kids[[2]])
      strsplit(link,"#")->eleLinkInfo
      matrix(unlist(eleLinkInfo),2,)->eleLinkInfo.m
      eleLinkInfo.m[1,]->page
      eleLinkInfo.m[2,]->loc
      df<-data.frame(attr=attr, element=elements, anim=anim,
                link=link, page=page, loc=loc,
                stringsAsFactors=F
      )
    }
    return(df)
  } 
  
  lapply(ns.tr, extractAttrRow)->rows 
  #remove empty row
  #rbind
}

getAEAL()->res
df<-do.call(rbind, res)


df<-do.call(rbind, res)
