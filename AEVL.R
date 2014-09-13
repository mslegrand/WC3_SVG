library(XML)
library(RCurl)
library(data.table)

AttributeElementValueTable<-function(){
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
      dt<-data.table()
      #df<-data.frame()
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
        dt<-data.table(attr=attr, element=elements, anim=anim,
                       link=link, page=page, loc=loc
        )
      }
      return(dt)
    } 
    
    lapply(ns.tr, extractAttrRow)->rows 
    rows
  }
  
  getAEAL()->res
  AVEL.dt<-rbindlist(res)
  
  
  dt_valLnk<-function(ns){
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
      return(c(val=val, link=link))    
    }
    txt<-xmlValue(ns[[1]])
    val<-gsub("^.+=", "", txt)
    link<-getHTMLLinks(ns[[1]])[1]
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
  
  
  #' returns data.table
  doValLink<-function(doc,id){
    idDes<-paste("//*[@id='",id,"']",sep="")
    nodeSet<-getNodeSet(doc,idDes)
    valLnk.dt<-data.table()
    if(length(nodeSet)>0){
      ntag<-xmlName(nodeSet[[1]])  
      #showMe(ntag)
      valLnk<-switch(ntag,
                     "dt"= dt_valLnk(nodeSet),
                     "a" = a_valLnk(nodeSet),
                     "p" = p_valLnk(nodeSet),
                     "h2"= dt_valLnk(nodeSet),
                     c(NA,NA)
      )
      if(length(valLnk)>2){ #debug code
        cat("length(valLnk)=",length(valLnk)," tnag=",ntag," id=",id,"\n")
      }
      val<-valLnk[1]
      tmpval<-strsplit(val, "=")[[1]]
      val<-tmpval[length(tmpval)]
      val<-cleanValues(val)
      valLnk.dt<-data.table(val=val, lnk=valLnk[2])
      
    }
    valLnk.dt
  }
  
  
  #' returns data.table
  do.Page<-function(avel, onePage){
    page.dt<-avel[page==onePage,]
    ids<-page.dt$loc
    url<-paste("http://www.w3.org/TR/SVG/",onePage, sep="") 
    script<-getURL(url)
    doc <- htmlParse(script) 
    #instead of using ids, we should go over each row in pages.dt
    res1<-lapply(ids, function(id){ doValLink(doc,id) } )
    res2<-rbindlist(res1)
    page.dt<-cbind(page.dt,res2)
  }
  
  #' returns data.table
  do.all.Pages<-function(avel){
    pages<-unique(avel$page)
    res11<-lapply(pages, function(page){ dt<-do.Page(avel, page)})
    res22<-rbindlist(res11)
    res22
  }
  
  
  
  AVEL2.dt<-do.all.Pages(AVEL.dt)
  
  treatValueAs<-(AVEL2.dt$lnk)
  strsplit(treatValueAs,"#")->treatValueAs
  lapply(treatValueAs, function(x){ifelse(length(x)>1,tolower( gsub("DataType","",x[[2]]) ), NA)})->treatValueAs
  treatValueAs<-unlist(treatValueAs)
  treatValueAs<-gsub("numberoptionalnumber","number-optional-number",treatValueAs)
  AVEL2.dt$treatValueAs<-treatValueAs
  grep("\\|",AVEL2.dt$val)->choiceIndx
  AVEL2.dt[choiceIndx,"treatValueAs"]<-"choice"
  
  
  getMissingInfo<-function(AVEL2.dt){
    
    missing.dt<-subset(AVEL2.dt,is.na(treatValueAs))
    setkey(missing.dt, val)
    missing.dt<-subset(unique(missing.dt))
    missing.dt[,example:=NA] #Note: the original version removed page and loc  
    
    #setMissing('xml:lang', "string", 'xml:lang=""en-GB"')
    missing.dt[attr=='xml:lang', ':='(treatValueAs="string", example='xml:lang="en-GB"')]
    missing.dt[attr=='id', ':='(treatValueAs="id", example='id="string_wo_colon"')]
    missing.dt[attr=='class', ':='(treatValueAs='wsp-list', example='(just called "list" in documentationclass="info attr-def"')]
    missing.dt[attr=='style', ':='(treatValueAs='cln-scln-list', example='(named list?)] style="fill: red; stroke: blue; stroke-width: 3"')]
    #
    missing.dt[attr=='requiredExtensions', ':='(treatValueAs='wsp-list', example='list of IRI references: http://example.com/requiredExtension1.svg http://example.com/requiredExtension2.svg')]
    missing.dt[attr=='requiredFeatures', ':='(treatValueAs='wsp-list', example='list of feature strings: http://www.w3.org/TR/SVG11/feature#CoreAttribute')]
    missing.dt[attr=="systemLanguage", ':='(treatValueAs='cmm-list', example='comma-separated list of language names:systemLanguage="mi, en"')]
    missing.dt[attr=="xlink:arcrole", ':='(treatValueAs='string', example=' http://www.example.org/D<c3><bc>rst')]
    #
    missing.dt[attr=="xlink:role", ':='(treatValueAs='string', example=' http://www.example.org/D<c3><bc>rst')]
    missing.dt[attr=="xlink:title", ':='(treatValueAs='string', example=' http://www.example.org/D<c3><bc>rst')]
    missing.dt[attr=="xlink:type", ':='(treatValueAs='string', example=' http://www.example.org/D<c3><bc>rst')]
    missing.dt[attr=="xlink:actuate", ':='(treatValueAs='string', example='xlink:actuate = "onLoad"')]
    
    missing.dt[attr=="transform", ':='(treatValueAs="transform-list", example="TODO!!!!!!!!")]
    missing.dt[attr=="result", ':='(treatValueAs="string", example='<feGaussianBlur in="SourceAlpha" stdDeviation="4" result="blur" />')]
    missing.dt[attr=="horiz-adv-x", ':='(treatValueAs="number", example="??")]
    missing.dt[attr=="keySplines", ':='(treatValueAs="cmm-scln-list", example='keySplines="0,0.5,0.5,1; 0.5,0,1,0.5; 0,0.5,0.5,1; 0,0.5,0.5,1" ')]
    #‘keyPoints’ takes a semicolon-separated list of floating point values between 0 and 1
    missing.dt[attr=="keyPoints", ':='(treatValueAs="scln-list", example='in doc: <list-of-numbers>, a semicolon-separated list of floating point values between 0 and 1 : keyPoints="0; 0.5; 1"')]
    # appears that space-semicolon seperated list will also work
    # calcMode="spline" keySplines="0 0 1 1; 0 0 1 1" 
    missing.dt[attr=="keyTimes", ':='(treatValueAs="cmm-scln-list", example='keySplines="0,0.5,0.5,1; 0.5,0,1,0.5; 0,0.5,0.5,1; 0,0.5,0.5,1" ')]
    missing.dt[attr=="bbox", 
               ':='(treatValueAs="cmm-list {4}", example=
                      'comma-separated list of exactly four numbers specifying, in order, the lower left x, lower left y, upper right x, and upper right y of the bounding box for the complete font'
               )]
    missing.dt[attr=="viewBox", ':='(treatValueAs="cmm-list {4}", example='(<min-x>, <min-y>, <width> and <height>)]: viewBox="0 0 1500 1000"' )]
    missing.dt[attr=="begin", ':='(treatValueAs="scln-list", example='beginValueList')]
    
    missing.dt[attr=='end', ':='(treatValueAs="scln-list", example="'end-value-list'")]
    missing.dt[attr=='preserveAspectRatio', ':='(treatValueAs='special-string', example='preserveAspectRatio="[defer] <align> [<meetOrSlice>]"')]
    missing.dt[attr=='g1', ':='(treatValueAs="cmm-list", example='equence (comma-separated)] of glyph names')]
    missing.dt[attr=="attributeName", ':='(treatValueAs="string", example='ex: attributeName="bar"')]
    missing.dt[attr=="contentStyleType", ':='(treatValueAs="string", example='Identifies the default style sheet language: ex: contentStyleType = "text/css"')]
    missing.dt[attr=="d", ':='(treatValueAs="path-data-list", example="ToDO!!!!! path-data")]
    missing.dt[attr=="path", ':='(treatValueAs="path-data-list", example="ToDO!!!!! path-data")]
    
    missing.dt[attr=="panose-1", ':='(treatValueAs="wsp-list {10}", example="The Panose-1 number, consisting of ten decimal integers, separated by whitespace" )]
    missing.dt[attr=="kernelUnitLength" , ':='(treatValueAs="number-optional-number", example="TODO !!!!!!!!!!!!!!")]
    missing.dt[attr=="lang", ':='(treatValueAs="cmm-list", example="comma-separated list of language names" )]
    missing.dt[attr=="xlink:actuate", ':='(treatValueAs="choice", example="for <a> must be 'onRequest', for rest is 'onLoad'")]
    
    missing.dt[attr=="media", ':='(treatValueAs="cmm-list", example="comma-seperated list of media-descriptors")]
    missing.dt[attr=="xlink:href", ':='(treatValueAs="iri", example="iri")]
    missing.dt[attr=="viewTarget", ':='(treatValueAs="string", example='viewTarget = "XML_Name [XML_NAME]*"')]
    missing.dt[attr=="to",':='(treatValueAs="string", example="in doc is <value>")]
    #missing.dt[attr=="to",':='(treatValueAs="cmm-list", example="animate")]
    #missing.dt[attr=="from",':='(treatValueAs="cmm-list", example="animate")]
    missing.dt[attr=="baseProfile", ':='(treatValueAs="string", example='Describes the minimum SVG language profile that the author believes is necessary to correctly render the content: ex "none"')]
    
    missing.dt[attr=="target", ':='(treatValueAs="choice", example='target = "_replace" | "_self" | "_parent" | "_top" | "_blank" | "<XML-Name>"')]
    missing.dt[attr=='kernelMatrix', ':='(treatValueAs='wsp-list', example='the list of <number>s that make up the kernel matrix for the convolution. Values are separated by space characters and/or a comma. The number of entries in the list must equal <orderX> times <orderY>.'
    )] # dim specified by order, hmmm! maybe we should overload the order!!
    missing.dt[attr=="origin",  ':='(treatValueAs="default", example="literally the string default, has no effect in SVG!!!")]
    missing.dt[attr=="type", ':='(treatValueAs="string", example='defaluts to "text/css"')]
    missing.dt[attr=="title", ':='(treatValueAs="string", example='example: <A href="http://someplace.com/neatstuff.gif" title="Me scuba diving" me scuba diving last summer </A> ')] 
    #may provide tooltip
    missing.dt[attr=="name",  ':='(treatValueAs="string", example="color profile: complicated???")]
    missing.dt[attr=='y', ':='(treatValueAs='string', example='a single value')]
    missing.dt[attr=='by', ':='(treatValueAs='value', example='a single value')]
    missing.dt[attr=='unicode-range', ':='(treatValueAs='cmm-list', example=
                                             'list of comma seperated unicodes, unicode-range: U+26               /* single_codepoint */
             unicode-range: U+0025-00FF        /* codepoint_range */
             unicode-range: U+4??              /* wildcard_range */
             unicode-range: U+0025-00FF, U+4??
             ')]
    missing.dt
  }
  
  missing.dt<-getMissingInfo(AVEL2.dt)
  
  which(is.na(AVEL2.dt$treatValueAs))->NA.indx
  
  for(n in 1:nrow(missing.dt)){
    mrow<-missing.dt[n,]
    val<-mrow$val
    which(AVEL2.dt$val==val)->val.indx
    r.indx<-intersect(val.indx, NA.indx)
    treatAs<-mrow$treatValueAs
    AVEL2.dt[r.indx, treatValueAs:=treatAs]  
  }
  
  #final processing
  adjList<-list(
  "ignore"= c( "coordinate", "string", "number", "choice", "anything", "string", "iri", 
                  "length", "integer", "default", "coordinate", "id", "sourcegraphic" , 
                  "termoutermostsvgelement", "filterprimitiveinattribute",  "funciri" ),
  "wsp-list "=c("lengths",  "numbers", "coordinates", "special-string" ),
  "cmm-wsp-list"="pointsbnf",
  "cmm-list"="value"
  )
  
  for(n in names(adjList)){
    for(i in adjList[[n]]){    
      AVEL2.dt[treatValueAs==i, treatValueAs:=n]
    }
  }
  
  
  AVEL2.dt[treatValueAs=="transformlist", treatValueAs:="transform-list"]
  AVEL2.dt[treatValueAs=="cmm-list {4}", treatValueAs:="cmm-list"]
  AVEL2.dt[treatValueAs=="wsp-list {10}", treatValueAs:="wsp-list"]
  AVEL2.dt[treatValueAs=="wsp-list ", treatValueAs:="wsp-list"]
  
  
  
  AVEL2.dt
}


AVETable<-AttributeElementValueTable()

write.csv(AVETable, "AVETable.csv")
#write.tableAVETable, "AVETable.table")
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

