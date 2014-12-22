# tend to use as.quoted, stealing from the way @hadley implements .() in the plyr package.
#library(data.table)
# library(plyr)
# dat <- data.table(x_one=1:10, x_two=1:10, y_one=1:10, y_two=1:10) 
# myfun <- function(name) {
#   one <- paste0(name, '_one')
#   two <- paste0(name, '_two')
#   out <- paste0(name,'_out')
#   as.quoted(paste('list(',out, '=',one, '-', two,')'))[[1]]
# }
#dat[, eval(myfun('x')),]

library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)
source("showMe.R")

fread("dataTableLink/elementSummary.tsv")->es.DT
#@# we grap all table of type class info from page
#   getPresentationLinkInfo<-function(page){
#page="painting.html"
extractOnePropPage<-function(page){
  url<-paste("http://www.w3.org/TR/SVG/",page,sep="")
  showMe(url)
  script<-getURL(url)
  doc <- htmlParse(script)
  
  getNodeSet(doc, paste("//table[@class='propinfo']",sep=""))->ns.prop.info
  tableExtractor<-function(node){
    attrs<-xmlAttrs(node)
    if("summary" %in% names(attrs)){
      cbind(attr=attrs["summary"],
            data.table(readHTMLTable(node, 
              header=c("variable", "value"))))
    } else {
      data.table()
    }    
  }
  propTables<-lapply(ns.prop.info, tableExtractor)
  propTables<-rbindlist(propTables)
}

extractAllProps<-function(){
  pages<-c("text.html",  "masking.html",  "painting.html", "color.html",
           "interact.html", "filters.html", "pservers.html")
  
  propTables<-lapply(pages, extractOnePropPage)
  rbindlist(propTables)
 
}

extractAllProps()->presAttr.DT

cleanPropsTable<-function(prop.dt){
  
  prop.dt[,attr:=gsub(" property","",attr) ]
  prop.dt[,variable:=sapply(str_split(variable,":"), function(x)x[[1]])] 
  prop.dt[,value:=gsub("[‘’]","",value) ]
  prop.dt[,value:=gsub("\\s+"," ",value) ]
  
  
  prop.dt[, variable:=str_trim(variable)]  
  prop.dt[, attr:=str_trim(attr)]  
  prop.dt[, value:=str_trim(value)]  
  
  prop.dt
}
 
cleanPropsTable(presAttr.DT)

#write.table(prop.dt,"dataTableLink/presentationAttr.tsv",sep="\t",row.names=FALSE,quote=FALSE)

#presAttr.DT<-prop.dt

# postProcessAppliesTo<-function( presAttr.DT, es.DT ){
#   #process applies To field
#    appliesToDict<- list(
#      "text elements"=c("text"),                                                                                                                                 
#      "text content elements"= c("Text Content Element"),                                                                                                                         
#      "tspan, tref, altGlyph, textPath element"  =c("tspan", "tref", "altGlyph", "textPath"),                                                                                                    
#      "elements which establish a new viewport, pattern elements and marker elements" = c("pattern", "marker",  "svg", "symbol", "image",  'foreignObject'),
#      "container elements, graphics elements and clipPath" =c( "Container Element", "Graphics Element",  "clipPath"),                                                                                           
#      "graphics elements within a clipPath element" =c('Graphics Element' ),                                                                                                 
#      "container elements and graphics elements"=c("Container Element:",   "Graphics Element"),                                                                                                    
#      "container elements (except mask) and graphics elements"  = c("a", "svg", "g", "defs", "symbol", "switch", "glyph", "missing-glyph", "pattern", "marker",  "graphics element:"),
#      "shapes and text content elements"=c( "Shape Element", " Text Content Element"),                                                                                                             
#      "svg, g, switch, a, foreignObject, graphics elements (including the text element) and text sub-elements (i.e., tspan, tref, altGlyph, textPath)"= c("svg", "g", "switch", "a", "foreignObject", "Graphics Element",  "text",  "tspan","tref", "altGlyph", "textPath"),
#      "graphics elements (including the text element) and text sub-elements (i.e., tspan, tref, altGlyph, textPath and a)" =   c("Graphics Element",  "text", "tspan", "tref", "altGlyph", "textPath",  "a"),
#      "path, line, polyline and polygon elements" =c("path", "line", "polyline" , "polygon" ),
#      "container elements, graphics elements, animate and animateColor"  = c("Container Element", "Graphics Element", "animate",  "animateColor"), 
#      "filter primitives" =c("Filter Primitive Element"),                                                                                                                             
#      "shapes"  = c("Shape Element"),                                                                                                                                        
#      "images" =c("image"),                                                                                                                                       
#      "elements to which properties fill, stroke, stop-color, flood-color and lighting-color apply" = c( "stop", "feFlood", 'feDiffuseLighting' , 'feSpecularLighting', 'Shape Element',  "Text Content Element"),
#      "image elements that refer to raster images" = c('image'),                                                                                                    
#      "graphics elements"  =c("Graphics Element" ),                                                                                                                           
#      "container elements"   =c("Container Element" ) ,                                                                                                                        
#      "feDiffuseLighting and feSpecularLighting elements" =c("feDiffuseLighting", "feSpecularLighting"  ),                                                                                            
#      "feFlood elements"   =c("feFlood"),                                                                                                                           
#      "stop elements"    =c("stop")
#    )
#    
# 
#    tmp.DT<-es.DT[variable=="category"]
#    
#    cats<-unique(tmp.DT$value)
#    ca.list<-sapply(cats,function(category) tmp.DT[value==category]$element)
#    
#    fn2<-function(x){
#      rtv<-unlist(c( ca.list[x], setdiff(x,cats)), use.names=FALSE) 
#      if(is.null(rtv)){
#        browser()
#      }
#      rtv
#    }
#    
# 
#    fn<-function(arg.DT){
#      #indx<-arg.DT$value
#      vals<-arg.DT$value
#      #vals<-appliesToDict[[indx]]
#       vals<-fn2(vals)
#       tmp.df<-data.frame(arg.DT)
#       tmp.df[["value"]]=NULL
#       data.table(tmp.df, value=vals)
#    }
#    tmp1<-presAttr.DT[variable=="Applies to", fn(.SD), by="attr"]
#    tmp2<-presAttr.DT[variable!="Applies to"]
#    rbind(tmp1,tmp2)    
# }
# #end of postProcessAppliesTo
# 

postProcessAppliesTo2<-function( presAttr.DT, es.DT ){
  appliesToDict2<- list(
    "text elements"=c("text"),      #????                                                                                                                           
    "text content elements"= c("text content elements"),   
    "tspan, tref, altGlyph, textPath elements" = 
      c("tspan", "tref", "altGlyph", "textPath"),                                                                                                    
    "elements which establish a new viewport, pattern elements and marker elements" = 
      c("pattern", "marker",  "svg", "symbol", "image",  'foreignObject'),
    "container elements, graphics elements and clipPath" =
      c( "container elements", "graphics elements",  "clipPath"),                                                                                           
    "graphics elements within a clipPath element" =c('graphics elements' ),                                                                                                 
    "container elements and graphics elements"=
      c("container elements",   "graphics elements"),                                                                                                    
    "container elements (except mask) and graphics elements"  = 
      c("a", "svg", "g", "defs", "symbol", "switch", "glyph", "missing-glyph", "pattern", "marker",  "graphics elements"),
    "shapes and text content elements"=c( "shape elements", "text content elements"),                                                                                                             
    "svg, g, switch, a, foreignObject, graphics elements (including the text element) and text sub-elements (i.e., tspan, tref, altGlyph, textPath)"= 
      c("svg", "g", "switch", "a", "foreignObject", "graphics elements",  "text",  "tspan","tref", "altGlyph", "textPath"),
    "graphics elements (including the text element) and text sub-elements (i.e., tspan, tref, altGlyph, textPath and a)" =   
      c("graphics elements",  "text", "tspan", "tref", "altGlyph", "textPath",  "a"),
    "path, line, polyline and polygon elements" =
      c("path", "line", "polyline" , "polygon" ),
    "container elements, graphics elements, animate and animateColor"  = 
      c("container elements", "graphics elements", "animate",  "animateColor"),  
    "filter primitives" =c("filter primitive elements"),                                                                                                                             
    "shapes"  = c("shape elements"),                                                                                                                                        
    "images" =c("image"),   #???                                                                                                                                    
    "elements to which properties fill, stroke, stop-color, flood-color and lighting-color apply" = 
      c( "stop", "feFlood", 'feDiffuseLighting' , 'feSpecularLighting', 'shape elements', "text content element"),
    "image elements that refer to raster images" = c('image elements'),                                                                                                    
    "graphics elements"  =c("graphics elements" ),                                                                                                                           
    "container elements"   =c("container elements" ) ,                                                                                                                        
    "feDiffuseLighting and feSpecularLighting elements" =c("feDiffuseLighting", "feSpecularLighting"  ),                                                                                            
    "feFlood elements"   =c("feFlood"),                                                                                                                           
    "stop elements"    =c("stop")
  )
  
  
  # furthermore:
  
  expand.dict<-list(
    'shapes elements'=
      c('path', 'rect', 'circle', 'ellipse', 'line', 'polyline', 'polygon'),
    'text content elements'=
      c('altGlyph', 'textPath', 'text', 'tref' , 'tspan'),
    'filter primitive elements'=
      c('feBlend', 'feColorMatrix', 'feComponentTransfer', 'feComposite', 
        'feConvolveMatrix', 'feDiffuseLighting', 'feDisplacementMap', 
        'feFlood', 'feGaussianBlur', 'feImage', 'feMerge', 'feMorphology', 
        'feOffset', 'feSpecularLighting', 'feTile', 'feTurbulence'),
    'container elements'=
      c( 'a', 'defs', 'glyph', 'g', 'marker', 'mask', 'missing-glyph', 
        'pattern', 'svg', 'switch', 'symbol'),
    "graphics elements"=
      c('circle', 'ellipse', 'image', 'line', 'path', 
        'polygon', 'polyline', 'rect', 'text', 'use')
  )

  expand.vect<-function(one.vect, dict){
    indx<-match(one.vect,names(dict),0L)
    c(
      unlist(dict[indx[indx>0]]),
      one.vect[indx==0]
    )
  }
    
  expanded.dict<-lapply(appliesToDict2, expand.vect, dict=expand.dict)
  
  
  tmp.DT<-es.DT[variable=="category"]
  cats<-unique(tmp.DT$value)
  ca.list<-sapply(cats,function(category) tmp.DT[value==category]$element)
    
  expand.table<-function(arg.DT){ #arg.DT is the data.table of all applies to
    #indx<-arg.DT$value
    vals<-arg.DT$value #the values we need to substitute
    vals<-expand.vect(vals, expanded.dict)
    tmp.df<-data.frame(arg.DT)
    tmp.df[["value"]]=NULL
    data.table(tmp.df, value=vals)
  }
  
  tmp1<-presAttr.DT[variable=="Applies to", expand.table(.SD), by="attr"]
  
  tmp2<-presAttr.DT[variable!="Applies to"]
  rbind(tmp1,tmp2)      
}
#end of  postProcessAppliesTo2


#print(es.DT)
postProcessAppliesTo2(presAttr.DT, es.DT)->presAttr2.DT

postProcessValues<-function(presAttr.DT){
  presAttr.DT[variable=="Values", variable:="Value"]
  fn<-function(arg.DT){
    value<-arg.DT$value
   # cat("value=",value,"\n")
    if(!grepl("\\[", value) ){
      value<-str_split(value,"\\|")
    }
    tmp.df<-data.frame(arg.DT)
    tmp.df[["value"]]=NULL   
    data.table(tmp.df,value=value)
  }
  fn1<-function(arg.DT){
    length(arg.DT$value)
  }
  
  tmp1<-presAttr.DT[variable=="Value", fn(.SD), by=attr]
  tmp2<-presAttr.DT[variable!="Value"]
  rbind(tmp1,tmp2)
}

postProcessValues(presAttr2.DT)->presAttr3.DT

write.table(presAttr3.DT,
            "dataTableLink/presentationAttr.tsv",sep="\t",
            row.names=FALSE,quote=FALSE)

# tmp.DT[variable=="Value"]$value
# unique(tmp.DT[variable=="Value"]$value)->tmp.values
# indx<-grep("\\[",tmp.values)

