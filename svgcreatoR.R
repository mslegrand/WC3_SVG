library(data.table)
library(XML)

fread("AVETable.csv")->ave.dt
ele.tags<-unique(ave.dt$element)

# matrix=
#   lable="matrix"
#   paste(label, "(", 
#                paste(x,collapse=","), 
#               ") ", sep="")
# translate =
# scale
# rotate
# skewX
# skewY
# 


svgPreproc<-list(
  "cmm-list" = function(x){paste(x, collapse=",")} ,
  "path-data-list" = function(x){ #at this point we do no length check                       
                          names<-names(x)
                          if("z" %in% names){ x[["z"]]<-""}
                          if("Z" %in% names){ x[["Z"]]<-""}
                          tmp<-lapply(names, function(ns){
                                paste( ns, paste(x[[ns]], collapse=","), sep="" )
                                 })
                          tmp<-paste(tmp, collapse=" ")
                          tmp
              } ,  
  "wsp-list" = function(x){paste(x, collapse=" ")} ,
  "scln-list" = function(x){paste(x, collapse=";")} ,
  "cmm-scln-list" = function(x){ paste(
                                    sapply(x, function(y){paste(y, collapse=",")}),
                                    collapse=";"
                                     )} ,
  "number-optional-number" = function(x){paste(x, collapse=",")} ,
  "cln-scln-list"  = function(x){ paste(
                                    sapply(x, function(y){paste(y, collapse=":")}),
                                    collapse=";"
                                  )} ,
  "cmm-wsp-list" = function(x){ paste(
                    sapply(x, function(y){paste(y, collapse=",")}),
                    collapse=" "
                  )} ,
  "transform-list" = function(x){ #at this point we do no length check
            names<-names(x)
            tmp<-lapply(names, function(ns){
              paste( ns, "(", paste(x[[ns]], collapse=","), ")", sep="" )
            })
            tmp<-paste(tmp, collapse=" ")
            tmp
  } 
)


#Helper functions
named <- function(x) {
  if (is.null(names(x))) return(NULL)
  x<-x[names(x) != ""]
  names(x)<-gsub(".\\.","-", names(x)) # dot to dashes 
  x
}

unnamed <- function(x) {
  if (is.null(names(x))) return(x)
  x[names(x) == ""]
}

mapArg<-function(attrs, seqArg, toArgs){
  if(!is.null(attrs[[seqArg]])){
    for(i in 1:length(toArgs)){
      attrs[[toArgs[i]]]<-attrs[[seqArg]][i]
    }
    attrs[[seqArg]]<-NULL
  }
  attrs    
}

mapCenteredXY<-function(attrs){
  if( !is.null(attrs[["cxy"]]) & !is.null(attrs[["width"]]) & !is.null(attrs[["height"]]) ){
    wh<-c(as.numeric(attrs[["width"]]), as.numeric(attrs[["height"]]))
    attrs[["xy"]]<- as.numeric(attrs[["cxy"]]) - wh/2
    attrs[["cxy"]]<-NULL
  }
  attrs 
}

#preprocXtras
#xy, cxy, rxy, xy1, xy2, wh
attrSplitX<-function(attrs, a12, a1, a2){
  if(a12 %in% names(attrs)){
    attrs[c(a1,a2)]<-attrs[[a12]]
    attrs[[a12]]<-NULL
  }
  attrs
}


splitAtt<-function(etag, x){
  ifelse(
    nrow(ave.dt[element==ele.tag & (attr==x['a1'] | attr==x['a2']) ,])==2,
    paste("attrSplitX(attrs, '" ,x['a1'], "','" ,x['a2'], "','" ,x['a12'], "')", sep=""),
    ""
  )
}

centerable<-function(){
  ifelse(
    nrow(ave.dt[  element==ele.tag & 
                  (attr=='x' | attr=='y' | attr=='width' | attr=='height') ,]
         )==5,
    "attrs<-mapCenteredXY(attrs)",
    ""
  )  
}

preprocXtras=list(
  c(a12="xy",a1='x',a2="y"),
  c(a12="cxy",a1='cx',a2="cy"),
  c(a12="rxy",a1='rx',a2="ry"),
  c(a12="xy1",a1='x1',a2="y1"),
  c(a12="xy2",a1='x2',a2="y2"),
  c(a12="wh",a1='width',a2="height")
)





#"ignore cmm-list path-data-list wsp-list scln-list cmm-scln-list number-optional-number cln-scln-list cmm-wsp-list transform-list"

linesVal<-function(tva, V1){
  c(
    txt1<-gsub( 'pat1',  V1, "indx<-sapply(names(attrs),function(x)grepl(x, 'pat1' ))"),
    txt2<-gsub( 'pat2',  tva, "attrs[indx]<-lapply(attrs[indx], function(x){ ifelse(inherits(x,'list'), svgPreproc['pat2'](x), x) })")
  )
} 

ele.tag<-ele.tags[1]

createEleFn<-function(ele.tag, ave.dt){
  ave.dt[element==ele.tag & treatValueAs!="ignore",]->ele.dt
  ele.treatments<-unique(ele.dt$treatValueAs)
  ele.dt[, paste(attr, collapse=" "), by=treatValueAs]->treat_attrs.dt
  
  lapply(preprocXtras,splitAtt, etag=ele.tag )->tmp22
  tmp22<-c(tmp22, centerable())
  tmp22[tmp22==""]<-NULL
  body1<-unlist(tmp22)
  split(treat_attrs.dt, rownames(treat_attrs.dt))->tmp
  lapply(tmp, function(x){linesVal(x$treatValueAs, x$V1)})->body2
  unlist(body2, use.names=F)->body3
  as.list(body3)->body4
  
  body.list<-c(
    "args<- list(...)",
    "attrs=named(args)",
    body1,
    body3,
    paste("node<-newXMLNode('", ele.tag, "', attrs=attrs, .children=unnamed(args))",sep="") 
  )
  
  ex<-parse(text=paste(body.list,collapse=";"))
  fn<-function(...){}
  body(fn)<-as.call(c(as.name("{"),ex))
  fn  
}

fn<-createEleFn(ele.tag, ave.dt)

svgFn<-lapply(ele.tags, createEleFn, ave.dt=ave.dt )
names(svgFn)<-ele.tags
# body<-c()
# for(i in 1:nrow(treat_attrs.dt)){
#   xx<-linesVal( treat_attrs.dt[i,treatValueAs], treat_attrs.dt[i,V1] )
#   body<-c(body,xx)
# }


# tmp.d<-list(M=c(100,100), L=c(300,100), L=c(200,300), z="xx")
# tmp.r<-svgPreproc[["path-data-list"]](tmp.d)

#nrow(ave.dt[element==ele.tag & (attr=="x" | attr=="y") ,])==2
