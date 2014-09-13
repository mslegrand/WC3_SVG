library(data.table)
library(XML)

fread("AVETable.csv")->ave.dt
ele.tags<-unique(ave.dt$element)

# svgPreprocHelper<-function(x,sep1, sep2){  
#   if(inherits(x,"list")){ #list
#     paste(  sapply(x, function(y){paste(y, collapse=sep1)}), collapse=sep2  )   
#   } else { #matrix
#     paste(apply(x, 2, function(y)paste(y,collapse=sep1)), collapse=sep2)     
#   }
# }

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
  "cmm-scln-list" = function(x){ 
    if(inherits(x,"list")){ #list
      paste(  sapply(x, function(y){paste(y, collapse=",")}), collapse=";"  )   
    } else { #matrix
      paste(apply(x, 2, function(y)paste(y,collapse=",")), collapse=";")     
    }                                
  } ,
  "number-optional-number" = function(x){paste(x, collapse=",")} ,
  "cln-scln-list"  = function(x){ 
    if(inherits(x,"list")){ #list
      paste(  sapply(x, function(y){paste(y, collapse=":")}), collapse=";"  )   
    } else { #matrix
      paste(apply(x, 2, function(y)paste(y,collapse=":")), collapse=";")     
    }
  } ,
  "cmm-wsp-list" = function(x){ 
    if(inherits(x,"list")){ #list
        paste(  sapply(x, function(y){paste(y, collapse=",")}), collapse=" "  )   
    } else { #matrix
      paste(apply(x, 2, function(y)paste(y,collapse=",")), collapse=" ")     
    }
  } ,
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
    attrs<-attrSplitX(attrs,"x","y","xy")
  }
  attrs 
}

#preprocXtras
#xy, cxy, rxy, xy1, xy2, wh
attrSplitX<-function(attrs,  a1, a2, a12){
  if(a12 %in% names(attrs)){
    attrs[c(a1,a2)]<-attrs[[a12]]
    attrs[[a12]]<-NULL
  }
  attrs
}


splitAtt<-function(etag, x){
  ifelse(
    nrow(ave.dt[element==etag & (attr==x['a1'] | attr==x['a2']) ,])==2,
    paste("attrs<-attrSplitX(attrs, '" ,x['a1'], "','" ,x['a2'], "','" ,x['a12'], "')", sep=""),
    ""
  )
}

centerable<-function(ele.tag, ave.dt){
  ifelse(
    nrow(ave.dt[  element==ele.tag & 
                  (attr=='x' | attr=='y' | attr=='width' | attr=='height') ,]
         )==4,
    "attrs<-mapCenteredXY(attrs)",
    ""
  )  
}


#"ignore cmm-list path-data-list wsp-list scln-list cmm-scln-list number-optional-number cln-scln-list cmm-wsp-list transform-list"

linesVal<-function(tva, V1){
  c(
    txt1<-gsub( 'pat1',  V1, "indx<-sapply(names(attrs),function(x)grepl(x, 'pat1' ))"),
    txt2<-gsub( 'pat2',  tva, "attrs[indx]<-lapply(attrs[indx], function(x){ 
                ifelse(inherits(x,c('list','matrix')), svgPreproc[['pat2']](x), x) })")
  )
} 

#ele.tag<-ele.tags[1]

createEleFn<-function(ele.tag, ave.dt){
  ave.dt[element==ele.tag & treatValueAs!="ignore",]->ele.dt
  ele.treatments<-unique(ele.dt$treatValueAs)
  ele.dt[, paste(attr, collapse=" "), by=treatValueAs]->treat_attrs.dt

  preprocXtras=list(
    c(a12="xy",a1='x',a2="y"),
    c(a12="cxy",a1='cx',a2="cy"),
    c(a12="rxy",a1='rx',a2="ry"),
    c(a12="xy1",a1='x1',a2="y1"),
    c(a12="xy2",a1='x2',a2="y2"),
    c(a12="wh",a1='width',a2="height")
  )
  
  
  lapply(preprocXtras,splitAtt, etag=ele.tag )->tmp22
  tmp22<-c(tmp22, centerable(ele.tag, ave.dt) )
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



#fn<-createEleFn(ele.tag, ave.dt)

svgFn<-lapply(ele.tags, createEleFn, ave.dt=ave.dt )
names(svgFn)<-ele.tags

svgFn<-c(svgFn,
         list(
           svgDoc=function(width=1150, height=860,  ... ){
             args<-unlist(c(list( width=width, height=height), list(...)))
             namespaceDefinitions<- list(
               "http://www.w3.org/2000/svg",
               xlink="http://www.w3.org/1999/xlink"
             )
             newXMLNode("svg", attrs=named(args), namespaceDefinitions = namespaceDefinitions, .children=unnamed(args)) 
           },
           getNode=function(rootNode,id){
             kidV<-getNodeSet(rootNode, paste( '//*[@id="',id,'"]' ) )
           }         
         )
         )

#we customize a little : Todo!!!  text , textPath , tspan
svgFn$text=function(...){ 
  args<-list(...) 
  attrs<-named(args)
  #abbreviations: will accept size or font-size, family or font-faminly, ...
  attr.names<-names(attrs)
  attr.names<-gsub("^(((style))|((weight))|((variant))|((size))|((family)))$", "font-\\1",attr.names, fixed=F)
  attr.names<-gsub("^anchor$","text-anchor",attr.names)
  names(attrs)<-attr.names
  if(!is.null(attrs[["cxy"]])){
    attrs[["text-anchor"]]<-'middle'
    attrs[["dominant-baseline"]]="central"
    attrs[["xy"]]=attrs[["cxy"]]
    attrs[["cxy"]]=NULL
  }
  attrs<-mapArg(attrs,"xy", c("x","y"))
  if( is.null(attrs[["text-anchor"]])){
    attrs<-c(attrs, list("text-anchor"='c'))
  }
  if( is.null(attrs[["font-size"]])){
    attrs<-c(attrs, list("font-size"="12px"))
  }
  if( is.null(attrs[["text-anchor"]])){
    attrs<-c(attrs, list("text-anchor"='c'))
  }
  if(!(attrs[["text-anchor"]] %in% c("start","end", "middle"))){
    attrs[["text-anchor"]]=switch(attrs[["text-anchor"]], "l"="start","r"="end","c"="middle","middle")
  } 
  text<-NULL
  if("text" %in% attr.names){ ### use value instead of text???
    text<-attrs["text"]
    attrs["text"]<-NULL
  }
  #isolate(print(text))
  node<-newXMLNode("text", attrs=attrs, .children=unnamed(args))
  if(!is.null(text)){
    xmlValue(node)<-text
  } 
  node  
}

#, colors=c("white","black"), offsets=c(0,100)
#todo!!! add stops for linearGradient,  ‘radialGradien
svgFn$linearGradient=function( ...){
  as.p<-function(x){ 
    if(!is.character(x))
      paste(x,"%",sep="")
  }
  args<-list(...)
  attrs<-named(args)
  attrs<-mapArg(attrs,"xy1", c("x1","y1"))
  attrs<-mapArg(attrs,"xy2", c("x2","y2"))

  if("colors" %in% names(attrs)){
    colors<-attrs[["colors"]]
    attrs[["colors"]]<-NULL
    if("offsets" %in% names(attrs)){
      offsets<-attrs[["offsets"]]
      attrs[["offsets"]]<-NULL
    } else {
      offsets<-seq(0,100,length.out=length(colors))
    }
    for(i in 1:length(colors)){
      attrs.si<-list(offset=sprintf("%d%%", offsets[i]), "stop-color"= colors[i])
      stopi<-newXMLNode("stop", attrs=attrs.si)
      args<-c(args,stopi)
    }
  }
  
  grad<-newXMLNode("linearGradient", attrs=attrs, .children=unnamed(args))
  grad    
} 



#todo 
#1. add default doc consuctor

#2. add find node with a given id
#3. alternative to with_svg?? at_svgNode[id](...)

svgDoc.new<-function(width=1150, height=860,  ... ){
  args<-unlist(c(list( width=width, height=height), list(...)))
  namespaceDefinitions<- list(
    "http://www.w3.org/2000/svg",
    xlink="http://www.w3.org/1999/xlink"
  )
  rootNode<-newXMLNode("svg", attrs=named(args), namespaceDefinitions = namespaceDefinitions, .children=unnamed(args)) 
  #todo: add options (such as duration)
  x<-0
  fn<-function(code=NULL){
    s<-substitute(code)
    if(length(s)==0){
      rootNode
    } else {
      x<-eval(s, svgFn, parent.frame() ) 
      x
    }
  }
  class(fn)<-c("svgDoc",fn)
  fn
}




"[[.svgDoc"<-function(doc,id=""){
  rootNode<-doc()
  if(id==''){
    fn<-function(...){
      s<-substitute(list(...))
      kids<-eval(s, list2env(svgFn, parent=parent.frame() ) )
      kids
    }
  } else {
    if(id=='root'){
      parent<-rootNode
    } else {
      parent<-getNodeSet(rootNode, paste( '//*[@id="',id,'"]' , sep=""))[[1]]
    }  
    fn<-function(...){
      s<-substitute(list(...))
      #print(ls(list2env(svgFn, parent=parent.frame())))
      kids<-eval(s, list2env(svgFn, parent.frame() ) )
      addChildren(parent, kids=kids)
      list(parent)
    }    
  }
  fn
}

# "[[.svgDoc"<-function(doc,id="root"){
#   rootNode<-doc()
#   #   parent<-ifelse(id=="root", rootNode, 
#   #                  getNodeSet(rootNode, paste( '//*[@id="',id,'"]') ) )
#   
#   if(id=='root'){
#     parent<-rootNode
#   } else {
#     parent<-getNodeSet(rootNode, paste( '//*[@id="',id,'"]') ) 
#   }
#   
#   fn<-function(code){
#     kid<-eval(substitute(code),svgFn, parent.frame() ) 
#     addChildren(parent, kids=list(kid))
#     #eval on w 
#     #add the list to parent
#     parent
#   }
#   fn
# }
# 
# 
# 
# 

# doc<-svgDoc.new()
# textBox<-function(doc, id, cxy, wh, txt){
#   cat("cxy=c(",cxy[1],",",cxy[2],")\n")
#   doc[["root"]]({
#     (cat("root: cxy=c(",cxy[1],",",cxy[2],")\n"))
#     g(id="my.group",
#       rect( cxy=cxy, wh=wh,  fill="lightblue", stroke="black"),
#       text( cxy=cxy ,text=txt)      
#     )
#   })
# }
# 
# tmp<-textBox(doc, id="my.box", cxy=c(50,50), wh=c(60,15), txt="hello")


with_svg<-function( code ){
  svg.env<-svgFn
  eval(substitute(code), svg.env, parent.frame() )
}

unique(ave.dt[anim==TRUE,]$attr)->ani.atts

# svgDoc.new()->doc
#doc[["root"]](rect(id="1", cxy=c(100,100), wh=c(100,100)))
# doc[["root"]](
#   rect(id="1", cxy=c(100,100), wh=c(100,100)),
#               polygon(id="poly.my", 
# #                       points=c(50,50,  0,100, 100,100)+10, 
#                       #points=list(c(50,50),  c(0,100), c(100,100)), 
#                       points=matrix(c(50,50,  0,100, 100,100)+10, 2,3),
#                       fill="lime",
#                       #stroke="blue",
#                       "stroke-width"=10
# 
# ))

# doc[["root"]](
#   linearGradient(id="my.grad",xy1=c("0%","0%"), xy2=c("0%","100%"), colors=c( "rgb(255,255,0)", "rgb(255,0,0)" ) )
# )
