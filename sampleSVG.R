library(stringr)
library(XML)
library(RCurl)
library(data.table)
library(dplyr)


#nice helper for debugging
showMe<-function(...){
  tmp2<-substitute(list(...))
  tmp3<-sapply(tmp2,deparse)[-1]
  tmp1<-unlist(list(...))
  txt.list<-lapply(1:length(tmp1), function(i){paste(tmp3[i],"=",tmp1[i])} )
  txt<-paste(txt.list,collapse="; ")
  cat(txt,"\n")
  invisible(txt)
}

# helpes to get svg file urls
getSVGWeb_file.urls<-function(){
  example.url<-"http://dev.w3.org/SVG/tools/svgweb/samples/svg-files/"
  #http://dev.w3.org/SVG/tools/svgweb/samples/svg-files/410.svg
  readHTMLTable(example.url)[[1]]->tmp
  file.names<-tmp$Name
  file.names<-as.character(tmp$Name)
  indx<-grep("svg$",file.names)
  file.names<-file.names[indx]
  #example.url<-"http://dev.w3.org/SVG/tools/svgweb/samples/svg-files/"
  build.url<-function(file.name){paste("http://dev.w3.org/SVG/tools/svgweb/samples/svg-files/",file.name,sep="")}
  file.urls<-sapply(file.names, build.url, USE.NAMES=F)
}

getSVGJenkins_file.urls<-function(){
  indx.url<-"http://tutorials.jenkov.com/svg/examples/"
  df<-readHTMLTable(indx.url)[[1]]
  file.names<-as.character(df[[1]])
  do.call(rbind, str_split(file.names, "\\."))[,1]->file.name.stems
  build.url<-function(file.name){paste("http://tutorials.jenkov.com/svg/examples/",file.name, ".jsp",sep="")}
  file.urls<-sapply(file.name.stems, build.url, USE.NAMES=F)
}

getSVGCodingParidise_files.urls<-function(){
  indx.url="http://codinginparadise.org/projects/svgweb/samples/svg-files/"
  script<-getURL(indx.url)
  doc <- htmlParse(script)
  getHTMLLinks(doc)->links
  build.url<-function(file.name){paste(indx.url,file.name,sep="")}
  file.urls<-sapply(links, build.url, USE.NAMES=F)
}

#should I download all the files?
getCharCounts<-function(str1){
  comm<-str_count(str1, ',')
  semi<-str_count(str1, ';')
  colo<-str_count(str1, ':')
  spac<-str_count(str1, ' ') 
  c1<-c(comm=comm, semi=semi, colo=colo, spac=spac)
  c2<-c1>0
  codex<-c(codex=prod(c(2,3,5,7)^c2))
  c(c1,codex) 
}

do.OneNode<-function(node, pos){
  df<-data.frame()
   tag<-xmlName(node)
   attributes<-xmlAttrs(node)
   if(length(attributes)>0){
     tmp<-structure(attributes, names=NULL)   
     counts<-sapply(attributes,getCharCounts)
     df<-data.table(tag=tag, attrs=names(attributes), 
                    comm=counts[1,], semi=counts[2,],
                    colo=counts[3,], spac=counts[4,],
                    codex=counts[5,], pos=pos)
   }
   df
}

read.svg<-function(file.url){
  #url=paste("http://dev.w3.org/SVG/tools/svgweb/samples/svg-files/",file.Name,sep="")
  #url="http://dev.w3.org/SVG/tools/svgweb/samples/svg-files/AJ_Digital_Camera.svg"
  showMe(file.url)
  script<-getURL(file.url)
  doc <- htmlParse(script)
  #getNodeSet(doc, "//tr")->ns.tr
  #doc
  getNodeSet(doc,"//*")->ns
  lapply(1:length(ns), function(i){do.OneNode(ns[[i]], i) })->tmp
  rbindlist(tmp)->tmp.df
  #do.call(rbind, tmp)->tmp.df
  tmp.df$file<-file.url
  tmp.df
}

#not used anymore
extractAE<-function(file.name, attr, ele="*"){
  url=paste("http://dev.w3.org/SVG/tools/svgweb/samples/svg-files/",file.name,sep="")
  script<-getURL(url)
  doc <- htmlParse(script)
  cmd<-paste("//",ele,"[@",attr,"]",sep="")
  showMe(cmd)
  getNodeSet(doc,cmd)->ns 
}


findAttrs<-function(attr, s.df=big.df ){
  unique(s.df$file[grep(attr, s.df$attrs)])
}
# all.files<-function(file.names){
# 
# }
# getNodeSet(doc,"//*")->ns
# lapply(ns, do.OneNode)->tmp
#read.svg(file.names[1])

#create url list
getSVGWeb_file.urls()->file.urls1
getSVGJenkins_file.urls()->file.urls2
file.urls<-c(file.urls1, file.urls2)

#create list of data.tables for each url
lapply(file.urls, read.svg)->df.list

#combine into a single data.table
#do.call(rbind, df.list)->big.df
rbindlist(df.list)->big.df

# big.df$comm<-as.numeric(big.df$comm)
# big.df$semi<-as.numeric(big.df$semi)
# big.df$colo<-as.numeric(big.df$colo)
# big.df$spac<-as.numeric(big.df$spac)
# subset(big.df, big.df$colo>0)->df.colon
# subset(big.df, big.df$semi>0)->df.semicolon
# subset(big.df, big.df$comm>0)->df.comma
# subset(big.df, big.df$spac>0)->df.spac

# colon.attrs<-unique(df.colon$attrs)
# semicolon.attrs<-unique(df.semicolon$attrs)
# comma.attrs<-unique(df.comma$attrs)
# space.attrs<-unique(df.spac$attrs)

# codec<-function(x){
#   tmp<-x[3:6]>0
#   rtv<-prod(c(2,3,5,7)^tmp)
#   return(rtv)
# }
# 
# 
# codecm<-function(x){
#   tmp<-x>0
#   rtv<-prod(c(2,3,5,7)^tmp)
#   return(rtv)
# }
# 
# m<-as.matrix(big.df[,3:6])
# apply(m, 1, codecm)->codex
# 
# big.df$codex<-codex

# extractAE("height", "*","car.svg")->tmp
# 

elements.big.df<-sort(unique(as.character(big.df$tag)))
attrs.big.df<-sort((unique(as.character(big.df$attrs))))

# 
# allSeqOfTF<-expand.grid(colon=c(T,F), semicolon=c(T,F), comma=c(T,F), space=c(T,F))

# 5   7   2   1  35 210  14   3 105  15  21  42   6  70
#comm=2 semi=3 colo=5 spac=7  
# subset(big.df, tmp==210)->all
# unique(all$attrs)
# #style
# unique(all$tag)
#stop path rect text g use
#105 is all style

#70 is tag svgtestcase, with attr testname

codexVals<-sort(unique(big.df$codex), decreasing = T)
#codex Vals 210 105  70  42  35  21  15  14   7   6   5   3   2   1

#helper for human readability
codexVal2Name<-function(x){
  tmp<-c(comma=2, semicolon=3, colon=5, space=7)
  tmp2<-names(tmp)[which(x%%tmp==0)]
  val<-paste(tmp2, collapse=", ")
  if(nchar(val)==0){
    val<-"none"
  }
  val
}
codexNames<-sapply(codexVals, codexVal2Name)

#all svg attributes (should probably make this into a data.table too!)
attrs.svg<-sort(unique(AVEL2.df$attr))
elems.svg<-sort(unique(AVEL2.df$element))


attribsFor<-function(codx){
   att<-as.character(unique(filter(big.df, codex==codx)$attrs))
   sort(intersect(att,attrs.svg))
}

lapply(codexVals, attribsFor)->attr4

#names(attr4)<-codexNames
names(attr4)<-codexVals
attr4

attr4['70']<-NULL #clean up piece with character(0) as it's member


# getcodexExample<-function(codex, attr){
#   url<-subset(big.df, big.df$codex==codex & big.df$attr=attr)$file
#   script<-getURL(url)
#   doc <- htmlParse(script)
#   cmd<-paste("//*[@",attr,"]",sep="")
#   #showMe(cmd)
#   getNodeSet(doc,cmd)->ns
#   #now searg for those with the same codex
#   
# }

#subset(big.df$codex==42)
#attr4
# todo for each unique codex, extract  attr-eles illustrating that codex
# and for each such (uniaue) ele-attr, extract a sample node illustrating the triple
# (or maybe just do codex-attr??? for attr4)

#the idea: use element-attribute-codex as a key, get unique keys, then use page and pos to get examples
#but first, must filter by svg attributes and elements
big.svg.dt<-filter(big.df, tag %in% elems.svg , attrs %in% attrs.svg)
setkey(big.svg.dt, tag, attrs, codex)
unique(big.svg.dt)->examples.dt
examples.dt[order(codex, attrs, tag, decreasing=T)]->examples.dt


# getExample<-function(codx, attr, ele="*", num=1){
#   rows<-filter(big.df, attrs==attr , codex==codx)
#   url<-rows$file[num]
#   pos<-rows$pos[num]
#   script<-getURL(url)
#   doc <- htmlParse(script)
#   #cmd<-paste("//*[@",attr,"]",sep="")
#   cmd<-paste("//",ele,"[@",attr,"]",sep="")
#   getNodeSet(doc,cmd)->ns
#   ns[[pos]]
# }

#create a data.table of all existing svg element-attribute pairs
# ele.attr.dt<-select(big.df, tag, attrs)
# unique(ele.attr.dt)->ele.att.u.dt
# filter(ele.att.u.dt, tag %in%  elems.svg, attrs %in% attrs.svg)->ele.att.u.dt

getExampleFromRow<-function(x){
  url<-x["file"]
  #showMe(url)
  script<-getURL(url)
  doc <- htmlParse(script)
  getNodeSet(doc,"//*")->ns
  pos<-as.numeric(x["pos"])
  #showMe(pos)
  rtv<-ns[[pos]]
  #showMe(rtv)
  rtv
}

apply(as.matrix(examples.dt), 1, getExampleFromRow)->examples
#getExample(210, 'style')

head(examples.dt)
# 
file.out<-"./examples3.txt"
send<-"\n-----------------------------------------\n\n"
txt<-c("examples\n",send)
separ<-"=================================================="

for(i in 1:nrow(examples.dt)){
  r<-examples.dt[i,]
  shead<-paste("codex=",r$codex, "; expanded:", codexVal2Name(r$codex),
               ";\n tag=",r$tag, " attrib=", r$attrs)
  #sbod<-toString.XMLNode(examples[[i]])
  sbod<-xmlAttrs(examples[[i]])[r$attrs]
  txt<-c( txt, shead, separ, sbod, send)
}

txt<-c(txt,send)
txt<-paste(txt, collapse="\n")
write(txt, file=file.out)


# 
