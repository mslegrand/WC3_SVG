library(XML)
library(RCurl)
library(data.table)
library(stringr)

cleanVals1<-function(vals){
  vals<-gsub(" — .*$",":",vals) #truncate ending
  vals<-gsub("in’", "in1", vals) #be aware of this kludge
  vals<-gsub("[‘’,]","",vals) 
  vals<-gsub("\n"," ",vals)
  vals<-gsub(" +"," ",vals)
  vals<-gsub("^ ","",vals)
  vals<-gsub(" $","",vals)
}

# Property Index
url="http://www.w3.org/TR/SVG/propidx.html"
readHTMLTable(url, stringsAsFactors = FALSE)->tmp.table
tmp.table[[1]]->tmp.df
lapply(tmp.df,cleanVals1)->tmp2

as.data.table(tmp2)->propIndx.DT
write.table(propIndx.DT,file="dataTableLink/propIndex.tsv",
            sep="\t",
            row.names=FALSE,
            quote=FALSE)


#getHTMLLinks(url, relative = TRUE)->tmp.links
# #sapply(tmp.links,function(x){ }
# unique(gsub("#.*$","", tmp.links))->pages
# 
# for(page in pages){
#   cat("1: ",page,"\n")
# }
# 
# #getHTMLExternalFiles(url, asNodes=T)->link.nodes
# #getHTMLExternalFiles(url, asNodes=TRUE, xpQuery="//a/@href")->link.nodes
# 
# url="http://www.w3.org/TR/SVG/eltindex.html"
# readHTMLList(url)[[1]]->tmp.list
# tmp.list<-gsub( "[‘’]","",tmp.list)
# getHTMLLinks(url, relative = TRUE)->tmp.links2
# #sapply(tmp.links,function(x){ }
# unique(gsub("#.*$","", tmp.links))->pages2
# 
# for(page in pages2){
#   cat("2: ",page,"\n")
# }
