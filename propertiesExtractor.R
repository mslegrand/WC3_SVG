url<-"http://www.w3.org/TR/SVG/propidx.html" #gives the properties

#url<-"http://www.w3.org/TR/SVG/eltindex.html"
script<-getURL(url)
doc <- htmlParse(script)
li<-getNodeSet(doc, "//tr")

clean<-function(x){
  gsub("[‘’\\n|]","", x)
}

toList<-function(x){
  x<-clean(x)
  x
}
# 
# 
# td1<-lapply(2:length(li), function(i){ xmlValue( li[[i]][[1]][[1]]) })
# td2<-lapply(2:length(li), function(i){ xmlValue( li[[i]][[2]]) })
# td3<-lapply(2:length(li), function(i){ xmlValue( li[[i]][[3]]) })

td<-lapply( 1:8, function(i) sapply(1:length(li), function(n){xmlValue(li[[n]][[i]])}) )
#do.call(cbind,td)->td.m

makePropertyValueTable<-function(td){
  property<-td[[1]]
  #property<-property[-1]
  values<-td[[2]]
  #values<-values[-1]
  #values<-clean(values)
  values<-gsub("[‘’]","", values)
  values<-gsub("\\|","",values)
  values<-gsub("\\n","", values)
  values<-gsub(" +"," ",values)
  values<-str_split(values, " ")
  
  v.df<-lapply(1:length(property), function(i){ data.frame( property=property[i], value=values[[i]], stringsAsFactors = F)})
  propertyValues<-do.call(rbind, v.df)  
}

makePropertyElementTable<-function(td, allElements, elementTypes){
  property<-td[[1]]
  #property<-property[-1]
  pelements<-td[[4]]
  #pelements<-pelements[-1]
  pelements<-gsub("[‘’]","", pelements)
  #pelements<-gsub("\\|","",pelements)
  pelements<-gsub("\\n","", pelements)
  pelements<-gsub(" +"," ",pelements)
  
  #splitting won't work so we need to search for keywords:
  keywords<-allElements together with key.etypes
  "container elements" , "graphic elementa" 
  "image elements"  "filter primitives"  "text context elements" 
  "shapes" "pattern" "marker" "new viewport"

  key.etypes<-allElements together with 
  "container" , "graphics" 
  "image elements"  
  "filter primitive"  "text content" 
  "shape" 
  "pattern" "marker" "new viewport"
  
  #values<-clean(values)
  
  elements<-str_split(elements, " ")
  
  v.df<-lapply(1:length(property), function(i){ data.frame( property=property[i], value=values[[i]], stringsAsFactors = F)})
  propertyValues<-do.call(rbind, v.df)  
}



#sapply(1:8, function(i)td[[i]][1])->names

#td.df<-data.frame(td)