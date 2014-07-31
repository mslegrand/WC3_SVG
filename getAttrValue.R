url="http://www.w3.org/TR/SVG/animate.html"
getAttrValue<-function(url){
  script<-getURL(url)
  doc <- htmlParse(script)
  #a_name<-getNodeSet(doc, "//span[@class='adef']")
  #a_val<-getNodeSet(doc, "//span[@class='attr-value']")
  ns.dt<-getNodeSet(doc, "//dt")
  
#   kids<-function(n)length(xmlElementsByTagName(n, 'span'))
#   sapply(ns.dt, kids)
  
  sapply(ns.dt, xmlValue)->tmp2
  tmp2[grep("=",tmp2)]->tmp2
  tmp2

#   tmp<-dt
#   att_name<-sapply(a_name, xmlValue)
#   att_val<-sapply(a_val, xmlValue)
#   data.frame(attribute=att_name, value=att_val)
}

getAttrValue(url)->tmp