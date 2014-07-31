# # <span class="element-name">‘a’</span>
# #url<-"http://www.w3.org/TR/SVG/propidx.html"
# tables<-readHTMLTable(url)

url<-"http://www.w3.org/TR/SVG/eltindex.html"
script<-getURL(url)
doc <- htmlParse(script)
#xpathApply(raw.data, "//td", xmlValue)->tmp
#doc<-xmlParse(script)
li<-getNodeSet(doc, "//span[@class='element-name']")
elements2<-sapply(li, xmlValue)
elements2<-gsub("[‘’]","", elements2)

# sapply(els, function(el) xmlGetAttr(el, "class"))