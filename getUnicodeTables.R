library(XML)
library(RCurl)
library(data.table)
library(stringr)
library(devtools)
library(assertthat)

# hershey fonts:
#http://emergent.unpythonic.net/software/hershey

url<-"http://www.ssec.wisc.edu/~tomw/java/unicode.html"
readHTMLTable(url, stringsAsFactors = FALSE, header=T)->codeTables
script<-getURL(url)
doc <- htmlParse(script)
getNodeSet(doc, "//h2")->ns.h2
sapply(ns.h2, xmlValue)->val.h2

val.h2<-str_trim(val.h2)
val.h2<-val.h2[-length(val.h2)]
val.h2[89]<-"Specials2"

names(codeTables)<-val.h2
names(codeTables)
str(codeTables)


writeToFile<-function(cname, ctable){
  write.csv(ctable,paste0("Unicode/", cname,".csv"), row.names=F)
}

toWrite<-c("Greek", "Hebrew", "Superscripts and Subscripts", "Mathematical Operators",
           "Miscellaneous Technical", "Dingbats", "Arrows")

sapply(toWrite, function(nm){ writeToFile(nm, codeTables[[nm]]) } )

url<-"http://www.johndcook.com/math_symbols.html"

ur<-"http://en.wikipedia.org/wiki/List_of_logic_symbols" #(small set)
 
url<-"http://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode" #lots, but just the unicode

url<-"http://milde.users.sourceforge.net/LUCR/Math/unimathsymbols.xhtml"

# and the best todate!
url<-"http://milde.users.sourceforge.net/LUCR/Math/unimathsymbols.xhtml"
readHTMLTable(url, stringsAsFactors = FALSE, header=T)->codeTables2
rbindlist(codeTables2)->code.DT
code.DT<-code.DT[,.(no.,LaTeX,requirements)]
write.csv(code.DT, "Unicode/unicodeLatex.csv", row.names=F)

# want to save no. , LateX, class and requrirements

