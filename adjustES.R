#1.  Adjust categority membership in es.tsv (use elCatDes.tsv, elementsummary.tsv )
#2.  Adjust content.model in es.tsv
#3.  Add descriptions to es.tsv (use elCatDes.tsv )
#4.  Add custom attributes, ie. xy, cxy, ...

#5.  Rewrite code to use es.tsv for element fns.
#6.  Rewrite function docs
#7.  Rewrite attribute docs (use att.cat.tsv, )
#8.  Write value docs
 
#1.  Adjust categority membership in es.tsv (use elCatDes.tsv, elementsummary.tsv )

# read in elementsummary.tsv, elCatDes.tsv
# twocopies of es: es.orig, es.repl
elCatDes.DT<-fread("dataTableLink/elCatDes.tsv")
es.orig.DT<-fread("dataTableLink/elementSummary.tsv")
# melt elCatDes
mdt<-melt(elCatDes.DT, id="element", measure=c("category","description"))
# replace categoreis in es.repl with add cats from melt and add descriptions
es.repl.DT<-rbind(es.orig.DT[variable!="category"],mdt)

#3.  Replace content.model in es.repl (this is the hardest part)
#    Since only Unclassified loses member, we can safely keep all
#     other members, we have only 2 considerations
#      A. All members of font are available (then add font)
#      B. This element is already included (then remove element)

#4.  Add custom attributes, ie. xy, cxy, ...
#    Go to code builder and for reference. 

#5.  Rewrite code builder to use es.tsv for element fns.

#6.  Mine for attribute definitions / attribute values

