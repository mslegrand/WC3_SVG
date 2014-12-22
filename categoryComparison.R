library(data.table)
# we have 14 categories from mdn, but 10 from wc3.
# how do the correspond?
# given a category from mdn, iterate through it's elements
# and intersect with wc3
es.DT<-fread("dataTableLink/elementSummary.tsv")
w3cCats.DT<-es.DT[variable=="category"]
w3cCats.DT<-w3cCats.DT[,c(1,3), with=F]
setNames(w3cCats.DT, c("element","category"))
w3cCats.DT<-w3cCats.DT[, category:=paste(category,"s",sep="")]


mdnCats.DT<-fread("dataTableLink/elCatDes.tsv")
mdnCats.DT<-mdnCats.DT[,1:2, with=F]

sort(unique(w3cCats.DT$category))->wc
sort(unique(mdnCats.DT$category))->mc


intersect(wc,mc)->commonCats

#in the common cats the differences are:
for(acat in commonCats){
  mE<-mdnCats.DT[category==acat]$element
  wE<-w3cCats.DT[category==acat]$element
  dffM<-setdiff(mE,wE)
  dffW<-setdiff(wE,mE)
  if(length(dffM)>0 | length(dffW)>0){
    cat("Category::",acat,"\n")
    if(length(dffM)>0){
      cat(paste("Mdn:",dffM,collapse=", "),"\n")
    }
    if(length(dffW)>0){
      cat(paste("W3C:",dffW,collapse=", "),"\n")
    }    
  }
}

dffW<-setdiff(wc,mc)
for(acat in dffW){
  cat("Category::",acat,"\n")
  wE<-w3cCats.DT[category==acat]$element
  cat(paste("W3C:",wE,collapse=", "),"\n") 
}

dffM<-setdiff(mc,wc)
for(acat in dffM){
  cat("Category::",acat,"\n")
  mE<-mdnCats.DT[category==acat]$element
  cat(paste("MDN:",mE,collapse=", "),"\n") 
}


