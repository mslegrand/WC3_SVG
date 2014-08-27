# showMe<-function(what){
#   w<-deparse(substitute(what))
#   cat(w,"=",what,"\n", collapse="")
# }

showMe<-function(...){
  tmp2<-substitute(list(...))
  tmp3<-sapply(tmp2,deparse)[-1]
  tmp1<-unlist(list(...))
  txt.list<-lapply(1:length(tmp1), function(i){paste(tmp3[i],"=",tmp1[i])} )
  txt<-paste(txt.list,collapse="; ")
  cat(txt,"\n")
  invisible(txt)
}
showMe(file.name, rile.name)
