# Custom attributes

#Todo!!! # "xChannelSelector"
# aev.

#Done:
#search for x, y, z
#search for X, Y, Z
#search for x and y
#search for X and Y
#search for 1 and 2
#search for 1, 2, 3, 4, ...
#search for width and height

library(data.table)

elements<-unique(ave.DT$element)


el.contains.attrs<-function(el, sought.attrs){
  elAttr<-ave.DT[element==el]$attr
  prod(match(sought.attrs, elAttr, 0L))>0
}

getDT.Containing.Attrs<-function(el, variable, sought.attrs){
  if( el.contains.attrs(el,sought.attrs) ){
    DT<-data.table(element=el, variable=variable, sought.attrs=sought.attrs)
  } else {
    DT<-data.table()
  }
  DT
}

# returns DT with attrs that match attrs ending in x
# that is look for attrs of el ending with x, then
# for each attrx ending in x, find all attr* that matches
# and return DT with 
# element=el, 
# variable=attrN, (attrN is the name attrxy.. where attry matches, ...)
# values=attr*
getDT.Matching.XEndAttrs<-function(el, x){
    xd<-paste0(x,'$')
    #cat(xd,"\n")
    elAttr<-ave.DT[element==el]$attr
    elAttrx<-elAttr[grep(xd,elAttr)]
    
    fn<-function(ax){
      pat<-paste0("^",substr(ax,1,nchar(ax)-nchar(x)),".$")
      #pat<-sub(xd,'.$', ax) 
      #cat(ax,ay,"\n")
      elMatch<-elAttr[grep(pat,elAttr)]
      if(length(elMatch)>1 ){ 
        len<-nchar(elMatch[1])
        suffix<-sort(substr(elMatch,len,len))
        prefix<-substr(elMatch[1],1,len-1)
        variable<-paste(c(prefix,suffix), collapse='')
        data.table(element=el, variable=variable , value=elMatch )
      } else
        data.table()    
    }
    
    tmp<-lapply(elAttrx, fn)
    rbindlist(tmp)   
}

tmp<-c(
  lapply(elements, function(el){getDT.Matching.XEndAttrs(el,'x')}),
  lapply(elements, function(el){getDT.Matching.XEndAttrs(el,'X')}),
  lapply(elements, function(el){getDT.Matching.XEndAttrs(el,'1')}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'wh', 
                                                       c('width','height'))}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'in12', 
                                                       c('in', 'in2') )}),  
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'xy1', 
                                                       c('x1', 'y1') )}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'xy2', 
                                                       c('x2', 'y2') )}), 
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'in1', 'in')}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, '.', ':')}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, '.', '-')})
)

rbindlist(tmp)->comboParams.DT

write.table(comboParams.DT,file="dataTables/comboParams.tsv",
            sep="\t",
            row.names=FALSE,
            quote=FALSE)


specParamsCXY.list<-lapply(elements, function(el){
    getDT.Containing.Attrs(el, 'cxy', 
                           c('width','height', 'x', 'y'))
})

specParamsCXY.DT<-rbindlist(specParamsCXY.list)
write.table(specParamsCXY.DT,file="dataTables/specParamsCXY.tsv",
            sep="\t",
            row.names=FALSE,
            quote=FALSE)

