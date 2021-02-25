if (!require(dplyr)){install.packages("dplyr");require(dplyr)}

spr_nq_f<-function(material=NULL,number_of_filler=NULL,startfrom=NULL,name_of_filler=NULL){
  
  br1<-rep(c("[["),each=nrow(material))
  nf<-rep(c(name_of_filler),number_of_filler)
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  item<-startfrom:(number_of_filler+startfrom-1)
  br2<-rep(c("],"),each=nrow(material))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(material))
  s<-rep(c("{s:"),each=nrow(material))
  br3<-rep(c("["),each=nrow(material))
  nOfcol<-ncol(material)
  nOfrow<-nrow(material)
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-material[j]
    d1<-as.character(t(d1))
    d1<-gsub('_' ,' ',d1)
    d1<-gsub('^','"',d1)
    d1<-gsub('$','",',d1)
    list[,j]<-d1
  }
  material2<-ifelse(list=='"",'|list=='""',"",list)
  material2<-gsub('([:.:])([:":])([:,:])','."',material2)
  material2<-data.frame(material2)
  br4<-rep(c("]}],"),each=nrow(material))
  dat<-data.frame(br1,nf,item,br2,task,s,br3,material2,br4)
  colnames(dat)<-1:ncol(dat)
  
  var_itemE<-data.frame("];")
  colnames(var_itemE)<-1
  dat<-dplyr::bind_rows(dat,var_itemE)
  dat[is.na(dat)] <- " "
  dat
}