if (!require(dplyr)){install.packages("dplyr");require(dplyr)}

spr_nq<-function(material=NULL,number_of_condition=NULL,number_of_item_set=NULL,
                 startfrom=NULL,name_of_target_material=NULL){
  
  br1<-rep(c("[["),each=nrow(material))
  ncond2<-rep(c(1:number_of_condition),times=number_of_item_set)
  cond<-paste(name_of_target_material,ncond2,sep="")
  cond<-gsub('^','"',cond)
  cond<-gsub('$','",',cond)
  item<-as.character(rep(c(startfrom:(startfrom+number_of_item_set-1)),each=number_of_condition))
  br2<-rep(c("],"),each=nrow(material))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(material))
  s<-rep(c("{s:"),each=nrow(material))
  br3<-rep(c("["),each=nrow(material))
  nOfcol<-ncol(material)
  nOfrow<-nrow(material)
  list<-matrix(rep(NA,nOfrow*(nOfcol-1)),ncol=(nOfcol-1))
  for (j in 1:(nOfcol-1)){
    d1<-material[j]
    d1<-as.character(t(d1))
    d1<-gsub('_' ,' ',d1)
    d1<-gsub('^','"',d1)
    d1<-gsub('$','",',d1)
    list[,j]<-d1
  }
  d1<-as.data.frame(list)
  d2<-material[nOfcol]
  d2<-as.character(t(d2))
  d2<-gsub('_' ,' ',d2)
  d2<-gsub('^','"',d2)
  d2<-gsub('$','"',d2)
  material2<-data.frame(d1,d2)
  br4<-rep(c("]}],"),each=nrow(material))
  dat<-data.frame(br1,cond,item,br2,task,s,br3,material2,br4)
  dat
}