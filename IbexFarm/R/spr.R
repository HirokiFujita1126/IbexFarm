if (!require(dplyr)){install.packages("dplyr");require(dplyr)}

spr<-function(material=NULL,question=NULL,option_of_question=NULL,correct_response=NULL,
               number_of_condition=NULL,number_of_item_set=NULL,startfrom=NULL,name_of_experimental_material=NULL){
  
  br1<-rep(c("[["),each=nrow(material))
  ncond2<-rep(c(1:number_of_condition),times=number_of_item_set)
  cond<-paste(name_of_experimental_material,ncond2,sep="")
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
  br4<-rep(c("]},"),each=nrow(material))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(material))
  br5<-rep(c("{q:"),each=nrow(material))
  question<-as.character(t(question))
  question<-gsub('^','"',question)
  question<-gsub('$','",',question)
  as<-rep(c("as:["),each=nrow(material))
  qoption1<-as.character(t(option_of_question[1]))
  qoption1<-gsub('^','"',qoption1)
  qoption1<-gsub('$','",',qoption1)
  qoption2<-as.character(t(option_of_question[2]))
  qoption2<-gsub('^','"',qoption2)
  qoption2<-gsub('$','"',qoption2)
  correct_response<-as.character(t(correct_response))
  correct_response<-gsub('^','"',correct_response)
  correct_response<-gsub('$','"',correct_response)
  br6<-rep(c("], hasCorrect:"),each=nrow(material))
  br7<-rep(c("}],"),each=nrow(material))
  dat<-data.frame(br1,cond,item,br2,task,s,br3,material2,br4,cq,br5,question,as,qoption1,qoption2,br6,correct_response,br7)
  dat
}