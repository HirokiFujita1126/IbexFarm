if (!require(dplyr)){install.packages("dplyr");require(dplyr)}

contoller<-function(name_of_practice=NULL,name_of_filler=NULL,name_of_target_material=NULL,random_order_for_question=NULL){
  
  seqenceS<-"var shuffleSequence=seq("
  intro<-"intro"
  intro<-gsub('^','"',intro)
  intro<-gsub('$','",',intro)
  setcounter<-"setcounter"
  setcounter<-gsub('^','"',setcounter)
  setcounter<-gsub('$','",',setcounter)  
  practice<-name_of_practice
  practice<-gsub('^','"',practice)
  practice<-gsub('$','",',practice)
  shuffleFS<-"shuffle(randomize("
  filler<-name_of_filler
  filler<-gsub('^','"',filler)
  filler<-gsub('$','"',filler)
  shuffleFE<-"),"
  shuffleES<-"shuffle(randomize(anyOf(startsWith("
  experimental<-name_of_target_material
  experimental<-gsub('^','"',experimental)
  experimental<-gsub('$','"',experimental)
  shuffleEE<-"))))),"
  sr<-"sr"
  sr<-gsub('^','"',sr)
  sr<-gsub('$','",',sr)
  finish<-"finish"
  finish<-gsub('^','"',finish)
  finish<-gsub('$','"',finish)
  seqenceE<-");"
  seqence<-data.frame(seqenceS,intro,setcounter,practice,shuffleFS,filler,shuffleFE,shuffleES,experimental,shuffleEE,sr,finish,seqenceE)
  colnames(seqence)<-1:ncol(seqence)
  
  var_practiceS<-"var practiceItemTypes=["
  practice<-name_of_practice
  practice<-gsub('^','"',practice)
  practice<-gsub('$','"',practice)
  var_practiceE<-"];"
  var_practice<-data.frame(var_practiceS,practice,var_practiceE)
  colnames(var_practice)<-1:ncol(var_practice)
  
  var_defalutS<-data.frame("var defaults=[")
  colnames(var_defalutS)<-1
  
  spr<-"DashedSentence"
  spr<-gsub('^','"',spr)
  spr<-gsub('$','",',spr)
  modeS<-"{mode:"
  task<-"self-paced reading"
  task<-gsub('^','"',task)
  task<-gsub('$','"',task)
  modeE<-"},"
  DashedSentence<-data.frame(spr,modeS,task,modeE)
  colnames(DashedSentence)<-1:ncol(DashedSentence)
  q<-"Question"
  q<-gsub('^','"',q)
  q<-gsub('$','",',q)
  hs<-"{hasCorrect: true,"
  if(random_order_for_question==TRUE){ro<-"randomOrder: true},"} 
  if(random_order_for_question==FALSE){ro<-"randomOrder: false},"} 
  Question<-data.frame(q,hs,ro)
  colnames(Question)<-1:ncol(Question)
  
  form<-"Form"
  form<-gsub('^','"',form)
  form<-gsub('$','",',form)
  contentOfform<-"{hideProgressBar: true,continueOnReturn: true,saveReactionTime: true}"
  Form<-data.frame(form,contentOfform)
  colnames(Form)<-1:ncol(Form)
  
  var_defalutE<-data.frame("];")
  colnames(var_defalutE)<-1
  
  var_itemS<-data.frame("var items=[")
  colnames(var_itemS)<-1
  
  introS<-"["
  intro_var<-"intro"
  intro_var<-gsub('^','"',intro_var)
  intro_var<-gsub('$','",',intro_var)
  form<-form
  formS<-"{html: { include:"
  example_intro<-"example_intro.html"
  example_intro<-gsub('^','"',example_intro)
  example_intro<-gsub('$','"',example_intro)
  introE<-"},} ],"
  Introduction<-data.frame(introS,intro_var,form,formS,example_intro,introE)
  colnames(Introduction)<-1:ncol(Introduction)
  
  scS<-"["
  sc_var<-"setcounter"
  sc_var<-gsub('^','"',sc_var)
  sc_var<-gsub('$','",',sc_var)
  Sc_var<-"__SetCounter__"
  Sc_var<-gsub('^','"',Sc_var)
  Sc_var<-gsub('$','",',Sc_var)
  scE<-"{ }],"
  SC<-data.frame(scS,sc_var,Sc_var,scE)
  colnames(SC)<-1:ncol(SC)
  
  srS<-"["
  sr_var<-"sr"
  sr_var<-gsub('^','"',sr_var)
  sr_var<-gsub('$','",',sr_var)
  Sr_var<-"__SendResults__"
  Sr_var<-gsub('^','"',Sr_var)
  Sr_var<-gsub('$','",',Sr_var)
  srE<-"{ }],"
  SR<-data.frame(srS,sr_var,Sr_var,srE)
  colnames(SR)<-1:ncol(SR)
  
  finishS<-"["
  finish_var<-"finish"
  finish_var<-gsub('^','"',finish_var)
  finish_var<-gsub('$','",',finish_var)
  message<-"Message"
  message<-gsub('^','"',message)
  message<-gsub('$','",',message)
  messageS<-"{html:"
  messageContent<-"That is the end of the experiment. Thank you for your participation"
  messageContent<-gsub('^','"',messageContent)
  messageContent<-gsub('$','",',messageContent)
  finishE<-"transfer: null} ],"
  Finish<-data.frame(finishS,finish_var,message,messageS,messageContent,finishE)
  colnames(Finish)<-1:ncol(Finish)
  
  dat<-dplyr::bind_rows(seqence,var_practice,var_defalutS,DashedSentence,Question,Form,var_defalutE,var_itemS,Introduction,SC,SR,Finish)
  dat[is.na(dat)] <- " "
  dat
}

if (!require(dplyr)){install.packages("dplyr");require(dplyr)}

binder<-function(sequence_controller=NULL,practice=NULL,experimental_material=NULL,filler=NULL){
  
  colnames(sequence_controller)<-1:ncol(sequence_controller)
  colnames(practice)<-1:ncol(practice)
  colnames(experimental_material)<-1:ncol(experimental_material)
  colnames(filler)<-1:ncol(filler)
  dat<-dplyr::bind_rows(sequence_controller,practice,experimental_material,filler)
  dat[is.na(dat)] <- " "
  write.table(dat,"example_data.js",quote=FALSE,row.names=FALSE,col.names=FALSE)
}

practice_nq<-function(material=NULL,number_of_practice_trial=NULL,name_of_practice=NULL){
  
  br1<-rep(c("["),each=nrow(material))
  nf<-rep(c(name_of_practice),number_of_practice_trial)
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
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
  dat<-data.frame(br1,nf,task,s,br3,material2,br4)
  colnames(dat)<-1:ncol(dat)
  dat
}

practice<-function(material=NULL,question=NULL,option_of_question=NULL,
                   number_of_practice_trial=NULL,name_of_practice=NULL){
  
  br1<-rep(c("["),each=nrow(material))
  nf<-rep(c(name_of_practice),number_of_practice_trial)
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
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
  br6<-rep(c("]}],"),each=nrow(material))
  dat<-data.frame(br1,nf,task,s,br3,material2,br4,cq,br5,question,as,qoption1,qoption2,br6)
  dat
}

spr_f<-function(material=NULL,question=NULL,option_of_question=NULL,correct_response=NULL,
                number_of_filler=NULL,startfrom=NULL,name_of_filler=NULL){
  
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
  dat<-data.frame(br1,nf,item,br2,task,s,br3,material2,br4,cq,br5,question,as,qoption1,qoption2,br6,correct_response,br7)
  colnames(dat)<-1:ncol(dat)
  
  var_itemE<-data.frame("];")
  colnames(var_itemE)<-1
  dat<-dplyr::bind_rows(dat,var_itemE)
  dat[is.na(dat)] <- " "
  dat
}

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


