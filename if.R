if (!require(tidyverse)){install.packages("tidyverse");require(tidyverse)}
all_na<-function(x)any(!is.na(x))

spr_contoller<-function(name_of_practice=NULL,name_of_filler=NULL,name_of_target_material=NULL,random_order_for_question=NULL){
  
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

maze_contoller<-function(name_of_practice=NULL,name_of_filler=NULL,name_of_target_material=NULL){
  
  seqenceS<-"var shuffleSequence=seq("
  intro<-"intro"
  intro<-gsub('^','"',intro)
  intro<-gsub('$','",',intro)
  setcounter<-"setcounter"
  setcounter<-gsub('^','"',setcounter)
  setcounter<-gsub('$','",',setcounter)  
  followEachWith<-"followEachWith("
  sep<-"sep"
  sep<-gsub('^','"',sep)
  sep<-gsub('$','",',sep)  
  practice<-name_of_practice
  practice<-gsub('^','"',practice)
  practice<-gsub('$','"',practice)
  practice_E<-")"
  practice_E<-gsub('$',',',practice_E)  
  shuffleFS<-"shuffle(randomize("
  filler<-name_of_filler
  filler<-gsub('^','"',filler)
  filler<-gsub('$','"',filler)
  shuffleFE<-"),"
  shuffleES<-"shuffle(randomize(anyOf(startsWith("
  experimental<-name_of_target_material
  experimental<-gsub('^','"',experimental)
  experimental<-gsub('$','"',experimental)
  shuffleEE<-")))))),"
  sr<-"sr"
  sr<-gsub('^','"',sr)
  sr<-gsub('$','",',sr)
  finish<-"finish"
  finish<-gsub('^','"',finish)
  finish<-gsub('$','"',finish)
  seqenceE<-");"
  seqence<-data.frame(seqenceS,intro,setcounter,followEachWith,sep,practice,practice_E,followEachWith,sep,shuffleFS,filler,shuffleFE,shuffleES,experimental,shuffleEE,sr,finish,seqenceE)
  colnames(seqence)<-1:ncol(seqence)
  
  var_practiceS<-"var practiceItemTypes=["
  practice<-name_of_practice
  practice<-gsub('^','"',practice)
  practice<-gsub('$','"',practice)
  var_practiceE<-"];"
  var_practice<-data.frame(var_practiceS,practice,var_practiceE)
  colnames(var_practice)<-1:ncol(var_practice)
  
  var_itemS<-data.frame("var items=[")
  colnames(var_itemS)<-1
  
  introS<-"["
  intro_var<-"intro"
  intro_var<-gsub('^','"',intro_var)
  intro_var<-gsub('$','",',intro_var)
  form<-"Form"
  form<-gsub('^','"',form)
  form<-gsub('$','",',form)
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
  
  spS<-"["
  sp_var<-"sep"
  sp_var<-gsub('^','"',sp_var)
  sp_var<-gsub('$','",',sp_var)
  msp_var<-"MazeSeparator"
  msp_var<-gsub('^','"',msp_var)
  msp_var<-gsub('$','",',msp_var)
  nmessageS<-"{normalMessage: "
  nmessageContent<-"Correct! Press any key to continue"
  nmessageContent<-gsub('^','"',nmessageContent)
  nmessageContent<-gsub('$','",',nmessageContent)
  emessageS<-"errorMessage: "
  emessageContent<-"Incorrect! Press any key to continue."
  emessageContent<-gsub('^','"',emessageContent)
  emessageContent<-gsub('$','"',emessageContent)
  spE<-"}],"
  SP<-data.frame(spS,sp_var,msp_var,nmessageS,nmessageContent,emessageS,emessageContent,spE)
  colnames(SP)<-1:ncol(SP)
  
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
  
  dat<-dplyr::bind_rows(seqence,var_practice,var_itemS,Introduction,SC,SP,SR,Finish)
  dat[is.na(dat)] <- " "
  dat
}

spr<-function(controller=NULL,
              material=NULL,
              name_of_practice=NULL,
              number_of_condition=NULL,
              name_of_experimental_material=NULL,
              name_of_filler=NULL){
  
  n<-1:ncol(material)
  colnames(material)<-paste("c",n,sep="")
  pitem<-material[material$c1=="p:",]
  pitem$c1<-NULL
  br1<-rep(c("["),each=nrow(pitem))
  nf<-rep(c(name_of_practice),nrow(pitem))
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(pitem))
  s<-rep(c("{s:"),each=nrow(pitem))
  br3<-rep(c("["),each=nrow(pitem))
  nOfcol<-ncol(pitem)
  nOfrow<-nrow(pitem)
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-pitem[j]
    d1<-as.character(t(d1))
    d1<-gsub('_' ,' ',d1)
    d1<-gsub('^','"',d1)
    d1<-gsub('$','",',d1)
    list[,j]<-d1
  }
  material2<-ifelse(list=='"",'|list=='""',"",list)
  material2<-gsub('([:.:])([:":])([:,:])','."',material2)
  material2<-data.frame(material2)
  br4<-rep(c("]},"),each=nrow(pitem))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(pitem))
  br5<-rep(c("{q:"),each=nrow(pitem))
  question<-material[material$c1=="pq:",]
  question$c1<-NULL
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','",',qe)
  question<-cbind(qs,question,qe)
  as<-rep(c("as:["),each=nrow(pitem))
  
  qoption<-material[material$c1=="po:",]
  qoption$c1<-NULL
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-qoption[j]
    d1<-as.character(t(d1))
    d1<-gsub(',','","',d1)
    list[,j]<-d1
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  qoption<-cbind(qs,qoption,qe)
  qoption<-qoption %>% mutate_if(is.character, na_if, "")
  qoption<-qoption %>% select_if(all_na)
  
  list<-matrix(rep(NA,nOfrow*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:nrow(qoption)){
    d1<-qoption[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  
  br6<-rep(c("]}],"),each=nrow(pitem))
  pdat<-data.frame(br1,nf,task,s,br3,material2,br4,cq,br5,question,as,qoption,br6)
  pdat<-pdat %>% mutate_if(is.character, na_if, "")
  pdat<-pdat %>% select_if(all_na)
  colnames(pdat)<-1:ncol(pdat)
  pdat
  
  
  
  sitem<-material[material$c1=="s:",]
  sitem$c1<-NULL
  sset<-nrow(sitem)/number_of_condition
  br1<-rep(c("[["),each=nrow(sitem))
  ncond2<-rep(c(1:number_of_condition),times=sset)
  cond<-paste(name_of_experimental_material,ncond2,sep="")
  cond<-gsub('^','"',cond)
  cond<-gsub('$','",',cond)
  item<-as.character(rep(c(1:sset),each=number_of_condition))
  br2<-rep(c("],"),each=nrow(sitem))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(sitem))
  s<-rep(c("{s:"),each=nrow(sitem))
  br3<-rep(c("["),each=nrow(sitem))
  nOfcol<-ncol(sitem)
  nOfrow<-nrow(sitem)
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-sitem[j]
    d1<-as.character(t(d1))
    d1<-gsub('_' ,' ',d1)
    d1<-gsub('^','"',d1)
    d1<-gsub('$','",',d1)
    list[,j]<-d1
  }
  material2<-ifelse(list=='"",'|list=='""',"",list)
  material2<-gsub('([:.:])([:":])([:,:])','."',material2)
  material2<-data.frame(material2)
  br4<-rep(c("]},"),each=nrow(sitem))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(sitem))
  br5<-rep(c("{q:"),each=nrow(sitem))
  
  question<-material[material$c1=="sq:",]
  question$c1<-NULL
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','",',qe)
  question<-cbind(qs,question,qe)
  
  as<-rep(c("as:["),each=nrow(sitem))
  
  qoption<-material[material$c1=="so:",]
  qoption$c1<-NULL
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-qoption[j]
    d1<-as.character(t(d1))
    d1<-gsub(',','","',d1)
    list[,j]<-d1
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  qoption<-cbind(qs,qoption,qe)
  qoption<-qoption %>% mutate_if(is.character, na_if, "")
  qoption<-qoption %>% select_if(all_na)
  
  list<-matrix(rep(NA,nOfrow*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:nrow(qoption)){
    d1<-qoption[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  
  correct_response<-material[material$c1=="sco:",]
  correct_response$c1<-NULL
  qs=rep(c("a"),times=nrow(correct_response))
  qe=rep(c("z"),times=nrow(correct_response))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  correct_response<-cbind(qs,correct_response,qe)
  correct_response<-correct_response %>% mutate_if(is.character, na_if, "")
  correct_response<-correct_response %>% select_if(all_na)
  list<-matrix(rep(NA,nOfrow*ncol(correct_response)),ncol=ncol(correct_response))
  for (j in 1:nrow(correct_response)){
    d1<-correct_response[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  correct_response<-ifelse(list=='"",'|list=='""',"",list)
  correct_response<-gsub('([:.:])([:":])([:,:])','."',correct_response)
  correct_response<-data.frame(correct_response)
  
  br6<-rep(c("], hasCorrect:"),each=nrow(sitem))
  br7<-rep(c("}],"),each=nrow(sitem))
  sdat<-data.frame(br1,cond,item,br2,task,s,br3,material2,br4,cq,br5,question,as,qoption,br6,correct_response,br7)
  sdat<-sdat %>% mutate_if(is.character, na_if, "")
  sdat<-sdat %>% select_if(all_na)
  colnames(sdat)<-1:ncol(sdat)
  sdat
  
  fitem<-material[material$c1=="f:",]
  fitem$c1<-NULL
  
  br1<-rep(c("[["),each=nrow(fitem))
  nf<-rep(c(name_of_filler),nrow(fitem))
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  item<-(sset+1):(nrow(fitem)+sset)
  br2<-rep(c("],"),each=nrow(fitem))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(fitem))
  s<-rep(c("{s:"),each=nrow(fitem))
  br3<-rep(c("["),each=nrow(fitem))
  nOfcol<-ncol(fitem)
  nOfrow<-nrow(fitem)
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-fitem[j]
    d1<-as.character(t(d1))
    d1<-gsub('_' ,' ',d1)
    d1<-gsub('^','"',d1)
    d1<-gsub('$','",',d1)
    list[,j]<-d1
  }
  material2<-ifelse(list=='"",'|list=='""',"",list)
  material2<-gsub('([:.:])([:":])([:,:])','."',material2)
  material2<-data.frame(material2)
  br4<-rep(c("]},"),each=nrow(fitem))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(fitem))
  br5<-rep(c("{q:"),each=nrow(fitem))
  question<-material[material$c1=="fq:",]
  question$c1<-NULL
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','",',qe)
  question<-cbind(qs,question,qe)
  as<-rep(c("as:["),each=nrow(fitem))
  
  qoption<-material[material$c1=="fo:",]
  qoption$c1<-NULL
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-qoption[j]
    d1<-as.character(t(d1))
    d1<-gsub(',','","',d1)
    list[,j]<-d1
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  qoption<-cbind(qs,qoption,qe)
  qoption<-qoption %>% mutate_if(is.character, na_if, "")
  qoption<-qoption %>% select_if(all_na)
  
  list<-matrix(rep(NA,nOfrow*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:nrow(qoption)){
    d1<-qoption[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  
  correct_response<-material[material$c1=="fco:",]
  correct_response$c1<-NULL
  qs=rep(c("a"),times=nrow(correct_response))
  qe=rep(c("z"),times=nrow(correct_response))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  correct_response<-cbind(qs,correct_response,qe)
  correct_response<-correct_response %>% mutate_if(is.character, na_if, "")
  correct_response<-correct_response %>% select_if(all_na)
  list<-matrix(rep(NA,nOfrow*ncol(correct_response)),ncol=ncol(correct_response))
  for (j in 1:nrow(correct_response)){
    d1<-correct_response[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  correct_response<-ifelse(list=='"",'|list=='""',"",list)
  correct_response<-gsub('([:.:])([:":])([:,:])','."',correct_response)
  correct_response<-data.frame(correct_response)
  
  br6<-rep(c("], hasCorrect:"),each=nrow(fitem))
  br7<-rep(c("}],"),each=nrow(fitem))
  
  fdat<-data.frame(br1,nf,item,br2,task,s,br3,material2,br4,cq,br5,question,as,qoption,br6,correct_response,br7)
  
  fdat<-fdat %>% mutate_if(is.character, na_if, "")
  fdat<-fdat %>% select_if(all_na)
  colnames(fdat)<-1:ncol(fdat)
  
  var_itemE<-data.frame("];")
  colnames(var_itemE)<-1
  fdat<-dplyr::bind_rows(fdat,var_itemE)
  fdat[is.na(fdat)] <- " "
  fdat
  
  if(nrow(material[material$c1=="fnq:",])>0){
    
    fnqitem<-material[material$c1=="fnq:",]
    fnqitem$c1<-NULL
    
    br1<-rep(c("[["),each=nrow(fnqitem))
    nf<-rep(c(name_of_filler),nrow(fnqitem))
    nf<-gsub('^','"',nf)
    nf<-gsub('$','",',nf)
    item2<-item
    item2<-(max(item2)+1):(max(item2)+nrow(fnqitem))
    br2<-rep(c("],"),each=nrow(fnqitem))
    nmOftask<-"DashedSentence"
    nmOftask<-gsub('^','"',nmOftask)
    nmOftask<-gsub('$','",',nmOftask)
    task<-rep(c(nmOftask),each=nrow(fnqitem))
    s<-rep(c("{s:"),each=nrow(fnqitem))
    br3<-rep(c("["),each=nrow(fnqitem))
    nOfcol<-ncol(fnqitem)
    nOfrow<-nrow(fnqitem)
    list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
    for (j in 1:nOfcol){
      d1<-fnqitem[j]
      d1<-as.character(t(d1))
      d1<-gsub('_' ,' ',d1)
      d1<-gsub('^','"',d1)
      d1<-gsub('$','",',d1)
      list[,j]<-d1
    }
    material2<-ifelse(list=='"",'|list=='""',"",list)
    material2<-gsub('([:.:])([:":])([:,:])','."',material2)
    material2<-data.frame(material2)
    br4<-rep(c("]}],"),each=nrow(fnqitem))
    fdat2<-data.frame(br1,nf,item2,br2,task,s,br3,material2,br4)
    fdat2<-fdat2 %>% mutate_if(is.character, na_if, "")
    fdat2<-fdat2 %>% select_if(all_na)
    
    colnames(fdat2)<-1:ncol(fdat2)
    var_itemE<-data.frame("];")
    colnames(var_itemE)<-1
    fdat2<-dplyr::bind_rows(fdat2,var_itemE)
    fdat2[is.na(fdat2)] <- " "
    
    fdat<-fdat[1:(nrow(fdat)-1),]
    fdat<-dplyr::bind_rows(fdat,fdat2)
    fdat[is.na(fdat)] <- " "
  }
  
  dat<-dplyr::bind_rows(controller,pdat,sdat,fdat)
  dat[is.na(dat)] <- " "
  write.table(dat,"spr.js",quote=FALSE,row.names=FALSE,col.names=FALSE)
  
  
}

maze<-function(controller=NULL,
               material=NULL,
               name_of_practice=NULL,
               number_of_condition=NULL,
               name_of_experimental_material=NULL,
               name_of_filler=NULL){
  
  n<-1:ncol(material)
  colnames(material)<-paste("c",n,sep="")
  pitem<-material[material$c1=="p:",]
  pitem$c1<-NULL
  br1<-rep(c("["),each=nrow(pitem))
  nf<-rep(c(name_of_practice),nrow(pitem))
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  nmOftask<-"Maze"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(pitem))
  se<-rep(c("{s:"),each=nrow(pitem))
  
  list<-matrix(rep(NA,nrow(pitem)*ncol(pitem)),ncol=ncol(pitem))
  for (j in 1:nrow(pitem)){
    d1<-pitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  pitem<-ifelse(list=='"",'|list=='""',"",list)
  pitem<-gsub('([:.:])([:":])([:,:])','."',pitem)
  pitem<-data.frame(pitem)
  
  comma=rep(c(","),times=nrow(pitem))
  
  a<-rep(c("a:"),each=nrow(pitem))
  pmitem<-material[material$c1=="pm:",]
  pmitem$c1<-NULL
  list<-matrix(rep(NA,nrow(pmitem)*ncol(pmitem)),ncol=ncol(pmitem))
  for (j in 1:nrow(pmitem)){
    d1<-pmitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  pmitem<-ifelse(list=='"",'|list=='""',"",list)
  pmitem<-gsub('([:.:])([:":])([:,:])','."',pmitem)
  pmitem<-data.frame(pmitem)
  
  br4<-rep(c("}],"),each=nrow(pitem))
  pdat<-data.frame(br1,nf,task,se,pitem,comma,a,pmitem,br4)
  pdat<-pdat %>% mutate_if(is.character, na_if, "")
  pdat<-pdat %>% select_if(all_na)
  colnames(pdat)<-1:ncol(pdat)
  pdat
  
  
  
  
  sitem<-material[material$c1=="s:",]
  sitem$c1<-NULL
  sset<-nrow(sitem)/number_of_condition
  br1<-rep(c("[["),each=nrow(sitem))
  ncond2<-rep(c(1:number_of_condition),times=sset)
  cond<-paste(name_of_experimental_material,ncond2,sep="")
  cond<-gsub('^','"',cond)
  cond<-gsub('$','",',cond)
  item<-as.character(rep(c(1:sset),each=number_of_condition))
  br2<-rep(c("],"),each=nrow(sitem))
  nmOftask<-"Maze"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(sitem))
  se<-rep(c("{s:"),each=nrow(sitem))
  
  list<-matrix(rep(NA,nrow(sitem)*ncol(sitem)),ncol=ncol(sitem))
  for (j in 1:nrow(sitem)){
    d1<-sitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  sitem<-ifelse(list=='"",'|list=='""',"",list)
  sitem<-gsub('([:.:])([:":])([:,:])','."',sitem)
  sitem<-data.frame(sitem)
  
  comma=rep(c(","),times=nrow(sitem))
  
  a<-rep(c("a:"),each=nrow(sitem))
  smitem<-material[material$c1=="sm:",]
  smitem$c1<-NULL
  list<-matrix(rep(NA,nrow(smitem)*ncol(smitem)),ncol=ncol(smitem))
  for (j in 1:nrow(smitem)){
    d1<-smitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  smitem<-ifelse(list=='"",'|list=='""',"",list)
  smitem<-gsub('([:.:])([:":])([:,:])','."',smitem)
  smitem<-data.frame(smitem)
  
  br4<-rep(c("}],"),each=nrow(sitem))
  sdat<-data.frame(br1,cond,item,br2,task,se,sitem,comma,a,smitem,br4)
  sdat<-sdat %>% mutate_if(is.character, na_if, "")
  sdat<-sdat %>% select_if(all_na)
  colnames(sdat)<-1:ncol(sdat)
  sdat
  
  fitem<-material[material$c1=="f:",]
  fitem$c1<-NULL
  
  br1<-rep(c("[["),each=nrow(fitem))
  nf<-rep(c(name_of_filler),nrow(fitem))
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  item<-(sset+1):(nrow(fitem)+sset)
  br2<-rep(c("],"),each=nrow(fitem))
  nmOftask<-"Maze"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(fitem))
  se<-rep(c("{s:"),each=nrow(fitem))
  
  list<-matrix(rep(NA,nrow(fitem)*ncol(fitem)),ncol=ncol(fitem))
  for (j in 1:nrow(fitem)){
    d1<-fitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  fitem<-ifelse(list=='"",'|list=='""',"",list)
  fitem<-gsub('([:.:])([:":])([:,:])','."',fitem)
  fitem<-data.frame(fitem)
  
  comma=rep(c(","),times=nrow(fitem))
  
  a<-rep(c("a:"),each=nrow(fitem))
  fmitem<-material[material$c1=="fm:",]
  fmitem$c1<-NULL
  
  list<-matrix(rep(NA,nrow(fitem)*ncol(fmitem)),ncol=ncol(fmitem))
  for (j in 1:nrow(fmitem)){
    d1<-fmitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  fmitem<-ifelse(list=='"",'|list=='""',"",list)
  fmitem<-gsub('([:.:])([:":])([:,:])','."',fmitem)
  fmitem<-data.frame(fmitem)
  
  br4<-rep(c("}],"),each=nrow(fitem))
  
  fdat<-data.frame(br1,nf,item,br2,task,se,fitem,comma,a,fmitem,br4)
  
  fdat<-fdat %>% mutate_if(is.character, na_if, "")
  fdat<-fdat %>% select_if(all_na)
  colnames(fdat)<-1:ncol(fdat)
  
  var_itemE<-data.frame("];")
  colnames(var_itemE)<-1
  fdat<-dplyr::bind_rows(fdat,var_itemE)
  fdat[is.na(fdat)] <- " "
  fdat
  
  dat<-dplyr::bind_rows(controller,pdat,sdat,fdat)
  dat[is.na(dat)] <- " "
  write.table(dat,"maze.js",quote=FALSE,row.names=FALSE,col.names=FALSE)
  
}

cq<-function(controller=NULL,
             material=NULL,
             name_of_practice=NULL,
             number_of_condition=NULL,
             name_of_experimental_material=NULL,
             name_of_filler=NULL){
  
  n<-1:ncol(material)
  colnames(material)<-paste("c",n,sep="")
  pitem<-material[material$c1=="p:",]
  pitem$c1<-NULL
  br1<-rep(c("["),each=nrow(pitem))
  nf<-rep(c(name_of_practice),nrow(pitem))
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(pitem))
  se<-rep(c("{s:"),each=nrow(pitem))
  br3<-rep(c("["),each=nrow(pitem))
  
  list<-matrix(rep(NA,nrow(pitem)*ncol(pitem)),ncol=ncol(pitem))
  for (j in 1:nrow(pitem)){
    d1<-pitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  pitem<-ifelse(list=='"",'|list=='""',"",list)
  pitem<-gsub('([:.:])([:":])([:,:])','."',pitem)
  material2<-data.frame(pitem)
  br4<-rep(c("]},"),each=nrow(pitem))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(pitem))
  br5<-rep(c("{q:"),each=nrow(pitem))
  question<-material[material$c1=="pq:",]
  question$c1<-NULL
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','",',qe)
  question<-cbind(qs,question,qe)
  as<-rep(c("as:["),each=nrow(pitem))
  
  qoption<-material[material$c1=="po:",]
  qoption$c1<-NULL
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:ncol(qoption)){
    d1<-qoption[j]
    d1<-as.character(t(d1))
    d1<-gsub(',','","',d1)
    list[,j]<-d1
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  qoption<-cbind(qs,qoption,qe)
  qoption<-qoption %>% mutate_if(is.character, na_if, "")
  qoption<-qoption %>% select_if(all_na)
  
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:nrow(qoption)){
    d1<-qoption[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  
  br6<-rep(c("]}],"),each=nrow(pitem))
  pdat<-data.frame(br1,nf,task,se,br3,material2,br4,cq,br5,question,as,qoption,br6)
  pdat<-pdat %>% mutate_if(is.character, na_if, "")
  pdat<-pdat %>% select_if(all_na)
  colnames(pdat)<-1:ncol(pdat)
  pdat
  
  
  sitem<-material[material$c1=="s:",]
  sitem$c1<-NULL
  sset<-nrow(sitem)/number_of_condition
  br1<-rep(c("[["),each=nrow(sitem))
  ncond2<-rep(c(1:number_of_condition),times=sset)
  cond<-paste(name_of_experimental_material,ncond2,sep="")
  cond<-gsub('^','"',cond)
  cond<-gsub('$','",',cond)
  item<-as.character(rep(c(1:sset),each=number_of_condition))
  br2<-rep(c("],"),each=nrow(sitem))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(sitem))
  se<-rep(c("{s:"),each=nrow(sitem))
  br3<-rep(c("["),each=nrow(sitem))
  list<-matrix(rep(NA,nrow(sitem)*ncol(sitem)),ncol=ncol(sitem))
  for (j in 1:nrow(sitem)){
    d1<-sitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  sitem<-ifelse(list=='"",'|list=='""',"",list)
  sitem<-gsub('([:.:])([:":])([:,:])','."',sitem)
  material2<-data.frame(sitem)
  br4<-rep(c("]},"),each=nrow(sitem))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(sitem))
  br5<-rep(c("{q:"),each=nrow(sitem))
  
  question<-material[material$c1=="sq:",]
  question$c1<-NULL
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','",',qe)
  question<-cbind(qs,question,qe)
  
  as<-rep(c("as:["),each=nrow(sitem))
  
  qoption<-material[material$c1=="so:",]
  qoption$c1<-NULL
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:ncol(qoption)){
    d1<-qoption[j]
    d1<-as.character(t(d1))
    d1<-gsub(',','","',d1)
    list[,j]<-d1
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  qoption<-cbind(qs,qoption,qe)
  qoption<-qoption %>% mutate_if(is.character, na_if, "")
  qoption<-qoption %>% select_if(all_na)
  
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:nrow(qoption)){
    d1<-qoption[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  
  correct_response<-material[material$c1=="sco:",]
  correct_response$c1<-NULL
  qs=rep(c("a"),times=nrow(correct_response))
  qe=rep(c("z"),times=nrow(correct_response))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  correct_response<-cbind(qs,correct_response,qe)
  correct_response<-correct_response %>% mutate_if(is.character, na_if, "")
  correct_response<-correct_response %>% select_if(all_na)
  list<-matrix(rep(NA,nrow(correct_response)*ncol(correct_response)),ncol=ncol(correct_response))
  for (j in 1:nrow(correct_response)){
    d1<-correct_response[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  correct_response<-ifelse(list=='"",'|list=='""',"",list)
  correct_response<-gsub('([:.:])([:":])([:,:])','."',correct_response)
  correct_response<-data.frame(correct_response)
  
  br6<-rep(c("], hasCorrect:"),each=nrow(sitem))
  br7<-rep(c("}],"),each=nrow(sitem))
  sdat<-data.frame(br1,cond,item,br2,task,se,br3,material2,br4,cq,br5,question,as,qoption,br6,correct_response,br7)
  sdat<-sdat %>% mutate_if(is.character, na_if, "")
  sdat<-sdat %>% select_if(all_na)
  colnames(sdat)<-1:ncol(sdat)
  sdat
  
  
  
  fitem<-material[material$c1=="f:",]
  fitem$c1<-NULL
  br1<-rep(c("[["),each=nrow(fitem))
  nf<-rep(c(name_of_filler),nrow(fitem))
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  item<-(sset+1):(nrow(fitem)+sset)
  br2<-rep(c("],"),each=nrow(fitem))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(fitem))
  se<-rep(c("{s:"),each=nrow(fitem))
  br3<-rep(c("["),each=nrow(fitem))
  
  list<-matrix(rep(NA,nrow(fitem)*ncol(fitem)),ncol=ncol(fitem))
  for (j in 1:nrow(fitem)){
    d1<-fitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  fitem<-ifelse(list=='"",'|list=='""',"",list)
  fitem<-gsub('([:.:])([:":])([:,:])','."',fitem)
  material2<-data.frame(fitem)
  
  br4<-rep(c("]},"),each=nrow(fitem))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(fitem))
  br5<-rep(c("{q:"),each=nrow(fitem))
  question<-material[material$c1=="fq:",]
  question$c1<-NULL
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','",',qe)
  question<-cbind(qs,question,qe)
  as<-rep(c("as:["),each=nrow(fitem))
  
  qoption<-material[material$c1=="fo:",]
  qoption$c1<-NULL
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:ncol(qoption)){
    d1<-qoption[j]
    d1<-as.character(t(d1))
    d1<-gsub(',','","',d1)
    list[,j]<-d1
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  qoption<-cbind(qs,qoption,qe)
  qoption<-qoption %>% mutate_if(is.character, na_if, "")
  qoption<-qoption %>% select_if(all_na)
  
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:nrow(qoption)){
    d1<-qoption[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  
  correct_response<-material[material$c1=="fco:",]
  correct_response$c1<-NULL
  qs=rep(c("a"),times=nrow(correct_response))
  qe=rep(c("z"),times=nrow(correct_response))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  correct_response<-cbind(qs,correct_response,qe)
  correct_response<-correct_response %>% mutate_if(is.character, na_if, "")
  correct_response<-correct_response %>% select_if(all_na)
  list<-matrix(rep(NA,nrow(correct_response)*ncol(correct_response)),ncol=ncol(correct_response))
  for (j in 1:nrow(correct_response)){
    d1<-correct_response[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  correct_response<-ifelse(list=='"",'|list=='""',"",list)
  correct_response<-gsub('([:.:])([:":])([:,:])','."',correct_response)
  correct_response<-data.frame(correct_response)
  
  br6<-rep(c("], hasCorrect:"),each=nrow(fitem))
  br7<-rep(c("}],"),each=nrow(fitem))
  
  fdat<-data.frame(br1,nf,item,br2,task,se,br3,material2,br4,cq,br5,question,as,qoption,br6,correct_response,br7)
  
  fdat<-fdat %>% mutate_if(is.character, na_if, "")
  fdat<-fdat %>% select_if(all_na)
  colnames(fdat)<-1:ncol(fdat)
  
  var_itemE<-data.frame("];")
  colnames(var_itemE)<-1
  fdat<-dplyr::bind_rows(fdat,var_itemE)
  fdat[is.na(fdat)] <- " "
  fdat
  
  if(nrow(material[material$c1=="fnq:",])>0){
    
    fnqitem<-material[material$c1=="fnq:",]
    fnqitem$c1<-NULL
    
    br1<-rep(c("[["),each=nrow(fnqitem))
    nf<-rep(c(name_of_filler),nrow(fnqitem))
    nf<-gsub('^','"',nf)
    nf<-gsub('$','",',nf)
    item2<-item
    item2<-(max(item2)+1):(max(item2)+nrow(fnqitem))
    br2<-rep(c("],"),each=nrow(fnqitem))
    nmOftask<-"DashedSentence"
    nmOftask<-gsub('^','"',nmOftask)
    nmOftask<-gsub('$','",',nmOftask)
    task<-rep(c(nmOftask),each=nrow(fnqitem))
    se<-rep(c("{s:"),each=nrow(fnqitem))
    br3<-rep(c("["),each=nrow(fnqitem))
    
    list<-matrix(rep(NA,nrow(fnqitem)*ncol(fnqitem)),ncol=ncol(fnqitem))
    for (j in 1:nrow(fnqitem)){
      d1<-fnqitem[j,]
      d1<-d1 %>% mutate_if(is.character, na_if, "")
      d1<-d1 %>% select_if(all_na)
      s<-dim(d1)[1]
      s<-d1[[s]]
      s<-as.character(s)
      s<-gsub('^','"',s)
      e<-dim(d1)[2]
      e<-d1[[e]]
      e<-as.character(e)
      e<-gsub('$','"',e)
      d2<-d1
      d2<-d2[-dim(d2)[1]]
      d2<-d2[-dim(d2)[2]]
      d1<-cbind(s,d2,e)
      
      for (i in 1:dim(d1)[2]){
        list[j,i]<-d1[[i]]
      }
    }
    fnqitem<-ifelse(list=='"",'|list=='""',"",list)
    fnqitem<-gsub('([:.:])([:":])([:,:])','."',fnqitem)
    material2<-data.frame(fnqitem)
    
    br4<-rep(c("]}],"),each=nrow(fnqitem))
    fdat2<-data.frame(br1,nf,item2,br2,task,se,br3,material2,br4)
    fdat2<-fdat2 %>% mutate_if(is.character, na_if, "")
    fdat2<-fdat2 %>% select_if(all_na)
    
    colnames(fdat2)<-1:ncol(fdat2)
    var_itemE<-data.frame("];")
    colnames(var_itemE)<-1
    fdat2<-dplyr::bind_rows(fdat2,var_itemE)
    fdat2[is.na(fdat2)] <- " "
    
    fdat<-fdat[1:(nrow(fdat)-1),]
    fdat<-dplyr::bind_rows(fdat,fdat2)
    fdat[is.na(fdat)] <- " "
  }
  
  dat<-dplyr::bind_rows(controller,pdat,sdat,fdat)
  dat[is.na(dat)] <- " "
  write.table(dat,"cq.js",quote=FALSE,row.names=FALSE,col.names=FALSE)
  
  
}


spr_indiv_ex<-function(material=NULL,
              number_of_condition=NULL,
              name_of_experimental_material=NULL,
              startfrom=NULL){

  n<-1:ncol(material)
  colnames(material)<-paste("c",n,sep="")
  
  sitem<-material[material$c1=="s:",]
  sitem$c1<-NULL
  sset<-nrow(sitem)/number_of_condition
  br1<-rep(c("[["),each=nrow(sitem))
  ncond2<-rep(c(1:number_of_condition),times=sset)
  cond<-paste(name_of_experimental_material,ncond2,sep="")
  cond<-gsub('^','"',cond)
  cond<-gsub('$','",',cond)
  item<-as.character(rep(c(startfrom:(sset+startfrom-1)),each=number_of_condition))#
  br2<-rep(c("],"),each=nrow(sitem))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(sitem))
  s<-rep(c("{s:"),each=nrow(sitem))
  br3<-rep(c("["),each=nrow(sitem))
  nOfcol<-ncol(sitem)
  nOfrow<-nrow(sitem)
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-sitem[j]
    d1<-as.character(t(d1))
    d1<-gsub('_' ,' ',d1)
    d1<-gsub('^','"',d1)
    d1<-gsub('$','",',d1)
    list[,j]<-d1
  }
  material2<-ifelse(list=='"",'|list=='""',"",list)
  material2<-gsub('([:.:])([:":])([:,:])','."',material2)
  material2<-data.frame(material2)
  br4<-rep(c("]},"),each=nrow(sitem))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(sitem))
  br5<-rep(c("{q:"),each=nrow(sitem))
  question<-material[material$c1=="sq:",]
  question$c1<-NULL
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','",',qe)
  question<-cbind(qs,question,qe)
  as<-rep(c("as:["),each=nrow(sitem))
  qoption<-material[material$c1=="so:",]
  qoption$c1<-NULL
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-qoption[j]
    d1<-as.character(t(d1))
    d1<-gsub(',','","',d1)
    list[,j]<-d1
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  qoption<-cbind(qs,qoption,qe)
  qoption<-qoption %>% mutate_if(is.character, na_if, "")
  qoption<-qoption %>% select_if(all_na)
  list<-matrix(rep(NA,nOfrow*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:nrow(qoption)){
    d1<-qoption[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  correct_response<-material[material$c1=="sco:",]
  correct_response$c1<-NULL
  qs=rep(c("a"),times=nrow(correct_response))
  qe=rep(c("z"),times=nrow(correct_response))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  correct_response<-cbind(qs,correct_response,qe)
  correct_response<-correct_response %>% mutate_if(is.character, na_if, "")
  correct_response<-correct_response %>% select_if(all_na)
  list<-matrix(rep(NA,nOfrow*ncol(correct_response)),ncol=ncol(correct_response))
  for (j in 1:nrow(correct_response)){
    d1<-correct_response[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  correct_response<-ifelse(list=='"",'|list=='""',"",list)
  correct_response<-gsub('([:.:])([:":])([:,:])','."',correct_response)
  correct_response<-data.frame(correct_response)
  br6<-rep(c("], hasCorrect:"),each=nrow(sitem))
  br7<-rep(c("}],"),each=nrow(sitem))
  sdat<-data.frame(br1,cond,item,br2,task,s,br3,material2,br4,cq,br5,question,as,qoption,br6,correct_response,br7)
  sdat<-sdat %>% mutate_if(is.character, na_if, "")
  sdat<-sdat %>% select_if(all_na)
  colnames(sdat)<-1:ncol(sdat)
  
  sdat[is.na(sdat)] <- " "
  write.table(sdat,"spr_indiv_ex.js",quote=FALSE,row.names=FALSE,col.names=FALSE)
}


spr_indiv_f<-function(material=NULL,
                      name_of_filler=NULL,
                      startfrom=NULL){
n<-1:ncol(material)
colnames(material)<-paste("c",n,sep="")
  
fitem<-material[material$c1=="f:",]
fitem$c1<-NULL

br1<-rep(c("[["),each=nrow(fitem))
nf<-rep(c(name_of_filler),nrow(fitem))
nf<-gsub('^','"',nf)
nf<-gsub('$','",',nf)
item<-as.character(rep(c(startfrom:(nrow(fitem)+startfrom-1))))
br2<-rep(c("],"),each=nrow(fitem))
nmOftask<-"DashedSentence"
nmOftask<-gsub('^','"',nmOftask)
nmOftask<-gsub('$','",',nmOftask)
task<-rep(c(nmOftask),each=nrow(fitem))
s<-rep(c("{s:"),each=nrow(fitem))
br3<-rep(c("["),each=nrow(fitem))
nOfcol<-ncol(fitem)
nOfrow<-nrow(fitem)
list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
for (j in 1:nOfcol){
  d1<-fitem[j]
  d1<-as.character(t(d1))
  d1<-gsub('_' ,' ',d1)
  d1<-gsub('^','"',d1)
  d1<-gsub('$','",',d1)
  list[,j]<-d1
}
material2<-ifelse(list=='"",'|list=='""',"",list)
material2<-gsub('([:.:])([:":])([:,:])','."',material2)
material2<-data.frame(material2)
br4<-rep(c("]},"),each=nrow(fitem))
nmOfq<-"Question"
nmOfq<-gsub('^','"',nmOfq)
nmOfq<-gsub('$','",',nmOfq)
cq<-rep(c(nmOfq),each=nrow(fitem))
br5<-rep(c("{q:"),each=nrow(fitem))
question<-material[material$c1=="fq:",]
question$c1<-NULL
qs=rep(c("a"),times=nrow(question))
qe=rep(c("z"),times=nrow(question))
qs<-gsub('^a','"',qs)
qe<-gsub('z$','",',qe)
question<-cbind(qs,question,qe)
as<-rep(c("as:["),each=nrow(fitem))

qoption<-material[material$c1=="fo:",]
qoption$c1<-NULL
list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
for (j in 1:nOfcol){
  d1<-qoption[j]
  d1<-as.character(t(d1))
  d1<-gsub(',','","',d1)
  list[,j]<-d1
}
qoption<-ifelse(list=='"",'|list=='""',"",list)
qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
qoption<-data.frame(qoption)
qs=rep(c("a"),times=nrow(question))
qe=rep(c("z"),times=nrow(question))
qs<-gsub('^a','"',qs)
qe<-gsub('z$','"',qe)
qoption<-cbind(qs,qoption,qe)
qoption<-qoption %>% mutate_if(is.character, na_if, "")
qoption<-qoption %>% select_if(all_na)

list<-matrix(rep(NA,nOfrow*ncol(qoption)),ncol=ncol(qoption))
for (j in 1:nrow(qoption)){
  d1<-qoption[j,]
  d1<-d1 %>% select_if(all_na)
  d1<-as.matrix(d1)
  list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
}
qoption<-ifelse(list=='"",'|list=='""',"",list)
qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
qoption<-data.frame(qoption)

correct_response<-material[material$c1=="fco:",]
correct_response$c1<-NULL
qs=rep(c("a"),times=nrow(correct_response))
qe=rep(c("z"),times=nrow(correct_response))
qs<-gsub('^a','"',qs)
qe<-gsub('z$','"',qe)
correct_response<-cbind(qs,correct_response,qe)
correct_response<-correct_response %>% mutate_if(is.character, na_if, "")
correct_response<-correct_response %>% select_if(all_na)
list<-matrix(rep(NA,nOfrow*ncol(correct_response)),ncol=ncol(correct_response))
for (j in 1:nrow(correct_response)){
  d1<-correct_response[j,]
  d1<-d1 %>% select_if(all_na)
  d1<-as.matrix(d1)
  list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
}
correct_response<-ifelse(list=='"",'|list=='""',"",list)
correct_response<-gsub('([:.:])([:":])([:,:])','."',correct_response)
correct_response<-data.frame(correct_response)

br6<-rep(c("], hasCorrect:"),each=nrow(fitem))
br7<-rep(c("}],"),each=nrow(fitem))

fdat<-data.frame(br1,nf,item,br2,task,s,br3,material2,br4,cq,br5,question,as,qoption,br6,correct_response,br7)

fdat<-fdat %>% mutate_if(is.character, na_if, "")
fdat<-fdat %>% select_if(all_na)
colnames(fdat)<-1:ncol(fdat)

if(nrow(material[material$c1=="fnq:",])>0){
  
  fnqitem<-material[material$c1=="fnq:",]
  fnqitem$c1<-NULL
  
  br1<-rep(c("[["),each=nrow(fnqitem))
  nf<-rep(c(name_of_filler),nrow(fnqitem))
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  item2<-as.numeric(item)
  item2<-as.character((max(item2)+1):(max(item2)+nrow(fnqitem)))
  br2<-rep(c("],"),each=nrow(fnqitem))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(fnqitem))
  s<-rep(c("{s:"),each=nrow(fnqitem))
  br3<-rep(c("["),each=nrow(fnqitem))
  nOfcol<-ncol(fnqitem)
  nOfrow<-nrow(fnqitem)
  list<-matrix(rep(NA,nOfrow*nOfcol),ncol=nOfcol)
  for (j in 1:nOfcol){
    d1<-fnqitem[j]
    d1<-as.character(t(d1))
    d1<-gsub('_' ,' ',d1)
    d1<-gsub('^','"',d1)
    d1<-gsub('$','",',d1)
    list[,j]<-d1
  }
  material2<-ifelse(list=='"",'|list=='""',"",list)
  material2<-gsub('([:.:])([:":])([:,:])','."',material2)
  material2<-data.frame(material2)
  br4<-rep(c("]}],"),each=nrow(fnqitem))
  fdat2<-data.frame(br1,nf,item2,br2,task,s,br3,material2,br4)
  fdat2<-fdat2 %>% mutate_if(is.character, na_if, "")
  fdat2<-fdat2 %>% select_if(all_na)
  
  colnames(fdat2)<-1:ncol(fdat2)
  fdat2[is.na(fdat2)] <- " "
  
  fdat<-dplyr::bind_rows(fdat,fdat2)
  fdat[is.na(fdat)] <- " "
}

fdat[is.na(fdat)] <- " "
write.table(fdat,"spr_indiv_f.js",quote=FALSE,row.names=FALSE,col.names=FALSE)


}


maze_indiv_ex<-function(material=NULL,
               number_of_condition=NULL,
               name_of_experimental_material=NULL,
               startfrom=NULL){
  
  n<-1:ncol(material)
  colnames(material)<-paste("c",n,sep="")
  
  sitem<-material[material$c1=="s:",]
  sitem$c1<-NULL
  sset<-nrow(sitem)/number_of_condition
  br1<-rep(c("[["),each=nrow(sitem))
  ncond2<-rep(c(1:number_of_condition),times=sset)
  cond<-paste(name_of_experimental_material,ncond2,sep="")
  cond<-gsub('^','"',cond)
  cond<-gsub('$','",',cond)
  item<-as.character(rep(c(startfrom:(sset+startfrom-1)),each=number_of_condition))
  br2<-rep(c("],"),each=nrow(sitem))
  nmOftask<-"Maze"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(sitem))
  se<-rep(c("{s:"),each=nrow(sitem))
  
  list<-matrix(rep(NA,nrow(sitem)*ncol(sitem)),ncol=ncol(sitem))
  for (j in 1:nrow(sitem)){
    d1<-sitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  sitem<-ifelse(list=='"",'|list=='""',"",list)
  sitem<-gsub('([:.:])([:":])([:,:])','."',sitem)
  sitem<-data.frame(sitem)
  
  comma=rep(c(","),times=nrow(sitem))
  
  a<-rep(c("a:"),each=nrow(sitem))
  smitem<-material[material$c1=="sm:",]
  smitem$c1<-NULL
  list<-matrix(rep(NA,nrow(smitem)*ncol(smitem)),ncol=ncol(smitem))
  for (j in 1:nrow(smitem)){
    d1<-smitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  smitem<-ifelse(list=='"",'|list=='""',"",list)
  smitem<-gsub('([:.:])([:":])([:,:])','."',smitem)
  smitem<-data.frame(smitem)
  
  br4<-rep(c("}],"),each=nrow(sitem))
  sdat<-data.frame(br1,cond,item,br2,task,se,sitem,comma,a,smitem,br4)
  sdat<-sdat %>% mutate_if(is.character, na_if, "")
  sdat<-sdat %>% select_if(all_na)
  colnames(sdat)<-1:ncol(sdat)
  sdat[is.na(sdat)] <- " "
  write.table(sdat,"maze_indiv_ex.js",quote=FALSE,row.names=FALSE,col.names=FALSE)
}


maze_indiv_f<-function(material=NULL,
                      name_of_filler=NULL,
                      startfrom=NULL){
  
  n<-1:ncol(material)
  colnames(material)<-paste("c",n,sep="")
  
  fitem<-material[material$c1=="f:",]
  fitem$c1<-NULL
  
  br1<-rep(c("[["),each=nrow(fitem))
  nf<-rep(c(name_of_filler),nrow(fitem))
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  item<-as.character(rep(c(startfrom:(nrow(fitem)+startfrom-1))))
  br2<-rep(c("],"),each=nrow(fitem))
  nmOftask<-"Maze"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(fitem))
  se<-rep(c("{s:"),each=nrow(fitem))
  
  list<-matrix(rep(NA,nrow(fitem)*ncol(fitem)),ncol=ncol(fitem))
  for (j in 1:nrow(fitem)){
    d1<-fitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  fitem<-ifelse(list=='"",'|list=='""',"",list)
  fitem<-gsub('([:.:])([:":])([:,:])','."',fitem)
  fitem<-data.frame(fitem)
  
  comma=rep(c(","),times=nrow(fitem))
  
  a<-rep(c("a:"),each=nrow(fitem))
  fmitem<-material[material$c1=="fm:",]
  fmitem$c1<-NULL
  
  list<-matrix(rep(NA,nrow(fitem)*ncol(fmitem)),ncol=ncol(fmitem))
  for (j in 1:nrow(fmitem)){
    d1<-fmitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  fmitem<-ifelse(list=='"",'|list=='""',"",list)
  fmitem<-gsub('([:.:])([:":])([:,:])','."',fmitem)
  fmitem<-data.frame(fmitem)
  
  br4<-rep(c("}],"),each=nrow(fitem))
  
  fdat<-data.frame(br1,nf,item,br2,task,se,fitem,comma,a,fmitem,br4)
  
  fdat<-fdat %>% mutate_if(is.character, na_if, "")
  fdat<-fdat %>% select_if(all_na)
  colnames(fdat)<-1:ncol(fdat)
  
  fdat[is.na(fdat)] <- " "
  
  fdat[is.na(fdat)] <- " "
  write.table(fdat,"maze_indiv_f.js",quote=FALSE,row.names=FALSE,col.names=FALSE)
  
}


cq_indiv_ex<-function(material=NULL,
                      number_of_condition=NULL,
                      name_of_experimental_material=NULL,
                      startfrom=NULL){
  
  n<-1:ncol(material)
  colnames(material)<-paste("c",n,sep="")
  
  sitem<-material[material$c1=="s:",]
  sitem$c1<-NULL
  sset<-nrow(sitem)/number_of_condition
  br1<-rep(c("[["),each=nrow(sitem))
  ncond2<-rep(c(1:number_of_condition),times=sset)
  cond<-paste(name_of_experimental_material,ncond2,sep="")
  cond<-gsub('^','"',cond)
  cond<-gsub('$','",',cond)
  item<-as.character(rep(c(startfrom:(sset+startfrom-1)),each=number_of_condition))
  br2<-rep(c("],"),each=nrow(sitem))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(sitem))
  se<-rep(c("{s:"),each=nrow(sitem))
  br3<-rep(c("["),each=nrow(sitem))
  list<-matrix(rep(NA,nrow(sitem)*ncol(sitem)),ncol=ncol(sitem))
  for (j in 1:nrow(sitem)){
    d1<-sitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  sitem<-ifelse(list=='"",'|list=='""',"",list)
  sitem<-gsub('([:.:])([:":])([:,:])','."',sitem)
  material2<-data.frame(sitem)
  br4<-rep(c("]},"),each=nrow(sitem))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(sitem))
  br5<-rep(c("{q:"),each=nrow(sitem))
  
  question<-material[material$c1=="sq:",]
  question$c1<-NULL
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','",',qe)
  question<-cbind(qs,question,qe)
  
  as<-rep(c("as:["),each=nrow(sitem))
  
  qoption<-material[material$c1=="so:",]
  qoption$c1<-NULL
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:ncol(qoption)){
    d1<-qoption[j]
    d1<-as.character(t(d1))
    d1<-gsub(',','","',d1)
    list[,j]<-d1
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  qoption<-cbind(qs,qoption,qe)
  qoption<-qoption %>% mutate_if(is.character, na_if, "")
  qoption<-qoption %>% select_if(all_na)
  
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:nrow(qoption)){
    d1<-qoption[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  
  correct_response<-material[material$c1=="sco:",]
  correct_response$c1<-NULL
  qs=rep(c("a"),times=nrow(correct_response))
  qe=rep(c("z"),times=nrow(correct_response))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  correct_response<-cbind(qs,correct_response,qe)
  correct_response<-correct_response %>% mutate_if(is.character, na_if, "")
  correct_response<-correct_response %>% select_if(all_na)
  list<-matrix(rep(NA,nrow(correct_response)*ncol(correct_response)),ncol=ncol(correct_response))
  for (j in 1:nrow(correct_response)){
    d1<-correct_response[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  correct_response<-ifelse(list=='"",'|list=='""',"",list)
  correct_response<-gsub('([:.:])([:":])([:,:])','."',correct_response)
  correct_response<-data.frame(correct_response)
  
  br6<-rep(c("], hasCorrect:"),each=nrow(sitem))
  br7<-rep(c("}],"),each=nrow(sitem))
  sdat<-data.frame(br1,cond,item,br2,task,se,br3,material2,br4,cq,br5,question,as,qoption,br6,correct_response,br7)
  sdat<-sdat %>% mutate_if(is.character, na_if, "")
  sdat<-sdat %>% select_if(all_na)
  colnames(sdat)<-1:ncol(sdat)
  sdat[is.na(sdat)] <- " "
  write.table(sdat,"cq_indiv_ex.js",quote=FALSE,row.names=FALSE,col.names=FALSE)
}


cq_indiv_f<-function(material=NULL,
                     name_of_filler=NULL,
                     startfrom=NULL){
  
  n<-1:ncol(material)
  colnames(material)<-paste("c",n,sep="")
  
  fitem<-material[material$c1=="f:",]
  fitem$c1<-NULL
  br1<-rep(c("[["),each=nrow(fitem))
  nf<-rep(c(name_of_filler),nrow(fitem))
  nf<-gsub('^','"',nf)
  nf<-gsub('$','",',nf)
  item<-as.character(rep(c(startfrom:(nrow(fitem)+startfrom-1))))  
  br2<-rep(c("],"),each=nrow(fitem))
  nmOftask<-"DashedSentence"
  nmOftask<-gsub('^','"',nmOftask)
  nmOftask<-gsub('$','",',nmOftask)
  task<-rep(c(nmOftask),each=nrow(fitem))
  se<-rep(c("{s:"),each=nrow(fitem))
  br3<-rep(c("["),each=nrow(fitem))
  
  list<-matrix(rep(NA,nrow(fitem)*ncol(fitem)),ncol=ncol(fitem))
  for (j in 1:nrow(fitem)){
    d1<-fitem[j,]
    d1<-d1 %>% mutate_if(is.character, na_if, "")
    d1<-d1 %>% select_if(all_na)
    s<-dim(d1)[1]
    s<-d1[[s]]
    s<-as.character(s)
    s<-gsub('^','"',s)
    e<-dim(d1)[2]
    e<-d1[[e]]
    e<-as.character(e)
    e<-gsub('$','"',e)
    d2<-d1
    d2<-d2[-dim(d2)[1]]
    d2<-d2[-dim(d2)[2]]
    d1<-cbind(s,d2,e)
    
    for (i in 1:dim(d1)[2]){
      list[j,i]<-d1[[i]]
    }
  }
  fitem<-ifelse(list=='"",'|list=='""',"",list)
  fitem<-gsub('([:.:])([:":])([:,:])','."',fitem)
  material2<-data.frame(fitem)
  
  br4<-rep(c("]},"),each=nrow(fitem))
  nmOfq<-"Question"
  nmOfq<-gsub('^','"',nmOfq)
  nmOfq<-gsub('$','",',nmOfq)
  cq<-rep(c(nmOfq),each=nrow(fitem))
  br5<-rep(c("{q:"),each=nrow(fitem))
  question<-material[material$c1=="fq:",]
  question$c1<-NULL
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','",',qe)
  question<-cbind(qs,question,qe)
  as<-rep(c("as:["),each=nrow(fitem))
  
  qoption<-material[material$c1=="fo:",]
  qoption$c1<-NULL
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:ncol(qoption)){
    d1<-qoption[j]
    d1<-as.character(t(d1))
    d1<-gsub(',','","',d1)
    list[,j]<-d1
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  qs=rep(c("a"),times=nrow(question))
  qe=rep(c("z"),times=nrow(question))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  qoption<-cbind(qs,qoption,qe)
  qoption<-qoption %>% mutate_if(is.character, na_if, "")
  qoption<-qoption %>% select_if(all_na)
  
  list<-matrix(rep(NA,nrow(qoption)*ncol(qoption)),ncol=ncol(qoption))
  for (j in 1:nrow(qoption)){
    d1<-qoption[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  qoption<-ifelse(list=='"",'|list=='""',"",list)
  qoption<-gsub('([:.:])([:":])([:,:])','."',qoption)
  qoption<-data.frame(qoption)
  
  correct_response<-material[material$c1=="fco:",]
  correct_response$c1<-NULL
  qs=rep(c("a"),times=nrow(correct_response))
  qe=rep(c("z"),times=nrow(correct_response))
  qs<-gsub('^a','"',qs)
  qe<-gsub('z$','"',qe)
  correct_response<-cbind(qs,correct_response,qe)
  correct_response<-correct_response %>% mutate_if(is.character, na_if, "")
  correct_response<-correct_response %>% select_if(all_na)
  list<-matrix(rep(NA,nrow(correct_response)*ncol(correct_response)),ncol=ncol(correct_response))
  for (j in 1:nrow(correct_response)){
    d1<-correct_response[j,]
    d1<-d1 %>% select_if(all_na)
    d1<-as.matrix(d1)
    list[j,1:dim(d1)[2]]<-d1[1:dim(d1)[2]]
  }
  correct_response<-ifelse(list=='"",'|list=='""',"",list)
  correct_response<-gsub('([:.:])([:":])([:,:])','."',correct_response)
  correct_response<-data.frame(correct_response)
  
  br6<-rep(c("], hasCorrect:"),each=nrow(fitem))
  br7<-rep(c("}],"),each=nrow(fitem))
  
  fdat<-data.frame(br1,nf,item,br2,task,se,br3,material2,br4,cq,br5,question,as,qoption,br6,correct_response,br7)
  
  fdat<-fdat %>% mutate_if(is.character, na_if, "")
  fdat<-fdat %>% select_if(all_na)
  colnames(fdat)<-1:ncol(fdat)
  fdat[is.na(fdat)] <- " "
  
  if(nrow(material[material$c1=="fnq:",])>0){
    
    fnqitem<-material[material$c1=="fnq:",]
    fnqitem$c1<-NULL
    
    br1<-rep(c("[["),each=nrow(fnqitem))
    nf<-rep(c(name_of_filler),nrow(fnqitem))
    nf<-gsub('^','"',nf)
    nf<-gsub('$','",',nf)
    item2<-as.numeric(item)
    item2<-as.character((max(item2)+1):(max(item2)+nrow(fnqitem)))
    br2<-rep(c("],"),each=nrow(fnqitem))
    nmOftask<-"DashedSentence"
    nmOftask<-gsub('^','"',nmOftask)
    nmOftask<-gsub('$','",',nmOftask)
    task<-rep(c(nmOftask),each=nrow(fnqitem))
    se<-rep(c("{s:"),each=nrow(fnqitem))
    br3<-rep(c("["),each=nrow(fnqitem))
    
    list<-matrix(rep(NA,nrow(fnqitem)*ncol(fnqitem)),ncol=ncol(fnqitem))
    for (j in 1:nrow(fnqitem)){
      d1<-fnqitem[j,]
      d1<-d1 %>% mutate_if(is.character, na_if, "")
      d1<-d1 %>% select_if(all_na)
      s<-dim(d1)[1]
      s<-d1[[s]]
      s<-as.character(s)
      s<-gsub('^','"',s)
      e<-dim(d1)[2]
      e<-d1[[e]]
      e<-as.character(e)
      e<-gsub('$','"',e)
      d2<-d1
      d2<-d2[-dim(d2)[1]]
      d2<-d2[-dim(d2)[2]]
      d1<-cbind(s,d2,e)
      
      for (i in 1:dim(d1)[2]){
        list[j,i]<-d1[[i]]
      }
    }
    fnqitem<-ifelse(list=='"",'|list=='""',"",list)
    fnqitem<-gsub('([:.:])([:":])([:,:])','."',fnqitem)
    material2<-data.frame(fnqitem)
    
    br4<-rep(c("]}],"),each=nrow(fnqitem))
    fdat2<-data.frame(br1,nf,item2,br2,task,se,br3,material2,br4)
    fdat2<-fdat2 %>% mutate_if(is.character, na_if, "")
    fdat2<-fdat2 %>% select_if(all_na)
    
    colnames(fdat2)<-1:ncol(fdat2)
    fdat2[is.na(fdat2)] <- " "
    
    fdat<-dplyr::bind_rows(fdat,fdat2)
    fdat[is.na(fdat)] <- " "
  }
  
  fdat[is.na(fdat)] <- " "
  write.table(fdat,"cq_indiv_f.js",quote=FALSE,row.names=FALSE,col.names=FALSE)
  
  
}
