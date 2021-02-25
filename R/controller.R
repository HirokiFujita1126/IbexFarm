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