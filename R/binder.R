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

