#This creates a reclassification matrix for a vector of length n with n values.
subst_mat<-function(clss_names, from, to, time){
clssname <- class_names #(val or vector)
oldclass <- from #(val or vector)                                                                                                                                                                                                                                                                                                
rec_df<-c(oldclass, oldclass) #creates dataframe. The value remains the same                                                                                                                                                                                                                                          
df <- data.frame(matrix(length(to), data=rec_df))
  return(df)}
###########################                                                                                                                                        

  clssname : vector con la lista de clases (char)
  oldclass : vector con la(s) clase(s) a sustituir
  newclass : vector con la(s) clase(s) nueva(s)
# Esto se usa para mapear.   
time : vector con las unidades de tiempo
