histoFloods_Anduze=read.table('HistoData_Anduze.txt',header=T)
histoFloods_Anduze$Value[histoFloods_Anduze$Value<0]<-NA
