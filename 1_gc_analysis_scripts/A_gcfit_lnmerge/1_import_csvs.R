import_csvs<-function(folder_name,obj_name){
  
  #folder_name is full address of folder containing all csv files to import
  file_names<-list.files(folder_name,pattern="*.csv")
  
  #was not able to get function to work when called, but once the varibles were set,
    #the for loop works when executed 
  
  for (i in 1:length(file_names)){
    
    x<-read.csv(paste0(folder_name,file_names[i]),header = FALSE)
    
    assign(paste0(obj_name,i),x)
    
  }
  
}

