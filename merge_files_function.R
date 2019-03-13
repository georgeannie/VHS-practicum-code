#------------------------------------------------------------------------------------#
#                 MERGE FILES FUNCTION                                              #  
#------------------------------------------------------------------------------------#
merge_files =function (file_list){
  for (file in file_list){
    
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset_name")){
      dataset_name= read.csv(file, header=TRUE, na.strings=c(""," ", "NA"), stringsAsFactors = FALSE)
      
    }
    
    # if the merged dataset does exist, append to it
    if (exists("dataset_name")){
      temp_dataset = read.csv(file, header=TRUE, na.strings=c(""," ", "NA"), stringsAsFactors = FALSE)
      dataset_name = rbind(dataset_name, temp_dataset)
      rm(temp_dataset)
    }
  }
  return(dataset_name)
}

