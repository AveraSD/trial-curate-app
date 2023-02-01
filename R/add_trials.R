library(data.table)
library(DT)
library(dplyr)
# function to write trial.ndjson after final Submit button is clicked

outSubmit <- function() {
  #tr2 <- isolate(disAd$resultsdf)
  # print(tr)
  tr2 <- isolate(disAd$rsdf)
  #print(tr2)
  
  outjson <- paste0(here(trial_data_dir), 
                    paste0(tr2 %>% unnest(c(info, disease, query)) %>% select(NCT) %>% as.character(), ".full.ndjson"))
  
  # Connect to the MongoDB server
  mongo <- mongo(db = "aci", 
                 collection = "ClinicalTrials", 
                 url = "mongodb://127.0.0.1:27017")
  
  
  writeLines(tr2 %>% toJSON(pretty = T), outjson)
  
  json_data_file <- do.call(rbind, 
                            lapply(paste(readLines(outjson, warn=FALSE),
                                         collapse=""), 
                                   jsonlite::fromJSON))
  mongo$insert(as.data.frame(json_data_file))
  
  
  
  #message(paste0("Written to file: ", outjson))
}

