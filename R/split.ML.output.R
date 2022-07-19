# Script for dividing and cleaning up raw output from MassLynx.
  # Sample text is divided into "Type", "Sample", and "Time. Separated by "_"

# Library ----
if(require(tidyverse) == FALSE) {                          # Check for tidyverse and either load
  install.packages("tidyverse")                            #  the package or install it
  library(tidyverse)
}else{
  library(tidyverse)
}

# Load file ----
sourceFile = file.choose()                                 # Choose source file
dataLines = 
  read_lines(sourceFile,                                   # Load file
             skip_empty_rows = T) %>%
  as.list()                                                # Coerce file into list
dataLines[[length(dataLines)+1]] = "END"                   # Add marker for end of file

# Create variables ----
fileSep = "\t"                                             # Delimiter in file
tempDF = ""                                                # Stores temp df
tempDF_name = ""                                           # Stores df name
tempDF_header = ""                                         # Stores header
tempDF_rows = list()                                       # Stores lines for temp df
listDF = list()                                            # List to store compiled df:s in
checkNewDF = FALSE                                         # Create Boolean check for 1st df

# Split file by data frame ----
for(i in 1:length(dataLines)) {                            # Iterate over data lines
  if( str_detect(dataLines[[i]], "^Compound") == TRUE ) {  # Check if line is Compound name
    if( checkNewDF == TRUE ) {                             # Check if df needs to be compiled
      tempDF =                                             # Compile df
        tempDF_rows %>% 
        map_dfr(function(x) {
          x = unlist(x) 
          x = set_names(x, tempDF_header[[1]]) 
          x
        } ) %>% 
        as_tibble()
      listDF[[tempDF_name]] = tempDF                       # Save df in list
    }
    checkNewDF = TRUE                                      # New df started
    tempDF_name =                                          # Save compound name
      gsub(": ", "", dataLines[[i]])                       # Remove ": "
    
    tempDF_header = dataLines[[i+1]]                       # Save header
    tempDF_rows = list()                                   # Empty tempDF_rows
    next                                                   # Skip to next iteration/line
  }
  if( dataLines[[i]] == tempDF_header ) {                  # Check if line is header
    tempDF_header = str_split(tempDF_header, fileSep)      # Split header into columns
    next                                                   # Skip to next iteration/line
  }
  if( str_detect(dataLines[[i]], "^END") == TRUE ) {       # Check if line is end of file
    tempDF =                                               # Compile df
      tempDF_rows %>% 
      map_dfr(function(x) {
        x = unlist(x) 
        x = set_names(x, tempDF_header[[1]]) 
        x
      } ) %>% 
      as_tibble()
    listDF[[tempDF_name]] = tempDF                         # Save df in list
    tempDF_rows = list()                                   # Empty tempDF_rows
    break                                                  # End for loop
  }
  new_row = str_split(dataLines[[i]], fileSep)             # Split line by delimiter
  tempDF_rows[[length(tempDF_rows)+1]] = new_row           # Add new row to list
}                                                          # Repeat for loop

# Clean data frames ----
for(i in 1:length(listDF)) {
  listDF[[i]] =
    listDF[[i]] %>%
    select(Vial, Name, `Sample Text`,                      # Keep columns
           RT, Response, Area, nM) %>%         
    filter(`Sample Text` != "blank") %>%                   # Drop "blank"
    
    
    # filter(Response != "") %>%                            # Drop "" (NA)
    # drop_na(Response) %>%                                 # Drop NA
    
    
    separate(col = `Sample Text`,                          # Split "Sample text" into columns
             into = c("Compound",                          # List of new columns
                      "Type/Timepoint", 
                      "Replicate"),  
             sep = "_")# %>%                                # Separator to split by
    # mutate(Type_Sample = str_c(Type, Sample,               # Combine Type & Sample again.
    #                            sep = "_")) %>%
    # select(Type_Sample, Time, Response) %>%
    
    
    # group_by(Type_Sample) %>%                              # Hot fix to pivot wider more nicely.
    # mutate(row = row_number()) %>%
    # pivot_wider(names_from = Type_Sample,                  # Reshape with pivot_wider()
    #             values_from = Response) %>%
    # select(!row)
  
  
}

# Write each data frame to a separate .txt file ----
old_directory = getwd()                                    # Save current working directory
sourceFile %>%
  dirname() %>%                                            # Get directory of source file
  setwd()                                                  # Set file directory as working directory
sourceFile_name =                                          # Name of source file
  sourceFile %>%
  basename()
sourceFile_name =                                          # Replace file extension
  gsub("\\..{1, }$", " - ", sourceFile_name)

for(i in 1:length(listDF)) {
  newFileName =                                            # Concatenate new file name
    paste(sourceFile_name, names(listDF[i]), ".txt",
          sep = "")
  listDF[[i]] %>%                                          # Write to file
    write_tsv(file = newFileName)
}
setwd(old_directory)                                       # Go back to old working directory

# DONE! :)