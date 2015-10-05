################################################################################
# Define the available Flowframe or Flowset datasets for plotting.
################################################################################

get_loaded_data = reactive({
  if(!is.null(input$upload_nfile$name) || !is.null(fileNames())){
    # Add uploaded data, if provided, and it is phyloseq-class.
    # Load user-data into a new environment (essentially sandbox)
    read.flowSet(fcs_user_dir,alter.names = "T")
    env_userdata = new.env()
    objectNames = load(input$file1$datapath, envir = env_userdata)
    loadedObjects = mget(objectNames, envir = env_userdata)
    arePhyloseq = sapply(loadedObjects, inherits, "phyloseq")
    if(any(arePhyloseq)){
      loadedObjects <- loadedObjects[which(arePhyloseq)]
    } else {
      loadedObjects <- NULL
    }
    datalist <<- c(loadedObjects, datalist)
  }
  return(NULL)
})


