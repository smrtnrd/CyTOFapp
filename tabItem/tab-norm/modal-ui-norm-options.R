# ui norm select input
user_select_ninput <- uifiles(
  "uselected_file",
  width = "100%",
  size = 10,
  selectize = F,
  multiple = T
)

# ui norm dna channel
beads_select_ninput <- uichannel(
  "nbeads_channel",
  "Beads Channels",
  selectize = F,
  multiple = T
)
dna_select_ninput<- uichannel(
  "ndna_channel",
  "Dna Channels",
  selectize = F,
  multiple = F
)

# ui upload files
user_nupload <- fileInput('upload_nfile', "Add a file", multiple = TRUE, width = "100%")

# ui remove files
remove_nfile_button <- actionButton("abutton_remove_file", "remove", width = "100")

# ui run app files
run_nAbutton <- actionButton("abutton_run_norm", "create Plots", width = "100%", inline = T)

################################################################################
# Define  options modal dor normalisation module  
################################################################################
norm_bsModal <- bsModal( 
  "norm_options", "Please set the parameter for normalisation", "abutton_show_opt", size = "large",
  fluidRow(
    column(width = 4,
           user_select_ninput,
           user_nupload
  ),
    column(width = 4,
          beads_select_ninput,
          remove_nfile_button
  ),
  column(width = 4,
         dna_select_ninput,
         run_nAbutton
  ))
)
