################################################################################
# Define  header  
# 
################################################################################
dm <- dropdownMenuOutput("messages")
mm <- dropdownMenuOutput("notifications")
tm <- dropdownMenuOutput("tasks")

# Header elements for the visualization
header <- dashboardHeader(title = "CyTOF Analysiser v.0", dm, mm, tm)
