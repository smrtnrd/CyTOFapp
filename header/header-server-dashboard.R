################################################################################
# Define the notifications
################################################################################

notifs.msg <- data.frame(
  matrix(vector(), 0, 2, dimnames = list(c(), c("text", "status"))), 
  stringsAsFactors =  F)

tasks.msg <- data.frame(
  matrix(vector(), 0, 3, dimnames = list(c(), c("text","value", "color"))), 
  stringsAsFactors = F)

notifs.msg[nrow(notifs.msg) + 1,] <- list("thi is a test", "info")
tasks.msg[nrow(tasks.msg) + 1,] <- list("Creating Flowset",50, "blue")

################################################################################
# Define the notifications
################################################################################

addNotif <- function(text,status, notifs.msg) {
  l <- nrow(notifs.msg) + 1
  if (is.null(notifs.msg)) {
    notifs.msg[l,] <- list("info","info")
  }else{
    notifs.msg[l,] <- list(text,status)
  }
}

doNotifs <- reactive({
  if (nrow(notifs.msg) > 0) {
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageNotif is a data frame with 3 columns, 'text', 'icon' and 'status'.
    notifs <- apply(notifs.msg, 1, function(row) {
      notificationItem(text = row["text"],  status = row["status"])
    })
  }
})

# notification ===============================================================
output$notifications <- renderMenu({
  #checkData()
  #DropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
  dropdownMenu(
    type = "notifications", .list = doNotifs(), icon = icon("fa-question")
  )
})


################################################################################
# Define the task
################################################################################
addTask <- function(text, value, color, tasks.msg) {
  if (is.null(tasks.msg)) {
    tasks.msg[1,] <- list("tst",80 ,"Red")
  }else{
    l <- nrow(tasks.msg) + 1
    tasks.msg[l,] <- list(text,value,color)
    cat(paste(tasks.msg))
  }
}

# update task
updateTask <- function(id, value, text = NULL, tasks.msg) {
  if (is.null(tasks.msg)) {
    tasks.msg$value[id] <- value
    if (!is.null(text)) {
      tasks.msg$text[id] <- text
      cat(paste(tasks.msg$text[id],"\n\n\n\n\n\n\n\n"))
    }
  }
}

output$tasks <- renderMenu({
  # checkData()
  # This is equivalent to calling:
  # DropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
  dropdownMenu(type = "tasks",  badgeStatus = "success", .list = doTasks())
  
})

# add tasks ===============================================================
doTasks <- reactive({
  # Code to generate each of the messageItems here, in a list. This assumes
  # that messageNotif is a data frame with 3 columns, 'text', 'icon' and 'status'.
  if (!is.null(v$nrow.task)) {
    tasks <- apply(tasks.msg, 1, function(row) {
      taskItem(text = row["text"], value = as.numeric(row["value"]),  color = row["color"])
    })
  }
})