# Function select_table_dialog created a modal dialog UI for selecting
# tables from a list of table descriptions.
# The id determines the input slots that can be used to access the
# components of the dialog:
# Input slots are: 
#    id + "_ok"     ok button pressed
#    id + "_desc"   selected table
#
# INPUT
#  id:    character string that determines the input slots connected to this
#         dialog (see text above
#  label: label
# table_descriptions: a character vector with table descriptions.
select_table_dialog <- function(id, label, table_descriptions) {
  nopts <- length(table_descriptions)
  modalDialog(
    selectizeInput(paste0(id, "_desc"), label = label, 
                choices = table_descriptions, width = "200%",
                options = list(maxOptions = nopts)),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(paste0(id, "_ok"), "OK")
    ),
    easyClose = TRUE
  )
}