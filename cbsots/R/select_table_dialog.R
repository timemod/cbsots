# Function select_table_dialog creates a modal dialog UI for selecting
# tables from a list of table descriptions. Argument id determines the input 
# slots that can be used to access the components of the dialog:
#    id + "_ok"     ok button pressed
#    id + "_desc"   selected table
#
# INPUT
#  id                 : character string that determines the input slots 
#                       connected to this dialog (see text above)
#  label:             : label (description of the dialog)
#  table_descriptions : a character vector with table descriptions
select_table_dialog <- function(id, label, table_descriptions) {
 
  choices <- create_table_choices(table_descriptions)
  
  modalDialog(
    h3(label),
    "You can enter a search query in the text field below.",
    "When necessary, use Backspace to erase the text field.",
    selectizeInput(paste0(id, "_desc"), label = "",
                   choices = choices, width = "200%",
                   options = list(maxOptions = length(choices))),
    p(),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(paste0(id, "_ok"), "OK")
    ),
    easyClose = TRUE
  )
}

# Function that creates a dialog asking for a table and an optional base table.
select_new_table_dialog <- function(table_descriptions, 
                                    base_table_descriptions) {
 
  choices <- create_table_choices(table_descriptions)
  base_table_choices <- c("Do not use base table" = "", base_table_descriptions)

  modalDialog(
    h3("New Table"),
    "You can enter a search query in the text field below.",
    "When necessary, use Backspace to erase the text field.",
    selectizeInput("new_table_desc", label = "",
                choices = choices, width = "200%",
                options = list(maxOptions = length(choices))),
    p(),
    "Optionally specify an existing table (the \"base table\") used to fill in",
    "the new table. Leave the text field empty to create an empty new table.",
    "When necessary, use Backspace to erase the text field.",
    selectizeInput("new_table_base_desc", label = "",
                   choices = base_table_choices, width = "200%",
                   options = list(maxOptions = length(base_table_choices))),
    p(),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("new_table_ok", "OK")
    ),
    easyClose = TRUE
  )
}