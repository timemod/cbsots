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
select_table_dialog <- function(id, label, table_descriptions, 
                                old_table_descriptions) {
  nopts <- length(table_descriptions)
  choices <- create_table_choices(table_descriptions)
  old_table_choices <- c("Do not use base table" = "", old_table_descriptions)
  old_table_nopts <- length(old_table_descriptions)
  modalDialog(
    h3(label),
    "You can enter a search query in the text field below.",
    "When necessary, use Backspace to erase the text field.",
    selectizeInput(paste0(id, "_desc"), label = "",
                choices = choices, width = "200%",
                options = list(maxOptions = nopts)),
    p(),
    "Optionally specify an existing table (the \"base table\") used to fill in",
    "the new table. Leave the text field empty to create an empty new table",
    selectizeInput(paste0(id, "_base_desc"), label = "",
                   choices = old_table_choices, width = "200%",
                   options = list(maxOptions = old_table_nopts)),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(paste0(id, "_ok"), "OK")
    ),
    easyClose = TRUE
  )
}