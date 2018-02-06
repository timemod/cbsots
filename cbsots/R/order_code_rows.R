order_code_rows <- function(code, orig_key_order) {
  selected <- code$Key[code$Select]
  not_selected <- code$Key[!code$Select]
  cat("orig_key_order\n")
  print(orig_key_order)
  selected_ordered <- selected[match(intersect(orig_key_order, selected),
                                     selected)]
  not_selected_ordered <- not_selected[match(intersect(orig_key_order, 
                                                       not_selected), 
                                             not_selected)]
  required_order <- c(selected_ordered, not_selected_ordered) 
  cat("required_order\n")
  print(required_order)
  order <-  match(required_order, code$Key)
  return(code[order, , drop = FALSE])
}