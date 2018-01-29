fix_ts <- function(ts, cbs, groep) {
  # Herstel spaties in de ts-codering op basis van de cbs-codering,
  # en controleer op geldige keys in de tijdreekscodering.
  #
  # Bij het inlezen van de ts-codering worden spaties aan het begin en
  # einde van de teksten altijd verwijderd. Dit is niet het geval
  # voor de cbs-codering. Dat is ook niet mogelijk, omdat deze spaties
  # ook gebruikt moeten worden voor de filters die doorgegeven worden
  # aan de functie get_data. Om deze reden voegen we hier weer de
  # spaties toe.
  #
  # controleer op ts-keys die niet in de cbs-codering voorkomen:
  cbs_key_trim <- trimws(cbs$Key)
  onbekende_keys <- setdiff(ts$Key, cbs_key_trim)
  if (length(onbekende_keys) > 0) {
    sheetnaam <- substr(groep, 1, 31)
    stop(paste0("Onbekende keys in de tijdreekscodes in sheet \"",
                sheetnaam, "\":\n", 
                paste(onbekende_keys, collapse = "\n")), "\n.")
  }
  
  # Pas nu de ts-keys aan: als er spaties aan het begin/einde stonden, dan 
  # in de tijdreekscode-keys dezelfde spaties gebruiken.
  sel <- match(ts$Key, cbs_key_trim)
  ts$Key <- cbs$Key[sel]
  return(ts)
}
