character2factor <- function(data) {
  data <- as.data.frame(data)
  dataIsCharacter <- sapply(data, is.character)
  if (any(dataIsCharacter)) {
    dataCharacterColumns <- names(dataIsCharacter[dataIsCharacter])
    for (dataCol in dataCharacterColumns) {
      data[[dataCol]] <- as.factor(data[[dataCol]])
    }
  }

  data
}