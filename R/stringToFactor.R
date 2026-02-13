##' Function to convert any character columns in a data.frame to factors. Used
##' within NMwork:::EtaPairsUpperTriangle(), NMwork:::EtaPairsScatMat()  and
##' NMwork::EtaPairsLowerTriangle.
##'  Taken with minimal modifications from
##' GGally::upgrade_scatmat_data().
##' @export
##' 
stringToFactor = function(data) {
  data <- as.data.frame(data)
  dataIsCharacter <- sapply(data, is.factor)
  if (any(dataIsCharacter)) {
    dataCharacterColumns <- names(dataIsCharacter[dataIsCharacter])
    for (dataCol in dataCharacterColumns) {
      data[[dataCol]] <- as.factor(data[[dataCol]])
    }
  }
  
  return(data)
}