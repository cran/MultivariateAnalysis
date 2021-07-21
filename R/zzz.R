#' @importFrom utils setTxtProgressBar txtProgressBar
# --------------------------------------
# on loading MultivariateAnalysis
.welcome <- function(text = NULL)
{
  if(is.null(text))
    text <- "MultivariateAnalysis!"
  if(!inherits(text, "character") || length(text) != 1)
    stop("'text' must be a character vector of length 1!")
  vec <- strsplit(text, "")[[1]]
  lab <- c(vec, "\n")
  for(i in 1:length(lab)) {
    setTxtProgressBar(txtProgressBar(char = lab[i]), 0.005)
    Sys.sleep(0.05)
  }
}
Mensagem=function(){
  {
    cat("_____________________________________________________________________","\n")
    cat("Obrigado por utilizar o MultivariateAnalysis","\n")
    cat("Veja tutoriais sobre este e outros pacotes no canal: ","\n")
    cat("https://www.youtube.com/channel/UCDGyvLCJnv9RtTY1YMBMVNQ","\n")
    cat("Duvidas e sugestoes podem nos ser enviados por essa midia.","\n")
    cat("Se inscreva no canal e compartilhe nossos materiais com os colegas para ajudar nosso canal a crescer.","\n")
    cat("Alcinei Mistico Azevedo (ICA-UFMG).","\n")
    cat("_____________________________________________________________________","\n")
    cat("","\n")
  }
}


.onAttach <- function(lib, pkg)
{
  vers <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  packageStartupMessage(.welcome(paste("MultivariateAnalysis version", vers)))
  packageStartupMessage(Mensagem())
}
