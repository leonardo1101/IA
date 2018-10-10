source("Trajeto.R")
##source("buscaDesinformada.R")
source("buscaInformada.R")

cidades <- c( "A", "B", "C", "D", "F", "G", "L", "M", "O", "P", "R", "S", "T", "U", "Z")

distanciasReais <- function() {
  m <- matrix(c(
    0,0,0,0,0,0,0,0,0,0,0,140,118,0,75
    ,0,0,0,0,211,90,0,0,0,101,0,0,0,85,0
    ,0,0,0,120,0,0,0,0,0,138,146,0,0,0,0
    ,0,0,120,0,0,0,0,75,0,0,0,0,0,0,0
    ,0,211,0,0,0,0,0,0,0,0,0,99,0,0,0
    ,0,90,0,0,0,0,0,0,0,0,0,0,0,0,0
    ,0,0,0,0,0,0,0,70,0,0,0,0,111,0,0
    ,0,0,0,75,0,0,70,0,0,0,0,0,0,0,0
    ,0,0,0,0,0,0,0,0,0,0,0,151,0,0,71
    ,0,101,138,0,0,0,0,0,0,0,97,0,0,0,0
    ,0,0,146,0,0,0,0,0,0,97,0,80,0,0,0
    ,140,0,0,0,99,0,0,0,151,0,80,0,0,0,0
    ,118,0,0,0,0,0,111,0,0,0,0,0,0,0,0
    ,0,0,85,0,0,0,0,0,0,0,0,0,0,0,0
    ,75,0,0,0,0,0,0,0,71,0,0,0,0,0,0
  ), nrow=15, ncol=15)
  rownames(m, do.NULL = FALSE)
  rownames(m) <- cidades
  colnames(m, do.NULL = FALSE)
  colnames(m) <- cidades
  return(m)
}


distanciasLinhaReta <- function(){
  distancias <- t(c(366,0,160,242,178,77,244,241,380,98,193,253,329,80,374)) ## t é utilizado para transformar em matriz e ser possível nomear as colunas
  colnames(distancias, do.NULL=FALSE)
  colnames(distancias) = cidades
  return(distancias)
}

inicial <- Trajeto(desc = "A", distanciasReais = distanciasReais, distanciasLinhaReta = distanciasLinhaReta)
final <- Trajeto(desc = "B", distanciasReais = distanciasReais, distanciasLinhaReta = distanciasLinhaReta)


##cat("====\tBusca em Largura\t====\n")
##print(unlist(buscaEmLargura(inicial, final)))

##cat("====\tBusca em Profundidade\t=====\n")
##print(buscaEmProfundidade(inicial, final))

##cat("====\tBusca de Custo Uniforme\t=====\n")
##print(buscaCustoUniforme(inicial, final))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, final, "Gulosa"))

cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, final, "AEstrela"))