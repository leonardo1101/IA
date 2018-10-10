##source("Estado.R")

## Classe e métodos para o problema do trajeto entre duas cidades
Trajeto <- function(desc = NULL, pai = NULL, distanciasReais = NULL, distanciasLinhaReta = NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("distanciasReais", distanciasReais, envir = e)
  assign("distanciasLinhaReta", distanciasLinhaReta, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Trajeto", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Trajeto = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Trajeto <- function(obj) {
  cat("(cidade atual): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Criação do método genérico "heuristica"
heuristica <- function(atual) {
  UseMethod("heuristica")
}

## Função padrão para o método genérico "heuristica", implementada para o problema específico
heuristica.default <- function(atual) {
  dlr <- distanciasLinhaReta()
  return(dlr[1,atual$desc])
}


## Criação do método genérico "geraFilhos"
geraFilhos <- function(obj) {
  UseMethod("geraFilhos")
}


## Função padrão para o método genérico "geraFilhos", implementada para o problema específico
geraFilhos.default <- function(obj) {
  filhos <- list()
  
  vizinhos <- t(obj$distanciasReais()[obj$desc,])  ## t é utilizado para transformar em matriz
  
  filhosDesc <- c(colnames(vizinhos)[apply(vizinhos,1,function(vizinhos){which(vizinhos != 0, arr.ind=T)})]) ## vetor com os vizinhos que não possuem valor 0
  
  for(filhoDesc in filhosDesc){
    filho <- Trajeto(desc=filhoDesc, pai=obj, distanciasReais=obj$distanciasReais, distanciasLinhaReta=obj$distanciasLinhaReta)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + vizinhos[1,filhoDesc]
    filho$f <- filho$h + filho$g
    filhos <- c(filhos, list(filho))
  }
  return(filhos)
}