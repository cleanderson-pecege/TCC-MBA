# Função para ler todas as imagens e retornar a taxa de germinação
############
# Anotações
# Circularidade = 4*pi *(area / perimeter^2) == relacionada com o perímetro e a área. 
# Ecentricidade = sqrt(1-minoraxis^2/majoraxis^2) == o quanto o ajuste de um objeto se aproxima de uma esfera perfeita ou vai se transformando em uma elipse
# solidez = area / area_ch  == relação da área do objeto em relação a area do poligono convexo

seed_percentage <- function(path_images){
  
  # Leitura dos pacotes
  library(pliman)
  library(tidyverse)
  library("EBImage")
  
  # declarando as quantidades necessárias
  
  tx_germ <- NULL
  numberGerm <- NULL
  numberSeeds <- NULL
  
  # lendo todas as imagens como uma lista
  
  img_train <- list.files(path = path_images, pattern = ".jpg", 
                          all.files = TRUE,full.names = TRUE,no.. = TRUE)
  
  ## image list
  img_list <- lapply(img_train, readImage)
  
  # laço para calcular a taxa de aceitação por imagem
  
  for(i in 1:length(img_list)){
    
    #img4 <- image_import(img_list[[1]])
    new_img <- NULL
    # diminuindo a imagem
    ix <- 80:1800
    iy <- 35:600
    new_img <- img_list[[i]][ix,iy,c(1,2,3)]
    
    r1_meas <- analyze_objects(new_img,
                               index = "B",
                               marker = "id",
                               invert = F,
                               tolerance = 35,
                               col_background = "white",
                               col_foreground = "black",
                               parallel = TRUE)
    
    # Primeira filtragem
    results <- as.data.frame(r1_meas$results) %>% filter(radius_mean<60)
    
    # laço para testar se há ou não um ponto de calibração na imagem
    
    if(dim(results)[1]==20){
      # sem levar em consideração os pontos de calibração
      results <- as.data.frame(r1_meas$results) %>% filter(radius_mean<60)
    }else{
      # levando em consideração os pontos de calibração
      results <- as.data.frame(r1_meas$results) %>%
        filter(radius_mean<60) %>%
        filter(radius_ratio>sort(radius_ratio)[1])
    }
    
    # contando o número de sementes
    
    numberSeeds[i] <- dim(results)[1]
    
    # Contando o número das que germinaram
    
    auxVar <- ifelse(results[c("solidity")]<1.01,1,0)
    
    numberGerm[i] <- sum(auxVar)
    
    # Taxa de germinação
    tx_germ[i] <- 100*(numberGerm[i]/numberSeeds[i])
  }
  
  cat("A Taxa de germinação geral é:", 
      round(100*(sum(numberGerm)/sum(numberSeeds)),2),
      "%", "\n")
  
  # resumindo as informações coletadas
  resumo <- data.frame("NumdeSementes" = numberSeeds,
                       "Germinadas" = numberGerm,
                       "Taxa" = round(tx_germ,2))
}
