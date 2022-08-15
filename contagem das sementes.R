# este algoritmo está contando corretamente a quantidade de sementes de soja na imagem.

# Existem dois casos para as figuras, por enquanto:
# 1 - Quando as imagens não apresentam raiz == sum(x) > 1800.
# 2 - Quando as imagens apresentam alguma raiz == sum(x) > 2110.

library(imager)

image <- load.image("A2-B-20-48H.jpg")

R<-R(image); G<-G(image); B<-B(image)
ExGreen<-G-R-B
plot(ExGreen)

list(R,G,B)

# blur before thresholding to fill some gaps
ExGreen <- isoblur(ExGreen, 3)
ExGreen <- threshold(ExGreen, thr="auto", 
                     approx=FALSE, adjust=1)
plot(ExGreen)

# split into connected component and keep only large CCs
ccs <- split_connected(ExGreen)
largeccs <- purrr::keep(ccs, 
                        function(x) {sum(x) > 2110})
#plot(add(largeccs))

# count CCs
cat(sprintf("Number of large CCs: %i\n", length(largeccs)))
