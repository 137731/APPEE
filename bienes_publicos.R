#install.packages(c("tidyverse", "readxl")) # Advertencia. No corras esta linea de codigo si
# ya descargaste estos paquetes 

library(tidyverse)
library(readxl)

getwd() # Revisa que el directorio que te arroja es el correcto

sin_castigo <- read_excel("Public-goods-experimental-data.xlsx",
                     range = "A2:Q12")
con_castigo <- read_excel("Public-goods-experimental-data.xlsx",
                     range = "A2:Q12")

# Ejercicio
# Calcula la contribucion promedio

# Metodo 1: usando un ciclo for.
# Ver: https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r?utm_source=adwords_ppc&utm_campaignid=1658343524&utm_adgroupid=63833881375&utm_device=c&utm_keyword=%2Bfor%20%2Bloop%20%2Br&utm_matchtype=b&utm_network=g&utm_adpostion=1t1&utm_creative=319519154568&utm_targetid=aud-522010995285:kwd-341515386944&utm_loc_interest_ms=&utm_loc_physical_ms=1010047&gclid=Cj0KCQiAkKnyBRDwARIsALtxe7jT7Ilc2XlL5rj9P1Jaj-qJCuFj_jd6g5tHhwLAdMKFSpkhmgWsKfoaAvoiEALw_wcB
nrow(sin_castigo) # muestra el numero de filas de la tabla sin_castigo
rowMeans(sin_castigo[1,2:17]) # cambia el 1 por el numero de periodo del que quieres conocer
# la contribucion promedio


sin_castigo$promedio <- 0


for (fila in 1:nrow(sin_castigo)) {
  sin_castigo$promedio[fila] <- rowMeans(sin_castigo[fila,2:17])
}

# Metodo 2: Usando la función apply
?apply

sin_castigo$promedio <- apply(sin_castigo[,2:17],1,mean)



plot(sin_castigo$Period, sin_castigo$promedio, type = "l",
     col = "blue", lwd = 2, 
     xlab = "Ronda", ylab = "Contribucion Promedio")

lines(con_castigo$promedio, col = "red", lwd = 2)

title("Contribuciones promedio en el juego de bienes publicos")
legend("bottomleft", lty = 1, cex = 1.2, lwd = 2,
       legend = c("Sin castigo", "Con castigo"),
       col = c("blue", "red"))



####################
tabla <- c(sin_castigo$promedio[1], sin_castigo$promedio[10],
           con_castigo$promedio[1], con_castigo$promedio[10])

tabla <- matrix(tabla, nrow = 2, byrow = TRUE)

barplot(tabla, 
        main = "Cambia esto al resultado correcto",
        ylab = "Que deberia decir aqui?",
        beside = TRUE, col = c("Blue", "Red"), 
        names.arg = c("Round 1", "Round 10"))
legend("bottomleft", pch = 1, col = c("Blue", "Red"),
       c("Sin castigo", "?????"))