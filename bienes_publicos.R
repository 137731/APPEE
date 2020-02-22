#install.packages(c("tidyverse", "readxl")) # Advertencia. No corras esta linea de codigo si
# ya descargaste estos paquetes 

library(tidyverse)
library(readxl)

getwd() # Revisa que el directorio que te arroja es el correcto

sin_castigo <- read_excel("Public-goods-experimental-data.xlsx",
                     range = "A2:Q12")
con_castigo <- read_excel("Public-goods-experimental-data.xlsx",
                     range = "A16:Q26")

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

con_castigo$promedio <- apply(con_castigo[,2:17],1,mean) # Cambiamos a que usara apply a la base con castigo



plot(sin_castigo$Period, sin_castigo$promedio, type = "l",
     col = "blue", lwd = 2, 
     xlab = "Ronda", ylab = "Contribucion Promedio")

lines(con_castigo$promedio, col = "red", lwd = 2)

title("Contribuciones promedio en el juego de bienes publicos")
legend("bottomleft", lty = 1, cex = 1.2, lwd = 2,
       legend = c("Sin castigo", "Con castigo"),
       col = c("blue", "red"))

# Otra forma de hacer los graficos con ggplot
library(ggplot2)

ggplot(data.frame(Period = sin_castigo$Period,
                  sin_castigo = sin_castigo$promedio,
                  con_castigo = con_castigo$promedio), aes(x = Period))+
  geom_line(aes(y = sin_castigo), color = "blue")+
  geom_line(aes(y = con_castigo), color = "red")+
  labs(x = "Ronda", y = "Contribucion promedio",
       title = "Contribuciones promedio en el juego de bienes publicos")


##############################################################
# Segunda parte. ¿Cómo se comparan las contribuciones al inicio y al final del periodo?

# Primero, extraemos en un solo vector las contribuciones promedio al inicio y al final en los dos juegos
contribuciones <- c(sin_castigo$promedio[1], # Esta es la contribucion inicial sin castigo
                    sin_castigo$promedio[10],# Esta es la contribucion final sin castigo
                    con_castigo$promedio[1], # Que es esto?
                  con_castigo$promedio[10]) # Que es esto?

contribuciones # Trata de adivinar lo que hará esta línea de código antes de ejecutarla

# Ahora la podemos acomodar como una matriz. Esto permitirá realizar fácilmente el gráfico

contribuciones <- matrix(contribuciones,
                         nrow = 2,
                         byrow = TRUE)
contribuciones


###################################
# Ahora realicemos un gráfico de barras
barplot(contribuciones,
        main = "Contribuciones promedio en un juego de bienes publicos",
        ylab = "Contribuciones",
        beside = TRUE, col = c("Blue", "Red"),
        names.arg = c("Ronda 1", "Ronda 10"))
legend("bottomleft", pch = 1, col = c("Blue", "Red"),
       c("Sin Castigo", "Con castigo"))

