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

############################################################
# Aplicando y entendiendo la desviacion estandar

# para conocer la varianza de una fila

sin_castigo$varS <- apply(sin_castigo[,2:17], 1, var)

sin_castigo$sdS <- apply(sin_castigo[,2:17], 1, sd)
con_castigo$varC <- apply(con_castigo[,2:17], 1, var)
con_castigo$sdC <- apply(con_castigo[,2:17], 1, sd)


# Conocemos como intervalo de confianza a la situacion en la que un cierto porcentaje de los datos
# está dentro de dos desviaciones estándar de la media. Como tenemos 16 países en cada periodo
# un intervalo de confianza significa que, para que el resultado no sea sólo producto de la casualidad...
# podríamos esperar que cerca de una observación esté fuera de este parámetro.

# Que hace esta linea de código?
lista_ciudades <- names(sin_castigo[2:17])

plot(sin_castigo$Period, sin_castigo$promedio, type = "l",
     col = "blue", lwd = 2, xlab = "Ronda",
     ylim = c(0,20), ylab = "Contribucion promedio")
# Explique con sus palabras lo que la siguiente linea de codigo realiza
lines(sin_castigo$promedio + 2 * sin_castigo$sdS,
      col = "red",
      lwd = 2) # Cambia el valor en esta linea y comenta lo que sucede
#
#

lines(sin_castigo$promedio - 2 * sin_castigo$sdS,
      col = "red",
      lwd = 2)

for(i in lista_ciudades) {
  points(sin_castigo[[1]], sin_castigo[[i]])
}


title("Ponle un titulo")

legend("bottomleft", legend = c("Promedio", "+/- 2 sd"),
       col = c("blue", "red"), lwd = 2, lty = 1, cex = 1.2)


#############################
# Encontrar los valores maximos y minimos
apply(sin_castigo[, 2:17], 1, range)


sin_castigo$minimo <- apply(sin_castigo[,2:17], 1, min)
sin_castigo$maximo <- apply(sin_castigo[,2:17], 1, max)

# Con el siguiente codigo haremos una tabla resumen
print("Juego de bienes publicos sin castigo")
round(sin_castigo[c(1, 10), c(1, 18:22)], digits = 2)


# La opcion del castigo se introdujo en el juego de los bienes publicos para
# ver si esto ayudaría a aumentar las contribuciones en comparacion de la opcion sin castigo
# Calcularemos el p-valor (https://es.wikipedia.org/wiki/Valor_p) para comparar
# los resultados de ambos experimentos de forma mas formal.

# Comparando los resultados del periodo 10, podemos ver que la contribucion media
# en el experimento con castigo es ____ unidades mayor que el experimento sin castigo
# Pero... esto podría ser debido al azar, no?

# Para ver cómo esto puede ser resultado del azar, considere el siguiente experimento.

# a) Lanza una moneda seis veces usando únicamente una mano (un experimento se debe repetir
# replicando las mismas condiciones) y registra los resultados. Luego, usando la misma mano,
# tira la moneda seis veces y registra los resultados.

# b) Compara los resultados de la pregunta (a) Obtuviste el mismo numero de caras
# en los dos casos? Incluso si si, la secuencia de los resultados (el orden) fue el mismo?


###############################################################
# Pruebas de hipótesis
# Calculo del valor p para la diferencia en medias

# Primero extraigamos la observacion del periodo 1 para la informacion con y sin
# castigo

p1_Sin <- sin_castigo[1,2:17]
p1_Con <- con_castigo[1,2:17]


t.test(x = p1_Sin, y = p1_Con) # corre esta linea de codigo
# desafortunadamente, esto arroja un error
# Error: Must use a vector in `[`, not an object of class matrix.

# esto es debido a que p1_Sin y p1_Con aun son 'tibbles' (bases de datos)
# y la funcion t.test() requiere que x y y sean variables. Esto lo podemos lograr
# usando la funcion t(). Si estas familiarizada(o) con el álgebra lineal, 
# esto es similar a usar la transpuesta de un vector.

p1_Sin <- t(sin_castigo[1,2:17])
p1_Con <- t(con_castigo[1,2:17])


t.test(x = p1_Sin, y = p1_Con) 

# Que podemos inferir de este valor p?


# Realiza lo mismo para el periodo 10.



##################################
# Discute las limitaciones de los experimentos y sugiere formas de sobreponerse a ellas
# (considera para esto la lectura de las paginas 158 a la 171 del siguiente paper
# de Levitt y List https://sites.duke.edu/niou/files/2012/04/Levitt-List_Experiments_2007.pdf)