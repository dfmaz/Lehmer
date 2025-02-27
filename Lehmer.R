### M�todo de Lehmer
lehmer = function(x0, c, long)
{
  n = floor(log10(x0)) + 1 # N�mero de d�gitos de x0
  k = floor(log10(c)) + 1 # N�mero de d�gitos de c
  x = numeric(long) # Vector para almacenar los n�meros aleatorios
  x[1] = x0 # Asignamos x0 al primer valor de x
  for (i in 2:long)
  {
    prod = x[i-1]*c # Producto de x*c
    left = prod%/%10^(n) # Parte izquierda (primeros c d�gitos de prod)
    right = prod%%10^(n) # Parte derecha (los restantes)
    x[i] = right - left # N� Aleatorio
  }
  x/10^n # Reescalado
}

### Generaci�n de n�meros aleatorios
x0 = 5673
c = 34
long = 5
datos = lehmer(x0, c, long)

### Estudio de la calidad del generador

## Representaciones gr�ficas
# Representaci�n de la secuencia de valores
plot(datos, cex=0.2)
# Una observaci�n frente a la siguiente
plot(datos[-long], datos[-1], cex=0.2, asp=1)
# Representaci�n en 3 dimensiones
library(plot3D)
points3D(datos[1:(long-2)], datos[2:(long-1)], datos[3:long],
         bty='f', cex=2, pch='.', col='black', phi=0, d=3, theta=150)

## Diagrama de autocorrelaci�n
acf(datos, ylim=c(-0.05,0.05))

## Contrastes de bondad de ajuste
# Chi^2
k = 10 
datoschi = table(trunc(datos*k)) 
chisq.test(datoschi)
# Kolmogorov
ks.test(datos, y='punif')

## Contrastes de independencia
# Box
Box.test(datos, 3)

## Contrastes de aleatoriedad
# Rachas
library(randtests)
runs.test(datos, threshold = mean(datos))
# Huecos
randtoolbox::gap.test(datos, lower=1/3,upper=2/3)
# Saltos
randtoolbox::order.test(datos[-long])
# Poker
randtoolbox::poker.test(datos)
