# Ejercicio 4
# Función de densidad
f = function(x)
  3*x^2

# Método de inversión
long = 10^4
u = runif(long)
lna = u^(1/3)
hist(lna, breaks='FD', freq=FALSE)
curve(f(x), col='red', lwd=2, add=TRUE)