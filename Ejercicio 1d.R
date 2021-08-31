# Ejercicio 1d
ej1d = function(p, b1, b2, n) {
  # Función de densidad
  f = function(x) p*b1*exp(-b1*x)+(1-p)*b2*exp(-b2*x)
  # Método de composición
  u1 = runif(long)
  u2 = runif(long)
  lna = ifelse(u1 < p, -log(u2)/b1, -log(u2)/b2) # Si p es grande simularemos muchas veces valores de f1 y pocas de f2
  hist(lna, freq = FALSE)
  curve(f, add=TRUE, col='red', from = 0, to = 10)
}

long = 10^4
ej1d(0.9, 0.5, 1.5, long)
