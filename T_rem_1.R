datos = c(0.143, 0.182, 0.256, 0.26, 0.27,
          0.437, 0.509, 0.611, 0.712, 1.04,
          1.09, 1.15, 1.46, 1.88, 2.08)

hist(datos)
n = length(datos)
alpha = 0.05 
B = 10^4

media = mean(datos)
s = sd(datos)

# Clásico
z = qt(1 - alpha/2, n-1)
ic_izq = media - z*s/sqrt(n)
ic_dch = media + z*s/sqrt(n)
IC = c(ic_izq, ic_dch)
IC

# Bootstrap
est = function(datos)
  sqrt(n)*(mean(datos)-media)/sd(datos)
estB = replicate(B, est(sample(datos, n, replace = TRUE)))
pto_crit = quantile(estB, c(alpha/2, 1 - alpha/2))
ext_izq = media - pto_crit[2]*s/sqrt(n)
ext_dch = media - pto_crit[1]*s/sqrt(n)
IC = c(ext_izq, ext_dch)
IC

# Intervalo de confianza para la media (Remuestreo)
estB = replicate(B, mean(datos[sample(n,n,replace=TRUE)]))
quantile(estB, c(alpha/2, 1-alpha/2))

# Por qué se parecen los resultados?
hist(estB, freq = FALSE, ylim = c(0, 0.4))
lines(density(estB))
curve(dt(x, n-1), add=TRUE, lty = 2)

t.test(datos)$conf.int

