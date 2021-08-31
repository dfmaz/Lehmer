## Integración de Montecarlo
g = function(x) sqrt(1+x^4)*(x>-2)*(x<2)
integrate(g,a,b)

# Uniforme
n = 10^3
a = -2
b = 2
ui = runif(n,a,b)
mean(g(ui))*(b-a)
sd(g(ui))/sqrt(n) 

# Mejora de la eficiencia
ui = runif(n)
f1 = function(x) cosh(x)/(2*sinh(2))
xi = asinh((ui-0.5)*2*sinh(2))
g1 = function(x) g(x)/f1(x)
g1i = g1(xi)
mean(g1i)
sd(g1i)/sqrt(n)

