
#install.packages("devtools")
#devtools::install_github("yukiyanai/rgamer")
library(rgamer)

#FUNCION PARA GRAFICAR

graficador<-function(a,b,l,f){
  x <- seq(a, b, length.out = l)
  y <- x
  z <- outer(x, y, f)
  z[z<0]=0
  op <- par(bg = "white")
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
        ltheta = 120, shade = 0.75, ticktype = "detailed",zlab = f
  ) -> res
}


#FUNCION DE PRECIO funcióon relu 

price_function<-function(p_1, p_2, k=200){
  return(1 - (
    exp(p_1 / k ) /
      (exp(p_1 / k ) + exp( p_2 / k ))
    )
  )
}



graficador(3,1000,60,"price_function")

 
#FUNCION DE DECISON
 

dicotomic_function_ps<-function(c_ps, c_xb){
  return(
     (1 -c_ps) * ( 1 - c_xb ) * 0.6+
      c_ps * (1 - c_xb) * 0.8 +
      c_xb * (1 - c_ps) * 0.45+
      c_ps * c_xb * 0.4
  )
}

dicotomic_function_xb<-function(c_ps, c_xb){
  return(
     (1 -c_ps) * ( 1 - c_xb ) * 0.4+
      c_ps * (1 - c_xb) * 0.35 +
      c_xb * (1 - c_ps) * 0.6+
     c_ps * c_xb * 0.2
  )
}
 
#CONTROL DE PRECIO
 

price_control<-function(p, m=1000){
  return(0+
           ( p < m )*
           ( 
             (
             ( p - m ) / m ) ^ 2 
             ) 
  )
}

price_control_ps<-function(p, m=2000,c_ps, c_xb){
  m=m*dicotomic_function_ps(c_ps ,c_xb)
  return(0+
           ( p < m )*
           ( 
             (
               ( p - m ) / m ) ^ 2 
           ) 
  )
}

price_control_xb<-function(p, m=2000,c_ps, c_xb){
  m=m*dicotomic_function_xb(c_ps ,c_xb)
  return(0+
           ( p < m )*
           ( 
             (
               ( p - m ) / m ) ^ 2 
           ) 
  )
}



 
#FUNCIONES DE VENTAS
 

ventas_ps<-function(p_ps, p_xb, c_ps=1, c_xb=0, k=400){
  return(
    20 * 
      dicotomic_function_ps(c_ps, c_xb ) * 
      price_function(p_ps, p_xb, k) *
      price_control(p_ps)
  )
}

ventas_xb<-function(p_ps, p_xb, c_ps=0, c_xb=0, k=400){
  return(
    20*
      dicotomic_function_xb(c_ps, c_xb)*
      price_function(p_xb, p_ps, k)* 
      price_control(p_xb)
  )
}
###############
ventas_ps<-function(p_ps, p_xb, c_ps=1, c_xb=0, k=400,m=2000){
  return(
    20 * 
      dicotomic_function_ps(c_ps, c_xb ) * 
      price_function(p_ps, p_xb, k) *
      price_control_ps(p_ps, m, c_ps, c_xb)
  )
}

ventas_xb<-function(p_ps, p_xb, c_ps=0, c_xb=0, k=400, m=2000){
  return(
    20*
      dicotomic_function_xb(c_ps, c_xb)*
      price_function(p_xb, p_ps, k)* 
      price_control_xb(p_xb, m, c_ps, c_xb)
  )
}


graficador(1,1000,60,"ventas_ps")

ventas_ps(100,100, c_ps=0, c_xb=1)
ventas_xb(100,100)
 
#FUNCIONES DE BENEFICIOS
 

beneficios_ps<-function( p_ps, p_xb, c_ps=0, c_xb=1, k=400 , coste = 100 ){
  ventas_ps(p_ps , p_xb , c_ps=c_ps , c_xb=c_xb , k=k ) *
    ( p_ps -  coste )
}

beneficios_xb<-function( p_ps , p_xb , c_ps=0 , c_xb=0 , k=400 , coste = 70 ){
  ventas_xb( p_ps , p_xb , c_ps=c_ps , c_xb=c_xb , k=k ) * 
    ( p_xb - coste )
}

graficador(1,1000,60,"beneficios_ps")

beneficios_ps(100,100)
 
#FUNCIONES DE RESOLUCIÓN
 

resolucion<- function( coste_ps , coste_xb , c_ps = 0 , c_xb = 0 , x0=0 , y0=0 , N=100 ){
  z = matrix(0,nrow=2,ncol=N+1); z[,1] <- c(x0,y0)
  for(i in 1:N){
    z[1,i+1] <- optim(z[1,i],function(x){-beneficios_ps( x , z[2,i] , c_ps , c_xb , coste = coste_ps )})$par
    z[2,i+1] <- optim(z[2,i],function(y){-beneficios_xb( z[1,i+1] , y , c_ps , c_xb , coste = coste_xb ) })$par
  }
  print(z[,i+1])
  return(z[,i+1]*c(
    ventas_ps(coste_ps, coste_xb, c_ps = c_ps , c_xb = c_xb),
    ventas_xb(coste_ps, coste_xb, c_ps = c_ps , c_xb = c_xb)
  ))
}

resolucion(100,150, c_ps = 0 , c_xb = 1)

#SOLUCIÓN

coste_ps = 100
coste_xb = 150

game1b <- normal_form(
  players = c("PlayStation", "Xbox"),
  s1 = c("No hace crossplay", "Hace crossplay"), 
  s2 = c("No hace crossplay", "Hace crossplay"),
  cells = list(resolucion(coste_ps, coste_xb), 
               resolucion(coste_ps, coste_xb, c_ps = 0 , c_xb = 1),
               resolucion(coste_ps, coste_xb, c_ps = 1 , c_xb = 0),
               resolucion(coste_ps, coste_xb, c_ps = 1 , c_xb = 1)
               ),
  byrow = TRUE)
game1b
s_game1 <- solve_nfg(game1b, mixed = TRUE, show_table = TRUE)

s_game1$br_plot
solucion<-function(coste_ps = 100, coste_xb = 100){
  game1b <- normal_form(
    players = c("PlayStation", "Xbox"),
    s1 = c("No hace crossplay", "Hace crossplay"), 
    s2 = c("No hace crossplay", "Hace crossplay"),
    cells = list(resolucion(coste_ps, coste_xb), 
                 resolucion(coste_ps, coste_xb, c_ps = 0 , c_xb = 1),
                 resolucion(coste_ps, coste_xb, c_ps = 1 , c_xb = 0),
                 resolucion(coste_ps, coste_xb, c_ps = 1 , c_xb = 1)
    ),
    byrow = TRUE)
  return(solve_nfg(game1b, mixed = TRUE, show_table = TRUE))
}

sol=solucion(100,500)

sol$br_plot

sol$msNE_prob


#ANÁLISIS DE SENSIBILIDAD

a=20
b=100
l=8+1


for (coste_xb in seq(a, b, length.out = l)) {
  
}







