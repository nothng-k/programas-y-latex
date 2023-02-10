
#install.packages("devtools")
#devtools::install_github("yukiyanai/rgamer")
install.packages("gt")
library(gt)
library(rgamer)
library(ggplot2)
library(stats, methods,grDevices )
#FUNCION PARA GRAFICAR

graficador<-function(a,b,l,f){
  x <- seq(a, b, length.out = l)
  y <- x
  z <- outer(x, y, f)
  z[z<0]=0
  op <- par(bg = "white")
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
        ltheta = 120, shade = 0.75,zlab = f,
        xlab=formalArgs(args(f))[1],ylab=formalArgs(args(f))[2]
  ) -> res
}


#FUNCION DE PRECIO función relu 

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
    50000000 * 
      dicotomic_function_ps(c_ps, c_xb ) * 
      price_function(p_ps, p_xb, k) *
      price_control_ps(p_ps, m, c_ps, c_xb)
  )
}

ventas_xb<-function(p_ps, p_xb, c_ps=0, c_xb=0, k=400, m=2000){
  return(
    50000000*
      dicotomic_function_xb(c_ps, c_xb)*
      price_function(p_xb, p_ps, k)* 
      price_control_xb(p_xb, m, c_ps, c_xb)
  )
}


graficador(1,1000,60,"ventas_ps")

ventas_ps(100,100, c_ps=0, c_xb=1)
ventas_xb(100,100)
 
#FUNCIONES DE BENEFICIOS
 

beneficios_ps<-function( p_ps, p_xb, c_ps=0, c_xb=0, k=400,m=2000 ){
  ventas_ps(p_ps , p_xb , c_ps=c_ps , c_xb=c_xb , k=k,m=m ) *
    ( p_ps -  100 )
}

beneficios_xb<-function( p_ps , p_xb , c_ps=0 , c_xb=0 , k=400,m=2000 ){
  ventas_xb( p_ps , p_xb , c_ps=c_ps , c_xb=c_xb , k=k,m=m) * 
    ( p_xb - 100 )
}

graficador(1,1000,60,"beneficios_ps")

 
#FUNCIONES DE RESOLUCIÓN
 

resolucion<- function( c_ps = 0 , c_xb = 0 ,k=400,m=2000, x0=0 , y0=0 , N=100 ){
  z = matrix(0,nrow=2,ncol=N+1); z[,1] <- c(x0,y0)
  for(i in 1:N){
    z[1,i+1] <- optim(z[1,i],function(x){
            -beneficios_ps( x , z[2,i] , c_ps , c_xb ,k,m )})$par
    z[2,i+1] <- optim(z[2,i],function(y){
            -beneficios_xb( z[1,i+1] , y , c_ps , c_xb,k,m )})$par
  }
  return(list(
    'beneficios'=c(
    beneficios_ps(p_ps=z[1,i+1], p_xb=z[2,i+1], c_ps = c_ps , c_xb = c_xb,k,m),
    beneficios_xb(p_ps=z[1,i+1], p_xb=z[2,i+1], c_ps = c_ps , c_xb = c_xb,k,m)),
    'precios'=z[,i+1]
  )
  )
}

res=resolucion( c_ps = 1 , c_xb = 0,k=400,m=2000)
res
#SOLUCIÓN

solucion<-function(k=400,m=2000){
  #calculamos los diferentes ejercicios de Bertrand
  r1=resolucion(k=k,m=m)
  r2=resolucion(k=k,m=m, c_ps = 0 , c_xb = 1)
  r3=resolucion(k=k,m=m, c_ps = 1 , c_xb = 0)
  r4=resolucion(k=k,m=m, c_ps = 1 , c_xb = 1)
  #almacenamos los precios que hemos obtenido
  precios=normal_form(
    players = c("PlayStation", "Xbox"),
    s1 = c("No hace crossplay", "Hace crossplay"), 
    s2 = c("No hace crossplay", "Hace crossplay"),
    cells =list(
      r1$precios, 
      r2$precios,
      r3$precios,
      r4$precios
    ) ,
    byrow = TRUE)
  #almacenamos los beneficios que hemos obtenido
  formanormal=normal_form(
        players = c("PlayStation", "Xbox"),
        s1 = c("No hace crossplay", "Hace crossplay"), 
        s2 = c("No hace crossplay", "Hace crossplay"),
        cells =list(
          r1$beneficios, 
          r2$beneficios,
          r3$beneficios,
          r4$beneficios
        ) ,
        byrow = TRUE)
  solution2x2=solve_nfg(#función que resuelve el problema 2x2 del paquete rgamer
    formanormal,#introducimos los beneficios
    mixed = TRUE)
  return(list(
    'solucion2x2'=solution2x2,#resultado de la función solve_nfg
    #obtenemos los beneficios esperados para cada jugador
    'beneficiosesperados'=list('ps'=solution2x2$msNE_prob$p%*%
                                 formanormal$mat$matrix1%*%
                                 cbind(solution2x2$msNE_prob$p),
                               'xb'=solution2x2$msNE_prob$p%*%
                                 formanormal$mat$matrix2%*%
                                 cbind(solution2x2$msNE_prob$p)),
    #obtenemos los precios que pone cada jugador en cada opción
    'preciosps'= precios$mat$matrix1,
    'preciosxb'= precios$mat$matrix2
    ))
}

sol=solucion()
#datos obtenidos
sol$beneficiosesperados
sol$solucion2x2$msNE_prob#Nos muestra la p y la q 
sol$preciosps
sol$preciosxb
sol$solucion2x2$table#tabla de beneficios
#ANÁLISIS DE SENSIBILIDAD


sensitivityk<-function(m=2000){
  
}


sol1=solucion(m=1000)
sol1$beneficiosesperados
sol1$solucion2x2$msNE_prob#Nos muestra la p y la q 
sol1$preciosps
sol1$preciosxb
sol1$solucion2x2$table#tabla de beneficios




