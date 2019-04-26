##NORMAN FIRMANSYAH DESTIAWAN
#Intelligent system
#PSO Praktikum 12

jml_partikel <- 4
#posisi x
position_x <- c()
#posisi y
position_y <- c()
#kecepatan x
velocity_x <- c()
#kecepatan y
velocity_y <- c()
#PbestINT0 sbnyk jml_partikel
x_pbest <- integer(jml_partikel)
y_pbest <- integer(jml_partikel)
nilai_pbest <- integer(jml_partikel)
#Gbest
x_gbest <- 0
y_gbest <- 0
nilai_gbest <- 0
#epoch
epoch <- 100

#search space
x_max <- 10
x_min <- 0
y_max <- 10
y_min <- 0
#target
x_target <- 2
y_target <- 2
#correction factor
c1 <- 0.2
c2 <- 0.8
## INISIALISASI SWARM ##
for (i in 1:jml_partikel){
  position_x[i] <- runif(1, x_min, x_max)
  position_y[i] <- runif(1, y_min, y_max)
  velocity_x[i] <- 0
  velocity_y[i] <- 0
  
  x_pbest[i] <- position_x[i]
  y_pbest[i] <- position_y[i]
  
  val <- 1/(1+20*((position_x[i]-x_target)^2) + (position_y[i]-y_target)^2)
  
  nilai_pbest[i] <- val
}

#Epoch/ iterasi
for (i in 1:epoch){
  for (j in 1:jml_partikel){
    #1/1+20
    #titiknya (2,2) target
    val <- 1/(1+20*((position_x[j]-x_target)^2) + (position_y[j]-y_target)^2)
    #update pbest
    val_pbest <- 1/(1+20*((x_pbest[j]-x_target)^2) + (y_pbest[j]-y_target)^2)
    if(val > nilai_pbest[j]){
      x_pbest[j] <- position_x[j]
      y_pbest[j] <- position_y[j]
      nilai_pbest[j]<-val
    }
  }
  #indeks pbest terbesar
  idx <- which.max(nilai_pbest)
  #update Gbest
  x_gbest <- x_pbest[idx]
  y_gbest <- y_pbest[idx]
  nilai_gbest <- nilai_pbest[idx]
  
  for(j in 1:jml_partikel){
    #updateVelocity
    velocity_x[j] <- 0.729843788 * velocity_x[j] + (c1*(x_pbest[j]-position_x[j])) + (c2*(x_gbest-position_x[j]))
    velocity_y[j] <- 0.729843788 * velocity_y[j] + (c1*(y_pbest[j]-position_y[j])) + (c2*(y_gbest-position_y[j]))
    
    #update position
    position_x[j] <- position_x[j] + velocity_x[j]
    position_y[j] <- position_y[j] + velocity_y[j]
  }
}
x_gbest
y_gbest
