#! /usr/bin/env Rscript
##implementation of PLA
count_loop = 0           #counting no of experiment

val = 0                  #counting no of iteration taken by PLA to converge

while(count_loop < 1000){                   
	N = 100                              #no of samples
 	dx <- runif(N,-1,1)                  #random data generation for experiment
 	dy <- runif(N,-1,1)
 	a = 1
 	b = sample (-1:1,1)
	while(a > 0){
		if(b > 0){
               	      fx <- c(-1,1)              
                      fy <- runif(2,-1,1)
                }
                else {
                      fx <-runif(2,-1,1) 
                      fy <- c(-1,1)
                }
        a=a-1
        } 
plot(fx , fy , xlim = c(-1,1) , ylim = c(-1,1) , type = "l" , col = "brown")            # plotting function or line f which is a desired function

m = ((fy[2] - fy[1]) / (fx[2] - fx[1]))         #calculating m,c of y=mx+c of line f
c = (fy[1] - (m*fx[1]))

i = 1 
j = 1
k = 1
gx <- vector()          #g for green points on graph and r for red
gy <- vector()  
rx <- vector()  
ry <- vector()
value <- vector()                        # vector containing desired output of data points
      
while(i < N+1){
	if((dy[i] - m*dx[i] - c) > 0){
		value[i] = 1
                gy[j] = dy[i]
        	gx[j] = dx[i]
          	j = j + 1
          }
        else{
                value[i] = -1
                ry[k] = dy[i]
                rx[k] = dx[i]
                k = k+1
          }
        i = i+1
        }

points(gx,gy,col = "green")                # plotting separated  points          
points(rx,ry,col = "red")

w <- c(0,0,0)
wm <- matrix(w,3,1)
py <- vector()
px <- c(1,-1)
cvalue <- vector()
itr = 0                                         
y = 1

while((y > 0) && (y < 100*N)){                      #body of PLA
i=1
	while(i < N+1){
		if (((wm[3,1]*dy[i]) + (wm[2,1]*dx[i]) + wm[1,1]) > 0) cvalue[i] = 1          # calculating values w.r.t given or updated weights
   		else cvalue[i] = -1
   		i = i+1
   		}
               
miss <- which(value != cvalue)
     if(length(miss) == 0){
	y = -1
     	print("converge")
	}
     else{
	index <- sample(miss,1)
   	wm <- wm + value[index]*(xm <- matrix(c(1,dx[index],dy[index]),3,1))      #updating weights
   	py[1] = (-wm[2,1] - wm[1,1]) / wm[3,1]
   	py[2] = (wm[2,1] - wm[1,1]) / wm[3,1]
   	points(px,py,col = "blue",type = "l")          # plotting line g 
   	itr = itr + 1
   	y = y + 1
   	}
}
val = val + itr
print(val)
print(count_loop)
count_loop = count_loop + 1
}

print(val / 1000)                   #taking average of all experiments