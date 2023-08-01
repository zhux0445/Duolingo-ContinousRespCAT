library(Hmisc)
#######################################################################################
#
#
#      CAT  (SRT model generate response + Likelihood method estimate theta)
#
#
########################################################################################

#Simulate true thetas
N<-20000 # No. of examinees
#set.seed(1)
true.thetas<-rnorm(N)
hist(true.thetas)

# see the distribution of generated theta. item difficulty can be generated on [-2,2]. 

#Simulate item bank of 1000 items
J<-1000 # No. of items in the item bank
b<-runif(J,-2,2)
hist(b)
item.num=seq(1,J,1)
item.bank<-cbind(item.num,b) #400x1 matrix
L<-100 #test length


#response generate funtion (SRT)
res.gen=function(num,theta,delta){
  u.list=runif(num)
  res=log(u.list*(exp(theta-delta)-1)+1)/(theta-delta)
  return(res)
}

# generate response matrix for all items all examinees 
resp.mat=matrix(0,N,J)
for (i in 1:N){
  resp.mat[i,]=res.gen(J,true.thetas[i],item.bank[,2])
}


#response generate funtion (RASCH)
#res.gen.rasch=function(theta,delta){
#  res=exp(theta-delta)/(1+exp(theta-delta))
#  return(res)
#}

# generate response matrix for all items all examinees 
#resp.mat.rasch=matrix(0,N,J)
#for (i in 1:N){
#  resp.mat.rasch[i,]=res.gen.rasch(true.thetas[i],item.bank[,2])
#}



# theta estimate function (Cross entropy)
# resp and bp are vectors for individual i 
#theta.est=function(resp,bp){
#  L=length(resp)
#  ll2= function(x){
#    temp=0
#    for (jj in 1:L){
#      temp=temp+(resp[jj]*log(exp(x-bp[jj])/(1+exp(x-bp[jj])))+(1-resp[jj])*log(1-exp(x-bp[jj])/(1+exp(x-bp[jj]))))
#    }
#    return(-temp)
#  }
#  est.thetas<-optim(0,ll2,lower=-4,upper=4,method="L-BFGS-B",hessian=T)$par
#  return(est.thetas)
#}

# theta estimate function (Likelihood)
# resp and bp are vectors for individual i 
theta.est.ll=function(resp,bp){
  L=length(resp)
  ll2= function(x){
    temp=0
    for (jj in 1:L){
      temp=temp+log(exp(resp[jj]*(x-bp[jj]))*(x-bp[jj])/(exp(x-bp[jj])-1))
    }
    return(-temp)
  }
  est.thetas<-optim(0,ll2,lower=-4,upper=4,method="L-BFGS-B",hessian=T)$par
  return(est.thetas)
}

# Start simulating data
# for difficulty parameters and theta, I only save the no DIF/ no impact version. The DIF/impact size are fixed. I add the fixed values when simulate response data, 

reps=25
setwd('F:/Duolingo/CATsim')

#Simulate item bank of 1000 items
G=2 #number of DIF groups
for (rep in 1:reps){
  set.seed(rep)
  b<-runif(J,-2,2)
  write.csv(b,file=paste0('CAT_Difficulty_',rep,'.csv'))
}

#Simulate true thetas
for (rep in 1:reps){
  set.seed(rep)
  true.thetas<-rnorm(N)
  write.csv(true.thetas,file=paste0('CAT_Theta_',rep,'.csv'))
}


# Do not generate response in the way below! (Do not use write.csv for response matrix. The matrix is too big. The write.csv and read.csv can cause problem in the response data)
item.num=seq(1,1000,1)
for (rep in 1:reps){
  b=read.csv(paste0('CAT_Difficulty_',rep,'.csv'))[,2]
  true.thetas=as.matrix(read.csv(paste0('CAT_Theta_',rep,'.csv'))[,2])
  item.bank<-cbind(item.num,b) 
  
  # sim 1  1% 0.25 DIF 
  item.bank2=item.bank
  item.bank2[301:310,2]<-item.bank2[301:310,2]+0.25 #400x1 matrix for focal group (1% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp1_',rep,'.csv'))
  
  # sim 2  5% 0.25 DIF 
  item.bank2=item.bank
  item.bank2[301:350,2]<-item.bank2[301:350,2]+0.25 #400x1 matrix for focal group (5% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp2_',rep,'.csv'))
  
  
  # sim 3  10% 0.25 DIF 
  item.bank2=item.bank
  item.bank2[301:400,2]<-item.bank2[301:400,2]+0.25 #400x1 matrix for focal group (10% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp3_',rep,'.csv'))
  
  
  # sim 4  1% 0.5 DIF 
  item.bank2=item.bank
  item.bank2[301:310,2]<-item.bank2[301:310,2]+0.5 #400x1 matrix for focal group (1% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp4_',rep,'.csv'))
  
  
  # sim 5  5% 0.5 DIF 
  item.bank2=item.bank
  item.bank2[301:350,2]<-item.bank2[301:350,2]+0.5 #400x1 matrix for focal group (5% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp5_',rep,'.csv'))
  
  
  # sim 6  10% 0.5 DIF 
  item.bank2=item.bank
  item.bank2[301:400,2]<-item.bank2[301:400,2]+0.5 #400x1 matrix for focal group (10% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp6_',rep,'.csv'))
  
  
  # sim 7  1% 1 DIF 
  item.bank2=item.bank
  item.bank2[301:310,2]<-item.bank2[301:310,2]+1 #400x1 matrix for focal group (1% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp7_',rep,'.csv'))
  
  
  # sim 8  5% 1 DIF 
  item.bank2=item.bank
  item.bank2[301:350,2]<-item.bank2[301:350,2]+1 #400x1 matrix for focal group (5% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp8_',rep,'.csv'))
  
  
  # sim 9  10% 1 DIF 
  item.bank2=item.bank
  item.bank2[301:400,2]<-item.bank2[301:400,2]+1 #400x1 matrix for focal group (10% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp9_',rep,'.csv'))
  
  
  # simulations with impact
  true.thetas[(N/2+1):N,]<-true.thetas[(N/2+1):N,]+0.5
  
  # sim 10  1% 0.25 DIF + 0.5 impact
  item.bank2=item.bank
  item.bank2[301:310,2]<-item.bank2[301:310,2]+0.25 #400x1 matrix for focal group (1% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp10_',rep,'.csv'))
  
  
  # sim 11  5% 0.25 DIF + 0.5 impact
  item.bank2=item.bank
  item.bank2[301:350,2]<-item.bank2[301:350,2]+0.25 #400x1 matrix for focal group (5% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp11_',rep,'.csv'))
  
  
  # sim 12  10% 0.25 DIF + 0.5 impact
  item.bank2=item.bank
  item.bank2[301:400,2]<-item.bank2[301:400,2]+0.25 #400x1 matrix for focal group (10% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp12_',rep,'.csv'))
  
  
  # sim 13  1% 0.5 DIF + 0.5 impact
  item.bank2=item.bank
  item.bank2[301:310,2]<-item.bank2[301:310,2]+0.5 #400x1 matrix for focal group (1% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp13_',rep,'.csv'))
  
  
  # sim 14  5% 0.5 DIF + 0.5 impact
  item.bank2=item.bank
  item.bank2[301:350,2]<-item.bank2[301:350,2]+0.5 #400x1 matrix for focal group (5% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp14_',rep,'.csv'))
  
  
  # sim 15  10% 0.5 DIF + 0.5 impact
  item.bank2=item.bank
  item.bank2[301:400,2]<-item.bank2[301:400,2]+0.5 #400x1 matrix for focal group (10% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp15_',rep,'.csv'))
  
  
  # sim 16  1% 1 DIF + 0.5 impact
  item.bank2=item.bank
  item.bank2[301:310,2]<-item.bank2[301:310,2]+1 #400x1 matrix for focal group (1% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp16_',rep,'.csv'))
  
  
  # sim 17  5% 1 DIF + 0.5 impact
  item.bank2=item.bank
  item.bank2[301:350,2]<-item.bank2[301:350,2]+1 #400x1 matrix for focal group (5% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp17_',rep,'.csv'))
  
  
  # sim 18  10% 1 DIF + 0.5 impact
  item.bank2=item.bank
  item.bank2[301:400,2]<-item.bank2[301:400,2]+1 #400x1 matrix for focal group (10% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  write.csv(resp.mat,file=paste0('CAT_resp18_',rep,'.csv'))
  
}


power.mat= numeric(25)
tyI.mat= numeric(25)
DIFitems.mat=matrix(0,25,1000)
bias1=bias2=numeric(25)
mse1=mse2=numeric(25)
corl1=corl2=numeric(25)
rho2=matrix(0,25,2)
resp.mat.all=array(0,c(N,J,25))
est.thetas.all=matrix(0,N,25)
test.items.all=array(0,c(N,L,25))
#for (rep in 1:25){

# Match b CAT
for (rep in 1:25){
  b=read.csv(paste0('CAT_Difficulty_',rep,'.csv'))[,2]
  true.thetas=as.matrix(read.csv(paste0('CAT_Theta_',rep,'.csv'))[,2])
  # with impact
  #true.thetas[(N/2+1):N,]<-true.thetas[(N/2+1):N,]+0.5
  
  item.bank<-cbind(item.num,b) 
  
  item.bank2=item.bank
  #item.bank2[301:310,2]<-item.bank2[301:310,2]+0.25 #400x1 matrix for focal group (1% DIF)
  #item.bank2[301:350,2]<-item.bank2[301:350,2]+0.25 #400x1 matrix for focal group (5% DIF)
  #item.bank2[301:400,2]<-item.bank2[301:400,2]+0.25 #400x1 matrix for focal group (10% DIF)
  #item.bank2[301:310,2]<-item.bank2[301:310,2]+0.5 #400x1 matrix for focal group (1% DIF)
  #item.bank2[301:350,2]<-item.bank2[301:350,2]+0.5 #400x1 matrix for focal group (5% DIF)
  #item.bank2[301:400,2]<-item.bank2[301:400,2]+0.5 #400x1 matrix for focal group (10% DIF)
  #item.bank2[301:310,2]<-item.bank2[301:310,2]+1 #400x1 matrix for focal group (1% DIF)
  item.bank2[301:350,2]<-item.bank2[301:350,2]+1 #400x1 matrix for focal group (5% DIF)
  #item.bank2[301:400,2]<-item.bank2[301:400,2]+1 #400x1 matrix for focal group (10% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  
  #item select funtion 
  item.select<-function(est.theta,selected.items){
    #find the closest b parameter for the estimated theta.
    btdif=abs(item.bank[,2]-est.theta)
    #use the indicator vector to make the information of all selected items 0 
    avail.btdif<-btdif[which(selected.items!=0)]
    selected.item.numbers=item.bank[,1]*selected.items
    selected.item.numbers.omitzeros=selected.item.numbers[which(selected.item.numbers!=0)]
    #return the index of the selected items
    return(selected.item.numbers.omitzeros[which.min(avail.btdif)])
  }
  # end of function
  
  #Create empty objects to store data from test
  est.thetas<-est.thetas.var<-vector(,N)
  test.items<-response<-interim.theta<-interim.var<-matrix(,N,L)
  
  for(i in 1:N) {
    item.temp<-est.temp<-est.mat<-NULL
    selected.items<-rep(1,J)
    initial.items=5
    item.temp<-test.items[i,1:initial.items]<-sample(1:J,initial.items)
    #item.temp<-test.items[i,1:initial.items]<-item.select(true.thetas[i],selected.items)
    selected.items[item.temp]<-0
    
    #simulate response to the first 5 item and estimate theta 
    response[i,1:initial.items]=resp.mat[i,item.temp]
    
    #estimate theta with all the current responses made
    response.temp<-response[i,1:initial.items,drop=F]
    test.temp<-test.items[i,1:initial.items]
    ip<-item.bank[test.temp,,drop=F]
    interim.theta[i,initial.items]<-theta.est.ll(response.temp[1,],ip[,2])
    
    #from sixth to the last item
    converge=0
    l=initial.items+1 #converge == 0  &&
    interim.var.temp=1
    while(l<L & interim.var.temp>((0.25)^2)){
      item.temp<-test.items[i,l]<-item.select(interim.theta[i,(l-1)],selected.items)
      selected.items[item.temp]<-0
      #simulate response for this item
      #set.seed(1)
      response[i,l]=resp.mat[i,item.temp]
      response.temp<-response[i,1:l,drop=F]
      test.temp<-test.items[i,1:l]
      ip<-item.bank[test.temp,,drop=F]
      est.thetas[i]=interim.theta[i,l]<-theta.est.ll(response.temp[1,],ip[,2])
      #estimate var
      temp2=0
      for (j in 1:l){
        #observed variance temp2=temp2+(1+exp(est.temp-ip[j,2])-est.temp*exp(est.temp-ip[j,2]))/(1+exp(est.temp-ip[j,2]))^2
        #temp2=temp2+(0.5+1/6*exp(interim.theta[i,l]-ip[j,2]))/(1+exp(interim.theta[i,l]-ip[j,2]))^2
        temp2=temp2+exp(interim.theta[i,l]-ip[j,2])/(1+exp(interim.theta[i,l]-ip[j,2]))^2
      }
      est.thetas.var[i]=interim.var.temp=interim.var[i,l]=1/(temp2)  
      test.length.all[i]=l
      l=l+1
    }
  }
  
  
  #### End of the loop####
  
  #Calculate mean square error
  (bias1[rep]=mean((est.thetas[1:(N/2)]-true.thetas[1:(N/2)]))) #0.00786942
  (mse1[rep]<-mean((est.thetas[1:(N/2)]-true.thetas[1:(N/2)])^2)) # 0.4636934
  (corl1[rep]=cor(est.thetas[1:(N/2)],true.thetas[1:(N/2)])) # 0.8771858
  
  (bias2[rep]=mean((est.thetas[(N/2+1):N]-true.thetas[(N/2+1):N]))) #0.00786942
  (mse2[rep]<-mean((est.thetas[(N/2+1):N]-true.thetas[(N/2+1):N])^2)) # 0.4636934
  (corl2[rep]=cor(est.thetas[(N/2+1):N],true.thetas[(N/2+1):N])) # 0.8771858
  
  #est.thetas=est.thetas.all[,rep]
  est.thetas.all[,rep]=est.thetas
  #test.items=test.items.all[,,rep]
  test.items.all[,,rep]=test.items
  #resp.mat=resp.mat.all[,,rep]
  resp.mat.all[,,rep]=resp.mat
}

save.image("F:/Duolingo/CATsim/CAT_Matchb_condition8.RData")

# Random select CAT

for (rep in 1:25){
  b=read.csv(paste0('CAT_Difficulty_',rep,'.csv'))[,2]
  true.thetas=as.matrix(read.csv(paste0('CAT_Theta_',rep,'.csv'))[,2])
  # with impact
  #true.thetas[(N/2+1):N,]<-true.thetas[(N/2+1):N,]+0.5
  
  item.bank<-cbind(item.num,b) 
  
  item.bank2=item.bank
  #item.bank2[301:310,2]<-item.bank2[301:310,2]+0.25 #400x1 matrix for focal group (1% DIF)
  #item.bank2[301:350,2]<-item.bank2[301:350,2]+0.25 #400x1 matrix for focal group (5% DIF)
  #item.bank2[301:400,2]<-item.bank2[301:400,2]+0.25 #400x1 matrix for focal group (10% DIF)
  item.bank2[301:310,2]<-item.bank2[301:310,2]+0.5 #400x1 matrix for focal group (1% DIF)
  #item.bank2[301:350,2]<-item.bank2[301:350,2]+0.5 #400x1 matrix for focal group (5% DIF)
  #item.bank2[301:400,2]<-item.bank2[301:400,2]+0.5 #400x1 matrix for focal group (10% DIF)
  #item.bank2[301:310,2]<-item.bank2[301:310,2]+1 #400x1 matrix for focal group (1% DIF)
  #item.bank2[301:350,2]<-item.bank2[301:350,2]+1 #400x1 matrix for focal group (5% DIF)
  #item.bank2[301:400,2]<-item.bank2[301:400,2]+1 #400x1 matrix for focal group (10% DIF)
  resp.mat=matrix(0,N,J)
  for (i in 1:(N/2)){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank[,2])
  }
  for (i in (N/2+1):N){
    resp.mat[i,]=res.gen(J,true.thetas[i,],item.bank2[,2])
  }
  
  #item select funtion 
  item.select<-function(est.theta,selected.items){
    #find the closest b parameter for the estimated theta.
    btdif=abs(item.bank[,2]-est.theta)
    #use the indicator vector to make the information of all selected items 0 
    avail.btdif<-btdif[which(selected.items!=0)]
    selected.item.numbers=item.bank[,1]*selected.items
    selected.item.numbers.omitzeros=selected.item.numbers[which(selected.item.numbers!=0)]
    #return the index of the selected items
    return(selected.item.numbers.omitzeros[which.min(avail.btdif)])
  }
  # end of function
  
  #Create empty objects to store data from test
  est.thetas<-est.thetas.var<-vector(,N)
  test.items<-response<-interim.theta<-interim.var<-matrix(,N,L)
  
  for(i in 1:N) {
    item.temp<-est.temp<-est.mat<-NULL
    selected.items<-rep(1,J)
    #the first 40 items are randomly selected (in random variate length CAT)
    initial.items=40
    random100=sample(1:J,100)
    item.temp<-test.items[i,1:initial.items]<-sample(1:J,initial.items)
    #item.temp<-test.items[i,1:initial.items]<-item.select(true.thetas[i],selected.items)
    selected.items[item.temp]<-0
    
    #simulate response to the first 5 item and estimate theta 
    response[i,1:initial.items]=resp.mat[i,item.temp]
    
    #estimate theta with all the current responses made
    response.temp<-response[i,1:initial.items,drop=F]
    test.temp<-test.items[i,1:initial.items]
    ip<-item.bank[test.temp,,drop=F]
    interim.theta[i,initial.items]<-theta.est.ll(response.temp[1,],ip[,2])
    
    #from sixth to the last item
    converge=0
    l=initial.items+1 #converge == 0  &&
    interim.var.temp=1
    while(l<L & interim.var.temp>((0.25)^2)){
      item.temp<-test.items[i,l]<-random100[l]
      selected.items[item.temp]<-0
      #simulate response for this item
      #set.seed(1)
      response[i,l]=resp.mat[i,item.temp]
      response.temp<-response[i,1:l,drop=F]
      test.temp<-test.items[i,1:l]
      ip<-item.bank[test.temp,,drop=F]
      est.thetas[i]=interim.theta[i,l]<-theta.est.ll(response.temp[1,],ip[,2])
      #estimate var
      temp2=0
      for (j in 1:l){
        #observed variance temp2=temp2+(1+exp(est.temp-ip[j,2])-est.temp*exp(est.temp-ip[j,2]))/(1+exp(est.temp-ip[j,2]))^2
        #temp2=temp2+(0.5+1/6*exp(interim.theta[i,l]-ip[j,2]))/(1+exp(interim.theta[i,l]-ip[j,2]))^2
        temp2=temp2+exp(interim.theta[i,l]-ip[j,2])/(1+exp(interim.theta[i,l]-ip[j,2]))^2
      }
      est.thetas.var[i]=interim.var.temp=interim.var[i,l]=1/(temp2)  
      test.length.all[i]=l
      l=l+1
    }
  }
  
  
  #### End of the loop####
  #### End of the loop####
  
  #Calculate mean square error
  (bias1[rep]=mean((est.thetas[1:(N/2)]-true.thetas[1:(N/2)]))) #0.00786942
  (mse1[rep]<-mean((est.thetas[1:(N/2)]-true.thetas[1:(N/2)])^2)) # 0.4636934
  (corl1[rep]=cor(est.thetas[1:(N/2)],true.thetas[1:(N/2)])) # 0.8771858
  
  (bias2[rep]=mean((est.thetas[(N/2+1):N]-true.thetas[(N/2+1):N]))) #0.00786942
  (mse2[rep]<-mean((est.thetas[(N/2+1):N]-true.thetas[(N/2+1):N])^2)) # 0.4636934
  (corl2[rep]=cor(est.thetas[(N/2+1):N],true.thetas[(N/2+1):N])) # 0.8771858
  
  #est.thetas=est.thetas.all[,rep]
  est.thetas.all[,rep]=est.thetas
  #test.items=test.items.all[,,rep]
  test.items.all[,,rep]=test.items
  #resp.mat=resp.mat.all[,,rep]
  resp.mat.all[,,rep]=resp.mat
}
  
save.image("F:/Duolingo/CATsim/CAT_Random_condition4.RData")

#########################################
#
#
#                CATSIB
#
#
#########################################
  
  #### Estimate rho_g in (4)
  #### (b)
  sigma_th_sq=numeric(G)
  sigma_th_sq[1]=var(est.thetas[1:(N/2)]);sigma_th_sq[2]=var(est.thetas[(N/2+1):N])
  sigma_e_sq=numeric(G)
  sigma_e_sq[1]=mean(est.thetas.SE.sq[1:(N/2)]);sigma_e_sq[2]=mean(est.thetas.SE.sq[(N/2+1):N])
  rho_gb_sq=1-sigma_e_sq/sigma_th_sq #sqrt(1-sigma_e_sq/sigma_th_sq) is correlation between theta and theta_hat
  rho2[rep,]=rho_gb_sq
  
  # Equation (4)
  theta.gp1=mean(est.thetas[1:(N/2)]);theta.gp2=mean(est.thetas[(N/2+1):N])
  Exptheta.gp1=theta.gp1+rho_gb_sq[1]*(est.thetas[1:(N/2)]-theta.gp1)
  Exptheta.gp2=theta.gp2+rho_gb_sq[2]*(est.thetas[(N/2+1):N]-theta.gp2)
  Exptheta=c(Exptheta.gp1,Exptheta.gp2)
  
  # Equation (5)
  Pbar_R=Pbar_F=RF_PDIF=obs.proportion=lengthR=lengthF=obs.variance.R=obs.variance.F=matrix(0,J,80)
  beta_hat=se_beta_hat=B=numeric(J)
  lQ=numeric(J)
  lq=numeric(J)
  for (j in 1:J){
    # which person answered item j
    groupq=which(test.items==j)%%N
    # which person in reference group answered item j
    groupq.r=groupq[which(groupq<=N/2)]
    # which person in focal group answered item j
    groupq.f=groupq[which(groupq>N/2)]
    
    # which item person i answered is item j
    whichitem=which(test.items==j)%/%N+1
    
    # find the interval length
    #start with a reasonably large number of intervals, say, 60, and then monitor how many cells may be eliminated due to getting rid of sparse cells.
    Q=81
    ref.elim=foc.elim=1
    while(ref.elim>0.075 & foc.elim>0.075){
      Q=Q-1
      quadnodes=seq(min(Exptheta[groupq]),max(Exptheta[groupq]),(max(Exptheta[groupq])-min(Exptheta[groupq]))/Q) 
      z1 <- cut2(Exptheta.gp1[groupq.r], quadnodes);z2 <- cut2(Exptheta.gp2[groupq.f-N/2], quadnodes)
      #number of eliminated individuals in two groups
      ref.elim=sum(table(z1)[which(table(z1)<3)])/length(groupq.r)
      foc.elim=sum(table(z2)[which(table(z2)<3)])/length(groupq.f)
      #which intervals to eliminate (lower bound)
      lb=unique(c(which(table(z1)<3),which(table(z2)<3)))
    }
    Nj=sum(table(z1)[-unique(c(which(table(z1)<3),which(table(z2)<3)))]+table(z2)[-unique(c(which(table(z1)<3),which(table(z2)<3)))])
    
    lQ[j]=Q
    lq[j]=length((1:80)[-lb])
    #Q
    for (q in (1:80)[-lb]){
      Pbar_R[j,q]=mean(diag(response[groupq[which(groupq%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))],whichitem[which(groupq%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))]]))
      lengthR[j,q]=length(diag(response[groupq[which(groupq%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))],whichitem[which(groupq%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))]]))
      # the second half individuals are in the focal group in this case. N/2=N1 if N1 change
      Pbar_F[j,q]=mean(diag(response[groupq[which(groupq%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])+N/2))],whichitem[which(groupq%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])+N/2))]])) 
      lengthF[j,q]=length(diag(response[groupq[which(groupq%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])+N/2))],whichitem[which(groupq%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])+N/2))]])) 
      #obs.proportion[j,q]=sum(Exptheta>=quadnodes[q] & Exptheta<quadnodes[q+1])/ N
      obs.proportion[j,q]=(lengthR[j,q]+lengthF[j,q])/  Nj
      obs.variance.R[j,q]=var(as.vector(diag(response[groupq[which(groupq%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))],whichitem[which(groupq%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))]])))
      obs.variance.F[j,q]=var(as.vector(diag(response[groupq[which(groupq%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])+N/2))],whichitem[which(groupq%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])+N/2))]])))
    }
    beta_hat[j]=sum((Pbar_R[j, which(Pbar_R[j,]>0)] - Pbar_F[j, which(Pbar_F[j,]>0)])*obs.proportion[j, which(obs.proportion[j,]>0)])
    se_beta_hat[j]=sqrt(sum((obs.variance.R[j, which(obs.variance.R[j,]>0)]/lengthR[j, which(lengthR[j,]>0)]+obs.variance.F[j, which(obs.variance.F[j,]>0)]/lengthF[j, which(lengthF[j,]>0)])*(obs.proportion[j, which(obs.proportion[j,]>0)]^2)))
  }
  DIFitems=which(abs(beta_hat/se_beta_hat)>1.96)
  power.mat[rep]=sum(which(abs(beta_hat/se_beta_hat)>1.96)%in%(301:400))/100
  tyI.mat[rep]=sum(which(abs(beta_hat/se_beta_hat)>1.96)%in%c(1:300,401:1000))/900
  print(rep)
  print(power.mat[rep])
  print(tyI.mat[rep])
  print(bias[rep])
  print(rmse[rep])
  
  #write.table(est.thetas,file=paste('CATSIBsim_estthetas1_',rep))
  #write.table(test.items,file=paste('CATSIB_testitems1_',rep))
  #write.table(DIFitems,file=paste('CATSIB_detectedDIF1_',rep))
  
}
mean(power.mat)
mean(tyI.mat)
colMeans(rho2)
mean(bias)
mean(rmse)

sd(power.mat)/sqrt(24)#sqrt(50-1)=7
sd(tyI.mat)/sqrt(24)#sqrt(50-1)=7

