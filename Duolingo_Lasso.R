load("F:/Duolingo/CATsim/CAT_Matchb_condition18.RData")
##############################################
#
#     Conditional Lasso (SRT likelihood)
#
##############################################
eta.vec=seq(7,31,2)
AIC.vec=BIC.vec=numeric(length(eta.vec))
BIC.vec2=numeric(length(eta.vec))
beta.est.allrep61=beta.est.allrep62=beta.est.allrep63=matrix(0,25,1000)
power.mat61 = power.mat62 =power.mat63 = numeric(25)
tyI.mat61 = tyI.mat62 = tyI.mat63 = numeric(25)
eta.selected61=eta.selected62=eta.selected63 = numeric(25)
for (rep in 1:25){
  b=read.csv(paste0('F:/Duolingo/CATsim/CAT_Difficulty_',rep,'.csv'))[,2]
  resp.mat=resp.mat.all[,,rep]
  est.thetas=est.thetas.all[,rep]
  test.items=test.items.all[,,rep]
  # extract response matrix (20000*100). the ith row are items answered by examinee i 
  response<-matrix(,N,L)
  for (i in 1:N){
    response[i,]=resp.mat[i,  test.items[i,]]
  }
  
  
  # starting values for all beta
  beta.mat=rep(1.5,J)
  eta.vec=seq(7,31,2)
  est.beta.mat=matrix(0,length(eta.vec),J)
  BIC.vec=numeric(length(eta.vec))
  for (k in 1:length(eta.vec)){
    eta=eta.vec[k]
    ll=numeric(J)
    est.beta=numeric(J)
    Njf.k=numeric(J)
    for (j in 1:J){
      bj=b[j]
      betaj=beta.mat[j]
      # which person answered item j
      groupq=which(test.items==j)%%N
      
      # which person in focal group answered item j
      groupq.f=groupq[which(groupq>N/2)]
      
      # which item person i answered is item j
      whichitem=which(test.items==j)%/%N+1
      
      # which item person i answered is item j
      whichitem.f=whichitem[which(groupq>N/2)]
      # response person i answered for item j
      responsesij=diag(response[groupq,whichitem])
      # response person i answered for item j
      responsesij.f=diag(response[groupq.f,whichitem.f])
      # estimated theta of person i
      thetai=est.thetas[groupq]
      # estimated theta of person i
      thetai.f=est.thetas[groupq.f]
      # number of test takers who answer item j
      Nj=length(groupq)
      # number of test takers in reference/focal group who answer item j
      Njf=length(groupq.f)
      #save number of examinees on each item
      Njf.k[j]=Njf
      obj= function(x){
        sumovernj=0
        #for (ii in 1:Njr){
        #  #reference group (don't need)
        #  sumovernj=sumovernj+(responsesij.r[ii]*((thetai.r[ii]-bj)-log(1+exp(thetai.r[ii]-bj)))+(1-responsesij.r[ii])*log(1-exp(thetai.r[ii]-bj)/(1+exp(thetai.r[ii]-bj))))
        #}
        for (ii in 1:Njf){
          #focal group
          sumovernj=sumovernj+log(exp(responsesij.f[ii]*(thetai.f[ii]-bj+x))*(thetai.f[ii]-bj+x)/(exp(thetai.f[ii]-bj+x)-1))
        }
        temp=sumovernj-eta*norm(as.matrix(x))
        return(-temp)
      }
      #x.list=seq(-4,4,0.1)
      #plot(obj(x.list[1]))
      #est.beta[j]<-optim(betaj,obj,lower=-2,upper=2,method="L-BFGS-B",hessian=T,control = list(trace = 5))$par
      est.beta[j]<-optimize(obj, c(-4, 4), tol = 0.0001)$minimum
      if (abs(est.beta[j])<1e-3){
        est.beta[j]=0
      }
      #re-est if beta not zero
      if (est.beta[j]!=0){
        obj.reest= function(x){
          sumovernj=0
          for (ii in 1:Njf){
            sumovernj=sumovernj+log(exp(responsesij.f[ii]*(thetai.f[ii]-bj+x))*(thetai.f[ii]-bj+x)/(exp(thetai.f[ii]-bj+x)-1))
          }
          temp=sumovernj
          return(-temp)
        }
        est.beta[j]<-optimize(obj.reest, c(-4, 4), tol = 0.0001)$minimum
      }
      
      llj=0 
      for (ii in 1:Njf){
        #focal group
        llj=llj+log(exp(responsesij.f[ii]*(thetai.f[ii]-bj+est.beta[j]))*(thetai.f[ii]-bj+est.beta[j])/(exp(thetai.f[ii]-bj+est.beta[j])-1))
      }
      ll[j]=llj
    }
    #est.beta[which(est.beta<0.05)]=0
    est.beta.mat[k,]=est.beta
    AIC=-2*sum(ll)+2*sum(est.beta!=0)
    BIC=-2*sum(ll)+log(mean(Njf.k))*sum(est.beta!=0)
    BIC2=-2*sum(ll)+log(min(Njf.k))*sum(est.beta!=0)
    AIC.vec[k]=AIC
    BIC.vec[k]=BIC
    BIC.vec2[k]=BIC2
  }
  kk=which.min(BIC.vec)
  eta.selected61[rep]=eta.vec[kk]
  print(rep)
  print(eta.selected61)
  beta.est.allrep61[rep,]=est.beta.mat[kk,]
  #power.mat61[rep]=sum(which(est.beta.mat[kk,]!=0)%in%(301:310))/10
  #tyI.mat61[rep]=sum(which(est.beta.mat[kk,]!=0)%in%c(1:300,311:1000))/990
  #power.mat61[rep]=sum(which(est.beta.mat[kk,]!=0)%in%(301:350))/50
  #tyI.mat61[rep]=sum(which(est.beta.mat[kk,]!=0)%in%c(1:300,351:1000))/950
  power.mat61[rep]=sum(which(est.beta.mat[kk,]!=0)%in%(301:400))/100
  tyI.mat61[rep]=sum(which(est.beta.mat[kk,]!=0)%in%c(1:300,401:1000))/900
  print(power.mat61[rep])
  print(tyI.mat61[rep])
  
  # plot solution path
  #sp.power=sp.FP=numeric(length(eta.vec))
  #for(kk in 1:length(eta.vec)){
  #  sp.power[kk]=sum(which(est.beta.mat[kk,]!=0)%in%(301:400))/100
  #  sp.FP[kk]=sum(which(est.beta.mat[kk,]!=0)%in%c(1:300,401:1000))/900
  #}
  #plot(eta.vec,sp.power,type="l",ylim = 0:1)
  #lines(eta.vec,sp.FP,col="blue")
  
  kk2=which.min(BIC.vec2)
  eta.selected62[rep]=eta.vec[kk2]
  print(eta.selected62)
  beta.est.allrep62[rep,]=est.beta.mat[kk2,]
  #power.mat62[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%(301:310))/10
  #tyI.mat62[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%c(1:300,311:1000))/990
  #power.mat62[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%(301:350))/50
  #tyI.mat62[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%c(1:300,351:1000))/950
  power.mat62[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%(301:400))/100
  tyI.mat62[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%c(1:300,401:1000))/900
  print(power.mat62[rep])
  print(tyI.mat62[rep])
  
  
  kk3=which.min(AIC.vec)
  eta.selected63[rep]=eta.vec[kk3]
  print(eta.selected63)
  beta.est.allrep63[rep,]=est.beta.mat[kk3,]
  #power.mat63[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%(301:310))/10
  #tyI.mat63[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%c(1:300,311:1000))/990
  #power.mat63[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%(301:350))/50
  #tyI.mat63[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%c(1:300,351:1000))/950
  power.mat63[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%(301:400))/100
  tyI.mat63[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%c(1:300,401:1000))/900
  print(power.mat63[rep])
  print(tyI.mat63[rep])
  
}
mean(power.mat61)
mean(power.mat62)
mean(power.mat63)
mean(tyI.mat61)
mean(tyI.mat62)
mean(tyI.mat63)

sd(power.mat61)/sqrt(24)#sqrt(50-1)=7
sd(tyI.mat61)/sqrt(24)#sqrt(50-1)=7

#sd(power.mat62)/sqrt(24)#sqrt(50-1)=7
#sd(tyI.mat62)/sqrt(24)

#sd(power.mat63)/sqrt(24)#sqrt(50-1)=7
#sd(tyI.mat63)/sqrt(24)

save.image("F:/Duolingo/CATsim/CondLasso_Matchb_condition18.RData")

save.image("F:/Duolingo/CATsim/CondLasso_Random_condition18.RData")

########################################################################################
#
#    Marginal likelihood (treat everyone's theta as UNknown) SRT + Likelihood
#
########################################################################################
load("C:/Users/zhux0445/Documents/GitHub/Duolingo2021/CAT_Matchb_condition8.RData")

N.vec=c(10000,10000)
beta.est.allrep81=beta.est.allrep82=beta.est.allrep83=beta.est.allrep84=matrix(0,25,1000)
power.mat81 = power.mat82 =power.mat83=power.mat84 = numeric(25)
tyI.mat81 = tyI.mat82 = tyI.mat83 =tyI.mat84 = numeric(25)
eta.selected81=eta.selected82=eta.selected83=eta.selected84= numeric(25)
for (rep in 1:25){
  b=read.csv(paste0('C:/Users/zhux0445/Documents/GitHub/Duolingo2021/CAT_Difficulty_',rep,'.csv'))[,2]
  item.num=seq(1,1000,1)
  item.bank<-cbind(item.num,b) #400x1 matrix
  
  # generate response matrix for all items all examinees 
  resp.mat=resp.mat.all[,,rep]
  test.items=test.items.all[,,rep]
  response<-matrix(,N,L)
  for (i in 1:N){
    response[i,]=resp.mat[i,  test.items[i,]]
  }
  #check if data (estimated theta) is read in correctly
  #true.thetas=read.table(paste('CATSIB_thetas2w_',rep))
  #est.thetas=est.thetas.all[,rep]
  #mean((matrix(est.thetas,N,1)-as.matrix(true.thetas)))
  #sqrt(mean((matrix(est.thetas,N,1)-as.matrix(true.thetas))^2))
  
  
  
  # starting values for all beta
  beta.mat=rep(-0.5,J)
  eta.vec=seq(10,31,3)
  est.beta.mat=matrix(0,length(eta.vec),J)
  AIC.vec=BIC.vec=BIC.vec2=Lik.vec=numeric(length(eta.vec))
  k=1
  eta=eta.vec[k]
  ll=numeric(J)
  # Gauss-Hermite quadrature nodes
  X1=seq(-3,3,by=0.2)
  r=1
  G=length(X1)^r
  gh=t(matrix(rep(X1,r),r,length(X1),byrow = T))
  idx <- as.matrix(expand.grid(rep(list(1:length(X1)),r)))
  X <- matrix(gh[idx,1],nrow(idx),r)
  ng <-  numeric(G)
  y=2
  y.allgroup=rbind(rep(0,y-1),diag(y-1)) #y1, y2
  # starting values
  grbeta=beta.mat #matrix(0,J,2)
  #grbeta=numeric(J)
  #grbeta[301:310]=-0.5
  grd=item.bank[,2]
  Sig.est=rbind(1,1) #rbind(Sig100,Sig200,Sig300)
  Mu.est=c(0,0) #c(mu100,mu200,mu300)
  df.beta <- df.Mu <- df.Sig <- 1
  iter <- 0
  # start EM cycle 
  while(max(df.beta)>1e-3 | max(df.Mu)>1e-3 | max(df.Sig)>1e-3)
  {
    betaold=grbeta
    muold=Mu.est
    sigold=Sig.est
    
    # E STEP
    Mu.est.mat=matrix(Mu.est,y,r,byrow = T)
    Sig.est.slice=array(0,c(r,r,y))
    for (yy in 1:y){
      Sig.est.slice[,,yy]=Sig.est[((yy-1)*r+1):((yy-1)*r+r),]
    }
    A.allgroups=numeric(nrow(X)*y) #A1, A2, A3
    for (yy in 1:y){
      A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))]=dnorm(X, Mu.est.mat[yy,],sqrt(Sig.est.slice[,,yy]))
    }
    #calculation of n_g 
    grbeta.allgrp= matrix(0,(J*y),1) #grbeta1,grbeta2,grbeta3 "ncol=1" is the number of parameter beta, if we use multiple covariates later, change this value
    for (yy in 1:y){
      grbeta.allgrp[((yy-1)*J+1):((yy-1)*J+J),]=y.allgroup[yy,]*grbeta
    }
    
    LiA=matrix(double(N*G),N,G)
    for (yy in 1:y){
      pij=array(1,dim=c(J,G,N.vec[yy]))
      for (j in 1:J)
      {
        # which person answered item j
        groupq=which(test.items==j)%%N
        # which person in reference group answered item j
        groupq.r=groupq[which(groupq<=N/2)];groupq.f=groupq[which(groupq>N/2)]
        group.rf=list(groupq.r,groupq.f-10000)[[yy]]
        # which item person i answered is item j
        whichitem=which(test.items==j)%/%N+1
        whichitem.r=whichitem[which(groupq<=N/2)]; whichitem.f=whichitem[which(groupq>N/2)]
        # response person i answered for item j
        responsesij.r=diag(response[groupq.r,whichitem.r]);responsesij.f=diag(response[groupq.f,whichitem.f])
        responsesij.rf=list(responsesij.r,responsesij.f)[[yy]]
        for (g in 1:G)
        {
          pij[j,g,group.rf]=exp(responsesij.rf*(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))/((exp((-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))-1)/(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))
        }
      }
      LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(apply(pij, c(2,3), prod)* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
    }
    # calculate n_g
    Pi = apply(LiA,1,sum)
    ng.all = apply(LiA/Pi,2,sum)
    ng.allgrp=numeric(G*y)
    for (yy in 1:y){
      ng.allgrp[((yy-1)*G+1):((yy-1)*G+G)]=apply(LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]/Pi[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy]))],2,sum)
    }
    ng=c(ng.all,ng.allgrp)
    
    #update mu hat and Sigma hat
    Mu.est=numeric(r*y)
    for (yy in 1:y){
      Mu.est[((yy-1)*r+1):((yy-1)*r+r)]=colSums(X*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
    }
    #update Sigma hat
    #Sig.hat.allgrp=Sig.est
    for (yy in 1:y){
      #Sig.hat.allgrp[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
      Sig.est[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
    }
    
    # M step
    active.set=1:J
    #for (j in active.set){
    for (j in 1:J){
      bj=b[j]
      # which person answered item j
      groupq=which(test.items==j)%%N
      groupq.f=groupq[which(groupq>N/2)]
      
      # which item person i answered is item j
      whichitem=which(test.items==j)%/%N+1
      whichitem.f=whichitem[which(groupq>N/2)]
      # response person i answered for item j
      responsesij=diag(response[groupq,whichitem])
      responsesij.f=diag(response[groupq.f,whichitem.f])
      # number of test takers who answer item j
      Nj=length(groupq)
      Njf=length(groupq.f)
      
      # calculate r_jgk
      #rLiA.f1 <- matrix(0, Njf,G)
      rLiA.f1 =responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
      rgk.f1= colSums(rLiA.f1)
      #plot(x.vals,rgk.f1)
      ngj=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
      
      obj= function(x){
        #focal group
        sumovernj=sum(rgk.f1*(X-bj+x)-ngj*log((exp(X-bj+x)-1)/(X-bj+x)))
        temp=sumovernj-eta*abs(x)
        return(-temp)
      }
      
      x.vals=seq(-3,3,0.2)
      f.vals=numeric(length(x.vals))
      for (i in 1: length(x.vals)){
        f.vals[i]=obj(x.vals[i])
      }
      #plot(x.vals,f.vals)
      
      grbeta[j]<-optimize(obj, c(-3, 3), tol = 0.0001)$minimum
      if (abs(grbeta[j])<1e-3){
        grbeta[j]=0
        active.set=active.set[-which(active.set==j)]
      }
    } #end of M step
    
    #sum(  grbeta[301:310]!=0)/10
    #sum(  grbeta[c(1:300,311:1000)]!=0)/990
    
    # M step ( Re-estimation)
    for (j in active.set){
      bj=b[j]
      # which person answered item j
      groupq=which(test.items==j)%%N
      # which person in reference group answered item j
      #groupq.r=groupq[which(groupq<=N/2)]
      # which person in focal group answered item j
      groupq.f=groupq[which(groupq>N/2)]
      
      # which item person i answered is item j
      whichitem=which(test.items==j)%/%N+1
      # which item person i answered is item j
      #whichitem.r=whichitem[which(groupq<=N/2)]; 
      whichitem.f=whichitem[which(groupq>N/2)]
      # response person i answered for item j
      responsesij=diag(response[groupq,whichitem])
      # response person i answered for item j
      #responsesij.r=diag(response[groupq.r,whichitem.r])
      responsesij.f=diag(response[groupq.f,whichitem.f])
      # number of test takers who answer item j
      Nj=length(groupq)
      # number of test takers in reference/focal group who answer item j
      Njf=length(groupq.f)
      
      # calculate r_jgk
      rLiA.f1=responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
      rgk.f1= colSums(rLiA.f1)
      ngj=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
      obj2= function(x){
        #focal group
        sumovernj=sum(rgk.f1*(X-bj+x)-ngj*log((exp(X-bj+x)-1)/(X-bj+x)))
        temp=sumovernj
        return(-temp)
      }
      grbeta[j]<-optimize(obj2, c(-3, 3), tol = 0.0001)$minimum
      
    } #end of M step
    df.beta <- abs(betaold-grbeta)
    df.Mu=abs(muold-Mu.est)
    df.Sig=abs(sigold-Sig.est)
    iter <- iter+1
  }
  
  # BIC/AIC
  Mu.est.mat=matrix(Mu.est,y,r,byrow = T)
  Sig.est.slice=array(0,c(r,r,y))
  for (yy in 1:y){
    Sig.est.slice[,,yy]=Sig.est[((yy-1)*r+1):((yy-1)*r+r),]
  }
  A.allgroups=numeric(nrow(X)*y) #A1, A2, A3
  for (yy in 1:y){
    A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))]=dnorm(X, Mu.est.mat[yy,],Sig.est.slice[,,yy])
  }
  #calculation of n_g 
  grbeta.allgrp= matrix(0,(J*y),1) #grbeta1,grbeta2,grbeta3 "ncol=1" is the number of parameter beta, if we use multiple covariates later, change this value
  for (yy in 1:y){
    grbeta.allgrp[((yy-1)*J+1):((yy-1)*J+J),]=y.allgroup[yy,]*grbeta
  }
  
  LiA=matrix(double(N*G),N,G)
  for (yy in 1:y){
    pij=array(1,dim=c(J,G,N.vec[yy]))
    for (j in 1:J)
    {
      # which person answered item j
      groupq=which(test.items==j)%%N
      # which person in reference group answered item j
      groupq.r=groupq[which(groupq<=N/2)];groupq.f=groupq[which(groupq>N/2)]
      group.rf=list(groupq.r,groupq.f-10000)[[yy]]
      # which item person i answered is item j
      whichitem=which(test.items==j)%/%N+1
      whichitem.r=whichitem[which(groupq<=N/2)]; whichitem.f=whichitem[which(groupq>N/2)]
      # response person i answered for item j
      responsesij.r=diag(response[groupq.r,whichitem.r]);responsesij.f=diag(response[groupq.f,whichitem.f])
      responsesij.rf=list(responsesij.r,responsesij.f)[[yy]]
      for (g in 1:G)
      {
        pij[j,g,group.rf]=exp(responsesij.rf*(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))/((exp((-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))-1)/(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))
      }
    }
    LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(apply(pij, c(2,3), prod)* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
  }
  Pi = apply(LiA,1,sum) #marginal likelihood
  ll=sum(log(Pi))
  Nj.k=numeric(J)
  for (j in 1:J){
    bj=b[j]
    betaj=grbeta[j]
    # which person answered item j
    groupq=which(test.items==j)%%N
    # which person in reference group answered item j
    groupq.r=groupq[which(groupq<=N/2)]
    # which person in focal group answered item j
    groupq.f=groupq[which(groupq>N/2)]
    
    # which item person i answered is item j
    whichitem=which(test.items==j)%/%N+1
    # which item person i answered is item j
    whichitem.r=whichitem[which(groupq<=N/2)]; 
    whichitem.f=whichitem[which(groupq>N/2)]
    # response person i answered for item j
    responsesij=diag(response[groupq,whichitem])
    # response person i answered for item j
    responsesij.r=diag(response[groupq.r,whichitem.r])
    responsesij.f=diag(response[groupq.f,whichitem.f])
    # number of test takers who answer item j
    Nj.k[j]=length(groupq)
    
  }
  est.beta.mat[k,]=grbeta
  BIC=-2*sum(ll)+log(mean(Nj.k))*sum(grbeta!=0)
  BIC.vec[k]=BIC
  BIC2=-2*sum(ll)+log(min(Nj.k))*sum(grbeta!=0)
  BIC.vec2[k]=BIC2
  AIC=-2*sum(ll)+2*sum(grbeta!=0)
  AIC.vec[k]=AIC
  Lik=sum(ll)
  Lik.vec[k]=Lik
  
  for (k in 2:length(eta.vec)){
    eta=eta.vec[k]
    ll=numeric(J)
    # Gauss-Hermite quadrature nodes
    X1=seq(-3,3,by=0.2)
    r=1
    G=length(X1)^r
    gh=t(matrix(rep(X1,r),r,length(X1),byrow = T))
    idx <- as.matrix(expand.grid(rep(list(1:length(X1)),r)))
    X <- matrix(gh[idx,1],nrow(idx),r)
    ng <-  numeric(G)
    y=2
    y.allgroup=rbind(rep(0,y-1),diag(y-1)) #y1, y2
    # starting values
    #grbeta=beta.mat #matrix(0,J,2)
    #grbeta=numeric(J)
    #grbeta[301:310]=-0.5
    grd=item.bank[,2]
    Sig.est=rbind(1,1) #rbind(Sig100,Sig200,Sig300)
    Mu.est=c(0,0) #c(mu100,mu200,mu300)
    df.beta <- df.Mu <- df.Sig <- 1
    iter <- 0
    # start EM cycle 
    while(max(df.beta)>1e-3 | max(df.Mu)>1e-3 | max(df.Sig)>1e-3)
    {
      betaold=grbeta
      muold=Mu.est
      sigold=Sig.est
      
      # E STEP
      Mu.est.mat=matrix(Mu.est,y,r,byrow = T)
      Sig.est.slice=array(0,c(r,r,y))
      for (yy in 1:y){
        Sig.est.slice[,,yy]=Sig.est[((yy-1)*r+1):((yy-1)*r+r),]
      }
      A.allgroups=numeric(nrow(X)*y) #A1, A2, A3
      for (yy in 1:y){
        A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))]=dnorm(X, Mu.est.mat[yy,],sqrt(Sig.est.slice[,,yy]))
      }
      #calculation of n_g 
      grbeta.allgrp= matrix(0,(J*y),1) #grbeta1,grbeta2,grbeta3 "ncol=1" is the number of parameter beta, if we use multiple covariates later, change this value
      for (yy in 1:y){
        grbeta.allgrp[((yy-1)*J+1):((yy-1)*J+J),]=y.allgroup[yy,]*grbeta
      }
      
      LiA=matrix(double(N*G),N,G)
      for (yy in 1:y){
        pij=array(1,dim=c(J,G,N.vec[yy]))
        for (j in 1:J)
        {
          # which person answered item j
          groupq=which(test.items==j)%%N
          # which person in reference group answered item j
          groupq.r=groupq[which(groupq<=N/2)];groupq.f=groupq[which(groupq>N/2)]
          group.rf=list(groupq.r,groupq.f-10000)[[yy]]
          # which item person i answered is item j
          whichitem=which(test.items==j)%/%N+1
          whichitem.r=whichitem[which(groupq<=N/2)]; whichitem.f=whichitem[which(groupq>N/2)]
          # response person i answered for item j
          responsesij.r=diag(response[groupq.r,whichitem.r]);responsesij.f=diag(response[groupq.f,whichitem.f])
          responsesij.rf=list(responsesij.r,responsesij.f)[[yy]]
          for (g in 1:G)
          {
            pij[j,g,group.rf]=exp(responsesij.rf*(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))/((exp((-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))-1)/(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))
          }
        }
        LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(apply(pij, c(2,3), prod)* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
      }
      # calculate n_g
      Pi = apply(LiA,1,sum)
      ng.all = apply(LiA/Pi,2,sum)
      ng.allgrp=numeric(G*y)
      for (yy in 1:y){
        ng.allgrp[((yy-1)*G+1):((yy-1)*G+G)]=apply(LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]/Pi[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy]))],2,sum)
      }
      ng=c(ng.all,ng.allgrp)
      
      #update mu hat and Sigma hat
      Mu.est=numeric(r*y)
      for (yy in 1:y){
        Mu.est[((yy-1)*r+1):((yy-1)*r+r)]=colSums(X*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
      }
      #update Sigma hat
      #Sig.hat.allgrp=Sig.est
      for (yy in 1:y){
        #Sig.hat.allgrp[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
        Sig.est[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
      }
      
      # M step
      active.set=1:J
      #for (j in active.set){
      for (j in 1:J){
        bj=b[j]
        # which person answered item j
        groupq=which(test.items==j)%%N
        groupq.f=groupq[which(groupq>N/2)]
        
        # which item person i answered is item j
        whichitem=which(test.items==j)%/%N+1
        whichitem.f=whichitem[which(groupq>N/2)]
        # response person i answered for item j
        responsesij=diag(response[groupq,whichitem])
        responsesij.f=diag(response[groupq.f,whichitem.f])
        # number of test takers who answer item j
        Nj=length(groupq)
        Njf=length(groupq.f)
        
        # calculate r_jgk
        #rLiA.f1 <- matrix(0, Njf,G)
        rLiA.f1 =responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
        rgk.f1= colSums(rLiA.f1)
        #plot(x.vals,rgk.f1)
        ngj=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
        
        obj= function(x){
          #focal group
          sumovernj=sum(rgk.f1*(X-bj+x)-ngj*log((exp(X-bj+x)-1)/(X-bj+x)))
          temp=sumovernj-eta*abs(x)
          return(-temp)
        }
        
        x.vals=seq(-3,3,0.2)
        f.vals=numeric(length(x.vals))
        for (i in 1: length(x.vals)){
          f.vals[i]=obj(x.vals[i])
        }
        #plot(x.vals,f.vals)
        
        grbeta[j]<-optimize(obj, c(-3, 3), tol = 0.0001)$minimum
        if (abs(grbeta[j])<1e-3){
          grbeta[j]=0
          active.set=active.set[-which(active.set==j)]
        }
      } #end of M step
      
      #sum(  grbeta[301:310]!=0)/10
      #sum(  grbeta[c(1:300,311:1000)]!=0)/990
      
      # M step ( Re-estimation)
      for (j in active.set){
        bj=b[j]
        # which person answered item j
        groupq=which(test.items==j)%%N
        # which person in reference group answered item j
        #groupq.r=groupq[which(groupq<=N/2)]
        # which person in focal group answered item j
        groupq.f=groupq[which(groupq>N/2)]
        
        # which item person i answered is item j
        whichitem=which(test.items==j)%/%N+1
        # which item person i answered is item j
        #whichitem.r=whichitem[which(groupq<=N/2)]; 
        whichitem.f=whichitem[which(groupq>N/2)]
        # response person i answered for item j
        responsesij=diag(response[groupq,whichitem])
        # response person i answered for item j
        #responsesij.r=diag(response[groupq.r,whichitem.r])
        responsesij.f=diag(response[groupq.f,whichitem.f])
        # number of test takers who answer item j
        Nj=length(groupq)
        # number of test takers in reference/focal group who answer item j
        Njf=length(groupq.f)
        
        # calculate r_jgk
        rLiA.f1=responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
        rgk.f1= colSums(rLiA.f1)
        ngj=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
        obj2= function(x){
          #focal group
          sumovernj=sum(rgk.f1*(X-bj+x)-ngj*log((exp(X-bj+x)-1)/(X-bj+x)))
          temp=sumovernj
          return(-temp)
        }
        grbeta[j]<-optimize(obj2, c(-3, 3), tol = 0.0001)$minimum
        
      } #end of M step
      df.beta <- abs(betaold-grbeta)
      df.Mu=abs(muold-Mu.est)
      df.Sig=abs(sigold-Sig.est)
      iter <- iter+1
    }
    
    # BIC/AIC
    Mu.est.mat=matrix(Mu.est,y,r,byrow = T)
    Sig.est.slice=array(0,c(r,r,y))
    for (yy in 1:y){
      Sig.est.slice[,,yy]=Sig.est[((yy-1)*r+1):((yy-1)*r+r),]
    }
    A.allgroups=numeric(nrow(X)*y) #A1, A2, A3
    for (yy in 1:y){
      A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))]=dnorm(X, Mu.est.mat[yy,],Sig.est.slice[,,yy])
    }
    #calculation of n_g 
    grbeta.allgrp= matrix(0,(J*y),1) #grbeta1,grbeta2,grbeta3 "ncol=1" is the number of parameter beta, if we use multiple covariates later, change this value
    for (yy in 1:y){
      grbeta.allgrp[((yy-1)*J+1):((yy-1)*J+J),]=y.allgroup[yy,]*grbeta
    }
    
    LiA=matrix(double(N*G),N,G)
    for (yy in 1:y){
      pij=array(1,dim=c(J,G,N.vec[yy]))
      for (j in 1:J)
      {
        # which person answered item j
        groupq=which(test.items==j)%%N
        # which person in reference group answered item j
        groupq.r=groupq[which(groupq<=N/2)];groupq.f=groupq[which(groupq>N/2)]
        group.rf=list(groupq.r,groupq.f-10000)[[yy]]
        # which item person i answered is item j
        whichitem=which(test.items==j)%/%N+1
        whichitem.r=whichitem[which(groupq<=N/2)]; whichitem.f=whichitem[which(groupq>N/2)]
        # response person i answered for item j
        responsesij.r=diag(response[groupq.r,whichitem.r]);responsesij.f=diag(response[groupq.f,whichitem.f])
        responsesij.rf=list(responsesij.r,responsesij.f)[[yy]]
        for (g in 1:G)
        {
          pij[j,g,group.rf]=exp(responsesij.rf*(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))/((exp((-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))-1)/(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))
        }
      }
      LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(apply(pij, c(2,3), prod)* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
    }
    Pi = apply(LiA,1,sum) #marginal likelihood
    ll=sum(log(Pi))
    Nj.k=numeric(J)
    for (j in 1:J){
      bj=b[j]
      betaj=grbeta[j]
      # which person answered item j
      groupq=which(test.items==j)%%N
      # which person in reference group answered item j
      groupq.r=groupq[which(groupq<=N/2)]
      # which person in focal group answered item j
      groupq.f=groupq[which(groupq>N/2)]
      
      # which item person i answered is item j
      whichitem=which(test.items==j)%/%N+1
      # which item person i answered is item j
      whichitem.r=whichitem[which(groupq<=N/2)]; 
      whichitem.f=whichitem[which(groupq>N/2)]
      # response person i answered for item j
      responsesij=diag(response[groupq,whichitem])
      # response person i answered for item j
      responsesij.r=diag(response[groupq.r,whichitem.r])
      responsesij.f=diag(response[groupq.f,whichitem.f])
      # number of test takers who answer item j
      Nj.k[j]=length(groupq)
      
    }
    est.beta.mat[k,]=grbeta
    BIC=-2*sum(ll)+log(mean(Nj.k))*sum(grbeta!=0)
    BIC.vec[k]=BIC
    BIC2=-2*sum(ll)+log(min(Nj.k))*sum(grbeta!=0)
    BIC.vec2[k]=BIC2
    AIC=-2*sum(ll)+2*sum(grbeta!=0)
    AIC.vec[k]=AIC
    Lik=sum(ll)
    Lik.vec[k]=Lik
  }
  kk=which.min(BIC.vec)
  eta.selected81[rep]=eta.vec[kk]
  beta.est.allrep81[rep,]=est.beta.mat[kk,]
  #power.mat81[rep]=sum(which(est.beta.mat[kk,]!=0)%in%(301:310))/10
  #tyI.mat81[rep]=sum(which(est.beta.mat[kk,]!=0)%in%c(1:300,311:1000))/990
  power.mat81[rep]=sum(which(est.beta.mat[kk,]!=0)%in%(301:350))/50
  tyI.mat81[rep]=sum(which(est.beta.mat[kk,]!=0)%in%c(1:300,351:1000))/950
  #power.mat81[rep]=sum(which(est.beta.mat[kk,]!=0)%in%(301:400))/100
  #tyI.mat81[rep]=sum(which(est.beta.mat[kk,]!=0)%in%c(1:300,401:1000))/900
  
  # plot solution path
  #sp.power=sp.FP=numeric(8)
  #for(kk in 1:8){
  #  sp.power[kk]=sum(which(est.beta.mat[kk,]!=0)%in%(301:310))/10
  #  sp.FP[kk]=sum(which(est.beta.mat[kk,]!=0)%in%c(1:300,311:1000))/990
  #}
  #plot(eta.vec,sp.power,type="l",ylim = 0:1)
  #lines(eta.vec,sp.FP,col="blue")
  
  kk2=which.min(BIC.vec2)
  eta.selected82[rep]=eta.vec[kk2]
  beta.est.allrep82[rep,]=est.beta.mat[kk2,]
  #power.mat82[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%(301:310))/10
  #tyI.mat82[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%c(1:300,311:1000))/990
  power.mat82[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%(301:350))/50
  tyI.mat82[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%c(1:300,351:1000))/950
  #power.mat82[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%(301:400))/100
  #tyI.mat82[rep]=sum(which(est.beta.mat[kk2,]!=0)%in%c(1:300,401:1000))/900
  
  kk3=which.min(AIC.vec)
  eta.selected83[rep]=eta.vec[kk3]
  beta.est.allrep83[rep,]=est.beta.mat[kk3,]
  #power.mat83[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%(301:310))/10
  #tyI.mat83[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%c(1:300,311:1000))/990
  power.mat83[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%(301:350))/50
  tyI.mat83[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%c(1:300,351:1000))/950
  #power.mat83[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%(301:400))/100
  #tyI.mat83[rep]=sum(which(est.beta.mat[kk3,]!=0)%in%c(1:300,401:1000))/900
  
  print(rep)
  print(c(power.mat81[rep],tyI.mat81[rep]))
  print(c(power.mat82[rep],tyI.mat82[rep]))
  print(c(power.mat83[rep],tyI.mat83[rep]))
  
  kk4=which.max(Lik.vec)
  eta.selected84[rep]=eta.vec[kk4]
  beta.est.allrep84[rep,]=est.beta.mat[kk4,]
  #power.mat84[rep]=sum(which(est.beta.mat[kk4,]!=0)%in%(301:310))/10
  #tyI.mat84[rep]=sum(which(est.beta.mat[kk4,]!=0)%in%c(1:300,311:1000))/990
  power.mat84[rep]=sum(which(est.beta.mat[kk4,]!=0)%in%(301:350))/50
  tyI.mat84[rep]=sum(which(est.beta.mat[kk4,]!=0)%in%c(1:300,351:1000))/950
  #power.mat84[rep]=sum(which(est.beta.mat[kk4,]!=0)%in%(301:400))/100
  #tyI.mat84[rep]=sum(which(est.beta.mat[kk4,]!=0)%in%c(1:300,401:1000))/900
  print(c(power.mat84[rep],tyI.mat84[rep]))
}  

mean(power.mat81)
mean(power.mat82)
mean(power.mat83)
mean(power.mat84)
mean(tyI.mat81)
mean(tyI.mat82)
mean(tyI.mat83)
mean(tyI.mat84)

sd(power.mat81)/sqrt(24)#sqrt(50-1)=7
sd(tyI.mat81)/sqrt(24)#sqrt(50-1)=7

sd(power.mat82)/sqrt(24)#sqrt(50-1)=7
sd(tyI.mat82)/sqrt(24)

sd(power.mat83)/sqrt(24)#sqrt(50-1)=7
sd(tyI.mat83)/sqrt(24)

sd(power.mat84)/sqrt(24)#sqrt(50-1)=7
sd(tyI.mat84)/sqrt(24)#sqrt(50-1)=7

save.image("C:/Users/zhux0445/Documents/GitHub/Duolingo2021/MargLasso_Matchb_condition7.RData")
