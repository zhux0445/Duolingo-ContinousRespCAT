library(Hmisc)
dresponse=read.csv("F:/Duolingo/chun_wang_data_v2/chun_data_responses.csv")
colnames(dresponse)
dim(dresponse)
dsession=read.csv("F:/Duolingo/chun_wang_data_v2/chun_data_sessions.csv")
params=read.csv("F:/Duolingo/chun_wang_data_v2/chun_data_parameters.csv")


length(unique(dresponse$person_id)) # 110260 individuals 
length(unique(dresponse$task_type)) # 15 types of
length(unique(dresponse$task_id)) # 19173 items

length(unique(params$task_type)) # 5 types of
length(unique(params$task_id)) # 15109 items

# 15977 in total
length(unique(dresponse[which(dresponse$task_type=="AUDIOVOCAB"),4]))#3130
length(unique(dresponse[which(dresponse$task_type=="LISTEN"),4]))#2726
length(unique(dresponse[which(dresponse$task_type=="VOCAB"),4]))#2794
length(unique(dresponse[which(dresponse$task_type=="SPEAK"),4]))#3193
length(unique(dresponse[which(dresponse$task_type=="CTEST"),4]))#4305

length(unique(params[which(params$task_type=="audiovocab"),3])) #3130
length(unique(params[which(params$task_type=="listen"),3])) #2726
length(unique(params[which(params$task_type=="vocab"),3])) #2794
length(unique(params[which(params$task_type=="speak"),3])) #3193
length(unique(params[which(params$task_type=="ctest"),3])) #3266
params[which(params$task_type=="speak"),]
params[which(params$task_type=="ctest"),8]

sum(unique(dresponse[which(dresponse$task_type=="AUDIOVOCAB"),4])%in%unique(params[which(params$task_type=="audiovocab"),3]))
sum(unique(dresponse[which(dresponse$task_type=="LISTEN"),4])%in%unique(params[which(params$task_type=="listen"),3]))
sum(unique(dresponse[which(dresponse$task_type=="VOCAB"),4])%in%unique(params[which(params$task_type=="vocab"),3]))
sum(unique(dresponse[which(dresponse$task_type=="SPEAK"),4])%in%unique(params[which(params$task_type=="speak"),3]))
sum(unique(dresponse[which(dresponse$task_type=="CTEST"),4])%in%unique(params[which(params$task_type=="ctest"),3]))

# Give item names
dresponse$itemname=0
for (i in 1:length(unique(dresponse$task_type))){
  dresponse_i=dresponse[which(dresponse$task_type==unique(dresponse$task_type)[i]),]
  items_i=unique(dresponse[which(dresponse$task_type==unique(dresponse$task_type)[i]),4])
  for (j in 1:length(items_i)){
    response_ij=dresponse_i[which(dresponse_i[,4]==items_i[j]),]
    dresponse$itemname[as.integer(rownames(response_ij))]=paste0(unique(dresponse$task_type)[i],j)
  }
}

allnames=NULL
for (i in 1:length(unique(dresponse$task_type))){
  dresponse_i=dresponse[which(dresponse$task_type==unique(dresponse$task_type)[i]),]
  items_i=unique(dresponse[which(dresponse$task_type==unique(dresponse$task_type)[i]),3])
  for (j in 1:length(items_i)){
    response_ij=dresponse_i[which(dresponse_i[,3]==items_i[j]),]
    allnames=c(allnames,paste0(unique(dresponse$task_type)[i],j))
  }
}

################################
# 
#     Speak data analysis 
#
################################
nrow(dresponse[which(dresponse$task_type=="SPEAK"),])
hist(table(dresponse[which(dresponse$task_type=="SPEAK"),3]))

# Get item parameters
dresponse_speak=dresponse[which(dresponse$task_type=="SPEAK"),]
for (i in 1:3193){
  delt_speak=params[which(params$task_type=="speak"),c(3,7)]
}

# give speak item names
spnames=NULL
items_i=unique(dresponse[which(dresponse$task_type=="SPEAK"),4])
for (j in 1:length(items_i)){
  spnames=c(spnames,paste0("SPEAK",j))
}
spnames=as.character(spnames)
spdifficulties=matrix(0,length(spnames),2)
spdifficulties[,1]=spnames
for (j in 1:length(spnames)){
  spdifficulties[j,2]=(params[which(params$task_id==(dresponse_speak[which(dresponse_speak$itemname==spnames[j]),4])[1]),"difficulty"])[1]
}
# standardize item parameter
spdifficulties=cbind(spdifficulties,(as.numeric(spdifficulties[,2])-5.21159)/2.036)
#write.csv(spdifficulties,file = "duolingo_speak_difficuties.csv")
#write.csv(dresponse_speak,file = "dresponse_speak_itemname.csv")
spdifficulties=read.csv("F:/Duolingo/duolingo_speak_difficuties.csv")
Speak_difficulty=spdifficulties[,3]
hist(Speak_difficulty)

spnames_chorder=spnames[order(spnames)]
spnames20cut=spnames_chorder[-as.integer(which(table(dresponse_speak$itemname)<20))] #remove 672 from 3193 items
sp_delt_unique=matrix(0, length(spnames), 2)
sp_delt_unique[,1]=spnames
for (j in 1:length(spnames)){
  sp_delt_unique[j,2]=(params[which(params$task_id==(dresponse_speak[which(dresponse_speak$itemname==spnames[j]),4])[1]),7])[1]
}
hist(as.numeric(sp_delt_unique[,2]))
mean(as.numeric(sp_delt_unique[,2]))
sd(as.numeric(sp_delt_unique[,2]))
hist((as.numeric(sp_delt_unique[,2])-mean(as.numeric(sp_delt_unique[,2])))/sd(as.numeric(sp_delt_unique[,2])))

# theta estimate function (maximize the cross-entropy)
# input: resp and bp are vectors for individual i 
theta.est=function(resp,bp){
  Len=length(resp)
  ll2= function(x){
    temp=0
    for (jj in 1:Len){
      temp=temp+(resp[jj]*log(exp(x-bp[jj])/(1+exp(x-bp[jj])))+(1-resp[jj])*log(1-exp(x-bp[jj])/(1+exp(x-bp[jj]))))
    }
    return(-temp)
  }
  est.thetas<-optim(0,ll2,lower=-4,upper=4,method="L-BFGS-B",hessian=T)$par
  return(est.thetas)
}

# Trait Estimation with Rasch model and standardized 
# 110260 individuals
# estimate theta
#sp.theta.est=numeric(length(unique(dresponse_speak$person_id)))
#sp.theta.est.SE.sq=numeric(length(unique(dresponse_speak$person_id)))
for (i in 1:length(unique(dresponse_speak$person_id))){
  respi=dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),6]
  delti=numeric(length(respi))
  for (j in 1:length(respi)){
    delti[j]=(params[which(params$task_id==(dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),4])[j])[1],7]-5.21159)/2.036
  }
  sp.theta.est[i]=theta.est(respi,delti)
  
  # SE
  sesqi=sum((exp(sp.theta.est[i]-delti))/(1+exp(sp.theta.est[i]-delti))^2)
  sp.theta.est.SE.sq[i]=1/(sesqi)  
}

#write.csv(sp.theta.est,file = "duolingo_speak_trait_score_est.csv")
#write.csv(sp.theta.est.SE.sq,file = "duolingo_speak_trait_score_est_SE.csv")

sp.theta=read.csv("F:/Duolingo/duolingo_speak_trait_score.csv")[,2]
sp.theta.est=read.csv("F:/Duolingo/duolingo_speak_trait_score_est.csv")[,2]
sp.theta.est.SE.sq=read.csv("duolingo_speak_trait_score_est_SE.csv")[,2]

plot(sp.theta.est[1:35000],(sp.theta[1:35000]-5.042616)/1.779473)



# estimate theta (not standaridize item parameters)

# theta estimate function (maximize the cross-entropy)
# input: resp and bp are vectors for individual i 
theta.est=function(resp,bp){
  Len=length(resp)
  ll2= function(x){
    temp=0
    for (jj in 1:Len){
      temp=temp+(resp[jj]*log(exp(x-bp[jj])/(1+exp(x-bp[jj])))+(1-resp[jj])*log(1-exp(x-bp[jj])/(1+exp(x-bp[jj]))))
    }
    return(-temp)
  }
  est.thetas<-optim(0,ll2,lower=-1,upper=11,method="L-BFGS-B",hessian=T)$par
  return(est.thetas)
}

#sp.theta.est.unscale=numeric(length(unique(dresponse_speak$person_id)))
#sp.theta.est.SE.sq.unscale=numeric(length(unique(dresponse_speak$person_id)))
for (i in 1:length(unique(dresponse_speak$person_id))){
  respi=dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),6]
  delti=numeric(length(respi))
  for (j in 1:length(respi)){
    delti[j]=params[which(params$task_id==(dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),4])[j])[1],7]
  }
  sp.theta.est.unscale[i]=theta.est(respi,delti)
  
  # SE
  sesqi=sum((exp(sp.theta.est.unscale[i]-delti))/(1+exp(sp.theta.est.unscale[i]-delti))^2)
  sp.theta.est.SE.sq.unscale[i]=1/(sesqi)  
}
#plot(sp.theta.est[1:35000],(sp.theta[1:35000]-5.042616)/1.779473)
#write.csv(sp.theta.est.unscale,file = "duolingo_speak_trait_score_est_unscale.csv")
#write.csv(sp.theta.est.SE.sq.unscale,file = "duolingo_speak_trait_score_est_SE_unscale.csv")
sp.theta.est.unscale=read.csv("duolingo_speak_trait_score_est_unscale.csv")[,2]
sp.theta.est.SE.sq.unscale=read.csv("duolingo_speak_trait_score_est_SE_unscale.csv")[,2]


#############################################################################################################################################
# SRT estimate theta ()

# theta estimate function (maximize the SRT likelihodd)
# input: resp and bp are vectors for individual i 
theta.est=function(resp,bp){
  Len=length(resp)
  ll2= function(x){
    temp=0
    for (jj in 1:Len){
      #temp=temp+(resp[jj]*log(exp(x-bp[jj])/(1+exp(x-bp[jj])))+(1-resp[jj])*log(1-exp(x-bp[jj])/(1+exp(x-bp[jj]))))
      temp=temp+log(exp(resp[jj]*(x-bp[jj]))*(x-bp[jj])/(exp(x-bp[jj])-1))
    }
    return(-temp)
  }
  est.thetas<-optim(0,ll2,lower=-6,upper=6,method="L-BFGS-B",hessian=T)$par
  return(est.thetas)
}

# 110260 individuals
# estimate theta
sp.theta.est.srt.SE.sq=numeric(length(unique(dresponse_speak$person_id)))
for (i in 63620:length(unique(dresponse_speak$person_id))){
  respi=dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),6]
  delti=numeric(length(respi))
  for (j in 1:length(respi)){
    delti[j]=(params[which(params$task_id==(dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),4])[j])[1],7]-5.21159)/2.036
  }
  sp.theta.est.srt[i]=theta.est(respi,delti)
  
  # SE
  sesqi=sum((exp(sp.theta.est.srt[i]-delti))/(1+exp(sp.theta.est.srt[i]-delti))^2)
  sp.theta.est.srt.SE.sq[i]=1/(sesqi)  
}

#write.csv(sp.theta.est.srt,file = "duolingo_speak_trait_score_SRT_est.csv")
#write.csv(sp.theta.est.srt.SE.sq,file = "duolingo_speak_trait_score_SRT_est_SE.csv")


############################################################################################################################################


# use score in data
nrow(dsession)
sp.theta=numeric(length(unique(dresponse_speak$person_id)))
for (i in 1:length(unique(dresponse_speak$person_id))){
  sp.theta[i]=dsession[which(dsession$person_id==unique(dresponse_speak$person_id)[i]),14]
}
mean(dsession[,14])
sd(dsession[,14])

sp.theta[i]
sp.theta_rescale=((sp.theta-5.042616)/1.779473)
#write.csv(sp.theta,file = "duolingo_speak_trait_score.csv")

# SE
sp.theta.SE.sq=numeric(length(unique(dresponse_speak$person_id)))
for (i in 1:length(unique(dresponse_speak$person_id))){
  respi=dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),6]
  delti=numeric(length(respi))
  for (j in 1:length(respi)){
    delti[j]=(params[which(params$task_id==(dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),4])[j])[1],7]-5.21159)/2.036
  }
  #sp.theta.est[i]=theta.est(respi,delti)
  
  # SE
  sesqi=sum((exp(sp.theta_rescale[i]-delti))/(1+exp(sp.theta_rescale[i]-delti))^2)
  sp.theta.SE.sq[i]=1/(sesqi)  
}
#write.csv(sp.theta.SE.sq,file = "duolingo_speak_trait_score_se_sq.csv")

# SE (not rescale)
sp.theta.SE.sq.unscale=numeric(length(unique(dresponse_speak$person_id)))
for (i in 1:length(unique(dresponse_speak$person_id))){
  respi=dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),6]
  delti=numeric(length(respi))
  for (j in 1:length(respi)){
    delti[j]=params[which(params$task_id==(dresponse_speak[which(dresponse_speak$person_id==unique(dresponse_speak$person_id)[i]),4])[j])[1],7]
  }
  #sp.theta.est[i]=theta.est(respi,delti)
  
  # SE
  sesqi=sum((exp(sp.theta[i]-delti))/(1+exp(sp.theta[i]-delti))^2)
  sp.theta.SE.sq.unscale[i]=1/(sesqi)  
}
#write.csv(sp.theta.SE.sq.unscale,file = "duolingo_speak_trait_score_se_sq_unscale.csv")
sp.theta.SE.sq.unscale=read.csv("duolingo_speak_trait_score_se_sq_unscale.csv")[,2]

# Impact estimate
mean(dsession$ext_speak_score[which(dsession$tt_gender=="FEMALE")])
mean(dsession$ext_speak_score[which(dsession$tt_gender=="MALE")])
var(dsession$ext_speak_score[which(dsession$tt_gender=="FEMALE")])
var(dsession$ext_speak_score[which(dsession$tt_gender=="MALE")])

##########################################################
#                                                        #
#      CATSIB Use speak_score_est (does not work)        #
#                                                        #
##########################################################


female_score2=sp.theta.est[which(unique(dresponse_speak$person_id)%in%female_id)]
female_score_SE_sq2=sp.theta.est.SE.sq[which(unique(dresponse_speak$person_id)%in%female_id)]

male_score2=sp.theta.est[which(unique(dresponse_speak$person_id)%in%male_id)]
male_score_SE_sq2=sp.theta.est.SE.sq[which(unique(dresponse_speak$person_id)%in%male_id)]

female_score3=sp.theta.est.unscale[which(unique(dresponse_speak$person_id)%in%female_id)]
male_score3=sp.theta.est.unscale[which(unique(dresponse_speak$person_id)%in%male_id)]

#### (b)
sigma_th_sq=numeric(G)
sigma_th_sq[1]=var(female_score2);sigma_th_sq[2]=var(male_score2)
sigma_e_sq=numeric(G)
sigma_e_sq[1]=mean(female_score_SE_sq2);sigma_e_sq[2]=mean(male_score_SE_sq2)
rho_gb_sq=1-sigma_e_sq/sigma_th_sq #sqrt(1-sigma_e_sq/sigma_th_sq) is correlation between theta and theta_hat


# Equation (4)
theta.gp1=mean(female_score2);theta.gp2=mean(male_score2)
Exptheta.gp1=theta.gp1+rho_gb_sq[1]*(female_score2-theta.gp1)
Exptheta.gp2=theta.gp2+rho_gb_sq[2]*(male_score2-theta.gp2)
Exptheta=c(Exptheta.gp1,Exptheta.gp2)


# Equation (5)
Pbar_R=Pbar_F=RF_PDIF=obs.proportion=lengthR=lengthF=obs.variance.R=obs.variance.F=matrix(0,J,60)
beta_hat=se_beta_hat=B=numeric(J)
for (j in 1:length(spnames20cut)){
  #female_score[which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])]
  groupq.r=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
  groupq.f=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
  groupqid.r=unique(female_id)[which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])]
  groupqid.f=unique(male_id)[which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])]
  
  # find the interval length
  #start with a reasonably large number of intervals, say, 60, and then monitor how many cells may be eliminated due to getting rid of sparse cells.
  Q=61
  ref.elim=foc.elim=1
  while(ref.elim>0.15 & foc.elim>0.15 & Q>1){
    Q=Q-1
    #print(Q)
    quadnodes=seq(min(c(female_score[groupq.r],male_score[groupq.f])),max(c(female_score[groupq.r],male_score[groupq.f])),(max(c(female_score[groupq.r],male_score[groupq.f]))-min(c(female_score[groupq.r],male_score[groupq.f])))/Q) 
    z1 <- cut2(Exptheta.gp1[groupq.r], quadnodes);z2 <- cut2(Exptheta.gp2[groupq.f], quadnodes)
    #number of eliminated individuals in two groups
    ref.elim=sum(table(z1)[which(table(z1)<3)])/length(groupq.r)
    foc.elim=sum(table(z2)[which(table(z2)<3)])/length(groupq.f)
    #which intervals to eliminate (lower bound)
    lb=unique(c(which(table(z1)<3),which(table(z2)<3)))
  }
  #Q
  for (q in (1:60)[-lb]){
    fem_i_resp_allj=female_resp[which(female_resp$person_id%in%groupqid.r[which(groupq.r%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))]),]
    Pbar_R[j,q]=mean(fem_i_resp_allj[which(fem_i_resp_allj$itemname==spnames20cut[j]),"score"])
    #a=(groupqid.r[which(groupq.r%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))])[2]
    #dsession[which(dsession$person_id==a),"ext_speak_score"]
    
    lengthR[j,q]=length(which(groupq.r%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1]))))
    # the second half individuals are in the focal group in this case. N/2=N1 if N1 change
    male_i_resp_allj=male_resp[which(male_resp$person_id%in%groupqid.f[which(groupq.f%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])))]),]
    Pbar_F[j,q]=mean(male_i_resp_allj[which(male_i_resp_allj$itemname==spnames20cut[j]),"score"])
    lengthF[j,q]=length(which(groupq.f%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1]))))
    
    obs.proportion[j,q]=mean(Exptheta>=quadnodes[q] & Exptheta<quadnodes[q+1])
    obs.variance.R[j,q]=var(female_score[groupq.r[which(groupq.r%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))]])
    obs.variance.F[j,q]=var(male_score[groupq.f[which(groupq.f%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])))]])
  }
  beta_hat[j]=sum((Pbar_F[j, which(Pbar_F[j,]>0)] - Pbar_R[j, which(Pbar_R[j,]>0)])*obs.proportion[j, which(obs.proportion[j,]>0)])
  se_beta_hat[j]=sqrt(sum((obs.variance.R[j, which(obs.variance.R[j,]>0)]/lengthR[j, which(lengthR[j,]>0)]+obs.variance.F[j, which(obs.variance.F[j,]>0)]/lengthF[j, which(lengthF[j,]>0)])*(obs.proportion[j, which(obs.proportion[j,]>0)]^2)))
}


DIFitems2=which(abs(beta_hat/se_beta_hat)>1.96)# 0.05; 5
length(DIFitems2)







#####################################################################
#                                                                   #
#          CATSIB use ext_speak_score (provided theta work)         #
#                                                                   #
#####################################################################
female_id
female_resp
#male_id=dresponse_speak[which(dresponse_speak$person_id%in%dsession[which(dsession$tt_gender=="MALE"),"person_id"]),2]
#male_resp=dresponse_speak[which(dresponse_speak$person_id%in%male_id),]

female_score=numeric(length(unique(female_id)))
female_score_SE_sq=numeric(length(unique(female_id)))
for (i in 1:length(unique(female_id))){
  female_score[i]=dsession[which(dsession$person_id==unique(female_id)[i]),"ext_speak_score"]
  female_score_SE_sq[i]=sp.theta.SE.sq[which(unique(dresponse_speak$person_id)==unique(female_id)[i])]
}

male_score=numeric(length(unique(male_id)))
male_score_SE_sq=numeric(length(unique(male_id)))
for (i in 1:length(unique(male_id))){
  male_score[i]=dsession[which(dsession$person_id==unique(male_id)[i]),"ext_speak_score"]
  male_score_SE_sq[i]=sp.theta.SE.sq[which(unique(dresponse_speak$person_id)==unique(male_id)[i])]
}


sp.theta.est.SE.sq

#### (b)
sigma_th_sq=numeric(G)
sigma_th_sq[1]=var(female_score);sigma_th_sq[2]=var(male_score)
sigma_e_sq=numeric(G)
sigma_e_sq[1]=mean(female_score_SE_sq);sigma_e_sq[2]=mean(male_score_SE_sq)
rho_gb_sq=1-sigma_e_sq/sigma_th_sq #sqrt(1-sigma_e_sq/sigma_th_sq) is correlation between theta and theta_hat

# Equation (4)
theta.gp1=mean(female_score);theta.gp2=mean(male_score)
Exptheta.gp1=theta.gp1+rho_gb_sq[1]*(female_score-theta.gp1)
Exptheta.gp2=theta.gp2+rho_gb_sq[2]*(male_score-theta.gp2)
Exptheta=c(Exptheta.gp1,Exptheta.gp2)

# Equation (5)
Pbar_R=Pbar_F=RF_PDIF=obs.proportion=lengthR=lengthF=obs.variance.R=obs.variance.F=matrix(0,J,60)
beta_hat2=se_beta_hat2=B=numeric(J)
for (j in 1:length(spnames20cut)){
  #female_score[which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])]
  groupq.r=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
  groupq.f=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
  groupqid.r=unique(female_id)[which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])]
  groupqid.f=unique(male_id)[which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])]
  
  # find the interval length
  #start with a reasonably large number of intervals, say, 60, and then monitor how many cells may be eliminated due to getting rid of sparse cells.
  Q=61
  ref.elim=foc.elim=1
  while(ref.elim>0.15 & foc.elim>0.15 & Q>1){
    Q=Q-1
    #print(Q)
    quadnodes=seq(min(Exptheta),max(Exptheta),(max(Exptheta)-min(Exptheta))/Q) 
    z1 <- cut2(Exptheta.gp1[groupq.r], quadnodes);z2 <- cut2(Exptheta.gp2[groupq.f], quadnodes)
    #number of eliminated individuals in two groups
    ref.elim=sum(table(z1)[which(table(z1)<3)])/length(groupq.r)
    foc.elim=sum(table(z2)[which(table(z2)<3)])/length(groupq.f)
    #which intervals to eliminate (lower bound)
    lb=unique(c(which(table(z1)<3),which(table(z2)<3)))
  }
  #Q
  for (q in (1:60)[-lb]){
    fem_i_resp_allj=female_resp[which(female_resp$person_id%in%groupqid.r[which(groupq.r%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))]),]
    Pbar_R[j,q]=mean(fem_i_resp_allj[which(fem_i_resp_allj$itemname==spnames20cut[j]),"score"])
    #a=(groupqid.r[which(groupq.r%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))])[2]
    #dsession[which(dsession$person_id==a),"ext_speak_score"]
    
    lengthR[j,q]=length(which(groupq.r%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1]))))
    # the second half individuals are in the focal group in this case. N/2=N1 if N1 change
    male_i_resp_allj=male_resp[which(male_resp$person_id%in%groupqid.f[which(groupq.f%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])))]),]
    Pbar_F[j,q]=mean(male_i_resp_allj[which(male_i_resp_allj$itemname==spnames20cut[j]),"score"])
    lengthF[j,q]=length(which(groupq.f%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1]))))
    
    obs.proportion[j,q]=mean(Exptheta>=quadnodes[q] & Exptheta<quadnodes[q+1])
    obs.variance.R[j,q]=var(female_score[groupq.r[which(groupq.r%in%(which(Exptheta.gp1>=quadnodes[q] & Exptheta.gp1<quadnodes[q+1])))]])
    obs.variance.F[j,q]=var(male_score[groupq.f[which(groupq.f%in%(which(Exptheta.gp2>=quadnodes[q] & Exptheta.gp2<quadnodes[q+1])))]])
  }
  beta_hat2[j]=sum((Pbar_F[j, which(Pbar_F[j,]>0)] - Pbar_R[j, which(Pbar_R[j,]>0)])*obs.proportion[j, which(obs.proportion[j,]>0)])
  se_beta_hat2[j]=sqrt(sum((obs.variance.R[j, which(obs.variance.R[j,]>0)]/lengthR[j, which(lengthR[j,]>0)]+obs.variance.F[j, which(obs.variance.F[j,]>0)]/lengthF[j, which(lengthF[j,]>0)])*(obs.proportion[j, which(obs.proportion[j,]>0)]^2)))
}
DIFitems2=which(abs(beta_hat2/se_beta_hat2)>1.96)# 0.05; 5
length(DIFitems2)


#############################################################################################################
#
#    Conditional Lasso with cross-entropy and Rasch model estimated theta (use female as focal group)
#
#############################################################################################################

#
# starting values for all beta

female_id=dresponse_speak[which(dresponse_speak$person_id%in%dsession[which(dsession$tt_gender=="FEMALE"),"person_id"]),2]
#check
#for (i in 1:10){
# print( dsession[which(dsession$person_id==female_id[i]),"tt_gender"])
#}
#female_resp=dresponse_speak[which(dresponse_speak$person_id%in%female_id),]
#female_resp$ext_speak_score=0
for (i in 204161:length(female_resp$ext_speak_score)){
  female_resp[i,"ext_speak_score"]=(dsession[which(dsession$person_id==female_resp[i,"person_id"]),"ext_speak_score"])[1]
}

#female_resp$speak_score_est=0
for (i in 63198:length(female_resp$speak_score_est)){
  female_resp[i,"speak_score_est"]=sp.theta.est[which(unique(dresponse_speak$person_id)==female_resp[i,"person_id"])]
  print(i)
}



J=length(spnames20cut)
#J=length(keep_for_reg)
beta.mat=rep(1,J)
eta.vec=seq(2,10,2)
est.beta.mat=matrix(1,length(eta.vec),J)
lls=AIC.vec=BIC.vec=BIC.vec2=numeric(length(eta.vec))
for (k in 1:length(eta.vec)){
  eta=eta.vec[k]
  ll=numeric(J)
  est.beta=numeric(J)
  Njf.k=numeric(J)
  
  Nperitem=numeric(J)
  for (j in 1:length(spnames20cut)){
    #bj=as.numeric(spdifficulties[which(spdifficulties[,2]==spnames20cut[j]),3])
    bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
    betaj=beta.mat[j]
    
    # response focal group person i answered for item j
    responsesij.f=female_resp[which(female_resp$itemname==spnames20cut[j]),"score"]
    
    # theta of focal group person i
    #thetai.f=female_resp[which(female_resp$itemname==spnames20cut[j]),"ext_speak_score"]
    #thetai.f=(female_resp[which(female_resp$itemname==spnames20cut[j]),"ext_speak_score"]-5.21159)/2.036
    thetai.f=female_resp[which(female_resp$itemname==spnames20cut[j]),"speak_score_est"]
    
    # number of test takers in focal group who answer item j
    Njf.k[j]=length(thetai.f)
    
    obj= function(x){
      #sumovernj=sum(log(exp(responsesij.f*(thetai.f-bj+x))*(thetai.f-bj+x)/(exp(thetai.f-bj+x)-1)))
      #cross-entropy
      sumovernj=sum(responsesij.f*log(exp(thetai.f-bj+x)/(1+exp(thetai.f-bj+x)))+(1-responsesij.f)*log(1-exp(thetai.f-bj+x)/(1+exp(thetai.f-bj+x))))
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
        #sumovernj=sum(log(exp(responsesij.f*(thetai.f-bj+x))*(thetai.f-bj+x)/(exp(thetai.f-bj+x)-1)))
        #cross-entropy
        sumovernj=sum(responsesij.f*log(exp(thetai.f-bj+x)/(1+exp(thetai.f-bj+x)))+(1-responsesij.f)*log(1-exp(thetai.f-bj+x)/(1+exp(thetai.f-bj+x))))
        temp=sumovernj
        return(-temp)
      }
      est.beta[j]<-optimize(obj.reest, c(-4, 4), tol = 0.0001)$minimum
    }
    ll[j]=sum(log(exp(responsesij.f*(thetai.f-bj+est.beta[j]))*(thetai.f-bj+est.beta[j])/(exp(thetai.f-bj+est.beta[j])-1)))
    Nperitem[j]=length(responsesij.f)
  }
  
  #eta=0
  #hist(est.beta)
  #est.beta[which(est.beta<0.05)]=0
  est.beta.mat[k,]=est.beta
  AIC=-2*sum(ll)+2*sum(est.beta!=0)
  BIC=-2*sum(ll)+log(mean(Njf.k))*sum(est.beta!=0)
  BIC2=-2*sum(ll)+log(min(Njf.k))*sum(est.beta!=0)
  lls[k]=sum(ll)
  AIC.vec[k]=AIC
  BIC.vec[k]=BIC
  BIC.vec2[k]=BIC2
}

plot(-2*lls)
plot(AIC.vec)
plot(BIC.vec)
plot(BIC.vec2)
kk=which.min(BIC.vec)
eta.vec[kk]
regbeta1=est.beta.mat[kk,]
length(which(est.beta.mat[kk,]!=0))
(regresult1=which(est.beta.mat[kk,]!=0))

kk2=which.min(BIC.vec2)
eta.vec[kk2]
est.beta.mat[kk2,]
length(which(est.beta.mat[kk2,]!=0))
which(est.beta.mat[kk2,]!=0)

#overlap between marginal and conditional lasso results
grbeta_eta60=read.csv("C:/Users/zhux0445/Dropbox/My PC (GIM30242574)/Downloads/marginalReg_grbeta_eta60.csv")[,2]
which(grbeta_eta60!=0)
length(which( regresult1%in%(which(grbeta_eta60!=0))))/length(regresult1)
length(which( regresult2%in%(which(grbeta_eta60!=0))))/length(regresult2)




############################################
#
#   Conditional Lasso  (Use male as focal)
#
###########################################

male_id=dresponse_speak[which(dresponse_speak$person_id%in%dsession[which(dsession$tt_gender=="FEMALE"),"person_id"]),2]
#check
#for (i in 1:10){
# print( dsession[which(dsession$person_id==female_id[i]),"tt_gender"])
#}
#female_resp=dresponse_speak[which(dresponse_speak$person_id%in%female_id),]
#male_resp$ext_speak_score=0
for (i in 1:length(male_resp$ext_speak_score)){
  male_resp[i,"ext_speak_score"]=(dsession[which(dsession$person_id==male_resp[i,"person_id"]),"ext_speak_score"])[1]
}

#male_resp$speak_score_est=0
for (i in 1:length(male_resp$speak_score_est)){
  male_resp[i,"speak_score_est"]=sp.theta.est[which(unique(dresponse_speak$person_id)==male_resp[i,"person_id"])]
}



J=length(spnames20cut)
#J=length(keep_for_reg)
beta.mat=rep(1,J)
eta.vec=seq(2,10,2)
est.beta.mat=matrix(1,length(eta.vec),J)
lls=AIC.vec=BIC.vec=BIC.vec2=numeric(length(eta.vec))
for (k in 1:length(eta.vec)){
  eta=eta.vec[k]
  ll=numeric(J)
  est.beta=numeric(J)
  Njf.k=numeric(J)
  
  Nperitem=numeric(J)
  for (j in 1:length(spnames20cut)){
    #bj=as.numeric(spdifficulties[which(spdifficulties[,2]==spnames20cut[j]),3])
    bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
    betaj=beta.mat[j]
    
    # response focal group person i answered for item j
    responsesij.f=male_resp[which(male_resp$itemname==spnames20cut[j]),"score"]
    
    # theta of focal group person i
    #thetai.f=female_resp[which(female_resp$itemname==spnames20cut[j]),"ext_speak_score"]
    #thetai.f=(female_resp[which(female_resp$itemname==spnames20cut[j]),"ext_speak_score"]-5.21159)/2.036
    thetai.f=male_resp[which(male_resp$itemname==spnames20cut[j]),"speak_score_est"]
    
    # number of test takers in focal group who answer item j
    Njf.k[j]=length(thetai.f)
    
    obj= function(x){
      #sumovernj=sum(log(exp(responsesij.f*(thetai.f-bj+x))*(thetai.f-bj+x)/(exp(thetai.f-bj+x)-1)))
      #cross-entropy
      sumovernj=sum(responsesij.f*log(exp(thetai.f-bj+x)/(1+exp(thetai.f-bj+x)))+(1-responsesij.f)*log(1-exp(thetai.f-bj+x)/(1+exp(thetai.f-bj+x))))
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
        #sumovernj=sum(log(exp(responsesij.f*(thetai.f-bj+x))*(thetai.f-bj+x)/(exp(thetai.f-bj+x)-1)))
        #cross-entropy
        sumovernj=sum(responsesij.f*log(exp(thetai.f-bj+x)/(1+exp(thetai.f-bj+x)))+(1-responsesij.f)*log(1-exp(thetai.f-bj+x)/(1+exp(thetai.f-bj+x))))
        temp=sumovernj
        return(-temp)
      }
      est.beta[j]<-optimize(obj.reest, c(-4, 4), tol = 0.0001)$minimum
    }
    ll[j]=sum(log(exp(responsesij.f*(thetai.f-bj+est.beta[j]))*(thetai.f-bj+est.beta[j])/(exp(thetai.f-bj+est.beta[j])-1)))
    Nperitem[j]=length(responsesij.f)
  }
  
  #eta=0
  #hist(est.beta)
  #est.beta[which(est.beta<0.05)]=0
  est.beta.mat[k,]=est.beta
  AIC=-2*sum(ll)+2*sum(est.beta!=0)
  BIC=-2*sum(ll)+log(mean(Njf.k))*sum(est.beta!=0)
  BIC2=-2*sum(ll)+log(min(Njf.k))*sum(est.beta!=0)
  lls[k]=sum(ll)
  AIC.vec[k]=AIC
  BIC.vec[k]=BIC
  BIC.vec2[k]=BIC2
}

plot(-2*lls)
plot(AIC.vec)
plot(BIC.vec)
plot(BIC.vec2)
kk=which.min(BIC.vec)
eta.vec[kk]
regbeta3=est.beta.mat[kk,]
length(which(est.beta.mat[kk,]!=0))
(regresult3=which(est.beta.mat[kk,]!=0))

table(Nperitem)

kk2=which.min(BIC.vec2)
eta.vec[kk2]
est.beta.mat[kk2,]
length(which(est.beta.mat[kk2,]!=0))
which(est.beta.mat[kk2,]!=0)





#######################################################################
#
#    Marginal lasso with SRT likelihood (use Female as focal group)
#
#######################################################################

grd=numeric(length(spnames20cut))
for (j in 1:length(spnames20cut)){
  grd[j]=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
}
J=length(spnames20cut)
#J=length(keep_for_reg)
beta.mat=rep(0.5,J)
eta.vec=seq(10,40,10)
est.beta.mat=matrix(1,length(eta.vec),J)
lls=AIC.vec=BIC.vec=BIC.vec2=numeric(length(eta.vec))
N.vec=c(length(unique(male_id)),length(unique(female_id)))
N=length(unique(male_id))+length(unique(female_id))
for (k in 1:length(eta.vec)){
  eta=eta.vec[k]
  ll=numeric(J)
  # Gauss-Hermite quadrature nodes
  X1=seq(-3,3,by=0.5)
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
  Sig.est=rbind(1,1) #rbind(Sig100,Sig200,Sig300)
  Mu.est=c(2,2) #c(mu100,mu200,mu300)
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
      #log.pij.prod=matrix(0,G,N.vec[yy])
      pij.prod=matrix(1,G,N.vec[yy])
      #pij17=numeric(J)
      for (j in 1:J)
      {
        pij=matrix(1,G,N.vec[yy])
        bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
        
        # response focal group person i answered for item j
        responsesij.f=female_resp[which(female_resp$itemname==spnames20cut[j]),"score"]
        responsesij.r=male_resp[which(male_resp$itemname==spnames20cut[j]),"score"]
        responsesij.rf=list(responsesij.r,responsesij.f)[[yy]]
        
        groupq.r=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
        groupq.f=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
        group.rf=list(groupq.r,groupq.f)[[yy]]
        for (g in 1:G)
        {
          pij[g,group.rf]=exp(responsesij.rf*(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))/((exp((-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))-1)/(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))
        }
        #log.pij.prod=log.pij.prod+log(pij)
        pij.prod=pij.prod*pij
        #pij17[j]=pij[10,7]
      }
      #LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(exp(log.pij.prod)* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
      LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(pij.prod* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
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
    Mu.est[1]=2
    for (yy in 2:y){
      Mu.est[((yy-1)*r+1):((yy-1)*r+r)]=colSums(X*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
    }
    #update Sigma hat
    #Sig.hat.allgrp=Sig.est
    for (yy in 1:y){
      #Sig.hat.allgrp[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
      Sig.est[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
    }
    
    Xprime=X/sqrt(Sig.est[1,])
    
    for (yy in 1:y){
      #Sig.hat.allgrp[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
      Sig.est[yy,]=sum((Xprime-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
    }
    
    # M step
    active.set=1:J
    for (j in active.set){
      #for (j in 1:J){
      bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
      betaj=beta.mat[j]
      
      # response focal group person i answered for item j
      responsesij.f=female_resp[which(female_resp$itemname==spnames20cut[j]),"score"]
      
      groupq.f=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
      
      Njf.k[j]=length(thetai.f)
      rLiA.f1 <- rLiA.f2 <- matrix(0,N.vec[2],G)
      rLiA.f1[groupq.f,]=responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
      rgk.f1= colSums(rLiA.f1)
      #plot(x.vals,rgk.f2)
      
      
      ngj=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
      
      obj= function(x){
        #focal group
        sumovernj=sum(rgk.f1*(X-bj+x)-ngj*log((exp(X-bj+x)-1)/(X-bj+x)))
        temp=sumovernj-eta*abs(x)
        return(-temp)
      }
      
      
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
      bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
      
      # response focal group person i answered for item j
      responsesij.f=female_resp[which(female_resp$itemname==spnames20cut[j]),"score"]
      
      groupq.f=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
      rLiA.f1 <- rLiA.f2 <- matrix(0,N.vec[2],G)
      rLiA.f1[groupq.f,]=responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
      rgk.f1= colSums(rLiA.f1)
      
      ngj=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
      
      obj2= function(x){
        sumovernj=0
        #focal group
        sumovernj=sum(rgk.f1*(X-bj+x)-ngj*log((exp(X-bj+x)-1)/(X-bj+x)))
        temp=sumovernj
        return(-temp)
      }
      #x.vals=seq(-3,3,0.2)
      #f.vals=numeric(length(x.vals))
      #for (i in 1: length(x.vals)){
      #  f.vals[i]=obj(x.vals[i])
      #}
      #plot(x.vals,f.vals)
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
    A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))]=dnorm(X, Mu.est.mat[yy,],sqrt(Sig.est.slice[,,yy]))
  }
  #calculation of n_g 
  grbeta.allgrp= matrix(0,(J*y),1) #grbeta1,grbeta2,grbeta3 "ncol=1" is the number of parameter beta, if we use multiple covariates later, change this value
  for (yy in 1:y){
    grbeta.allgrp[((yy-1)*J+1):((yy-1)*J+J),]=y.allgroup[yy,]*grbeta
  }
  
  LiA=matrix(double(N*G),N,G)
  for (yy in 1:y){
    #log.pij.prod=matrix(0,G,N.vec[yy])
    pij.prod=matrix(1,G,N.vec[yy])
    #pij17=numeric(J)
    for (j in 1:J)
    {
      pij=matrix(1,G,N.vec[yy])
      # response focal group person i answered for item j
      responsesij.f=female_resp[which(female_resp$itemname==spnames20cut[j]),"score"]
      responsesij.r=male_resp[which(male_resp$itemname==spnames20cut[j]),"score"]
      responsesij.rf=list(responsesij.r,responsesij.f)[[yy]]
      
      groupq.r=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
      groupq.f=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
      group.rf=list(groupq.r,groupq.f)[[yy]]
      for (g in 1:G)
      {
        pij[g,group.rf]=exp(responsesij.rf*(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))/((exp((-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))-1)/(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))
      }
      #log.pij.prod=log.pij.prod+log(pij)
      pij.prod=pij.prod*pij
      #pij17[j]=pij[10,7]
    }
    #LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(exp(log.pij.prod)* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
    LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(pij.prod* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
  }
  # calculate n_g
  Pi = apply(LiA,1,sum)
  
  ll=numeric(J)
  Njf.k=numeric(J)
  for (j in 1:J){
    bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
    betaj=grbeta[j]
    
    # response focal group person i answered for item j
    responsesij.f=female_resp[which(female_resp$itemname==spnames20cut[j]),"score"]
    groupq.r=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
    groupq.f=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
    Njf.k[j]=length(groupq.f)
    
    rLiA.f1 <- rLiA.f2 <- matrix(0,N.vec[2],G)
    rLiA.f1[groupq.f,]=responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
    rgk.f1= colSums(rLiA.f1)
    ngj.f=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
    ll[j]=sum(rgk.f1*(X-bj+betaj)-ngj.f*log((exp(X-bj+betaj)-1)/(X-bj+betaj)))
  }
  est.beta.mat[k,]=grbeta
  BIC=-2*sum(ll)+log(mean(Njf.k))*sum(grbeta!=0)
  BIC.vec[k]=BIC
  BIC2=-2*sum(ll)+log(min(Njf.k))*sum(grbeta!=0)
  BIC.vec2[k]=BIC2
  AIC=-2*sum(ll)+3*log(log(mean(Njf.k)))*log(mean(Njf.k))*sum(grbeta!=0)
  AIC.vec[k]=AIC
  lls[k]=sum(ll)
}

length(which( DIFitems2%in%(which(grbeta!=0))))
length(which(grbeta!=0))
length(which( DIFitems2%in%(which(grbeta!=0))))/length(DIFitems2)

grbeta_eta60=grbeta
which(grbeta_eta60!=0)
#write.csv(grbeta_eta10,file = "marginalReg_grbeta_eta10.csv")

plot(BIC.vec)

kk=which.min(BIC.vec2)
eta.vec[kk]
regresult5=which(est.beta.mat[1,]!=0)
length(which(est.beta.mat[kk,]!=0))
#which(est.beta.mat[kk,]!=0)
(regresult2=which(est.beta.mat[kk,]!=0))

table(Nperitem)

kk2=which.min(BIC.vec2)
eta.vec[kk2]
est.beta.mat[kk2,]
length(which(est.beta.mat[kk2,]!=0))
which(est.beta.mat[kk2,]!=0)


kk3=which.min(AIC.vec)
eta.vec[kk3]
est.beta.mat[kk3,]


which(est.beta.mat[1,]!=0)






#######################################################################
#
#    Marginal lasso with SRT likelihood (use Male as focal group)
#
#######################################################################



grd=numeric(length(spnames20cut))
for (j in 1:length(spnames20cut)){
  grd[j]=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
}
J=length(spnames20cut)
#J=length(keep_for_reg)
beta.mat=rep(0.5,J)
eta.vec=seq(1,5,1)
est.beta.mat=matrix(1,length(eta.vec),J)
lls=AIC.vec=BIC.vec=BIC.vec2=numeric(length(eta.vec))
N.vec_femref=c(52334,57792)
N=length(unique(male_id))+length(unique(female_id))
for (k in 1:length(eta.vec)){
  eta=eta.vec[k]
  ll=numeric(J)
  # Gauss-Hermite quadrature nodes
  X1=seq(-3,3,by=0.5)
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
  Sig.est=rbind(1,1) #rbind(Sig100,Sig200,Sig300)
  Mu.est=c(2,2) #c(mu100,mu200,mu300)
  df.beta <- df.Mu <- df.Sig <- 1
  iter <- 0
  # start EM cycle 
  while(max(df.beta)>1e-3 | max(df.Mu)>1e-3 | max(df.Sig)>1e-3)
    #while(max(df.Mu)>1e-3 | max(df.Sig)>1e-3)  
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
      #log.pij.prod=matrix(0,G,N.vec[yy])
      pij.prod=matrix(1,G,N.vec_femref[yy])
      pij17=numeric(J)
      for (j in 1:J)
      {
        pij=matrix(1,G,N.vec_femref[yy])
        bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
        
        # response focal group person i answered for item j
        responsesij.f=male_resp[which(male_resp$itemname==spnames20cut[j]),"score"]
        responsesij.r=female_resp[which(female_resp$itemname==spnames20cut[j]),"score"]
        responsesij.rf=list(responsesij.r,responsesij.f)[[yy]]
        
        groupq.r=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
        groupq.f=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
        group.rf=list(groupq.r,groupq.f)[[yy]]
        for (g in 1:G)
        {
          pij[g,group.rf]=exp(responsesij.rf*(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))/((exp((-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))-1)/(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))
        }
        #log.pij.prod=log.pij.prod+log(pij)
        pij.prod=pij.prod*pij
        pij17[j]=pij[10,7]
      }
      #LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(exp(log.pij.prod)* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
      LiA[(sum(N.vec_femref[1:yy])-N.vec_femref[yy]+1):(sum(N.vec_femref[1:yy])),]=t(pij.prod* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
    }
    # calculate n_g
    Pi = apply(LiA,1,sum)
    ng.all = apply(LiA/Pi,2,sum)
    ng.allgrp=numeric(G*y)
    for (yy in 1:y){
      ng.allgrp[((yy-1)*G+1):((yy-1)*G+G)]=apply(LiA[(sum(N.vec_femref[1:yy])-N.vec_femref[yy]+1):(sum(N.vec_femref[1:yy])),]/Pi[(sum(N.vec_femref[1:yy])-N.vec_femref[yy]+1):(sum(N.vec_femref[1:yy]))],2,sum)
    }
    ng=c(ng.all,ng.allgrp)
    
    #update mu hat and Sigma hat
    Mu.est=numeric(r*y)
    for (yy in 2:y){
      Mu.est[((yy-1)*r+1):((yy-1)*r+r)]=colSums(X*ng[(yy*G+1):(yy*G+G)])/N.vec_femref[yy]
    }
    #update Sigma hat
    #Sig.hat.allgrp=Sig.est
    for (yy in 1:y){
      #Sig.hat.allgrp[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
      Sig.est[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec_femref[yy]
    }
    
    Xprime=X/sqrt(Sig.est[1,])
    
    for (yy in 1:y){
      #Sig.hat.allgrp[yy,]=sum((X-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec[yy]
      Sig.est[yy,]=sum((Xprime-rep(Mu.est[yy],G))^2*ng[(yy*G+1):(yy*G+G)])/N.vec_femref[yy]
    }
    #df.Mu =abs(muold-Mu.est)
    #df.Sig=abs(sigold-Sig.est)
    
    # M step
    active.set=1:J
    for (j in active.set){
      #for (j in 1:J){
      bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
      betaj=beta.mat[j]
      
      # response focal group person i answered for item j
      responsesij.f=male_resp[which(male_resp$itemname==spnames20cut[j]),"score"]
      
      groupq.f=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
      
      Njf.k[j]=length(thetai.f)
      rLiA.f1 <- rLiA.f2 <- matrix(0,N.vec_femref[2],G)
      rLiA.f1[groupq.f,]=responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
      rgk.f1= colSums(rLiA.f1)
      #plot(x.vals,rgk.f2)
      
      
      ngj=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
      
      obj= function(x){
        #focal group
        sumovernj=sum(rgk.f1*(X-bj+x)-ngj*log((exp(X-bj+x)-1)/(X-bj+x)))
        temp=sumovernj-eta*abs(x)
        return(-temp)
      }
      
      
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
      bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
      
      # response focal group person i answered for item j
      responsesij.f=male_resp[which(male_resp$itemname==spnames20cut[j]),"score"]
      
      groupq.f=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
      rLiA.f1 <- rLiA.f2 <- matrix(0,N.vec_femref[2],G)
      rLiA.f1[groupq.f,]=responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
      rgk.f1= colSums(rLiA.f1)
      
      ngj=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
      
      obj2= function(x){
        sumovernj=0
        #focal group
        sumovernj=sum(rgk.f1*(X-bj+x)-ngj*log((exp(X-bj+x)-1)/(X-bj+x)))
        temp=sumovernj
        return(-temp)
      }
      #x.vals=seq(-3,3,0.2)
      #f.vals=numeric(length(x.vals))
      #for (i in 1: length(x.vals)){
      #  f.vals[i]=obj(x.vals[i])
      #}
      #plot(x.vals,f.vals)
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
    A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))]=dnorm(X, Mu.est.mat[yy,],sqrt(Sig.est.slice[,,yy]))
  }
  #calculation of n_g 
  grbeta.allgrp= matrix(0,(J*y),1) #grbeta1,grbeta2,grbeta3 "ncol=1" is the number of parameter beta, if we use multiple covariates later, change this value
  for (yy in 1:y){
    grbeta.allgrp[((yy-1)*J+1):((yy-1)*J+J),]=y.allgroup[yy,]*grbeta
  }
  
  LiA=matrix(double(N*G),N,G)
  for (yy in 1:y){
    #log.pij.prod=matrix(0,G,N.vec[yy])
    pij.prod=matrix(1,G,N.vec_femref[yy])
    #pij17=numeric(J)
    for (j in 1:J)
    {
      pij=matrix(1,G,N.vec_femref[yy])
      # response focal group person i answered for item j
      responsesij.f=male_resp[which(male_resp$itemname==spnames20cut[j]),"score"]
      responsesij.r=male_resp[which(female_resp$itemname==spnames20cut[j]),"score"]
      responsesij.rf=list(responsesij.r,responsesij.f)[[yy]]
      
      groupq.r=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
      groupq.f=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
      group.rf=list(groupq.r,groupq.f)[[yy]]
      for (g in 1:G)
      {
        pij[g,group.rf]=exp(responsesij.rf*(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))/((exp((-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))-1)/(-grd[j]+X[g,]+grbeta.allgrp[((yy-1)*J+j),]))
      }
      #log.pij.prod=log.pij.prod+log(pij)
      pij.prod=pij.prod*pij
      #pij17[j]=pij[10,7]
    }
    #LiA[(sum(N.vec[1:yy])-N.vec[yy]+1):(sum(N.vec[1:yy])),]=t(exp(log.pij.prod)* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
    LiA[(sum(N.vec_femref[1:yy])-N.vec_femref[yy]+1):(sum(N.vec_femref[1:yy])),]=t(pij.prod* A.allgroups[((yy-1)*nrow(X)+1):((yy-1)*nrow(X)+nrow(X))])
  }
  # calculate n_g
  Pi = apply(LiA,1,sum)
  
  ll=numeric(J)
  Njf.k=numeric(J)
  for (j in 1:J){
    bj=as.numeric(spdifficulties[which(spdifficulties[,1]==spnames20cut[j]),3])
    betaj=grbeta[j]
    
    # response focal group person i answered for item j
    responsesij.f=male_resp[which(male_resp$itemname==spnames20cut[j]),"score"]
    groupq.r=which(unique(female_id)%in%female_resp[which(female_resp$itemname==spnames20cut[j]),"person_id"])
    groupq.f=which(unique(male_id)%in%male_resp[which(male_resp$itemname==spnames20cut[j]),"person_id"])
    Njf.k[j]=length(groupq.f)
    
    rLiA.f1 <- rLiA.f2 <- matrix(0,N.vec_femref[2],G)
    rLiA.f1[groupq.f,]=responsesij.f*LiA[groupq.f,]/Pi[groupq.f]
    rgk.f1= colSums(rLiA.f1)
    ngj.f=apply(LiA[groupq.f,]/Pi[groupq.f],2,sum)
    ll[j]=sum(rgk.f1*(X-bj+betaj)-ngj.f*log((exp(X-bj+betaj)-1)/(X-bj+betaj)))
  }
  est.beta.mat[k,]=grbeta
  BIC=-2*sum(ll)+log(mean(Njf.k))*sum(grbeta!=0)
  BIC.vec[k]=BIC
  BIC2=-2*sum(ll)+log(min(Njf.k))*sum(grbeta!=0)
  BIC.vec2[k]=BIC2
  AIC=-2*sum(ll)+3*log(log(mean(Njf.k)))*log(mean(Njf.k))*sum(grbeta!=0)
  AIC.vec[k]=AIC
  lls[k]=sum(ll)
}
plot(lls)
plot(BIC.vec)
plot(BIC.vec2)
kk=which.min(BIC.vec)
eta.vec[kk]
(regresult_malefoc=which(est.beta.mat[kk,]!=0))
length(regresult_malefoc)

write.csv(est.beta.mat[kk,],file = "marginalReg_malefoc_grbeta_eta3.csv")

