#Either use Parameter sweep
#..and don't forget to uncomment the result recording and the two }} at the end of the file
#restable<-NULL
#for (up in seq(from=35,to=75,by=10)){
#for (down in seq(from=1,to=5,by=1)){
#if (up<=down) next;
#cat("Parameter sweep up/down:   ",up," ",down,"\n")

#Or set the autoscaling thresholds here. Units depend on algorithm chosen.
#up<-60
#down<-1

#This script models a single-tier autoscaler operating on a time-series trace
library(pdq)
library(queueing)

#Define
 #Simulation parameters - user settable
simstep<-15        #how long is one step in minutes?
simtime<-simstep*4*24*7*2 #how long will the simulation run?
machines<-1        #initial number of machines
machines_min<-1    #minimal number of machines
machines_max<-4    #maximal number of machines
machines2<-1        #initial number of machines
machines2_min<-1    #minimal number of machines
machines2_max<-2    #maximal number of machines
#demand<-0.0306     #service demand of one request in s
##from utilization law U=XS/M for bender(M=4)-hourly:0.0816,0.102,1.328, weekly:0.1516,0.0588,0.1484; oe(M=8)-hourly:0.02448,0.02584,0.0252, weekly:0.00965312,0.03069341,0.03051783
ApdexS_goal<-0.04  #parameter for GoS calculation in s
ApdexC_goal<-1     #leave at 1


 #Trace parameters - user settable
series<-oe_user_ts          #which time-series to use
demand<-0.0306
demand2<-0.02
offset<-4*24*7*4*0+4*24*25+4*10      #where to start
#series<-bender_user_ts      #which time-series to use
#demand<-0.15
#ApdexS_goal<-0.2
#offset<-4*24*7*4*1+4*24*5+4*10       #where to start
#series<-gaff_user_ts         #which time-series to use
#demand<-0.035
#offset<-4*24*7*4*0+4*24*11+4*10      #where to start
##oe_user$datum[] gives the original mysql timestamp

multiplier<-1/demand/100*16 #trace interpreted as requests per second. If it is load, write how many req/s is 1%. Multiplied by 16.
##set to 8 for gaff, 16 otherwise!

 #Initializations
steps<-2         #we actually start at index 2. Needed for stair-step graphs.
machines[2]<-machines[1]
machines2[2]<-machines2[1]
lambda<-0;
mu<-1/demand     #trivial formula
added_before<-0  #the cooldown timer
starts<-0
stops<-0
added_before2<-0
starts2<-0
stops2<-0

  #Result variables
xc<-0  #time coordinates
uc<-0  #PDQ utilizations
uc2<-0
qc<-0  #PDQ queue lenghts
qc2<-0
rc<-0  #PDQ response times
rc1<-0
rc2<-0

#Main loop
for (i in seq(from=simstep,to=simtime,by=simstep)){
  
 #Define load
  lambda[steps]<-series[offset+steps]*multiplier

  singlenodepdq<-function(lambda, machines, demand){
    #Define and solve the PDQ model
    pdq::Init("model")    
    pdq::CreateOpen("requests", lambda)
    pdq::CreateMultiNode(machines, "WebServer", CEN, MSQ)
    pdq::SetDemand("WebServer", "requests", demand)
    pdq::Solve(CANON)
    
    #Extract results  
    u <- pdq::GetUtilization("WebServer", "requests", TRANS)
    q <- pdq::GetQueueLength("WebServer", "requests", TRANS)
    r <- pdq::GetResponse(TRANS, "requests") * 10^3
    return(data.frame(cbind(u,r,q)))
  }
  
 #Simulate  
  #Check for overload condition (lambda>mu)
  if (lambda[steps]<(machines[steps]/demand) && lambda[steps]<(machines2[steps]/demand2)){
  ## not overloaded now
    
   #If there was an overload on the previous period
    if (qc[steps-1]>-simstep*60*(lambda[steps]-(machines[steps]/demand)) || qc2[steps-1]>-simstep*60*(lambda[steps]-(machines2[steps]/demand2))){
    ##queue bigger than can be serviced in one step in addition to new arrivals

    #Amortize queue 
      xc[steps] <- i
    
    #Determine, which tier was overloaded, then compute model for the other one with lambda=<mu of the other one>
      if (qc[steps-1]>-simstep*60*(lambda[steps]-(machines[steps]/demand)) && qc2[steps-1]>-simstep*60*(lambda[steps]-(machines2[steps]/demand2))){  
      ##both tiers
        uc[steps] <- 1
        uc2[steps] <- 1
        ##U=lambda/mu - goes over 100%
        
        qc[steps] <- qc[steps-1]+simstep*60*(lambda[steps]-(machines[steps]/demand))
        ##last step steady-state queue length + how many will have accumulated for one simulation step - the whole term should be negative
        if (qc[steps]<0) {qc[steps]<-0}
        rc1[steps] <- demand/machines[steps]*(1+qc[steps])*1000
        ##R=D/M*(1+q)
        
        qc2[steps] <- qc2[steps-1]+simstep*60*(lambda[steps]-(machines2[steps]/demand2))
        ##last step steady-state queue length + how many will have accumulated for one simulation step - the whole term should be negative
        if (qc2[steps]<0) {qc2[steps]<-0}
        rc2[steps] <- demand2/machines2[steps]*(1+qc2[steps])*1000
        ##R=D/M*(1+q)
      } else if (qc[steps-1]>-simstep*60*(lambda[steps]-(machines[steps]/demand))){
      ##first tier
        uc[steps] <- 1 ##lambda[steps]/(machines[steps]/demand)
        ##U=lambda/mu - goes over 100%
        qc[steps] <- qc[steps-1]+simstep*60*(lambda[steps]-(machines[steps]/demand))
        ##last step steady-state queue length + how many will have accumulated for one simulation step - the whole term should be negative
        if (qc[steps]<0) {qc[steps]<-0}
        rc1[steps] <- demand/machines[steps]*(1+qc[steps])*1000
        ##R=D/M*(1+q)
        
        singlenoderesult<-singlenodepdq(machines[steps]/demand,machines2[steps],demand2)
        uc2[steps]<-singlenoderesult$u
        rc2[steps]<-singlenoderesult$r
        qc2[steps]<-singlenoderesult$q
      } else if (qc2[steps-1]>-simstep*60*(lambda[steps]-(machines2[steps]/demand2))){
      ##second tier
        uc2[steps] <- 1 ##lambda[steps]/(machines[steps]/demand)
        ##U=lambda/mu - goes over 100%
        qc2[steps] <- qc2[steps-1]+simstep*60*(lambda[steps]-(machines2[steps]/demand2))
        ##last step steady-state queue length + how many will have accumulated for one simulation step - the whole term should be negative
        if (qc2[steps]<0) {qc2[steps]<-0}
        rc2[steps] <- demand2/machines2[steps]*(1+qc2[steps])*1000
        ##R=D/M*(1+q)
        
        singlenoderesult<-singlenodepdq(machines2[steps]/demand2,machines[steps],demand)
        uc[steps]<-singlenoderesult$u
        rc1[steps]<-singlenoderesult$r
        qc[steps]<-singlenoderesult$q
      }
          
      rc[steps]<-rc1[steps]+rc2[steps]
    } else {
    
    #Define and solve the PDQ model - the main one, no overload situations
      pdq::Init("model")    
      pdq::CreateOpen("requests", lambda[steps])
      pdq::CreateMultiNode(machines[steps], "WebServer", CEN, MSQ)
      pdq::CreateMultiNode(machines2[steps], "DBServer", CEN, MSQ)
      pdq::SetDemand("WebServer", "requests", demand)
      pdq::SetDemand("DBServer", "requests", demand2)
      pdq::Solve(CANON)
   
    #Extract results  
      xc[steps] <- i
      uc[steps] <- pdq::GetUtilization("WebServer", "requests", TRANS)
      qc[steps] <- pdq::GetQueueLength("WebServer", "requests", TRANS)
      uc2[steps] <- pdq::GetUtilization("DBServer", "requests", TRANS)
      qc2[steps] <- pdq::GetQueueLength("DBServer", "requests", TRANS)
      rc[steps] <- pdq::GetResponse(TRANS, "requests") * 10^3
      rc1[steps] <- pdq::GetResidenceTime("WebServer", "requests", TRANS) * 10^3
      rc2[steps] <- pdq::GetResidenceTime("DBServer", "requests", TRANS) * 10^3
    }
  } else {
  #Estimate overload parameters
    xc[steps] <- i
  #Determine, which tier was overloaded, then compute model for the other one with lambda=<mu of the other one>
    if (lambda[steps]>=(machines[steps]/demand) && lambda[steps]>=(machines2[steps]/demand2)){
    ##both tiers
      uc[steps] <- 1
      uc2[steps] <- 1
      #U=lambda/mu - goes over 100%
      qc[steps] <- qc[steps-1]+simstep*60*(lambda[steps]-(machines[steps]/demand))
      ##last step steady-state queue length + how many will have accumulated for one simulation step
      if (qc[steps]<0) {qc[steps]<-0}
      rc1[steps] <- demand/machines[steps]*(1+qc[steps])*1000
      ##R=D/M*(1+q)
      qc2[steps] <- qc2[steps-1]+simstep*60*(lambda[steps]-(machines2[steps]/demand2))
      ##last step steady-state queue length + how many will have accumulated for one simulation step
      if (qc2[steps]<0) {qc2[steps]<-0}
      rc2[steps] <- demand2/machines2[steps]*(1+qc2[steps])*1000
      ##R=D/M*(1+q)
    } else if (lambda[steps]>=(machines[steps]/demand)){
    ##first tier
      uc[steps] <- 1 ##lambda[steps]/(machines[steps]/demand)
      ##U=lambda/mu - goes over 100%
      qc[steps] <- qc[steps-1]+simstep*60*(lambda[steps]-(machines[steps]/demand))
      ##last step steady-state queue length + how many will have accumulated for one simulation step
      if (qc[steps]<0) {qc[steps]<-0}
      rc1[steps] <- demand/machines[steps]*(1+qc[steps])*1000
      ##R=D/M*(1+q)
      
      singlenoderesult<-singlenodepdq(machines[steps]/demand,machines2[steps],demand2)
      uc2[steps]<-singlenoderesult$u
      rc2[steps]<-singlenoderesult$r
      qc2[steps]<-singlenoderesult$q
    } else if (lambda[steps]>=(machines2[steps]/demand2)){
    ##second tier
      uc2[steps] <- 1 ##lambda[steps]/(machines[steps]/demand)
      ##U=lambda/mu - goes over 100%
      qc2[steps] <- qc2[steps-1]+simstep*60*(lambda[steps]-(machines2[steps]/demand2))
      ##last step steady-state queue length + how many will have accumulated for one simulation step
      if (qc2[steps]<0) {qc2[steps]<-0}
      rc2[steps] <- demand2/machines2[steps]*(1+qc2[steps])*1000
      ##R=D/M*(1+q)
      
      singlenoderesult<-singlenodepdq(machines2[steps]/demand2,machines[steps],demand)
      uc[steps]<-singlenoderesult$u
      rc1[steps]<-singlenoderesult$r
      qc[steps]<-singlenoderesult$q
    }
    
    rc[steps]<-rc1[steps]+rc2[steps]
  }

#Autoscaling decision
 #Implement autoscaling policy - user settable
  #Function definitions
  utilization<-function(fast_up,up,down){
    if (is.na(uc[steps])) return(machines[steps])
    if (uc[steps]>=fast_up) {
      return(machines[steps]+4)
    } else if (uc[steps]>=up) {
      return(machines[steps]+1)
    } else if (uc[steps]<=down && added_before<0){
      return(machines[steps]-1)
    } else {
      return(machines[steps])
    }
  }

  utilization2<-function(fast_up,up,down){
    if (is.na(uc2[steps])) return(machines2[steps])
    if (uc2[steps]>=fast_up) {
      return(machines2[steps]+4)
    } else if (uc2[steps]>=up) {
      return(machines2[steps]+1)
    } else if (uc2[steps]<=down && added_before2<0){
      return(machines2[steps]-1)
    } else {
      return(machines2[steps])
    }
  }


  latency<-function(fast_up,up,down){
    if (rc[steps]>=fast_up) {
      return(machines[steps]+4)
    } else if (rc[steps]>=up) {
      return(machines[steps]+1)
    } else if (rc[steps]<=down && added_before<0){
      return(machines[steps]-1)
    } else {
      return(machines[steps])
    }
  }
  
  queue<-function(fast_up,up,down){
    if (qc[steps]>=fast_up) {
      return(machines[steps]+4)
    } else if (qc[steps]>=up) {
      return(machines[steps]+1)
    } else if (qc[steps]<=down && added_before<0){
      return(machines[steps]-1)
    } else {
      return(machines[steps])
    }
  }

  hybridlq<-function(fast_up,up,down){
    if (qc[steps]>=fast_up) {
      return(machines[steps]+4)
    } else if (rc[steps]>=up) {
      return(machines[steps]+1)
    } else if (qc[steps]<=down && added_before<0){
      return(machines[steps]-1)
    } else {
      return(machines[steps])
    }
  }

  hybridlu<-function(fast_up,up,down){
    if (qc[steps]>=fast_up) {
      return(machines[steps]+4)
    } else if (rc[steps]>=up) {
      return(machines[steps]+1)
    } else if (uc[steps]<=down && added_before<0){
      return(machines[steps]-1)
    } else {
      return(machines[steps])
    }
  }

  #Call one:
  #machines[steps+1]<-utilization(1.95,up/100,down/100)  
  machines[steps+1]<-utilization(1.95,70/100,20/100)
  machines2[steps+1]<-utilization2(1.95,70/100,20/100)
  #machines[steps+1]<-latency(350000,up,down)
  #machines[steps+1]<-queue(1000000,up,down)
  #machines[steps+1]<-hybridlq(1000000,up,down)
  #machines[steps+1]<-hybridlu(1000000,up,down/100)
 
 #Keep the number of machines within bounds
  if (machines[steps+1]>machines_max){
    machines[steps+1]<-machines_max
  } else if (machines[steps+1]<machines_min) {
    machines[steps+1]<-machines_min
  }
  if (machines[steps]<=0) break #failsafe
  if (machines2[steps+1]>machines2_max){
    machines2[steps+1]<-machines2_max
  } else if (machines2[steps+1]<machines2_min) {
    machines2[steps+1]<-machines2_min
  }
  if (machines2[steps]<=0) break #failsafe

 #Simple accounting of starts/stops
  if (machines[steps+1]>machines[steps]){
    starts=starts+machines[steps+1]-machines[steps]
    added_before<-simstep*4 #Cooldown policy on removal
  } else if (machines[steps+1]<machines[steps]){
    stops=stops+machines[steps]-machines[steps+1]
  }
  if (machines2[steps+1]>machines2[steps]){
    starts2=starts2+machines2[steps+1]-machines2[steps]
    added_before2<-simstep*4 #Cooldown policy on removal
  } else if (machines2[steps+1]<machines2[steps]){
    stops2=stops2+machines2[steps]-machines2[steps+1]
  }

 #Advance in time
  added_before<-added_before-simstep
  added_before2<-added_before2-simstep
  steps<-steps+1
} #End main loop

#Plot results
 #Add missing first values
xc[1]<-0
lambda[1]<-lambda[2]
uc[1]<-uc[2]
qc[1]<-qc[2]
rc[1]<-rc[2]
machines[1]<-machines[2]
uc2[1]<-uc2[2]
qc2[1]<-qc2[2]
rc1[1]<-rc1[2]
rc2[1]<-rc2[2]
machines2[1]<-machines2[2]
 #Utilization in percent
uc=uc*100
uc2=uc2*100
 #Last autoscaling decision not needed
length(machines)<-length(machines)-1
length(machines2)<-length(machines2)-1
 
 #Compute Apdex
  #Function definitions
ECwaitGoS<-function(m,u,t) 1-C_erlang(m,u)*exp(-(m-u)*t/demand)
##Probability of waiting shorter than t; u needs to be in Erlangs: machines[steps]*uc[steps]/100

ECdelayGoS<-function(m,u,t){
##Probability of waiting+service shorter than t
  temp<-0
  for (i in 1:length(m)){  #function C_erlang is not vector-friendly, hence the "for"
    if (is.na(u[i])) return(temp)
    if (u[i]>m[i]) u[i]=m[i]  #the formula gives negative values if model is not stable (lambda>mu)
    temp[i]<-1-if (u[i]!=m[i]-1) {
      C_erlang(m[i],u[i])/(u[i]+1-m[i])*(exp(-(m[i]-u[i])*mu*t)-exp(-mu*t))+exp(-mu*t)
    } else {
      (mu*t*C_erlang(m[i],u[i])+1)*exp(-mu*t)
    }
  }  
  return(temp)
}

Apdex<-function(m,u,goal){
  satisfied<-ECdelayGoS(m,u,goal)
  tolerating<-ECdelayGoS(m,u,goal*4)-satisfied
  frustrated<-1-satisfied-tolerating
  overall<-satisfied+tolerating/2
  return(data.frame(cbind(overall,satisfied,tolerating,frustrated)))
}

ECdelayGoStwo<-function(m1,u1,m2,u2,goal){
  temp<-0
  samplepoints<-seq(from=0,to=goal*2,length.out=100)
  ##we will be sampling the two distribution functions
  for (i in 1:length(m1)){ #we need to create a function at every point and evaluate it against the goal
    samples1<-0
    samples2<-0
    for (j in 1:100){samples1[j]<-ECdelayGoS(m1[i],u1[i],samplepoints[j])}
    for (j in 1:100){samples2[j]<-ECdelayGoS(m2[i],u2[i],samplepoints[j])}
    ##this samples the cumulative distribution functions
    GoSdens<-convolve(diff(samples),rev(diff(samples2)),type="o")
    ##we transform them to probability density functions and convolve to get the distribution (density, then cumulative) that the sum of the two waiting times will be lower than T ("Distribution of sum of independent random variables Z=X+Y is P(Z)=P(X)*P(Y)")
    if (sum(GoSdens)>0) {GoSdens<-GoSdens/sum(GoSdens)} ##normalize to get correct PDF sum of 1    
    GoSfunction<-approxfun(samplepoints,diffinv(GoSdens)[1:100])
    ##the linear convolution of sequences gives longer result than input; truncate
    temp[i]<-GoSfunction(goal)
  }
  return(temp)
}

Apdextwo<-function(m1,u1,m2,u2,goal){
  satisfied<-ECdelayGoStwo(m1,u1,m2,u2,goal)
  tolerating<-ECdelayGoStwo(m1,u1,m2,u2,goal*4)-satisfied
  frustrated<-1-satisfied-tolerating
  overall<-satisfied+tolerating/2
  return(data.frame(cbind(overall,satisfied,tolerating,frustrated)))
}

  #Computation on model results
#ApdexC<-Apdex(machines,machines*uc/100,ApdexC_goal)
#ApdexS<-Apdex(machines,machines*uc/100,ApdexS_goal)
ApdexC<-Apdextwo(machines,machines*uc/100,machines2,machines2*uc2/100,ApdexC_goal)
ApdexS<-Apdextwo(machines,machines*uc/100,machines2,machines2*uc/100,ApdexS_goal)

 #Plot utilization, queue, and response time models
  #Function definitions
fiveplot<-function(){
  par(mfrow=c(5,1),cex=0.5)
  plot(xc, lambda, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Load", xlab="time (min)", ylab="req/s lambda(N)")
  plot(xc, uc, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Utilization", xlab="time (min)", ylab="% U(N)")
  plot(xc, qc, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Queue Length", xlab="time (min)", ylab="reqs Q(N)")
  plot(xc, rc, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Response Time", xlab="time (min)", ylab="ms R(N)")
  plot(xc, machines, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Machines", xlab="time (min)", ylab="machines")
}

apdexplot<-function(){
  par(mfrow=c(4,2),cex=0.5)
  plot(xc, ApdexS$overall, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="ApdexS", xlab="time (min)", ylab="overall")
  plot(xc, ApdexC$overall, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="ApdexC", xlab="time (min)", ylab="overall")
  plot(xc, ApdexS$satisfied, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="ApdexS", xlab="time (min)", ylab="satisfied")
  plot(xc, ApdexC$satisfied, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="ApdexC", xlab="time (min)", ylab="satisfied")
  plot(xc, ApdexS$tolerating, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="ApdexS", xlab="time (min)", ylab="tolerating")
  plot(xc, ApdexC$tolerating, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="ApdexC", xlab="time (min)", ylab="tolerating")
  plot(xc, ApdexS$frustrated, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="ApdexS", xlab="time (min)", ylab="frustrated")
  plot(xc, ApdexC$frustrated, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="ApdexC", xlab="time (min)", ylab="frustrated")  
}

sixplot<-function(){
  par(mfrow=c(3,2),cex=0.5)
  plot(xc, lambda, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Load", xlab="time (min)", ylab="req/s lambda(N)")
  plot(xc, uc, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Utilization", xlab="time (min)", ylab="% U(N)")
  plot(xc, qc, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Queue Length", xlab="time (min)", ylab="reqs Q(N)")
  plot(xc, rc, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Response Time", xlab="time (min)", ylab="ms R(N)")
  plot(xc, machines, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Machines", xlab="time (min)", ylab="machines")
  plot(xc, ApdexS$overall, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="ApdexS", xlab="time (min)", ylab="overall")
}

twolayerplot<-function(){
  par(mfrow=c(3,2),cex=0.5)
  plot(xc, lambda, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Load", xlab="time (min)", ylab="req/s lambda(N)")
  plot(xc, rc, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Response Time", xlab="time (min)", ylab="ms R(N)")
  plot(xc, uc, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Utilization Layer 1", xlab="time (min)", ylab="% U(N)")
  plot(xc, uc2, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Utilization Layer 2", xlab="time (min)", ylab="% U(N)")
  plot(xc, machines, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Machines Layer 1", xlab="time (min)", ylab="machines")
  plot(xc, machines2, type="S", xaxp=c(0,simtime,simtime/simstep), xlim=c(0,simtime), col="blue", main="Machines Layer 2", xlab="time (min)", ylab="machines")
}

  #Call one:
#fiveplot()
#sixplot()
#apdexplot()
twolayerplot()

 #Print as text
printout<-function(){
  cat("Simulation load:  ",lambda,"\n")
  cat("Simulation time:  ",xc,"\n")
  cat("Utilizations:     ",uc,"\n")
  cat("Queue lenghts:    ",qc,"\n")
  cat("Reponse times:    ",rc,"\n")
  cat("Number of servers:",machines,"\n")
}

#if (sum(uc=100)) cat("Overload occured!\n")
cat("Machine hours used Layer 1:",sum(machines*simstep/60),"\n")
cat("Starts Layer 1:            ",starts,"\n")
cat("Stops Layer 1:             ",stops,"\n")
cat("Machine hours used Layer 2:",sum(machines2*simstep/60),"\n")
cat("Starts Layer 2:            ",starts2,"\n")
cat("Stops Layer 2:             ",stops2,"\n")
cat("ApdexC under 0.95 absolute:",sum(ApdexC$overall<0.95),"\n")
cat("ApdexS under 0.7 percent:  ",sum(ApdexS$overall<0.7)/length(ApdexS$overall)*100,"\n")
cat("Machine hours used Total:  ",sum(machines*simstep/60)+sum(machines2*simstep/60),"\n")
cat("Machines used 95% Layer 1: ",quantile(machines,probs=c(0.05,0.95)),"\n")
cat("Machines used 95% Layer 2: ",quantile(machines2,probs=c(0.05,0.95)),"\n")

#restable<-rbind(restable,c(4,256,up,down,sum(machines*simstep/60),starts,stops,sum(ApdexC$overall<0.95),sum(ApdexS$overall<0.7)/length(ApdexS$overall)*100))
#}} #End parameter sweep
#names(restable)<-c("series","method","up","down","hours","starts","stops","ApdexC","ApdexS")
