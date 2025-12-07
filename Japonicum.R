schistoSEI = function(PARAMS)
{
  delta = 0.01
  ##------------------------------------------------------------------------
  q1 = combi[1,1]         #Recovery Rate Among Infected Humans
  q2 = combi[1,2]         #Recovery Rate Among Infected  Animals
  b1 = combi[1,3]         #Snail-to-Children Far from the S/RF/I Transmission Rate
  b2 = combi[1,4]         #Snail-to-Children Near the S/RF/I Transmission Rate
  b3 = combi[1,5]         #Snail-to-Adults Far from the S/RF/I Transmission Rate
  b4 = combi[1,6]         #Snail-to-Adults Near the S/RF/I Transmission Rate
  b5 = combi[1,7]         #Snail-to-Farm Animal Transmission Rate
  b6 = combi[1,8]         #Snail-to-Companion Animal Transmission Rate
  i1 = combi[1,9]        #Mortality Rate of Uninfected Snails
  i2 = combi[1,10]        #Mortality Rate of Infected Snails
  rC1 = combi[1,11]       #Probability of Eggs Reaching Water and Hatching on Children Far from the S/RF/I
  rC2 = combi[1,12]       #Probability of Eggs Reaching Water and Hatching on Children Near the S/RF/I
  rA1 = combi[1,13]       #Probability of Eggs Reaching Water and Hatching on Adults Far from the S/RF/I
  rA2 = combi[1,14]       #Probability of Eggs Reaching Water and Hatching on Adults Near the S/RF/I
  ##------------------------------------------------------------------------  
  aH = PARAMS$X1[1]    #Human Birth rate
  aF1 = PARAMS$X1[2]    #Farm Animal Birth rate
  aF2 = PARAMS$X1[3]    #Companion Animal Birth rate
  uH = PARAMS$X1[4]     #Lifespan of Humans (Death rate)
  uF1 = PARAMS$X1[5]     #Lifespan of Farm Animals (Death rate)
  uF2 = PARAMS$X1[6]     #Lifespan of Companion Animals (Death rate)
  KH1 = PARAMS$X1[7]    #Maximum number of children far
  KH2 = PARAMS$X1[8]    #Maximum number of children near
  KF1 = PARAMS$X1[11]    #Maximum number of Farm animal far
  KF2 = PARAMS$X1[12]   #Maximum number of Companion animal far
  KS = PARAMS$X1[13]    #Maximum Number of Snails in the Environment
  c = PARAMS$X1[14]     #Liters of Water in the Environment
  lh = PARAMS$X1[15]    #Latent Period in Humans
  lf1 = PARAMS$X1[16]    #Latent Period in Farm Animals
  lf2 = PARAMS$X1[17]    #Latent Period in Companion Animals
  fs = PARAMS$X1[18]    #Fecundity of Susceptible Snails
  fe = PARAMS$X1[19]    #Fecundity of Exposed Snails
  fi = PARAMS$X1[20]    #Fecundity of Infected Snails
  y = PARAMS$X1[21]     #Prepatent Period in Snails
  rF1 = PARAMS$X1[22]   #Probability of Eggs Reaching Water and Hatching on Farm Animals Far from the S/RF/I
  rF2 = PARAMS$X1[23]   #Probability of Eggs Reaching Water and Hatching on Companion Animals Near the S/RF/I
  g11 = PARAMS$X1[24]    #Eggs Excreted by an Infected Children
  g22 = PARAMS$X1[25]    #Eggs Excreted by an Infected Adults
  g33 = PARAMS$X1[26]    #Eggs Excreted by an Infected Animal
  dm = PARAMS$X1[27]    #Lifespan of Miracidia
  dc = PARAMS$X1[28]    #Lifespan of Cercariae
  s = PARAMS$X1[29]      #Shedding Rate of Snails
  J = PARAMS$X1[30]     #Proportion of children becoming adults after 1 year
  e = PARAMS$X1[31]         #Exposure Rate of Snails to Miracidia
  
  a = array(0,c(numSteps))
  SC1 = SC2 = SA1 = SA2 = SF1 = SF2 = SS = EC1 = EC2 = EA1 = EA2 = EF1 = EF2 = ES = a
  IC1 = IC2 = IA1 = IA2 = IF1 = IF2 = IS = M1 = M2 = M3 = M4 = Cer = a
  NH1 = NH2 = NH3 = NH4 = NH = NF1 = NF2 = NS = a
  time = array(0,numSteps)
  
  # Initialise the time-dependent variables, i.e. setting the values of the variables at time 0
  
  SC1[1] = VARS$X1[1];                          #Susceptible Children Far from the S/RF/I 
  SC2[1] = VARS$X1[2];                          #Susceptible Children Near the S/RF/I
  SA1[1] = VARS$X1[3];                          #Susceptible Adults Far from the S/RF/I 
  SA2[1] = VARS$X1[4];                          #Susceptible Adults Near the S/RF/I
  SF1[1] = VARS$X1[5];                           #Susceptible Farm Animal Far from the S/RF/I 
  SF2[1] = VARS$X1[6];                           #Susceptible Companion Animal Near the S/RF/I 
  SS[1] = VARS$X1[7];                            #Susceptible Snail
  EC1[1] = VARS$X1[8];                            #Exposed Children Far from the S/RF/I
  EC2[1] = VARS$X1[9];                            #Exposed Children Near the S/RF/I
  EA1[1] = VARS$X1[10];                            #Exposed Adults Far from the S/RF/I
  EA2[1] = VARS$X1[11];                            #Exposed Adults Near the S/RF/I 
  EF1[1] = VARS$X1[12];                            #Exposed Farm Animal Far from the S/RF/I
  EF2[1] = VARS$X1[13];                            #Exposed Companion Animal Near the S/RF/I
  ES[1] = VARS$X1[14];                             #Exposed Snail
  IC1[1] = VARS$X1[15];                            #Infected Children Far from the S/RF/I
  IC2[1] = VARS$X1[16];                            #Infected Children Near the S/RF/I
  IA1[1] = VARS$X1[17];                            #Infected Adults Far from the S/RF/I
  IA2[1] = VARS$X1[18];                            #Infected Adults Near the S/RF/I
  IF1[1] = VARS$X1[19];                            #Infected Farm Animal Far from the S/RF/I
  IF2[1] = VARS$X1[20];                            #Infected Companion Animal Near the S/RF/I
  IS[1] = VARS$X1[21];                             #Infected Snail
  M1[1] = VARS$X1[22];                             #Miracidia - Children to Snail
  M2[1] = VARS$X1[23];                             #Miracidia - Adults to Snail
  M3[1] = VARS$X1[24];                             #Miracidia - Animal to Snail
  Cer[1] = VARS$X1[25];                            #Cercariae - Snail to Human/Animal
  NH1[1] = SC1[1] + EC1[1] + IC1[1];     #Total Children Population Far (All Infection Statuses)
  NH2[1] = SC2[1] + EC2[1] + IC2[1];     #Total Children Population Near (All Infection Statuses)
  NH3[1] = SA1[1] + EA1[1] + IA1[1];     #Total Adult Population Far (All Infection Statuses)
  NH4[1] = SA2[1] + EA2[1] + IA2[1];     #Total Adult Population Near (All Infection Statuses)
  NH[1] = NH1[1]+NH2[1]+NH3[1]+NH4[1]    #Total Human population
  NF1[1] = SF1[1] + EF1[1] + IF1[1];     #Total Farm Animal Population Far (All Infection Statuses)
  NF2[1] = SF2[1] + EF2[1] + IF2[1];     #Total Companion Animal Population Near (All Infection Statuses)
  NS[1] = SS[1] + ES[1] + IS[1];         #Total Snail Population (All Infection Statuses)
  time[1] = 0;                           #time
  
  # SEI difference equations
  for (i in 1:(numSteps-1))
  { 
    g1 = ceiling(log(runif(1,0,1))/log(1-1/g11))
    g2 = ceiling(log(runif(1,0,1))/log(1-1/g22))
    g3 = ceiling(log(runif(1,0,1))/log(1-1/g33))
    
    NH1[i] <- SC1[i] + EC1[i] + IC1[i];
    NH2[i] <- SC2[i] + EC2[i] + IC2[i];
    
    NH3[i] <- SA1[i] + EA1[i] + IA1[i];
    NH4[i] <- SA2[i] + EA2[i] + IA2[i];
    
    NH[i] <- NH1[i] + NH2[i] + NH3[i] + NH4[i];
    
    NF1[i] <- SF1[i] + EF1[i] + IF1[i];
    NF2[i] <- SF2[i] + EF2[i] + IF2[i];
    
    NS[i] <- SS[i] + ES[i] + IS[i];
    
    SC1[i+1] <- SC1[i] + (aH*NH1[i]*(1 - NH1[i]/KH1) - J*NH[i] + q1*IC1[i] - b1*(Cer[i]/c)*SC1[i] - uH*SC1[i]) * delta;
      if(SC1[i+1] < 0) SC1[i+1] <- 0;
    EC1[i+1] <- EC1[i] + (b1*(Cer[i]/c)*SC1[i] - lh*EC1[i] - uH*EC1[i]) * delta;
      if(EC1[i+1] < 0) EC1[i+1] <- 0;
    IC1[i+1] <- IC1[i] + (lh*EC1[i] - uH*IC1[i] - q1*IC1[i]) * delta;
      if(IC1[i+1] < 0) IC1[i+1] <- 0;
    
    SC2[i+1] <- SC2[i] + (aH*NH2[i]*(1 - NH2[i]/KH2) - J*NH[i] + q1*IC2[i] - b2*(Cer[i]/c)*SC2[i] - uH*SC2[i]) * delta;
      if(SC2[i+1] < 0) SC2[i+1] <- 0;
    EC2[i+1] <- EC2[i] + (b2*(Cer[i]/c)*SC2[i] - lh*EC2[i] - uH*EC2[i]) * delta;
      if(EC2[i+1] < 0) EC2[i+1] <- 0;
    IC2[i+1] <- IC2[i] + (lh*EC2[i] - uH*IC2[i] - q1*IC2[i]) * delta;
      if(IC2[i+1] < 0) IC2[i+1] <- 0;
    
    SA1[i+1] <- SA1[i] + (J*NH[i] + q1*IA1[i] - b3*(Cer[i]/c)*SA1[i] - uH*SA1[i]) * delta;
      if(SA1[i+1] < 0) SA1[i+1] <- 0;
    EA1[i+1] <- EA1[i] + (b3*(Cer[i]/c)*SA1[i] - lh*EA1[i] - uH*EA1[i]) * delta;
      if(EA1[i+1] < 0) EA1[i+1] <- 0;
    IA1[i+1] <- IA1[i] + (lh*EA1[i] - uH*IA1[i] - q1*IA1[i]) * delta;
      if(IA1[i+1] < 0) IA1[i+1] <- 0;
    
    SA2[i+1] <- SA2[i] + (J*NH[i] + q1*IA2[i] - b4*(Cer[i]/c)*SA2[i] - uH*SA2[i]) * delta;
      if(SA2[i+1] < 0) SA2[i+1] <- 0;
    EA2[i+1] <- EA2[i] + (b4*(Cer[i]/c)*SA2[i] - lh*EA2[i] - uH*EA2[i]) * delta;
      if(EA2[i+1] < 0) EA2[i+1] <- 0;
    IA2[i+1] <- IA2[i] + (lh*EA2[i] - uH*IA2[i] - q1*IA2[i]) * delta;
      if(IA2[i+1] < 0) IA2[i+1] <- 0;
    
    SF1[i+1] <- SF1[i] + (aF1*NF1[i]*(1 - NF1[i]/KF1) + q2*IF1[i] - b5*(Cer[i]/c)*SF1[i] - uF1*SF1[i]) * delta;
      if(SF1[i+1] < 0) SF1[i+1] <- 0;
    EF1[i+1] <- EF1[i] + (b5*(Cer[i]/c)*SF1[i] - lf1*EF1[i] - uF1*EF1[i]) * delta;
      if(EF1[i+1] < 0) EF1[i+1] <- 0;
    IF1[i+1] <- IF1[i] + (lf1*EF1[i] - uF1*IF1[i] - q2*IF1[i]) * delta;
      if(IF1[i+1] < 0) IF1[i+1] <- 0;
    
    SF2 [i+1]<- SF2[i] + (aF2*NF2[i]*(1 - NF2[i]/KF2) + q2*IF2[i] - b6*(Cer[i]/c)*SF2[i] - uF2*SF2[i]) * delta;
      if(SF2[i+1] < 0) SF2[i+1] <- 0;
    EF2[i+1] <- EF2[i] + (b6*(Cer[i]/c)*SF2[i] - lf2*EF2[i] - uF2*EF2[i]) * delta;
      if(EF2[i+1] < 0) EF2[i+1] <- 0;
    IF2[i+1] <- IF2[i] + (lf2*EF2[i] - uF2*IF2[i] - q2*IF2[i]) * delta;
      if(IF2[i+1] < 0) IF2[i+1] <- 0;
    
    SS[i+1] <- SS[i] + ((fs*SS[i] + fe*ES[i] + fi*IS[i])*(1 - NS[i]/KS) - i1*SS[i] - e*(M1[i]/c)*SS[i] - e*(M2[i]/c)*SS[i] - e*(M3[i]/c)*SS[i]) * delta;
      if(SS[i+1] < 0) SS[i+1] <- 0;
    ES[i+1] <- ES[i] + (e*(M1[i]/c)*SS[i] + e*(M2[i]/c)*SS[i] + e*(M3[i]/c)*SS[i] - i2*ES[i] - y*ES[i]) * delta;
      if(ES[i+1] < 0) ES[i+1] <- 0;
    IS[i+1] <- IS[i] + (y*ES[i] - i2*IS[i]) * delta;
      if(IS[i+1] < 0) IS[i+1] <- 0;
    
    M1[i+1] <- M1[i] + ((rC1*IC1[i] + rC2*IC2[i])*g1 - dm*M1[i] - e*NS[i]*M1[i]/c) * delta;
      if(M1[i+1] < 0) M1[i+1] <- 0;
    M2[i+1] <- M2[i] + ((rA1*IA1[i] + rA2*IA2[i])*g2 - dm*M2[i] - e*NS[i]*M2[i]/c) * delta;
      if(M2[i+1] < 0) M2[i+1] <- 0;
    M3[i+1] <- M3[i] + ((rF1*IF1[i] + rF2*IF2[i])*g3 - dm*M3[i] - e*NS[i]*M3[i]/c) * delta;
      if(M3[i+1] < 0) M3[i+1] <- 0;
    
    Cer[i+1] <- Cer[i] + (s*IS[i] - dc*Cer[i]) * delta;
      if(Cer[i+1] < 0) Cer[i+1] <- 0;
    
    time[i+1] = time[i] + delta
  }
  
  NH1[i+1] <- SC1[i+1] + EC1[i+1] + IC1[i+1];
  NH2[i+1] <- SC2[i+1] + EC2[i+1] + IC2[i+1];
  
  NH3[i+1] <- SA1[i+1] + EA1[i+1] + IA1[i+1];
  NH4[i+1] <- SA2[i+1] + EA2[i+1] + IA2[i+1];
  
  NH[i+1] <- NH1[i+1] + NH2[i+1] + NH3[i+1] + NH4[i+1]
  
  NF1[i+1] <- SF1[i+1] + EF1[i+1] + IF1[i+1];
  NF2[i+1] <- SF2[i+1] + EF2[i+1] + IF2[i+1];
  
  NS[i+1] <- SS[i+1] + ES[i+1] + IS[i+1];
  
  out = list(SC1=SC1, EC1=EC1, IC1=IC1, SC2=SC2, EC2=EC2, IC2=IC2, SA1=SA1, EA1=EA1, IA1=IA1, 
             SA2=SA2, EA2=EA2, IA2=IA2, SF1=SF1, EF1=EF1, IF1=IF1, SF2=SF2, EF2=EF2, IF2=IF2, 
             SS=SS, ES=ES, IS=IS, M1=M1, M2=M2, M3=M3, Cer=Cer, time=time)
  return(out)
}