#DEVUELVE UN BAJO FACTIBLE DADO UNOS ACORDES

randBass<-function(p,t)
{
  bass<-NULL
  tbass<-NULL
  
  sumParcial=0
  sumTotal=0
  numBars<-sum(1/t)
  barIndex=(1/t[1])
  cualCompas=1
  i=1
  j=1
  
  while(sumTotal<numBars)
  {
    while(sumParcial<barIndex)
    {
      newTime<-(1/sample(c(1,2,4,8,16,32,64), size=1, replace=TRUE, prob=c(0,0.025,0.1,0.55,0.3,0.025,0)))
      while(newTime+sumParcial>barIndex)
      {
        newTime<-(1/sample(c(1,2,4,8,16,32,64), size=1, replace=TRUE, prob=c(0,0.025,0.1,0.55,0.3,0.025,0)))
      }
      #newNote<-darNotasKey("C")[sample(1:7,1)]
      newNote<-darNotas(p[cualCompas])[sample(1:3,1)]
      
      bass[i]=newNote
      tbass[i]=newTime
      i=i+1
      sumParcial=sumParcial+newTime
    }
    sumTotal=sumTotal+sumParcial
    sumParcial=0
    cualCompas=cualCompas+1
  }
  return(cbind(bass,1/tbass))
}
#=================================

#DEVUELVE LAS NOTAS EN UNA CLAVE
darNotasKey<-function(key)
{
  keys<-c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
  matKeys<-matrix(c("C","D","E","F","G","A","B","C#","D#","F","F#","G#","A#","C","D","E","F#","G","A","B","C#","D#","F","G","G#","A#","C","D","E","F#","G#","A","B","C#","D#","F","G","A","A#","C","D","E","F#","G#","A#","B","C#","D#","F","G","A","B","C","D","E","F#","G#","A#","C","C#","D#","F","G","A","B","C#","D","E","F#","G#","A#","C","D","D#","F","G","A","B","C#","D#","E","F#","G#","A#"),nrow = 12,ncol = 7,byrow = T)
  return(matKeys[which(keys==key),])
}
#=================================

#DEVUELVE LAS NOTAS EN UN ACORDE
darNotas<-function(chord)
{
  chords<-c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B","Cm","C#m","Dm","D#m","Em","Fm","F#m","Gm","G#m","Am","A#m","Bm","Cd","C#d","Dd","D#d","Ed","Fd","F#d","Gd","G#d","Ad","A#d","Bd")
  matProg<-matrix(c("C","E","G","C#","F","G#","D","F#","A","D#","G","A#","E","G#","B","F","A","C","F#","A#","C#","G","B","D","G#","C","D#","A","C#","E","A#","D","F","B","D#","F#","C","D#","G","C#","E","G#","D","F","A","D#","F#","A#","E","G","B","F","G#","C","F#","A","C#","G","A#","D","G#","B","D#","A","C","E","A#","C#","F","B","D","F#","C","D#","F#","C#","E","G","D","F","G#","D#","F#","A","E","G","A#","F","G#","B","F#","A","C","G","A#","C#","G#","B","D","A","C","D#","A#","C#","E","B","D","F"),nrow = 36,ncol = 3,byrow = T)
  return(matProg[which(chords==chord),])
}
#=================================

#DEVUELVE LA MATRIZ DE ENTRADA/SALIDA DEL BAJO ****
InOutBass<-function(times)#El times debe ser 1/8
{
  num<-length(times)
  matTimes<-matrix(0,nrow = num,ncol = 2)
  t<-times
  parcial<-0
  
  for (i in 1:num)
  {
    for(j in 1:2)
    {
      matTimes[i,j]=parcial
      if(j==1){parcial<-parcial+t[i]}
    }
  }
  return(matTimes)
}
#=================================

#DEVUELVE LA MATRIZ DE ENTRADA/SALIDA DE LA BATERIA ****
InOutDrum<-function(drum,times) #El times debe ser 1/8
{
  num<-length(drum)
  matTimes<-matrix(0,nrow = num,ncol = 3)
  t<-times
  parcial<-0
  
  for (i in 1:num)
  {
    for(j in 1:2)
    {
      matTimes[i,j]=parcial
      if(j==1){parcial<-parcial+t[i]}
    }
    matTimes[i,3]<-drum[i]
  }
  matTimes<- matTimes[matTimes[,3]!="S",]
  matTimes<-matTimes[,-3]
  
  return(matTimes)
}
#=================================

#DEVUELVE EL INTERVALO MUSICAL ENTRE UNA NOTA DEL BSS Y UNA DE LA MELODIA
intervalo<-function(mel,bass)
{
  temp<-NULL
  notas<-c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
  index=0
  val=0
  
  for (i in 1:length(notas))
  {
    if(mel==notas[i]){index=i}
    if(bass==notas[i]){val=i}
  }
  
  intervals<-c("Unisono","Segunda Menor","Segunda Mayor","Tercera Menor","Tercera Mayor","Cuarta Justa","Tritono","Quinta Justa","Sexta Menor","Sexta Mayor","Septima Menor","Septima Mayor")
  
  if(index<=val){temp<-intervals[val-index+1]}
  if(index>val){temp<-intervals[val-index+13]}
  if(index==0 || val==0){temp="---"}
  return(temp)
}
#=================================

#DEVUELVE LAS NOTAS DEL BAJO QUE SUENAN ENTRE DOS MOMENTOS ****
quesuena<-function(s,f,bass,tbass) #El tbass debe ser 1/8
{
  start=s
  final=f
  
  #tbass<-(1/tbass)
  temp<-NULL
  
  indice<-0
  las<-0
  nex<-0
  
  for (i in 1:length(bass))
  {
    nex<-nex+tbass[i] 
    
    if(start>=las && final<=nex)
    {
      indice<-indice+1
      temp[indice]<-bass[i]
    }
    
    if(start<=las && final > nex)
    {
      indice<-indice+1
      temp[indice]<-bass[i]
      
      cont=i
      
      while(final>nex)
      {
        cont=cont+1
        nex<-nex+tbass[cont]
        indice<-indice+1
        temp[indice]<-bass[cont]
      }
    }
    
    if(start>las && start<nex && final>nex)
    {
      indice<-indice+1
      temp[indice]<-bass[i]
      cont<-i
      
      while(final>nex)
      {
        cont<-cont+1
        nex<-nex+tbass[cont]
        indice<-indice+1
        temp[indice]<-bass[cont]
      }
    }
    
    las<-nex
  }
  return(temp)
}
#=================================

#DEVUELVE TODOS LOS INTERVALOS MUSICALES ENTRE EL BAJO Y LA MELODIA ****
melbass<-function(mel, tmel, bass, tbass) #tmel y tbass deben ser 1/8
{
  solutions<-NULL
  
  #tbass<-(1/tbass)
  #tmel<-(1/tmel)
  
  indice<-0
  lastindex<-0
  
  upper<-0
  lower<-0
  
  for (i in 1:length(mel))
  {
    upper<-upper+tmel[i]
    lastindex<-indice
    indice<-indice + length(quesuena(lower,upper,bass,tbass))
    
    algo<-0
    
    for(k in (lastindex+1):(length(quesuena(lower,upper,bass,tbass))+lastindex))
    {
      algo<-algo+1
      solutions[k]<-intervalo(mel[i],quesuena(lower,upper,bass,tbass)[algo])
    }
    lower<-upper
  }
  return(solutions)
}
#=================================

#DEVUELVE SI EL BAJO CUMPLE CON LOS CRITERIOS DE ACEPTACION
esBueno<-function(intervals)
{
  intervals<-intervals[intervals!="---"]
  total<-length(intervals)
  score<-NULL
  
  #lower<-c(0.108,0,0.0044,0,0,0.0372,0,0.0073,0,0,0.0078,0)
  lower<-c(0,0,0,0,0,0,0,0,0,0,0,0)
  upper<-c(0.5124,0.0391,0.269,0.1824,0.1666,0.4097,0.0693,0.3617,0.2353,0.4129,0.2042,0.1141)
  
  score[1]<-length(which(intervals=="Unisono"))/total
  score[2]<-length(which(intervals=="Segunda Menor"))/total
  score[3]<-length(which(intervals=="Segunda Mayor"))/total
  score[4]<-length(which(intervals=="Tercera Menor"))/total
  score[5]<-length(which(intervals=="Tercera Mayor"))/total
  score[6]<-length(which(intervals=="Cuarta Justa"))/total
  score[7]<-length(which(intervals=="Tritono"))/total
  score[8]<-length(which(intervals=="Quinta Justa"))/total
  score[9]<-length(which(intervals=="Sexta Menor"))/total
  score[10]<-length(which(intervals=="Sexta Mayor"))/total
  score[11]<-length(which(intervals=="Septima Menor"))/total
  score[12]<-length(which(intervals=="Septima Mayor"))/total
  
  ranking<-c(0,0,0,0,0,0,0,0,0,0,0,0)
  for (i in 1:length(score))
  {
    if(score[i]>=lower[i] && score[i]<=upper[i]){ranking[i]<-1}
  }
  return(ranking)
}
#=================================

#DEVUELVE LA MATRIZ DE TRASLAPACION ENTRE EL BAJO Y LA BATERIA
seTraslapan<-function(matBass,matDrum)
{
  
  mat<-matrix(0,nrow = length(matBass[,1]),ncol = length(matDrum[,1]))
  
  for (i in 1:length(matBass[,1]))
  {
    for(j in 1:length(matDrum[,1]))
    {
      if(matBass[i,1]==matDrum[j,1] && matBass[i,2]==matDrum[j,2]){mat[i,j]=1}
      if(matBass[i,1]>=matDrum[j,1] && matBass[i,1]<matDrum[j,2]){mat[i,j]=1}
      if(matDrum[j,1]>=matBass[i,1] && matDrum[j,1]<matBass[i,2]){mat[i,j]=1}
    }
  }
  return(mat)
}
#=================================

#DEVUELVE SI EL BAJO ES FACTIBLE POR SU RELACION CON EL KICK
kickFactible<-function(mBass,mDrum,mTrans)
{
  errors<-NULL
  cont<-1
  for ( i in 1:length(mBass[,1]))
  {
    for(j in 1:length(mDrum[,1]))
    {
      if(mTrans[i,j]==1)
      {
        if(mBass[i,1]!=mDrum[j,1] && mBass[i,2]!=mDrum[j,2])
        {
          errors[cont]=i
          cont<-cont+1
        }
      }
    }
  }
  return(errors)
}
#=================================

#DEVUELVE EL "COMPAS" EN EL QUE ESTA UNA NOTA DEL BASS
#***UN COMPAS PUEDE HACER REFERENCIA A UNO, MEDIO O UN CUARTO DE COMPAS SEGUN NUM DE ACORDES
darCompas<-function(tbass, index, mult)
{
  suma<-0
  
  for(i in 1:length(tbass))
  {
    suma<-suma+tbass[i]
    if(i==index){break}
  }
  
  suma<-suma*mult
  temp<-(floor(suma)+1)
  if(suma%%1==0){temp<-floor(suma)}
  if(index>length(tbass)){temp<-"ERROR"}
  if(index==0){temp<-"ERROR"}
  return(temp)
}
#=================================

#DEVUELVE EL GROOVE DEL BASS
bassProg<-function(bass)
{
  solutions<-NULL
  
  for(i in 1:(length(bass)-1))
  {
    solutions[i]<-intervalo(bass[i],bass[i+1])
  }
  return(solutions)
}
#=================================

groove<-function(intervals)
{
  intervals<-intervals[intervals!="---"]
  total<-length(intervals)
  score<-NULL
  
  lower<-c(0.108,0,0.0044,0,0,0.0372,0,0.0073,0,0,0.0078,0)
  upper<-c(0.5124,0.0391,0.269,0.1824,0.1666,0.4097,0.0693,0.3617,0.2353,0.4129,0.2042,0.1141)
  
  score[1]<-length(which(intervals=="Unisono"))/total
  score[2]<-length(which(intervals=="Segunda Menor"))/total
  score[3]<-length(which(intervals=="Segunda Mayor"))/total
  score[4]<-length(which(intervals=="Tercera Menor"))/total
  score[5]<-length(which(intervals=="Tercera Mayor"))/total
  score[6]<-length(which(intervals=="Cuarta Justa"))/total
  score[7]<-length(which(intervals=="Tritono"))/total
  score[8]<-length(which(intervals=="Quinta Justa"))/total
  score[9]<-length(which(intervals=="Sexta Menor"))/total
  score[10]<-length(which(intervals=="Sexta Mayor"))/total
  score[11]<-length(which(intervals=="Septima Menor"))/total
  score[12]<-length(which(intervals=="Septima Mayor"))/total
  
  ranking<-c(0,0,0,0,0,0,0,0,0,0,0,0)
  # for (i in 1:length(score))
  # {
  #   if(score[i]>=lower[i] && score[i]<=upper[i]){ranking[i]<-1}
  # }
  
  for(i in 1:length(score))
  {
    ranking[i]<-score[i]
  }
  return(ranking)
}

#===========================
dFunction<-function(key,chord)
{
  #if(((key %in% notes)==F) || ((chord %in% notes)==F)){stop("Se ingres?? un valor no valido")}
  
  funs<-c("I","ii","iii","IV","V","vi","vii")
  
  index<-0
  temp<-NULL
  
  if(key=="C"){temp<-c("C","Dm","Em","F","G","Am","Bd")}
  if(key=="C#"){temp<c-c("C#","D#m","Fm","F#","G#","A#m","Cd")}
  if(key=="D"){temp<-c("D","Em","F#m","G","A","Bm","C#d")}
  if(key=="D#"){temp<-c("D#","Fm","Gm","G#","A#","Cm","Dd")}
  if(key=="E"){temp<-c("E","F#m","G#m","A","B","C#m","D#d")}
  if(key=="F"){temp<-c("F","Gm","Am","A#","C","Dm","Ed")}
  if(key=="F#"){temp<-c("F#","G#m","A#m","B","C#","D#m","Fd")}
  if(key=="G"){temp<-c("G","Am","Bm","C","D","Em","F#d")}
  if(key=="G#"){temp<-c("G#","A#m","Cm","C#","D#","Fm","Gd")}
  if(key=="A"){temp<-c("A","Bm","C#m","D","E","F#m","G#d")}
  if(key=="A#"){temp<-c("A#","Cm","Dm","D#","F","Gm","Ad")}
  if(key=="B"){temp<-c("B","C#m","D#m","E","F#","G#m","A#d")}
  
  for(i in 1:length(temp)){if(chord==temp[i]){index=i}}
  
  if(index==0){stop("El acorde no pertenece a la tonalidad")}
  
  return(funs[index])
}



buenBass<-function(bass,tbass,chords,key,mult)
{
  score<-0
  mat<-matrix(c(5,5,2,5,5,5,0,0,-2,0,-2,-2,0,0,0,-2,-2,0,2,1,0,-2,1.5,0,-2,-2,2,0,2,-2,-2,3,0,-2,0,0,-2,2,-2,1,2,0,-2,-2,-2,1,-2,-2,0,2,3,1,3,2,3.5,0,1,1.5,0,-2,-2,0,0,0,-2,-2,1,2,-2,0,0,1,-2,0,0,0,0,1,-2,-2,2,-2,-2,0),nrow=12,ncol=7,byrow=T)
  intervals<-c("Unisono","Segunda Menor","Segunda Mayor","Tercera Menor","Tercera Mayor","Cuarta Justa","Tritono","Quinta Justa","Sexta Menor","Sexta Mayor","Septima Menor","Septima Mayor")
  funs<-c("I","ii","iii","IV","V","vi","vii")
  
  for(i in 1:length(bass))
  {
    raiz<-darNotas(chords[darCompas(tbass,i,mult)])[1]
    int <-intervalo(raiz,bass[i])
    df<-dFunction(key,chords[darCompas(tbass,i,mult)])
    
    #print(int)
    #print(df)
    score<-score+mat[which(intervals==int),which(funs==df)]
    #print(mat[which(intervals==int),which(funs==df)])
  }
  #print("***")
  return(score/length(bass))
  
}


#DEVUELVE LAS FIGURAS MUSICALES DEL BAJO QUE SUENAN ENTRE DOS MOMENTOS ****
queFigurasuena<-function(s,f,bass,tbass) #El tbass debe ser 1/8
{
  start=s
  final=f
  
  #tbass<-(1/tbass)
  temp<-NULL
  
  indice<-0
  las<-0
  nex<-0
  
  for (i in 1:length(bass))
  {
    nex<-nex+tbass[i] 
    
    if(start>=las && final<=nex)
    {
      indice<-indice+1
      temp[indice]<-tbass[i]
    }
    
    if(start<=las && final > nex)
    {
      indice<-indice+1
      temp[indice]<-tbass[i]
      
      cont=i
      
      while(final>nex)
      {
        cont=cont+1
        nex<-nex+tbass[cont]
        indice<-indice+1
        temp[indice]<-tbass[cont]
      }
    }
    
    if(start>las && start<nex && final>nex)
    {
      indice<-indice+1
      temp[indice]<-tbass[i]
      cont<-i
      
      while(final>nex)
      {
        cont<-cont+1
        nex<-nex+tbass[cont]
        indice<-indice+1
        temp[indice]<-tbass[cont]
      }
    }
    
    las<-nex
  }
  return(temp)
}
#=================================

#DEVUELVE TODAS LAS FIGURAS MUSICALES QUE SE TRASLAPAN ENTRE EL BAJO Y LA MELODIA ****
melbassFigures<-function(mel, tmel, bass, tbass) #tmel y tbass deben ser 1/8
{
  solutions1<-NULL
  solutions2<-NULL
  
  #tbass<-(1/tbass)
  #tmel<-(1/tmel)
  
  indice<-0
  lastindex<-0
  
  upper<-0
  lower<-0
  
  for (i in 1:length(mel))
  {
    upper<-upper+tmel[i]
    lastindex<-indice
    indice<-indice + length(queFigurasuena(lower,upper,bass,tbass))
    
    algo<-0
    
    for(k in (lastindex+1):(length(queFigurasuena(lower,upper,bass,tbass))+lastindex))
    {
      algo<-algo+1
      solutions1[k]<-1/tmel[i]
      solutions2[k]<-1/queFigurasuena(lower,upper,bass,tbass)[algo]
    }
    lower<-upper
  }
  return(cbind(solutions1,solutions2))
}
#=================================

#Metrica de figuras melbass
goodFigures<-function(mat)
{
  figs<-c(2,4,8,16,32)
  suma<-0
  matVal<-matrix(c(-1,1,1,-1,-1,-1,1,1,0,-1,-1,2.6,5,1,-1,-1,2.7,3.2,1,-1,-1,-1,1,-1,-1),nrow = 5,ncol=5,byrow = T)
  
  for(i in 1:length(mat[,1]))
  {
    suma<-suma+matVal[which(mat[i,1]==figs),which(mat[i,2]==figs)]
    #print(matVal[which(mat[i,1]==figs),which(mat[i,2]==figs)])
  }
  return(suma/length(mat[,1]))
}
#=================================

#DEVUELVE LA FRONTERA DE PARETO
paretoFront<-function(x,y)
{
  pareto = rep(1,length(x))
  for(i in 1:length(x))
  {
    cond1 = y[i]!=max(y[which(x==x[i])])
    cond2 = x[i]!=max(x[which(y==y[i])])
    for(n in 1:length(x))
    {
      if((x[i]<x[n]  &  y[i]<y[n]) | (x[i]==x[n] & cond1) | (y[i]==y[n] & cond2))
      {
        pareto[i] = 0
        break
      }
    }
  }
  
  return(pareto)
}
#=================================



newBass<-function(m,tm,b,tb,d,td,key,mult,prog)
{
  mel<-m
  tmel<-tm
  # tmel<-(1/tm)
  bass<-b
  tbass<-tb
  #tbass<-(1/tb)
  kick<-d
  tkick<-td
  #tkick<-(1/td)
  
  #contEND<-0
  
  while(sum(esBueno(melbass(mel,tmel,bass,tbass)))<12)
  {
    index<-sample(1:length(tbass),1)
    
    oldTime<-tbass[index]
    newTime<-(1/sample(c(1,2,4,8,16,32,64), size=1, replace=TRUE, prob=c(0,0.025,0.1,0.55,0.3,0.025,0)))
    sumaParcial<-0
    
    if(newTime<oldTime)
    {
      # print(index)
      # print(oldTime)
      # print(newTime)
      
      sumaParcial<-sumaParcial+newTime
      
      solutions<-NULL
      tsolutions<-NULL
      
      tsolutions[1]<-newTime
      
      timesBefore<-NULL
      timesAfter<-NULL
      notesBefore<-NULL
      notesAfter<-NULL
      
      for(i in 1:(index-1))
      {
        if(index>1)
        {
          timesBefore[i]<-tbass[i]
          notesBefore[i]<-bass[i]
        }
      }
      
      cont<-0
      for(i in (index+1):length(bass))
      {
        if((index+1)<=length(bass))
        {
          cont<-cont+1
          timesAfter[cont]<-tbass[i]
          notesAfter[cont]<-bass[i]
        }
      }
      
      # print(timesBefore)
      # print(timesAfter)
      # print(notesBefore)
      # print(notesAfter)
      
      contNew<-1
      while(sumaParcial<oldTime)
      {
        newNewTime<-(1/sample(c(1,2,4,8,16,32,64), size=1, replace=TRUE, prob=c(0,0.025,0.1,0.55,0.3,0.025,0)))
        while(sumaParcial+newNewTime>oldTime)
        {
          newNewTime<-(1/sample(c(1,2,4,8,16,32,64), size=1, replace=TRUE, prob=c(0,0.025,0.1,0.55,0.3,0.025,0)))
        }
        contNew<-contNew+1
        
        tsolutions[contNew]<-newNewTime
        sumaParcial<-sumaParcial+(newNewTime)
      }
      
      tbassX<-c(timesBefore,tsolutions,timesAfter)
      
      for(j in 1:length(tsolutions))
      {
        solutions[j]<-0
      }
      
      bassX<-c(notesBefore,solutions,notesAfter)
      
      for(j in (index):(index+length(tsolutions)-1))
      {
        bassX[j]<-darNotas(prog[darCompas(tbassX,j,mult)])[sample(1:3,1)]
      }
      
      if(is.null(kickFactible(InOutBass(as.numeric(tbassX)),InOutDrum(kick,tkick),seTraslapan(InOutBass(as.numeric(tbassX)),InOutDrum(kick,tkick))))==T)
      {
        bass<-bassX
        tbass<-tbassX
      }
      
      # print(bass)
      # print(tbass)
      # print(sum(tbass))
      # print(esBueno(melbass(mel,tmel,bass,tbass)))
      # print(groove(bassProg(bass)))
      # print(kickFactible(InOutBass(as.numeric(tbassX)),InOutDrum(kick,tkick),seTraslapan(InOutBass(as.numeric(tbassX)),InOutDrum(kick,tkick))))
      # print("_____")
    }#if
    
    if(newTime>oldTime )
    {
      toDrop<-NULL
      contDrop<-0
      
      bar<-darCompas(tbass,index,mult)
      
      # print(index)
      # print(oldTime)
      # print(newTime)
      
      cupo<-(newTime-oldTime)
      acabo<-F
      sumDrop<-0
      
      contBreak<-0
      
      while(sumDrop<cupo)
      {
        newIndex<-sample(1:length(tbass),1)
        
        while(newIndex==index || darCompas(tbass,newIndex,mult)!=bar || (newIndex %in% toDrop) ==T)
        {
          newIndex<-sample(1:length(tbass),1)
        }
        
        while((sumDrop+tbass[newIndex])>cupo)
        {
          newIndex<-sample(1:length(tbass),1)
          while(newIndex==index || darCompas(tbass,newIndex,mult)!=bar || (newIndex %in% toDrop) ==T){newIndex<-sample(1:length(tbass),1)}
          # contBreak<-contBreak+1
          # if(contBreak>500){break}
        }
        # if(contBreak>500){break}
        
        sumDrop<-sumDrop+tbass[newIndex]
        contDrop<-contDrop+1
        toDrop[contDrop]<-newIndex
        # print(newIndex)
        # print(toDrop)
      }
      
      # if(contBreak<500)
      # {
      tbassX<-tbass
      tbassX[index]<-newTime
      
      bassX<-bass[-c(toDrop)]
      tbassX<-tbassX[-c(toDrop)]
      
      if(is.null(kickFactible(InOutBass(as.numeric(tbassX)),InOutDrum(kick,tkick),seTraslapan(InOutBass(as.numeric(tbassX)),InOutDrum(kick,tkick))))==T)
      {
        bass<-bassX
        tbass<-tbassX
      }
      
      # print(bass)
      # print(tbass)
      # print(sum(tbass))
      # print(esBueno(melbass(mel,tmel,bass,tbass)))
      # print(groove(bassProg(bass)))
      # print(kickFactible(InOutBass(as.numeric(tbassX)),InOutDrum(kick,tkick),seTraslapan(InOutBass(as.numeric(tbassX)),InOutDrum(kick,tkick))))
      # print("____")
      # }
      
      
      
      
    }#if
    
    
  }#while
  
  # print(buenBass(bass,tbass,prog,key ,mult))
  # print(goodFigures(melbassFigures(mel,tmel,bass,tbass)))
  return(cbind(bass,tbass))
}


#=======================================
#=======================================

solve<-function(accRatio,nMiners,chords,tchords,kick,kickTime,mel,tmel,key)
{
  start.time <- Sys.time()
  
  contEval<-0
  contIt<-0
  kickTime<-1/kickTime
  tmel<-1/tmel
  mult<-tchords[1]
  
  mSol<-matrix(data=list(),nrow = nMiners,ncol = 5)
  for(i in 1:nMiners)
  {
    mSol[i,1]<-i
    
    bassSubZero<-randBass(chords,tchords)
    while(is.null(kickFactible(InOutBass(1/as.numeric(bassSubZero[,2])),InOutDrum(kick,kickTime),seTraslapan(InOutBass(1/as.numeric(bassSubZero[,2])),InOutDrum(kick,kickTime))))!=T)
    {
      bassSubZero<-randBass(chords,tchords)
    }
    mSol[[i,2]]<-newBass(mel,tmel,bassSubZero[,1],1/as.numeric(bassSubZero[,2]),kick,kickTime,key,mult,chords)
    mSol[i,3]<-buenBass(mSol[[i,2]][,1],as.numeric(mSol[[i,2]][,2]),chords,key,mult)
    mSol[i,4]<-goodFigures(melbassFigures(mel,tmel,mSol[[i,2]][,1],as.numeric(mSol[[i,2]][,2])))
    contEval<-contEval+1
  }
  mSol[,5]<-paretoFront(as.numeric(mSol[,3]),as.numeric(mSol[,4]))
  
  
  while(sum(as.numeric(mSol[,5]))<nMiners )
  {
    contIt<-contIt+1
    
    for(i in 1:nMiners)
    {
      if(as.numeric(mSol[i,5])==0)
      {
        queHago<-sample(c(1,0),size=1,replace=T,prob=c(accRatio,(1-accRatio)))
        
        if(queHago==1)
        {
          bassSubZero<-randBass(chords,tchords)
          while(is.null(kickFactible(InOutBass(1/as.numeric(bassSubZero[,2])),InOutDrum(kick,kickTime),seTraslapan(InOutBass(1/as.numeric(bassSubZero[,2])),InOutDrum(kick,kickTime))))!=T)
          {
            bassSubZero<-randBass(chords,tchords)
          }
          
          mSol[[i,2]]<-newBass(mel,tmel,bassSubZero[,1],1/as.numeric(bassSubZero[,2]),kick,kickTime,key,mult,chords)
          mSol[i,3]<-buenBass(mSol[[i,2]][,1],as.numeric(mSol[[i,2]][,2]),chords,key,mult)
          mSol[i,4]<-goodFigures(melbassFigures(mel,tmel,mSol[[i,2]][,1],as.numeric(mSol[[i,2]][,2])))
          contEval<-contEval+1
        }
        
        if(queHago==0)
        {
          mSol[[i,2]]<-mSol[[which(as.numeric(mSol[,5])==1)[sample(1:length(which(as.numeric(mSol[,5])==1)),1)],2]]
          mSol[i,3]<-buenBass(mSol[[i,2]][,1],as.numeric(mSol[[i,2]][,2]),chords,key,mult)
          mSol[i,4]<-goodFigures(melbassFigures(mel,tmel,mSol[[i,2]][,1],as.numeric(mSol[[i,2]][,2])))
          contEval<-contEval+1
        }
      }
      
    }#for
    
    mSol[,5]<-paretoFront(as.numeric(mSol[,3]),as.numeric(mSol[,4]))
    
  }#while
  
  end.time <- Sys.time()
  
  print("EVALUACIONES")
  print(contEval)
  print("------")
  print("ITERACION")
  print(contIt)
  print("------")
  
  time.taken <- end.time - start.time
  print(time.taken)
  
  return(mSol)
  
}

#=======================================
#=======================================
#=======