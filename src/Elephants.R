melEle<-c("G#","B","D#","G#","B","D#","G#","C#","G#","G#","G#","C#","E","G#","C#","E","G#","D#","G#","B")
tmelEle<-c(16,16,8,16,16,8,8,8,8,8,16,16,8,16,16,8,8,8,8,8)
#,"G#","C#","E","G#","C#","E","G#","D#","G#","B"
#,16,16,8,16,16,8,8,8,8,8

bassEle<-cbind(c("G#","G#","G#","C#","E","A","E","A","G#","G#","G#","C#","E","A","E","A"),c(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8))

kdEle<-c("K","S","K","S","K","S","S","K","S","S","K","S","K","S","K","S","S","K","S","S")
ktEle<-c(8,8,8,8,16,16,8,16,16,8,8,8,8,8,16,16,8,16,16,8)

buenBass(bassEle[,1],1/as.numeric(bassEle[,2]),c("C#m","B"),"E",1)
goodFigures(melbassFigures(melEle,1/tmelEle,bassEle[,1],1/as.numeric(bassEle[,2])))

algoEle<-solve(0.8,15,c("C#m","B"),c(1,1),kdEle,ktEle,melEle,tmelEle,"E")
algoEle
algoEle[[13,2]]

