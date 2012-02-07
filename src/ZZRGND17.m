ZZRGND17 ;CBR/ - Process FileMan calls ;02/07/12
 ;;7.3;TOOLKIT;**to be determined**;
NDXFMAN(GL,RTN,TAG,RESULT,SHORT)
 N I,J,K,CMDCNT,CMD,CX,ARG,VAL,IRTN,ITAG,FILE
 S I="",SHORT=+$G(SHORT)
 I TAG="" Q
 I '$D(@GL@(2,RTN,TAG)) Q
 F  S I=$O(@GL@(2,RTN,TAG,I)) Q:I=""  D 
 . Q:I="N"  Q:I="P"  Q:I="L"  Q:'$D(@GL@(2,RTN,TAG,I,"PRS"))
 . S CMDCNT=+@GL@(2,RTN,TAG,I,"PRS")
 . F J=1:1:CMDCNT D 
 . . S CNT=+@GL@(2,RTN,TAG,I,"PRS",J) Q:CNT=0
 . . F K=1:1:CNT D
 . . . S CMD=@GL@(2,RTN,TAG,I,"PRS",J,K)
 . . . S CX=$P(CMD,$C(9),1)
 . . . S ARG=$P(CMD,$C(9),2)
 . . . S VAL=$P(CMD,$C(9),3)
 . . . I CX="S" D  Q
 . . . . I ARG?1"DI".U,VAL'="",$E(VAL,1,2)="""^" D
 . . . . . S VAL=$E($P(VAL,",",1),2,999)
 . . . . . S:SHORT VAL=$P(VAL,"(",1) 
 . . . . . S RESULT("FMG",VAL)=""
 . . . I (CX="D")!(CX="X") D  Q
 . . . . S IRTN=$P(ARG," ",1)
 . . . . S ITAG=$P(ARG," ",2)
 . . . . I $$GETPKG^ZZRGND19(IRTN,GL)="DI",VAL'="" D
 . . . . . S FILE=$$GETFILE(RTN,TAG,I,VAL,$P(CMD,$C(9),4))
 . . . . . S:FILE'="" RESULT("FMG",FILE)=""
 Q
 ;
GETFILE(RTN,TAG,TAGO,ARG1,ARG2)
 N FILE,CMDCNT,I,J,K,STOP
 S FILE=$$ROOT^DILFD(ARG1)
 Q:FILE'="" FILE
 I $E(ARG1,1,2)?1."""".N S ARG1=$E(ARG1,2,$L(ARG1)-1)
 I $E(ARG2)="""" S ARG2=$E(ARG2,2,$L(ARG2)-1)
 S FILE=$$ROOT^DILFD(ARG1,ARG2)
 Q:FILE'="" FILE
 S STOP=0
 F I=TAGO-1:-1:1 D  Q:STOP
 . Q:'$D(@GL@(2,RTN,TAG,I,"PRS"))
 . S CMDCNT=+@GL@(2,RTN,TAG,I,"PRS")
 . F J=1:1:CMDCNT D  Q:STOP
 . . S CNT=+@GL@(2,RTN,TAG,I,"PRS",J) Q:CNT=0
 . . F K=1:1:CNT D  Q:STOP
 . . . S CMD=@GL@(2,RTN,TAG,I,"PRS",J,K)
 . . . S CX=$P(CMD,$C(9),1)
 . . . S ARG=$P(CMD,$C(9),2)
 . . . S VAL=$P(CMD,$C(9),3)
 . . . I CX="S" D  Q:STOP
 . . . . I ARG=ARG1 D
 . . . . S STOP=1
 . . . . S FILE=$$ROOT^DILFD(VAL)
 Q FILE
 ;
