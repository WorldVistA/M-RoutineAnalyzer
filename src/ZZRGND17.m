ZZRGND17 ;CBR/ - Process FileMan calls ;02/07/12
 ;;7.3;TOOLKIT;**to be determined**;
RFMCALLS(GLB) ; Prints FileMan calls per M TAG and globals used by these calls
 N PKG,RTN,TAG,LN
 S:$G(GLB)="" GLB="^ZZRG"
 S PKG="",LN=0
 F  S PKG=$O(@GLB@(7,PKG)) Q:PKG=""  S LN=LN+1 D
 . W !,"--------------------------------------------------------------",!
 . W !,LN,". PACKAGE NAME: "_$$GPKGNAME^ZZRGND19(PKG,GLB)
 . S RTN=""
 . F  S RTN=$O(@GLB@(7,PKG,RTN)) Q:RTN=""  D
 . . S TAG=""
 . . F  S TAG=$O(@GLB@(7,PKG,RTN,TAG)) Q:TAG=""  D
 . . . Q:'$D(@GLB@(7,PKG,RTN,TAG,"FMG"))
 . . . W !!," "_TAG_"^"_RTN
 . . . D REPORTGL^ZZRGND13("  Globals: ",GLB_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""FMG"")",0,";")
 . . . D REPORTGL^ZZRGND13("  FileMan calls: ",GLB_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""FMGC"")",0,";")
 . W !,"--------------------------------------------------------------",!
 Q
 ;
NDXFMAN(GL,RTN,TAG,RESULT,SHORT) ; Indexes FileMan calls
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
 . . . . I $$FMARG(ARG),VAL'="" D
 . . . . . I $E(VAL,1,2)="""^" D  I 1
 . . . . . . S VAL=$E($P(VAL,",",1),2,999)
 . . . . . . S:SHORT VAL=$P(VAL,"(",1) 
 . . . . . . S RESULT("FMG",VAL)=""
 . . . . . E  D
 . . . . . . S FILE=$$GETFILE(RTN,TAG,I,VAL)
 . . . . . . S:FILE'="" RESULT("FMG",FILE)=""
 . . . I (CX="D")!(CX="X") D  Q
 . . . . S IRTN=$P(ARG," ",1)
 . . . . S ITAG=$P(ARG," ",2)
 . . . . I $$GETPKG^ZZRGND19(IRTN,GL)="DI" D
 . . . . . S RESULT("FMGC",ITAG_"^"_IRTN)=""
 . . . . . Q:VAL=""
 . . . . . S FILE=$$GETFILE(RTN,TAG,I,VAL,$P(CMD,$C(9),4))
 . . . . . S:FILE'="" RESULT("FMG",FILE)=""
 Q
 ;
GETFILE(RTN,TAG,TAGO,ARG1,ARG2)
 N FILE,CMDCNT,I,J,K,STOP
 S FILE=""
 Q:'$D(ARG1) FILE
 S ARG2=$G(ARG2)
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
 . . . . . S STOP=1
 . . . . . S FILE=$$ROOT^DILFD(VAL)
 S:FILE="" FILE="UNDETERMINED ("_ARG1_")"
 Q FILE
 ;
FMARG(ARG) ; True if ARG is a valid argument in classic FileMan calls
 Q:$G(ARG)="" 0
 Q (ARG="DIC")!(ARG="DIE")!(ARG="DIFILE")!(ARG="DIK")!(ARG="DIU")!(ARG="DIWF")
 ;