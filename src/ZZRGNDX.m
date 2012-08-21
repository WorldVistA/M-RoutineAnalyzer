ZZRGNDX ;;CBR/AU - XINDEX based routines ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
 ;;
 G ^ZZRGND6
MAIN(SILENT) ;
 D SETUP^ZZRGND7(SILENT) ;Get ready to process
A2 S RTN=$O(^UTILITY($J,RTN)) I RTN="" D MAIN^ZZRGND5(SILENT) Q
 S INDLC=(RTN?1"|"1.4L.NP) D LOAD:'INDLC
 I $D(ZTQUEUED),$$S^%ZTLOAD S RTN="~",IND("QUIT")=1,ZTSTOP=1 G A2
 I 'INDDS,INDLC W:'SILENT !!?10,"Data Dictionaries",! S INDDS=1
 D BEGIN(SILENT)
 G A2
 ;
LOAD S X=RTN,XCNP=0,DIF="^UTILITY("_$J_",1,RTN,0," X ^%ZOSF("TEST") Q:'$T  X ^%ZOSF("LOAD") S ^UTILITY($J,1,RTN,0,0)=XCNP-1
 I $D(^UTILITY($J,1,RTN,0,0)) S ^UTILITY($J,1,RTN,"RSUM")="B"_$$SUMB^XPDRSUM($NA(^UTILITY($J,1,RTN,0)))
 Q
BEG ;
 N SILENT S SILENT=0
 D BEGIN(SILENT)
 Q
BEGIN(SILENT) ;
 Q:$E(RTN)="%"
 I 'SILENT S %=INDLC*5 W:$X+10+%>IOM ! W RTN,$J("",10+%-$L(RTN))
 S (IND("DO"),IND("SZT"),IND("SZC"),LABO)=0,LC=$G(^UTILITY($J,1,RTN,0,0))
 I LC="" W:'SILENT !,">>>Routine '",RTN,"' not found <<<",! Q
 S TXT="",LAB=$P(^UTILITY($J,1,RTN,0,1,0)," ") I RTN'=$P(LAB,"(") D E^ZZRGND1(17)
 I 'INDLC,LAB["(" D E^ZZRGND1(55) S LAB=$P(LAB,"(")
 ;if M routine(not compiled template or DD) and has more than 2 lines, check lines 1 & 2
 I 'INDLC,LC>2 D
 . N LABO S LABO=1
 . S LIN=$G(^UTILITY($J,1,RTN,0,1,0)),TXT=1
 . ;check 1st line (site/dev - ) patch 128
 . I $P(LIN,";",2,4)'?.E1"/".E.1"-".E D E^ZZRGND1(62)
 . S LIN=$G(^UTILITY($J,1,RTN,0,2,0)),TXT=2
 . ;check 2nd line (;;nn.nn[TV]nn;package;.anything)
 . I $P(LIN,";",3,99)'?1.2N1"."1.2N.1(1"T",1"V").2N1";"1A.AP1";".E D E^ZZRGND1(44) ;patch 121
 . I $L(INP(11)) X INP(11) ;Version number check
 . I $L(INP(12)) X INP(12) ;Patch number check
 D PRCSSLNS(RTN)
 S LAB="",LABO=0,TXT=0,^UTILITY($J,1,RTN,0)=IND("SZT")_"^"_LC_"^"_IND("SZC")
 I IND("SZT")>INP("MAX"),'INDLC S ERR=35,ERR(1)=IND("SZT") D ^ZZRGND1
 I IND("SZT")-IND("SZC")>INP("CMAX"),'INDLC S ERR=58,ERR(1)=IND("SZT")-IND("SZC") D ^ZZRGND1
 D POSTRTN
 Q
 ;
PRCSSLNS(RTN) ;
 N TXT,LIN,LAB,LABO
 F TXT=1:1:LC D
 . S LIN=^UTILITY($J,1,RTN,0,TXT,0)
 . S LN=$L(LIN),IND("SZT")=IND("SZT")+LN+2 
 . S (IND("COM"),IND("DOL"),IND("F"))=""
 . D PRCSSLN(RTN,LIN,'INDLC,.LAB,.LABO)
 . S ^UTILITY($J,1,RTN,"COM",TXT)=IND("COM")
 Q
 ; 
 ;
PRCSSLN(RTN,LIN,CHK,LAB,LABO) ;
 N V
 D UPDATLBL(RTN,LIN,'INDLC,.LAB,.LABO,.V)
 D LN(RTN,LAB,LABO,LIN,.V)
 D ST(RTN,LAB,LABO,.V)
 Q
 ;
GETLEVEL(LIN) ;
 N X,I 
 S I=0 ;Watch the scope of I, counts dots
 S X=0
 I " ."[$E(LIN) D  S X=$L($E(LIN,1,I),".")-1,LIN=$E(LIN,I,999)
 . F I=1:1:245 Q:". "'[$E(LIN,I)
 . Q
 Q X
 ; 
UPDATLBL(RTN,LIN,CHK,LAB,LABO,V) ;
 N VADDL,ARG,X,J,PREVLAB,PRSTREE
 S X=$P(LIN," ")
 I '$L(X) S LABO=LABO+1 Q 
 S PREVLAB=$G(LAB)
 S LAB=$P(X,"("),LABO=0
 S:PREVLAB]"" ^UTILITY($J,2,RTN,PREVLAB,"N")=LAB
 S ^UTILITY($J,2,RTN,LAB,"P")=PREVLAB
 S ^UTILITY($J,2,RTN,LAB,"L")=$$GETLEVEL($P(LIN," ",2,999))
 S IND("COM")=LAB
 S ARG=$P($P(X,"(",2),")")
 I $L(ARG) D     ;Process formal parameters as New list.
 . D NE^ZZRGND3(ARG,.V,.VADDL) 
 . F J=1:1:+$G(VADDL) S:$P(VADDL(J),$C(9),3)="~" $P(VADDL(J),$C(9),3)="n"
 . D UPDATGLB(RTN,LAB,LABO,.VADDL,0)
 I CHK,'$$VT^ZZRGND2(LAB) D E^ZZRGND1($S(LAB=$$CASE^ZZRGND52(LAB):37,1:55)) ;Check for bad labels
 I $D(^UTILITY($J,1,RTN,"T",LAB)) D E^ZZRGND1(15) I 1 ;DUP label
 E  S ^UTILITY($J,1,RTN,"T",LAB)=""
 Q
 ;  
 ;Proccess one line, RTN=Routine, LAB=Label, LABO=Offest, LN = Length, LIN = Line.
LN(RTN,LAB,LABO,LIN,V) ;
 N X
 I LN>245 D:'(LN=246&($E(RTN,1,3)="|dd")) E^ZZRGND1(19) ;patch 119
 D:LIN'?1.ANP E^ZZRGND1(18)
 S LIN=$P(LIN," ",2,999)
 I LIN="" D E^ZZRGND1(42) Q  ;Blank line ;p110
 S I=0 ;Watch the scope of I, counts dots
 S X=0
 I " ."[$E(LIN) D  S X=$L($E(LIN,1,I),".")-1,LIN=$E(LIN,I,999)
 . F I=1:1:245 Q:". "'[$E(LIN,I)
 . Q
 ;check dots against Do level IND("DO"), IND("DOL")=dot level
 D:'I&$G(IND("DO1")) E^ZZRGND1(51) 
 S IND("DO1")=0 
 S:'I IND("DO")=0
 I I D:X>IND("DO") E^ZZRGND1(51) S (IND("DO"),IND("DOL"))=X
 ;Count Comment lines, skip ;; lines
 I $E(LIN)=";",$E(LIN,2)'=";" S IND("SZC")=IND("SZC")+$L(LIN) ;p110
 D PRCSSCMS(RTN,LAB,LABO,LIN,X,.V)
 Q 
 ;
SEP(LIN,CH,ARG) ;Extract expression to first whitespace
 N I
 F I=1:1 S CH=$E(LIN,I) D:CH="""" QUOTE(.LIN,.I,.CH) Q:" "[CH
 S ARG=$E(LIN,1,I-1) S:CH=" " I=I+1 S LIN=$E(LIN,I,999)
 Q
 ;
QUOTE(LIN,I,CH) ;
 F I=I+1:1 S CH=$E(LIN,I) Q:CH=""!(CH="""")
 D:CH="" E^ZZRGND1(6)
 Q
 ;
PRCSSCMS(RTN,LAB,LABO,LIN,LEVEL,V) ;GRB
 N CH,ARG,GK,COM,INIFELSE
 S INIFELSE=0
 F  D:LIN]""  Q:LIN=""
 . S COM=$E(LIN),GK="",ARG=""
 . I COM=";" S LIN="" Q
 . I COM=" " D  Q
 . . I LIN?1." " S LIN="" D E^ZZRGND1(13) Q
 . . S LIN=$E(LIN,2,999)
 . S IND("COM")=IND("COM")_$C(9)_COM
 . D SEP(.LIN,.CH,.ARG) ;Extract command and postconditionals
 . S CM=$P(ARG,":",1),POST=$P(ARG,":",2,999)
 . I ARG[":",(POST="") D E^ZZRGND1(48) 
 . I POST]"" S IND("COM")=IND("COM")_":" D HANDLARG(RTN,LAB,LABO,POST,.V,LEVEL)
 . I CM?.E1L.E S CM=$$CASE^ZZRGND52(CM),COM=$E(CM) 
 . I CM="" D E^ZZRGND1(21) Q ;Missing command
 . S CX=$G(IND("CMD",CM)) I CX="" D  Q:CX=""
 . . I $E(CM)="Z" S CX="^Z" Q  ;Proccess Z commands
 . . D E^ZZRGND1(1) S LIN="" Q
 . D ADDCMD(RTN,LAB,LABO,$P(CX,"^",1),LEVEL)
 . S CX=$P(CX,"^",2,9)
 . D SEP(.LIN,.CH,.ARG) ;Extract command arguments
 . I '$L(LIN),CH=" " D E^ZZRGND1(13) ;trailing space
 . I ARG="","CGJMORSUWX"[COM D E^ZZRGND1(49) Q
 . I CX>0 D E^ZZRGND1(CX) S CX=""
 . I $L(CX),CX'="I",CX'="E" D HANDLCMD(RTN,LAB,LABO,CX,ARG,.V,POST,INIFELSE,LEVEL) I 1
 . E  D:ARG]"" HANDLARG(RTN,LAB,LABO,ARG,.V,LEVEL)
 . I CX="I"!(CX="E")!(CX="F") S INIFELSE=1
 . I CX="Q",'INIFELSE,'LEVEL,POST="" D ENDTRN(RTN,LAB,LABO)
 Q
 ;
ADDCMD(RTN,LAB,LABO,CMD,LEVEL) ;
 N VADDL,PRSTREE
 S VADDL=1
 S VADDL(1)="CMD"_$C(9)_CMD
 D UPDATGLB(RTN,LAB,LABO,.VADDL,LEVEL)
 Q 
 ; 
ENDTRN(RTN,LAB,LABO) ;
 N VADDL
 S VADDL=1
 S VADDL(1)="Q"_$C(9)_$C(9)_$C(9)
 D UPDATGLB(RTN,LAB,LABO,.VADDL,0)
 Q 
 ;
HANDLARG(RTN,LAB,LABO,ARG,V,LEVEL) ;
 N VADDL,PRSTREE
 D PRCSSEXP^ZZRGND2(ARG,.V,.VADDL) 
 D UPDATGLB(RTN,LAB,LABO,.VADDL,LEVEL)
 Q
 ;
HANDLCMD(RTN,LAB,LABO,CX,ARG,V,POST,INIFELSE,LEVEL) ;
 N VADDL,I,CNT,PRSTREE
 S PRSTREE=0
 D @CX^ZZRGND12(ARG,.V,.VADDL,POST)
 S CNT=+$G(VADDL)
 I CNT>0 D
 . F I=1:1:CNT S VADDL(I)=VADDL(I)_$C(9)_INIFELSE
 . D UPDATGLB(RTN,LAB,LABO,.VADDL,LEVEL)
 Q
 ;
UPDATGLB(RTN,LAB,LABO,VADDL,LEVEL) ;
 N CNT,VCNT,I,PTCNT
 Q:'$D(VADDL)
 S VCNT=+$G(VADDL)
 S CNT=+$G(^UTILITY($J,2,RTN,LAB,LABO))
 F I=1:1:VCNT D
 . S CNT=CNT+1
 . S ^UTILITY($J,2,RTN,LAB,LABO,CNT)=VADDL(I)
 . S ^UTILITY($J,2,RTN,LAB,LABO,CNT,"LVL")=LEVEL
 S ^UTILITY($J,2,RTN,LAB,LABO)=CNT
 I $D(PRSTREE) D
 . S CMDCNT=+$G(^UTILITY($J,2,RTN,LAB,LABO,"PRS"))+1
 . M ^UTILITY($J,2,RTN,LAB,LABO,"PRS",CMDCNT)=PRSTREE
 . S ^UTILITY($J,2,RTN,LAB,LABO,"PRS")=CMDCNT
 Q
 ;Save off items from line.
 ;Local variable, Global, Marked Items, Naked global, Internal ref, eXternal ref., Tag ref.
ST(RTN,LAB,LABO,V) ;
 N LOC,S,R
 S R=LAB_$S(LABO:"+"_LABO,1:"")
 S LOC="" 
 F  S LOC=$O(V(LOC)) Q:LOC=""  D
 . S S=""
 . F  S S=$O(V(LOC,S)) Q:S=""  D SET(RTN,LOC,S,V(LOC,S),R)
 Q
 ;
SET(RTN,LOC,S,VAL,R) ;
 N I,ARG,LTYPE,LVAL
 I VAL]"" D
 . F LTYPE="!","~" D
 . . S LVAL=$G(^UTILITY($J,1,RTN,LOC,S))
 . . Q:VAL'[LTYPE
 . . Q:LVAL[LTYPE
 . . S ^UTILITY($J,1,RTN,LOC,S)=LVAL_LTYPE
 F I=0:1 S ARG=$G(^UTILITY($J,1,RTN,LOC,S,I)) Q:$L(ARG)'>230
 S ^UTILITY($J,1,RTN,LOC,S,I)=ARG_R_VAL_","
 Q
 ; 
POSTRTN ;Do more overall checking
 N V,E,T,T1,T2
 S T="" ;Check for missing Labels
 F  S T=$O(^UTILITY($J,1,RTN,"I",T)),T2=T Q:T=""  S T1=$G(^(T,0)) D
 . Q:$E(T2,1,2)="@("
 . S:$E(T2,1,2)="$$" T2=$E(T2,3,99)
 . I T2]"",'$D(^UTILITY($J,1,RTN,"T",$P(T2,"+",1))) D
 . . F I=1:1:$L(T1,",")-1 S LAB=$P(T1,",",I),LABO=+$P(LAB,"+",2),LAB=$P(LAB,"+"),E=14,E(1)=T D E^ZZRGND1(.E)
 . . Q
 . Q
 S LAB="",LABO=0 ;Check for valid label names
 I 'INDLC F  S LAB=$O(^UTILITY($J,1,RTN,"T",LAB)) Q:LAB=""  D
 . I '$$VA^ZZRGND2(LAB) D E^ZZRGND1(55) Q
 . D:'$$VT^ZZRGND2(LAB) E^ZZRGND1(37)
 . Q
 S LAB="",LABO=0 ;Check for valid variable names.
 F  S LAB=$O(^UTILITY($J,1,RTN,"L",LAB)) Q:LAB=""  D
 . D VLNF^ZZRGND3($P(LAB,"("))
 . Q
 Q
 ;
