ZZRGND4 ;ISC/REL,GRK - PROCESS DO, GO TO, WRITE & FOR COMMANDS ;08/05/08  13:59
 ;;7.3;TOOLKIT;**20,61,68,110,128**;Apr 25, 1995;Build 1
 Q
 ;DO and GO; IND("DO1") checks if we already checked a DO at this level
DG1(ARG,V,VADDL,CM,HASPOST) ; 
 I ARG="" D  Q
 . S:'IND("DO1") IND("DO")=IND("DO")+1,IND("DO1")=1
 . S VADDL=1
 . S VADDL(1)="LVL"_$C(9)_IND("DO")
 D DG(ARG,.V,.VADDL,CM,HASPOST)
 Q
 ;
DG(ARG,V,VADDL,CM,HASPOST) ;
 N LBL,PGM,OFF,PRM,POST
 F  D  Q:ARG=""
 . S POST=0
 . S (LBL,PGM,OFF,PRM)="",S=1
 . I $E(ARG,1,2)="@^" S S=3
 . D LOOP(ARG,S,"+^:,",.I,.CH)
 . S LBL=$E(ARG,1,I-1)
 . I CH="+" D
 . . S (J,S)=I+1
 . . D:$E(ARG)'="@" E^ZZRGND1(30)
 . . S:$E(ARG,I)="^" S=I+1
 . . D LOOP(ARG,S,"+^:,",.I,.CH) 
 . . S OFF=$E(ARG,J,I-1)
 . . I OFF'?.N D PRCSSEXP^ZZRGND2(OFF,.V,.VADDL)
 . I CH="^" S S=I+1 D LOOP(ARG,S,"+^:,",.I,.CH) S PGM=$E(ARG,S,I-1)
 . I CH=":" D 
 . . S S=I+1 D LOOP(ARG,S,",",.I,.CH) 
 . . S S=$E(ARG,S,I-1) 
 . . I S'="" D PRCSSEXP^ZZRGND2(S,.V,.VADDL) S POST=1
 . S ARG=$E(ARG,I+1,999)
 . I $E(LBL)="@" D PRCSSEXP^ZZRGND2($E(LBL,2,999),.V,.VADDL) S LBL="@("
 . I $E(PGM)="@" D PRCSSEXP^ZZRGND2($E(PGM,2,999),.V,.VADDL) S PGM="@("
 . I LBL[")" S PRM=$$INSIDE(LBL,"(",")"),LBL=$P(LBL,"(")
 . I PGM[")" S PRM=$$INSIDE(PGM,"(",")"),PGM=$P(PGM,"(")
 . I $L(PRM) D 
 . . D ADDCMD^ZZRGND2("D")
 . . D ADDARG^ZZRGND2(PGM_" "_LBL)
 . . D PARAMS(PRM,.V,.VADDL)
 . I $G(IND("DOL")),CM="G",PGM]"" D E^ZZRGND1(63) ;can't goto another routine out of block structure
 . S:OFF'="" LBL=LBL_"+"_OFF
 . S S="",LOC="I" 
 . I PGM'="" S S=PGM_" ",LOC="X"
 . S:LBL_PGM["&" LOC="X"
 . S:LBL'="" S=S_LBL 
 . S POST=POST!HASPOST
 . I S'="" D ST(LOC,S,CM_POST)
 Q
 ;
PARAMS(PRM,V,VADDL) ;
 N LV,LI
 D PARSE^ZZRGND9(PRM,.LV,.LI,",") 
 D PARAMS^ZZRGND2(.LV,.LI,999)
 Q
 ; 
LOOP(ARG,S,L,I,CH) ;
 F I=S:1 S CH=$E(ARG,I) Q:L[CH  D
 . D:CH=Q QUOTE(ARG,.I)
 . D:CH="(" PAREN(ARG,.I)
 . D:CH=")" E^ZZRGND1(5)
 Q
 ;
PAREN(ARG,I) ;
 N PC,CH
 S PC=1
 F I=I+1:1 S CH=$E(ARG,I) Q:CH=""  D  Q:PC=0
 . D:CH="""" QUOTE(ARG,.I)
 . S:CH="(" PC=PC+1
 . S:CH=")" PC=PC-1
 D:PC E^ZZRGND1(5)
 Q
 ;
QUOTE(ARG,I) ;
 N CH,Q
 S Q=""""
 F I=I+1:1 S CH=$E(ARG,I) Q:CH=""!(CH=Q)
 D:CH="" E^ZZRGND1(6)
 Q
 ;
ST(LOC,S,GK) ;
 S R=$F(S,"(") S:R>1 S=$E(S,1,R-1) S:"IX"[LOC IND("COM")=IND("COM")_","_S
 S:'$D(V(LOC,S)) V(LOC,S)="" 
 D SETVADDL^ZZRGND2(LOC,S)
 Q
 ;
FR(ARG,V,VADDL,POST) ;
 N CH,I
 I $E(ARG,1)="@" Q
 . D SETVADDL^ZZRGND2("@","")  
 D LOOP(ARG,2,"=",.I,.CH) 
 I CH="" D E^ZZRGND1(8) Q
 S STR=$E(ARG,1,I-1)
 S ARG=$E(ARG,I+1,999)
 D:ARG]"" PRCSSEXP^ZZRGND2(ARG,.V,.VADDL)
 S GK="*" D ARGG^ZZRGND2(STR)
 Q
 ;
WR ;
 N S0,WR,LV,LI 
 S STR=ARG,WR="#!,",S0="" ;Need to handle /controlmnemonic
 D PARSE^ZZRGND9(STR,.LV,.LI) 
 S ARG=""
 F  D INC^ZZRGND2(.LV,.LI,.S,.S1) Q:S=""  D  S S0=S
 . I S="?" D:WR[S1 E^ZZRGND1(49) Q
 . I S="!",WR'[$E(S0) D E^ZZRGND1(59) Q  ;Look for var!
 . I S="!","#!?,"'[$E(S1) D E^ZZRGND1(59) Q  ;Look for !var
 . D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 . Q
 Q
 ;
SET S ARG=$E(ARG,1,I-1)_","_$E(ARG,I+1,999)
 Q
XE ;
 D:ARG]"" PRCSSEXP^ZZRGND2(ARG,.V,.VADDL)
 S ARG=""  
 Q
REP ;
 S S=1 D LOOP(ARG,1,":",.I,.CH) I CH=":" S ARG=$E(ARG,I+1,999),L="," D LOOP(ARG,S,":",.I,.CH)
 S ARG=$E(ARG,I+1,999) Q:ARG=""
 G REP
 ;
ZC I "ILRS"'[$E(CM,2)!($E(CM,2)="") S ARG="" Q  ;Zcommands
 S COM=$E(CM,1,2) Q:CM="ZI"  G:CM="ZR" ZR
U1 S S=1 D LOOP(ARG,S,",",.I,.CH) S S=$E(ARG,1,I-1),ARG=$E(ARG,I+1,999)
 I $E(S,1)="@" D
 . D SETVADDL^ZZRGND2("@","")
 . S S=$E(S,2,999) 
 . D:S]"" PRCSSEXP^ZZRGND2(S,.V,.VADDL) 
 Q:ARG=""  
 G U1
 ;
ZR Q:ARG=""  
 S S=1 D LOOP(ARG,S,":,",.I,.CH) S S=$E(ARG,1,I-1),ARG=$E(ARG,I+1,999)
 I $E(S,1)="@" D  G ZR
 . D SETVADDL^ZZRGND2("@","")
 . D PRCSSEXP^ZZRGND2(S,.V,.VADDL) 
 D:S["+" PRCSSEXP^ZZRGND2($P(S,"+",2,999),.V,.VADDL)
 G ZR
 ;
LO(ARG,V,VADDL) ;Lock -- Look for timeouts
 N LK
 I ARG="" Q
 S S=1
 F  D  Q:CH=""
 . I "+-"'[$E(ARG,S) D E^ZZRGND1(61)
 . S LK=0 D LOOP(ARG,S,"-:,",.I,.CH) S S=I+1
 . I CH="-" D LOOP(ARG,S,",",.I,.CH) S S=I+1 Q
 . I CH=":" S LK=1 D LOOP(ARG,S,",",.I,.CH) S S=I+1
 . I CH="," D:'LK E^ZZRGND1(60) S LK=0 Q
 . I CH="" D:'LK E^ZZRGND1(60) Q
 . Q
 D PRCSSEXP^ZZRGND2(ARG,.V,.VADDL)
 Q
 ;
Q ;QUIT followed by comment or in structure Do or For loop, must have 2 spaces
 I $E(ARG)=";"!$G(IND("DOL"))!$G(IND("F")) S ARG="",ERR=9 G ^ZZRGND1
 Q
PT(X) ;Tag for parameter passing
 S ^UTILITY($J,1,RTN,"P",LAB)=X
 Q
INSIDE(X,X1,X2) ;Return the data inside the param x1,x2
 S J=$L(X,X2)-1,J=$S(J<1:1,1:J)
 Q $P($P(X,X2,1,J),X1,2,99)
 ;
SEP(ST,SP,RV) ;String,Separters,Return array)
 N %,N,Q S Q=$C(34) ;QUOTE
 F N=1:1 S %=$E(ST,N) D SQT:%=Q Q:SP[%
 S RV=N-1,RV(1)=$E(ST,1,N)
 Q
 ;
SQT F N=N+1:1 Q:Q[$E(ST,N)
 Q
CNG(S1,S2,S3) ;String,replace,with
 ;
 F  Q:S1'[S2  S S1=$P(S1,S2)_S3_$P(S1,S2,2,999)
 Q S1
PRUNE(S1,S2) ;String,prune char from front and back
 F  Q:$E(S1)'=S2  S S1=$E(S1,2,999)
 F  Q:$E(S1,$L(S1))'=S2  S S1=$E(S1,1,$L(S1)-1)
 Q S1
 ;
 