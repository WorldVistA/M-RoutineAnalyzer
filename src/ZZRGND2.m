ZZRGND2 ;;CBR/AU - XINDEX based routines ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
 Q
 ;
PRCSSEXP(ARG,V,VADDL) ;
 N CM,COM,LV,S,LI,AC,LBL,CH,I,STR,NDX
 S (CM,COM)=""
 F NDX=1:1:$L(ARG,$C(9)) D
 . S STR=$P(ARG,$C(9),NDX)
 . D:STR]"" ARGG(STR)
 Q
 ; 
 ;Process argument
ARGG(STR) ;
 N LV,LI
 D PARSE^ZZRGND9(STR,.LV,.LI)
 D ARGS(.LV,.LI,999)
 Q
 ; 
ARGS(LV,LI,AC) ;Proccess all arguments at this level
 N DONE,S,S1
 S AC=LI+AC,DONE=0
 F  Q:AC'>LI  D  Q:DONE
 . D INC(.LV,.LI,.S,.S1)
 . I S="" S DONE=1 Q
 . D ARG(.LV,.LI,.S,.S1)
 Q
 ;
ARGSTEXT(LV,LI,AC) ;Proccess all arguments at this level for $TEXT
 N DONE,S,S1,CHTHIS,CHPREV
 S AC=LI+AC,DONE=0
 F  Q:AC'>LI  D  Q:DONE
 . D INC(.LV,.LI,.S,.S1)
 . I S="" S DONE=1 Q
 . S:$D(CHTHIS) CHPREV=CHTHIS
 . S CHTHIS=$E(S)
 . I '$D(CHPREV),((CHTHIS?1A)!(CHTHIS="%")) Q          ; Label
 . I CHTHIS="^",(('$D(CHPREV))!("$%)"""[$G(CHPREV))!($G(CHPREV)?1AN)) D  Q   ; Routine
 . . Q:S1="@" 
 . . F  Q:AC'>LI  D INC(.LV,.LI,.S,.S1) I S="" S DONE=1 Q
 . D ARG(.LV,.LI,.S,.S1)
 Q
 ;
PARAMS(LV,LI,AC) ; 
 N S,S1,GK,N,CH,FIRST,Y
 S AC=LI+AC,DONE=0,GK="",FIRST=1,N=1
 F  Q:AC'>LI  D INC(.LV,.LI,.S,.S1) Q:S=""  D
 . S CH=$E(S)
 . I CH=".",$E(S1)'?1.N,FIRST D  Q
 . . S FIRST=0
 . . S Y=$$PEEK(.LV,.LI)
 . . I Y="@" S GK="" Q
 . . S GK="p;"_N
 . I CH="," D  Q 
 . . S N=N+1,GK=""
 . . S FIRST=1
 . S FIRST=0
 . D ADDARG($$ASM^ZZRGND3(.LV,.LI,","))
 . D ARG(.LV,.LI,.S,.S1)
 Q
 ;   
ARG(LV,LI,S,S1) ;Process one argument
 N Y,CH
 S CH=$E(S)
 I CH="," D  Q
 . S Y=$$PEEK(.LV,.LI)
 . D:(","[Y)&($$FNC()'="$$") E^ZZRGND1(21)
 Q:CH=""""
 I (CH?1A)!(CH="%") D  Q
 . D LOC(.LV,.LI,.S,.S1)
 I CH="^" D  Q
 . S LOC="G"
 . I S="^" D NAK(.LV,.LI,.S,.S1) Q
 . I S["[" D EXTGLO(.LV,.LI,.S,.S1) Q
 . I S["|" D EXTGLO(.LV,.LI,.S,.S1) Q
 . D GLO(.LV,.LI,.S,.S1)
 I CH="$" D FUN(.LV,.LI,.S,.S1) Q
 I CH="?" D PAT(.LV,.LI,.S,.S1) Q
 I CH="(" D  Q
 . D INC(.LV,.LI,.S,.S1)
 . D DN(.LV,.LI,S)
 . D INC(.LV,.LI,.S,.S1)
 I CH="@" D SINDVADD(.LV,.LI) Q
 Q
 ; 
SINDVADD(LV,LI) ;
 N Y,P
 S Y=$$PEEK(.LV,.LI)
 S P=$$BACKN(.LV,.LI,1)
 I P=")",Y="(" Q
 I Y="(",$$BACKN(.LV,.LI,2)="@" N CH S CH=$E(P) Q:(CH?1A)!(CH="%")
 D SETVADDL("@",Y)
 Q
 ;
NAK(LV,LI,S,S1) ;
 S LOC="N"
 D GLO(.LV,.LI,.S,.S1)
 Q
 ;
EXTGLO(LV,LI,S,S1) ;
 D E^ZZRGND1(50)
 D EG(.LV,.LI,.S,.S1)
 D INC(.LV,.LI,.S,.S1)
 S S=U_S
 D GLO(.LV,.LI,.S,.S1)
 Q
 ;
EG(LV,LI,S,S1) ;
 N GK,LOC,DONE,CH
 S GK="",LOC="L" ;HANDLE EXTENDED GLOBAL
 S DONE=0
 F  D  Q:DONE
 . D INC(.LV,.LI,.S,.S1)
 . S CH=$E(S)
 . I ("]"[CH)!("|"[CH) S DONE=1 Q
 . D ARG(.LV,.LI,.S,.S1)
 Q
 ;
GLO(LV,LI,S,S1) ;
 N X,Y
 S X=$E(S,2,99)
 I X]"",S'["^$",X'?1(1U,1"%").7UN D E^ZZRGND1(12)
 I GK["*",$E(S,1,2)["^%" D E^ZZRGND1(45)
 I S1="(" D
 . S S=S_S1
 . S Y=$$PEEKDN(.LV,.LI)
 . I $E(Y)="." S Y=Y_$$PEEKDN2(.LV,.LI)
 . I ($A(Y)=34),$$PEEKDN2(.LV,.LI)="_" Q
 . S:(Y?.N.1".".N)!($A(Y)=34)!("^$J^$I^$H^"[(U_Y)) S=S_Y
 D ST(LOC,S)
 I S1="(" D
 . D INC2(.LV,.LI,.S,.S1)
 . D DN(.LV,.LI,S)
 . D INC(.LV,.LI,.S,.S1)
 Q
 ;
LOC(LV,LI,S,S1) ;
 N Y
 S LOC="L" ;Check variables at end. I S'?1.8UN,S'?1"%".7UN,S'?1.8LN,S'?1"%".7LN D E^ZZRGND1(11)
 I S1="(" D
 . S S=S_S1
 . S Y=$$PEEKDN(.LV,.LI)
 . S:(Y?1.N)!($A(Y)=34) S=S_Y
 D ST(LOC,S)
 I S1="(" D
 . D INC2(.LV,.LI,.S,.S1)
 . D DN(.LV,.LI,S)
 . D INC(.LV,.LI,.S,.S1)
 Q
 ;
PEEK(LV,LI) ;
 N Y
 S Y=$G(LV(LV,LI+1))
 Q Y
 ;
BACKN(LV,LI,N) ;
 N Y
 S Y=$G(LV(LV,LI-N))
 Q Y
 ;
PEEK2(LV,LI) ;
 N Y
 S Y=$G(LV(LV,LI+2))
 Q Y
 ;
INC2(LV,LI,S,S1) ;
 S LI=LI+1
 D INC(.LV,.LI,.S,.S1)
 Q
 ;
INC(LV,LI,S,S1) ;
 S LI=LI+1
 S S=$G(LV(LV,LI))
 S S1=$G(LV(LV,LI+1))
 I $A(S)=10 D
 . D E^ZZRGND1(43)
 . S (S,S1)=""
 Q
 ;
DN(LV,LI,NOA) ;
 S LI(LV)=LI,LV=LV+1,LI=LI(LV)
 D ARGS(.LV,.LI,NOA)
 D UP(.LV,.LI)
 Q
 ;
DNTEXT(LV,LI,NOA) ;
 S LI(LV)=LI,LV=LV+1,LI=LI(LV)
 D ARGSTEXT(.LV,.LI,NOA)
 D UP(.LV,.LI)
 Q
 ;
DNPARAMS(LV,LI,NOA) ;
 S LI(LV)=LI,LV=LV+1,LI=LI(LV)
 D PARAMS(.LV,.LI,NOA)
 D UP(.LV,.LI)
 Q
 ;
UP(LV,LI) ;Inc LI as we save to skip the $C(10).
 N Y
 S Y=$$PEEK(.LV,.LI)
 D:$A(Y)'=10 E^ZZRGND1(43)
 S LI(LV)=LI+1,LV=LV-1,LI=LI(LV)
 Q
 ;
PEEKDN(LV,LI) ;
 N Y
 S Y=$G(LV(LV+1,LI(LV+1)+1))
 Q Y
 ;
PEEKDN2(LV,LI) ;
 N Y
 S Y=$G(LV(LV+1,LI(LV+1)+2))
 Q Y
 ;
 ;functions
FUN(LV,LI,S,S1) ;
 N FUN,NOA
 S FUN=S
 I S["$$" D EXT(.LV,.LI,.S,.S1) Q
 I S["$&" D PKG(.LV,.LI,.S,.S1) Q
 I S1'["(" D SPV(X) Q
 S NOA=$P(S,"^",2)
 D INC2(.LV,.LI,.S,.S1)
 I S'>0 D E^ZZRGND1(43) ;Sit on NOA
 I FUN["$TE" D DNTEXT(.LV,.LI,S) Q  ;TEXT(.LV,.LI,.S,.S1) Q
 S Y=1
 F Z1=LI(LV+1)+1:1 S X=$G(LV(LV+1,Z1)) Q:$A(X)=10!(X="")  S:X="," Y=Y+1
 I NOA,Y<NOA!(Y>$P(NOA,";",2)) D E^ZZRGND1(43)
 D DN(.LV,.LI,S)
 D INC(.LV,.LI,.S,.S1)
 Q
 ;
TEXT(LV,LI,S,S1) ;
 N Y
 S Y=$$ASM^ZZRGND3(LV+1,LI(LV+1)+1,$C(10))
 D ST("MK","$T("_$S($E(Y)'="+":Y,1:""))
 I $$VT(Y) D ST("I",Y)
 I Y["^",$$VT($P(Y,"^",2)) D
 . N X1,X2
 . S X1=$P(Y,"^"),X2=$P(Y,"^",2)
 . D ST("$T",X2_$S($$VT(X1):" "_X1,1:""))
 I $E(Y)="@" D ARGG(Y)
 D FLUSH(LV+1)
 Q
 ;special variables
SPV(X) ;
 I "^$D^$EC^$ES^$ET^$I^$K^$P^$Q^$ST^$SY^"[("^"_X_"^") D ST("MK",X)
 Q
 ;
EXT(LV,LI,S,S1) ;Extrinsic functions
 N INFO
 S INFO=S
 I $E(S1)="^" S Y=$E(S1,2,99)_" "_S D INC(.LV,.LI,.S,.S1) S INFO=Y ;Build S and fall thru
 I S1["(" D
 . D ADDCMD("FUN")
 . D ADDARG(INFO)
 . D INC2(.LV,.LI,.S,.S1)
 . D DNPARAMS(.LV,.LI,S)
 . D INC(.LV,.LI,.S,.S1) ;Process param.
 D ST($S(INFO[" ":"X",1:"I"),INFO) ;Internal, eXternal
 Q
 ;
PKG(LV,LI,S,S1) ;External Package
 N INFO
 S INFO=S
 I S1["(" D
 . D INC2(.LV,.LI,.S,.S1)
 . D DN(.LV,.LI,S)
 . D INC(.LV,.LI,.S,.S1) ;Process param.
 D ST("X",INFO) ;Record External name
 Q
 ;
PAT(LV,LI,S,S1) ;
 D INC(.LV,.LI,.S,.S1)
 I $E(S)="@" D  Q
 . D SINDVADD(.LV,.LI)
 . D INC(.LV,.LI,.S,.S1)
 . D ARG(.LV,.LI,.S,.S1)
 F  D  Q:$E(S)=""
 . D REPCNT(.S)
 . D PATCODE(.S)
 Q
 ;
REPCNT(S) ;
 N I,X
 F I=1:1 Q:("0123456789."'[$E(S,I))!($E(S,I)="")
 S X=$E(S,1,I-1),S=$E(S,I,999)
 I ('$L(X))!($L(X,".")>2) S S="" D E^ZZRGND1(16)
 Q
 ;
PATCODE(S) ;
 N I
 I $E(S)=Q D PATQ(.S) Q
 F I=1:1 Q:("ACELNPUacelnpu()"'[$E(S,I))!($E(S,I)="")
 S X=$E(S,1,I-1),S=$E(S,I,999) I I=1 S S="" D E^ZZRGND1(16)
 I $E(S)="," S S=$E(S,2,999) ;Pull ',' out of alternation
 Q
 ;
PATQ(S) ;
 N Q,I,CH
 S Q=""""
 S I=1
 F  D  Q:CH'=Q
 . F I=I+1:1 S CH=$E(S,I) Q:CH=""!(CH=Q)
 . S I=I+1
 . D:CH="" E^ZZRGND1(6)
 . S CH=$E(S,I)
 S S=$E(S,I,999)
 S:$L(CH)&(",)"[CH) S=$E(S,2,999)
 Q
 ;
ST(LOC,S) ; 
 S:'$D(V(LOC,S)) V(LOC,S)=""
 S:'$D(GK) GK=""
 D SETVADDL(LOC,S)
 S:$E(GK)="p" GK=""
 I $D(GK),GK]"",V(LOC,S)'[GK S V(LOC,S)=V(LOC,S)_GK
 S GK=""
 Q
 ;
SETVADDL(LOC,S) ;
 N CNT
 S CNT=+$G(VADDL)+1
 S VADDL(CNT)=LOC_$C(9)_S_$C(9)_$G(GK)
 S VADDL=CNT
 Q
 ; 
VT(X) ;Check if a valid label
 Q (X?1A.31AN)!(X?1"%".31UN)!(X?1.31N)
 ;
VA(X) ;Check if VA Standard label
 Q (X?1U.7UN)!(X?1"%".7UN)!(X?1.8N)
 ;
FLUSH(L) ;Flush rest of list with this offset
 N I,CH
 S I=LI(L)+1
 F I=I:1 S CH=$G(LV(L,I)) Q:$C(10)[CH  D:CH="(" FLUSH(L+1)
 S LI(L)=I
 Q
 ;
FNC() ;Sets or returns the current function
 N W
 S W=+$S($D(LV(LV,"FNC",LI)):LI,$O(LV(LV,"FNC",LI),-1):$O(LV(LV,"FNC",LI),-1),1:$O(LV(LV,"FNC",LI))) ;patch 119,121
 Q $G(LV(LV,"FNC",W))
 ;
ADDARG(ARG) ;
 Q:$G(CX)=""
 N CNT
 S CNT=+$G(PRSTREE) ;+1
 Q:CNT=0
 N CURRENT
 S CURRENT=$G(PRSTREE(CNT))
 Q:$L(CURRENT,$C(9))>2
 S PRSTREE(CNT)=PRSTREE(CNT)_$C(9)_ARG
 Q
 ;
ADDCMD(CMD) ;
 N CNT
 S CNT=+$G(PRSTREE)+1
 I "SD"[CMD S PRSTREE(CNT)=CMD
 E  S PRSTREE(CNT)="X"
 S PRSTREE=CNT
 ;
