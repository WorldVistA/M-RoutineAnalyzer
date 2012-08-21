ZZRGND9 ;;CBR/AU - XINDEX based routines ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
 Q
 ;LV is a set of Linked Values
PARSE(STR,LV,LI,DUPABLE) ;
 N I,LL,CH,OP,CH1,EC,Q
 S (LI,I)=0,(LL,LV)=1,(OP,CH)="",Q=""""
 ;
 F  S I=I+1,CH=$E(STR,I),CH1=$E(STR,I+1) Q:(CH="")!(CH=";")!(CH'?1ANP)  D
 . I """$()"[CH D  Q
 . . I CH=Q D QUOTE(STR,OP,.I) Q
 . . D:CH="$" FUNC(STR,.LV,.LI,.I,.LL,.CH)
 . . D:CH="(" DN(STR,.LV,.LI,.I,.LL,.CH)
 . . D:CH=")" UP(STR,.LV,.LI,.I,.LL,.CH,CH1)
 . I CH="^",CH1="$" D SSVN(STR,.LV,.LI,.I,.LL,.CH) Q
 . I CH="^",I=LL Q:CH1'="["  D  Q 
 . . S I=I+1 
 . . D ADD($E(STR,LL,I),.LV,.LI) 
 . . S LL=I+1
 . I CH?1A!(CH="%") D VAR(STR,.I,.CH) Q
 . I CH?1N D NUM(STR,.I,.CH) Q
 . S:"+-#'/*_&![]<>?"[CH OP=CH
 . I CH="?",",!#"'[$E(STR,I-1) D  Q
 . . D AR(STR,I,.LV,.LI,.LL)
 . . D PAT(STR,.LV,.I,.CH) 
 . I CH=",",CH1=":" D E^ZZRGND1(21) ;P121
 . I CH?1P D  ;Check for dup operators
 . . D AR(STR,I,.LV,.LI,.LL)
 . . Q:(CH_CH1="]]")
 . . I $D(DUPABLE),DUPABLE[CH Q
 . . I ((CH=CH1)&(",_/\[]&|"[CH))&($$FNC(.LV,LI)'="$$") D E^ZZRGND1(21) ;p110
 D:CH="" PEND(STR,I,.LV,.LI,.LL)
 D:CH]"" E^ZZRGND1(11)
 S LI=0,AC=255 F %=0:0 S %=$O(LV(%)) Q:%'>0  S LI(%)=0
 Q
 ;End of parse
PEND(STR,I,LV,LI,LL) ;
 D AR(STR,I,.LV,.LI,.LL)
 D:LV>1 E^ZZRGND1(5)  ;LV>1 means mis-match ()
 I $D(DUPABLE),DUPABLE["," Q
 D:($G(LV(1,1))=",") E^ZZRGND1(21)
 Q
 ;
DN(STR,LV,LI,I,LL,CH) ;
 D STR(STR,I,.LL) 
 D ADD(CH,.LV,.LI)
 D NEW(I,.LL) 
 S LI(LV)=LI,LV=LV+1 
 S:'$D(LI(LV)) LI(LV)=0 
 S LI=LI(LV)
 S LI(LV-1,1)=LI
 Q
 ;
UP(STR,LV,LI,I,LL,CH,CH1) ;
 N EC
 I LV<2 D E^ZZRGND1(5) Q
 D STR(STR,I,.LL) 
 S EC=LI-LI(LV-1,1),X=$C(10) 
 D ADD(X,.LV,.LI)
 D NEW(I,.LL) 
 S LI(LV)=LI,LV=LV-1,LI=LI(LV)
 D ADD(EC,.LV,.LI) 
 D ADD(CH,.LV,.LI) 
 I CH1]"",",_=+-*/\#'):<>[]?&!@"'[CH1 D E^ZZRGND1(43)
 Q
 ;
NEW(I,LL) ;
 S LL=I+1
 Q
 ;
AR(STR,I,LV,LI,LL) ;
 D STR(STR,I,.LL) 
 D ADD(CH,.LV,.LI)
 D NEW(I,.LL) 
 Q
 ;
STR(STR,I,LL) ;
 N X
 S X=$E(STR,LL,I-1) 
 Q:'$L(X)
 D ADD(X,.LV,.LI)
 Q
 ;
ADD(X,LV,LI) ;
 S LI=LI+1
 S LV(LV,LI)=X 
 Q
 ;
SETFNC(F,LV,LI) ;
 S LV(LV+1,"FNC",$G(LI(LV))+1)=F
 Q
 ;
FNC(LV,LI) 
 N W 
 S W=+$S($D(LV(LV,"FNC",LI)):LI,$O(LV(LV,"FNC",LI)):$O(LV(LV,"FNC",LI)),1:$O(LV(LV,"FNC",LI),-1)) ;patch 119
 Q $G(LV(LV,"FNC",W))
 ;
QUOTE(STR,OP,I) ;
 N Q,CH
 S Q=""""
 F  D  Q:$E(STR,I+1)'=Q  S I=I+1
 . F I=I+1:1 S CH=$E(STR,I) Q:CH=""!(CH=Q)
 I OP'="?",$E(STR,I+1)]"","[]()<>\/+-=&!_#*,:'|"'[$E(STR,I+1) D E^ZZRGND1(46) Q
 Q:CH]""  
 D E^ZZRGND1(6) Q
 ;
GVAR(STR,I,CH,LL) ;EF get var
 N % 
 D VAR(STR,.I,.CH)
 S %=$E(STR,LL,I),LL=I+1
 Q %
 ;
VAR(STR,I,CH) ;
 N J
 F J=I+1:1 S CH=$E(STR,J) Q:CH'?1AN
 S I=J-1 
 Q
 ;
NUM(STR,I,CH) ;
 N J
 F  D  Q:CH'="E"  S CH=$E(STR,J+1) Q:'(CH?1N!("+-"[CH))  S I=J
 . F J=I+1:1 S CH=$E(STR,J) Q:"0123456789."'[CH!(CH="")
 I CH]"",CH'?1P D E^ZZRGND1(53)
 S I=J-1
 Q
 ;
INC(STR,I,CH) ;
 S I=I+1
 S CH=$E(STR,I)
 Q
 ;
FUNC(STR,LV,LI,I,LL,CH) ;Functions and special var's.
 N F1
 D INC(STR,.I,.CH) 
 S X=CH,S=$$GVAR(STR,.I,.CH,.LL)
 I S["$$" D EXT(S,.LV,.LI,.CH) Q
 I S["$&" D PKG(STR,.LV,.LI,.I,.LL,.CH) Q
 I CH'="(" D SPV(S,.LV,.LI,.CH) Q
 S S=$$CASE($E(S,2,11))
 S F1=$G(IND("FNC",S)) 
 I '$L(F1) D E^ZZRGND1(3) S F1=S I 1
 E  I "ZV"[X D E^ZZRGND1($S("Z"[X:31,1:27))
 S X="$"_F1,CH="" 
 D SETFNC("$F",.LV,.LI)
 D ADD(X,.LV,.LI)
 Q
 ;
SPV(S,LV,LI,CH) ;
 N X
 D SETFNC("$V",.LV,.LI)
 D ADD(S,.LV,.LI)
 S X=$E(S,2,9),CH=""
 I $E(S,2)="Z" D E^ZZRGND1(28) Q
 I '$D(IND("SVN",X)) D E^ZZRGND1(4)
 Q
 ;
EXT(S,LV,LI,CH) ;EXTRINSIC
 S CH="" 
 D SETFNC("$$",.LV,.LI)
 D ADD(S,.LV,.LI)
 Q
 ;
SSVN(STR,LV,LI,I,LL,CH) ;Error 54 access for Kernel only
 N X
 D INC(STR,.I,.CH) 
 S X=$$GVAR(STR,.I,.CH,.LL) 
 I '$D(IND("SSVN",$E(X,3,99))) D E^ZZRGND1(4) Q  ;Need new #
 D E^ZZRGND1(54)
 D ADD(X,.LV,.LI)
 Q
 ;
PKG(STR,LV,LI,I,LL,CH) ;External Function
 N J
 S J=$F(STR,"(",I),I=J-2,X=S_$E(STR,LL,I),LL=J-1,CH=""
 D ADD(X,.LV,.LI)
 D E^ZZRGND1(55) ;Not standard VA
 Q
 ;
PAT(STR,LV,I,CH) ;
 N PC 
 S PC=0
 F I=I+1:1 D  I CH=""!(CH'?1N&("ACELNPUacelnpu."'[CH)) Q
 . S CH=$E(STR,I) 
 . D:CH="""" PATQ(STR,.I,.CH)
 . D:CH="(" PATD(.PC,.CH)
 . D:CH=")" PATU(LV,.PC,.CH)
 . D:CH="," PATC(PC,.CH)
 D:PC E^ZZRGND1(5)
 S I=I-1 
 D:":),@+-_*/\!&'"'[CH E^ZZRGND1(16)
 Q
 ;Quote in Pattern
PATQ(STR,I,CH) ;
 N Q
 S Q=""""
 F  D  Q:CH'=Q
 . F I=I+1:1 S CH=$E(STR,I) Q:CH=""!(CH=Q)
 . D:CH="" E^ZZRGND1(6) 
 . S I=I+1,CH=$E(STR,I)
 Q
 ;
PATD(PC,CH) ;
 S PC=PC+1,CH="." ;p110 Start Alt.
 Q
 ;
PATU(LV,PC,CH) ;
 I 'PC,LV>1 S CH="" Q  ;End
 S PC=PC-1,CH="." ;p110 End Alt.
 Q
 ;
PATC(PC,CH) ;
 I PC<1 Q  ;
 S CH="." ;p110 Comma in Alt.
 Q
 ;
CASE(%) ;UpperCase
 Q $TR(%,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 ;
TEST ;
 ;;CH=""!(CH=Q)
 ;;NUMVAL?.1(1"+",1"-")1(1.N.1".".N,.N.1"."1.N).1(1"E".1(1"+",1"-")1.N)
 N LV,LI
 S STR=$E($T(TEST+1),4,999) 
 D PARSE^ZZRGND9(STR,.LV,.LI)
 Q
