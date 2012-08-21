ZZRGND3 ;;CBR/AU - XINDEX based routines ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
DN S LI(LV)=LI,LI(LV,1)=AC,LV=LV+1,LI=LI(LV),AC=NOA
 Q
UP ;Inc LI as we save to skip the $C(10).
 N Y
 S Y=$$PEEK^ZZRGND2(.LV,.LI)
 S:$A(Y)=10 LI=LI+1 
 S LI(LV)=LI,LV=LV-1,LI=LI(LV),AC=LI(LV,1) 
 Q
 ;
SWAPLRHS(VADDL,LHSSTART,RHSSTART) ; Swap LHS and RHS in VADDL order 
 N I,J,CNT,TMP,V,NSTART
 S RHSSTART=+$G(RHSSTART)
 S LHSSTART=+$G(LHSSTART)
 Q:RHSSTART'>LHSSTART
 S CNT=+$G(VADDL)
 S J=0
 F I=RHSSTART:1:CNT D
 . S TMP(J)=VADDL(I)
 . S J=J+1
 Q:J=0
 F I=CNT:-1:LHSSTART+J D
 . S VADDL(I)=VADDL(I-J)
 F I=0:1:J-1 D
 . S VADDL(I+LHSSTART)=TMP(I)
 Q
 ; 
S(STR,V,VADDL) ;Set
 N RHS,LV,LI,LHSSTART,RHSSTART
 S RHS=0,LHSSTART=$G(VADDL)+1 
 N CH
 D PARSE^ZZRGND9(STR,.LV,.LI)
 F  S GK="" D INC^ZZRGND2(.LV,.LI,.S,.S1) Q:S=""  D 
 . S CH=$E(S) 
 . I CH=",","!""#&)*+-,./:;<=?\]_~"[$E(S1),RHS=1 D E^ZZRGND1(10) Q
 . I CH="," D  Q
 . . D:$D(RHSSTART) SWAPLRHS(.VADDL,LHSSTART,RHSSTART)
 . . S RHS=0,LHSSTART=$G(VADDL)+1 
 . I CH="=" D  Q
 . . S RHS=1
 . . S RHSSTART=$G(VADDL)+1 
 . . D:"!#&)*,/:;<=?\]_~"[$E(S1) E^ZZRGND1(10)
 . . D ADDARG^ZZRGND2($E($$ASM(.LV,.LI,","),2,999)) 
 . I CH="$",'RHS D  D:% E^ZZRGND1(10) ;Can't be on RHS of set.
 . . S %=1
 . . I "$E$P$X$Y"[$E(S,1,2) S %=0 Q
 . . I "$EC$ET$QS"[$E(S,1,3) S %=0 Q
 . . I "$ZE$ZT"[$E(S,1,3) S %=0 Q  ;Pickup in ZZRGND9
 . . Q
 . ;I CH="^" D FL(.LV,.LI,.S,.S1) Q
 . I CH="@" D  Q
 . . S Y=$$ASM(LV,LI,",") 
 . . S:Y'["=" RHS=1
 . . D SINDVADD^ZZRGND2(.LV,.LI)
 . . D INC^ZZRGND2(.LV,.LI,.S,.S1)
 . . D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 . I CH="(" D  Q
 . . D ADDCMD^ZZRGND2("S")
 . . D ADDARG^ZZRGND2($$ASM^ZZRGND3(LV+1,LI(LV+1)+1,$C(10)))
 . . D MULT(.LV,.LI,.S,.S1) Q
 . I 'RHS,CH'=")" D
 . . D ADDCMD^ZZRGND2("S")
 . . D ADDARG^ZZRGND2($$ASM(.LV,.LI,"="))
 . D FL(.LV,.LI,.S,.S1) 
 D:'RHS E^ZZRGND1(10)
 D:RHS&$D(RHSSTART) SWAPLRHS(.VADDL,LHSSTART,RHSSTART)
 Q
 ;
MULT(LV,LI,S,S1) ;
 D INC^ZZRGND2(.LV,.LI,.S,.S1) 
 S NOA=S 
 I S'>0 D E^ZZRGND1(5) Q
 D DN 
 S AC=AC+LI 
 F  Q:AC'>LI  S:'RHS GK="*" D
 . D INC^ZZRGND2(.LV,.LI,.S,.S1)
 . D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 D UP 
 Q
 ;
FL(LV,LI,S,S1) ;
 S:'RHS GK="*" 
 D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 Q
 ;
VLNF(X) ;Drop into VLN
VLN ;Valid Local Name > Variable
 S ERR=0
 Q:X?1(1U,1"%").15UN
 I X?1(1A,1"%").15AN D E^ZZRGND1(57) Q  ;Lowercase
 D E^ZZRGND1(11) ;Too long or other problem
 Q
VGN ;Valid Global Name
 S ERR=0 I X'?1(1U,1"%").7UN D E^ZZRGND1(12)
 Q
KL ;Process KILL
 N LV,LI,CH
 S STR=ARG,ARG(1)=ARG,ARG="" 
 D PARSE^ZZRGND9(STR,.LV,.LI)
 F  D INC^ZZRGND2(.LV,.LI,.S,.S1) Q:S=""  D
 . S CH=$E(S) 
 . Q:CH=","
 . S LOC="L"
 . D @$S(CH="@":"KL1",CH="^":"KL2",CH="(":"KL4",1:"KL3")
 Q
 ;
KL1 ;
 D SINDVADD^ZZRGND2(.LV,.LI)
 D INC^ZZRGND2(.LV,.LI,.S,.S1)
 D ARG^ZZRGND2(.LV,.LI,.S,.S1) 
 Q
KL2 S GK="!"
 I S1'="(" S ERR=24 D ^ZZRGND1
 D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 Q
KL3 ;
 I "^DT^DTIME^DUZ^IOST^IOM^U^"[("^"_S_"^") S ERR=39,ERR(1)=S D ^ZZRGND1
 I "IO"=S D
 . S:S1="(" Y=$$PEEKDN^ZZRGND2(.LV,.LI) 
 . S ERR=39,ERR(1)=S_$S(S1["(":S1_Y_")",1:"") 
 . D:S1'="(" ^ZZRGND1 
 . I S1="(",("QC"'[$E(Y,2)) D ^ZZRGND1
KL5 ;
 S GK="!" D ARG^ZZRGND2(.LV,.LI,.S,.S1) Q  ;KILL SUBS
 Q
 ;
KL4 ;
 S NOA=S1 
 D DN
 D ARGS^ZZRGND2(.LV,.LI,NOA)
 D UP
 D INC2^ZZRGND2(.LV,.LI,.S,.S1) 
 Q
 ;
NE(STR,V,VADDL) ;Record newed variables
 N LV,LI,CH,ERTX,LVIND,ININD
 S ININD=0
 I "("[$E(STR) D  Q 
 . D E^ZZRGND1(26) ;Exclusive NEW command
 D PARSE^ZZRGND9(STR,.LV,.LI)
 F  D INC^ZZRGND2(.LV,.LI,.S,.S1) Q:S=""  D
 . S CH=$E(S)
 . ;I ININD,CH'="," Q
 . ;S ININD=0
 . S:CH="," ININD=0
 . Q:CH=","
 . S ERTX=""
 . I ('ININD),CH?1P,(CH'=S) D  Q:$G(ERTX)]""
 . . I "@("[CH,"$$E"'[$E(S,1,2),($P(S,CH,2)'?1A) D E^ZZRGND1(11) Q
 . . I "$"[CH,(LV(LV,1)'="@") D E^ZZRGND1(11) Q
 . I CH="@" D    ; Q
 . . S ININD=1
 . S GK="~" S:ININD GK="" D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 Q
 ;
RD(STR,V,VADDL) ;
 N LV,LI,CH
 D PARSE^ZZRGND9(STR,.LV,.LI) 
 F  D INC^ZZRGND2(.LV,.LI,.S,.S1) Q:S=""  D
 . S CH=$E(S) 
 . I '((CH="%")!(CH?1A)!(CH="*")) D RD3 Q
 . S Y=$$ASM(LV,LI,",")
 . I Y'[":" S ERR=33,RDTIME=1 D ^ZZRGND1
 . D RD2 
 Q
 ;
RD2 ;
 F  S CH=$E(S) Q:","[CH  D
 . I "*#"[CH D E^ZZRGND1(41)
 . I "#:"[CH D  Q
 . . D INC^ZZRGND2(.LV,.LI,.S,.S1)
 . . D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 . . D INC^ZZRGND2(.LV,.LI,.S,.S1)
 . I (CH="%")!(CH?1A) D  Q
 . . S LOC="L",GK="*" 
 . . D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 . . D INC^ZZRGND2(.LV,.LI,.S,.S1)
 . D INC^ZZRGND2(.LV,.LI,.S,.S1) 
 Q
 ;
RD3 ;
 N DONE
 S DONE=0
 F  S CH=$E(S) Q:","[CH  D  Q:DONE
 . I "!#?"[CH D INC^ZZRGND2(.LV,.LI,.S,.S1) Q
 . I (CH="%")!(CH?1A)!(CH="@") D  Q
 . . D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 . . D INC^ZZRGND2(.LV,.LI,.S,.S1)
 . S DONE=1 
 Q
 ;
O(STR,V,VADDL) ;
 N LV,LI
 D PARSE^ZZRGND9(STR,.LV,.LI)
 D INC^ZZRGND2(.LV,.LI,.S,.S1) 
 I S["@" D  Q 
 . D SETVADDL^ZZRGND2("@","")
 . D ARGS^ZZRGND2(.LV,.LI,255)
 D ARG^ZZRGND2(.LV,.LI,.S,.S1)
 D INC^ZZRGND2(.LV,.LI,.S,.S1)
 F  D INC^ZZRGND2(.LV,.LI,.S,.S1) Q:":"[S
 D INC^ZZRGND2(.LV,.LI,.S,.S1)
 D ARGS^ZZRGND2(.LV,.LI,255)
 Q
 ;
ASM(WL,SI,L,SEP) ;
 N %,CH,Y 
 S SEP=$G(SEP),Y="" 
 F %=SI:1 S CH=$G(LV(WL,%)) Q:L[CH  S Y=Y_SEP_CH
 Q Y
 ;
