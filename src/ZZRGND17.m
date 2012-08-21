ZZRGND17 ;;CBR/AU - Process FileMan calls ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
 Q  ; Call tags
 ;
RFMCALLS(GLB,FILEPATH,FILENAME) ; Prints FileMan calls per M TAG and globals used by these calls
 I FILEPATH]"",FILENAME]"" D TEXTOPEN^ZZRGND23("OUTFILE",FILEPATH,FILENAME)
 N PKG,RTN,LN
 S:$G(GLB)="" GLB="^ZZRG"
 S PKG="",LN=0
 F  S PKG=$O(@GLB@(7.5,PKG)) Q:PKG=""  S LN=LN+1 D
 . W "--------------------------------------------------------------",!!
 . W LN,". PACKAGE NAME: "_$$GPKGNAME^ZZRGND19(PKG,GLB),!!
 . S RTN=""
 . F  S RTN=$O(@GLB@(7.5,PKG,RTN)) Q:RTN=""  D
 . . I '$D(@GLB@(7.5,PKG,RTN,"FMG")),'$D(@GLB@(7.5,PKG,RTN,"FMGC"))
 . . W " "_RTN,!
 . . D REPORTGL^ZZRGND13("        Globals: ",GLB_"(7.5,"""_PKG_""","""_RTN_""",""FMG"")",0)
 . . D REPORTGL^ZZRGND13("  FileMan calls: ",GLB_"(7.5,"""_PKG_""","""_RTN_""",""FMGC"")",0)
 . . W !
 . W "--------------------------------------------------------------",!!
 I FILEPATH]"",FILENAME]"" D TEXTCLS^ZZRGND23("OUTFILE",FILENAME)
 Q
 ;
 ; Indexes FileMan calls
 ;  GLB: Where the information is stored
NDXFMAN(GLB) 
 N I,RTN,TAG,PKG
 S:$G(GLB)="" GLB="^ZZRG"
 K @GLB@(7.5)
 S I=""
 S RTN=""
 F  S RTN=$O(@GLB@(2,RTN)) Q:RTN=""  D
 . S PKG=$$GETPKG^ZZRGND19(RTN,GLB)
 . K RESULT
 . S TAG=""
 . F  S TAG=$O(@GLB@(2,RTN,TAG)) Q:TAG=""  D
 . . S I="" 
 . . F  S I=$O(@GLB@(2,RTN,TAG,I)) Q:I=""  D 
 . . . Q:I="N"  Q:I="P"  Q:I="L"
 . . . Q:'$D(@GLB@(2,RTN,TAG,I,"PRS"))
 . . . D NDXFMANG(GLB,RTN,TAG,.RESULT,I)
 . M:$D(RESULT("FMG")) @GLB@(7.5,PKG,RTN,"FMG")=RESULT("FMG")
 . M:$D(RESULT("FMGC")) @GLB@(7.5,PKG,RTN,"FMGC")=RESULT("FMGC")
 Q
 ;
NDXFMANG(GL,RTN,TAG,RESULT,NDX) ; Indexes FileMan calls
 N J,K,NAME,SUB,CMDCNT,CMD,CX,ARG,R,IRTN,ITAG,I
 Q:'$D(@GL@(2,RTN,TAG,NDX,"PRS"))
 Q:$$GETPKG^ZZRGND19(RTN,GL)="DI"  Q:$$GETPKG^ZZRGND19(RTN,GL)="XU"  
 Q:$$GETPKG^ZZRGND19(RTN,GL)="UNCATEGORIZED"
 S CMDCNT=+@GL@(2,RTN,TAG,NDX,"PRS")
 F J=1:1:CMDCNT D 
 . S CNT=+@GL@(2,RTN,TAG,NDX,"PRS",J) Q:CNT=0
 . F K=1:1:CNT D
 . . S CMD=@GL@(2,RTN,TAG,NDX,"PRS",J,K)
 . . S CX=$P(CMD,$C(9),1)
 . . S ARG=$P(CMD,$C(9),2)
 . . S VAL=$P(CMD,$C(9),3)
 . . S VAL=$P(VAL,$C(10),1)
 . . I CX="S" D  Q
 . . . I $$FMARG(ARG),VAL'="" D
 . . . . I $E(VAL,1,2)="""^" D  I 1
 . . . . . Q:$L(VAL,"""_")>1   ; Not implemented in java so no testing for now
 . . . . . S VAL=$E($P(VAL,",",1),2,999)
 . . . . . S NAME=$P(VAL,"(",1)
 . . . . . S R=NAME_"("
 . . . . . S SUB=$P(VAL,"(",2)
 . . . . . S:SUB]"" SUB=$P(SUB,",",1)
 . . . . . S:SUB]"" SUB=$P(SUB,"""",1)
 . . . . . I $$ISFILEN(SUB) S R=R_SUB
 . . . . . S RESULT("FMG",R)=""
 . . I (CX="D")!(CX="X") D  Q
 . . . S:$E(VAL)="""" VAL=$E(VAL,2,$L(VAL)-1)
 . . . Q:VAL=""  Q:'$$ISFILEN(VAL)
 . . . S IRTN=$P(ARG," ",1)
 . . . S ITAG=$P(ARG," ",2)
 . . . S:$E(ITAG)="$" ITAG=$E(ITAG,3,999)
 . . . I $$GETPKG^ZZRGND19(IRTN,GL)="DI" D
 . . . . S R=ITAG_"^"_IRTN_"("_VAL
 . . . . S RESULT("FMGC",R)=""
 Q
 ;
ISFILEN(V) ;
 N CNT,P
 S CNT=$L(V,".") 
 Q:CNT>2 0
 I CNT=1,V'?1.N Q 0
 S P=$P(V,".",2)
 I CNT=2,(P'?1.N) Q 0
 S V=$P(V,".",1) Q:V="" 1
 Q V?1.N
 ; 
FMARGS(ARG) ; True if ARG is a valid argument in classic FileMan calls
 Q (ARG="DIC")!(ARG="DIE")!(ARG="DIK")
 ;
FMARG(ARG) ; True if ARG is a valid argument in classic FileMan calls
 Q:$G(ARG)="" 0
 N RESULT
 S RESULT=$$FMARGS(ARG)
 Q:RESULT 1
  N CNT
 S CNT=$L(ARG,",")
 Q:CNT<2 0
 N I
 F I=1:1:CNT S RESULT=$$FMARGS($P(ARG,",",I)) Q:RESULT
 Q RESULT
 ;
