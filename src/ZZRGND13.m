ZZRGND13 ;CBR/ - REPORTS ;02/01/12
 ;;7.3;TOOLKIT;**to be determined**;
SAMEPKG(RTN,CMPRTN,GL) ;
 Q $$GETPKG(RTN,"",GL)=$$GETPKG(CMPRTN,"",GL) 
 ;
GPKGINFO(SOURCE,TARGET) ;
 N RTN,PKG,ID,INFO,P
 S RTN=""
 F  S RTN=$O(@SOURCE@(RTN)) Q:RTN=""  D
 . S PKG=$$GETPKG(RTN,.P)
 . S @TARGET@(PKG,P)=""
 . S @TARGET@(PKG,P,RTN)=""
 S PKG=""
 F  S PKG=$O(@TARGET@(PKG))  Q:PKG=""  D
 . I PKG="UNKNOWN" S INFO="UNKNOWN" I 1
 . E  I PKG="MXML" S INFO="M XML PARSER^MXML" I 1
 . E  I PKG="NVS" S INFO="NATIONAL VISTA SUPPORT^NVS" I 1
 . E  I PKG="VEX" S INFO="Vendor - Audiofax Inc^VEX" I 1
 . E  D
 . . S ID=$O(^DIC(9.4,"C",PKG,""))
 . . S INFO=^DIC(9.4,ID,0)
 . S P=""
 . F  S P=$O(@TARGET@(PKG,P)) Q:P=""  S @TARGET@(PKG,P)=INFO
 . S @TARGET@(PKG)=INFO
 Q
 ;
ERR(IDX,MSG) ;
 N CNT
 S CNT=+$G(^UTILITY($J,"FO","E",0))+1
 S ^UTILITY($J,"FO","E",0)=CNT
 S ^UTILITY($J,"FO","E",CNT)=MSG 
 Q
 ;
NEWINP(INP) ;
 N I
 F I=1:1:10 S INP(I)=0
 S INP(11)=""  ; Package
 S INP(12)=""  ; Build
 S INP("MAX")=20000  ;Max routine size
 S INP("CMAX")=15000 ;Max Code in routine
 Q  
 ;
NDXPRTNS(PKG,TARGET) ;
 N CNT,RTN,INP,NRO,DA,INDDA,RTNPKG 
 D NEWINP(.INP)
 S INP(10)=9.4,INP(1)=1,DA=-1,INDDA=DA
 S CNT=0,RTN=""
 K:TARGET]"" ^UTILITY($J) 
 S RTN=""
 F  S RTN=$O(^ROUTINE(RTN)) Q:RTN=""  D
 . I PKG]"",$$GETPKG(RTN)'=PKG Q 
 . S CNT=CNT+1
 . S ^UTILITY($J,RTN)=""
 S ^UTILITY($J,0)=CNT_";ROU",NRO=CNT
 D MAIN^ZZRGNDX(1)
 I TARGET]"" D
 . M @TARGET=^UTILITY($J)
 . K ^UTILITY($J) 
 Q 
 ;
NDXFI(GL) ;
 N RTN,TAGS,TAG,PKG,T,TRTN,TTAG,TPKG,I,J
 S RTN=""
 F  S RTN=$O(@GL@(1,RTN)) Q:RTN=""  D 
 . S PKG=$$GETPKG(RTN,"",GL)
 . S:'$D(@GL@(4,PKG)) @GL@(4,PKG)=0
 . S T=""
 . F  S T=$O(@GL@(1,RTN,"X",T)) Q:T=""  D
 . . S TRTN=$P(T," ",1)
 . . S TTAG=$P(T," ",2)
 . . Q:TTAG=""
 . . S TPKG=$$GETPKG(TRTN,"",GL)
 . . Q:PKG=TPKG
 . . I '$D(@GL@(4,TPKG,PKG)) S @GL@(4,TPKG)=+$G(@GL@(4,TPKG))+1,@GL@(4,TPKG,PKG)="" 
 . . S I=""
 . . F  S I=$O(@GL@(1,RTN,"X",T,I)) Q:I=""  D
 . . . S TAGS=@GL@(1,RTN,"X",T,I)
 . . . F J=1:1:$L(TAGS,",") D
 . . . . S TAG=$P(TAGS,",",J)
 . . . . S @GL@(8,PKG,TPKG,TRTN,TTAG)=""  
 . . . . S @GL@(9,TPKG,TRTN,TTAG,PKG,TAG_"^"_RTN)=""  
 Q
 ;
SETLOCAL(RESULT,PAR,LOC,SCRIPT) ;
 S RESULT(SCRIPT,LOC)=""
 S:PAR RESULT("(",LOC)=""
 Q
 ;  
MRGLOCAL(RESULT,IRESULT,FPAR,SAMEPKG) ;
 N LOC,PNDX,CMD,GLOB
 S LOC=""
 F  S LOC=$O(IRESULT("I",LOC)) Q:LOC=""  D
 . Q:$D(RESULT("S",LOC))
 . I $D(IRESULT("F",LOC)) D  I 1
 . . Q:'$D(IRESULT("I",LOC))
 . . S PNDX=IRESULT("F",LOC)
 . . D:$D(FPAR(PNDX)) SETLOCAL(.RESULT,$D(IRESULT("(",LOC)),FPAR(PNDX),"I") 
 . E  D SETLOCAL(.RESULT,$D(IRESULT("(",LOC)),LOC,"I")
 S LOC=""
 F  S LOC=$O(IRESULT("O",LOC)) Q:LOC=""  D
 . I $D(IRESULT("F",LOC)) D  Q
 . . S PNDX=IRESULT("F",LOC)
 . . D:$D(FPAR(PNDX)) SETLOCAL(.RESULT,$D(IRESULT("(",LOC)),FPAR(PNDX),"O")
 . Q:$D(RESULT("N",LOC))
 . D SETLOCAL(.RESULT,$D(IRESULT("(",LOC)),LOC,"O")
 S CMD=""
 F  S CMD=$O(IRESULT("CMD",CMD)) Q:CMD=""  D
 . I (CMD="X"),'SAMEPKG Q
 . I (CMD="@"),'SAMEPKG Q
 . S RESULT("CMD",CMD)=$G(RESULT("CMD",CMD))+IRESULT("CMD",CMD)
 S GLOB=""
 F  S GLOB=$O(IRESULT("G",GLOB)) Q:GLOB=""  D
 . S RESULT("G",GLOB)=""
 I SAMEPKG F  S GLOB=$O(IRESULT("GL",GLOB)) Q:GLOB=""  D
 . S RESULT("GL",GLOB)="" 
 Q
 ; 
NDXLOCAL(RTN,TAG,GL,RESULT,ITAGS,START,LEVEL) ;
 N I,J,L,T,CNT,FACNT,FPAR,Q,TAGLEVEL,LOCALLVL,GLOB
 N IRESULT,IRTNTAG,IRTN,ITAG,CMD
 N LOC,LT
 I TAG="" Q
 I '$D(@GL@(2,RTN,TAG)) Q
 S Q=0,FACNT=1
 I '$D(START) S J="" I 1
 E  S J=START
 S TAGLEVEL=@GL@(2,RTN,TAG,"L")
 S:'$D(LEVEL) LEVEL=TAGLEVEL
 S ITAGS(TAG_"^"_RTN)=""
 F  S J=$O(@GL@(2,RTN,TAG,J)) Q:J=""  Q:Q  D 
 . Q:J="N"  Q:J="P"  Q:J="L"
 . S CNT=+@GL@(2,RTN,TAG,J)
 . F I=1:1:CNT D  Q:Q
 . . S L=@GL@(2,RTN,TAG,J,I) Q:L=""
 . . S LOCALLVL=@GL@(2,RTN,TAG,J,I,"LVL")
 . . I LOCALLVL'=LEVEL S:LOCALLVL<LEVEL Q=1 Q
 . . S T=$P(L,$C(9),1)
 . . I T="@" D  Q
 . . . S RESULT("CMD","@")=+$GET(RESULT("CMD","@"))+1
 . . I T="CMD" D  Q
 . . . S CMD=$P(L,$C(9),2)
 . . . I CMD="XECUTE" S RESULT("CMD","X")=+$GET(RESULT("CMD","X"))+1
 . . . I CMD="READ" S RESULT("CMD","R")=+$GET(RESULT("CMD","R"))+1
 . . . I CMD="WRITE" S RESULT("CMD","W")=+$GET(RESULT("CMD","W"))+1
 . . I T="LVL" D  Q
 . . . K IRESULT
 . . . D NDXLOCAL(RTN,TAG,GL,.IRESULT,.ITAGS,J,LOCALLVL+1)
 . . . D MRGLOCAL(.RESULT,.IRESULT,"",1)
 . . I T="Q" S Q=1 Q
 . . I T="X"!(T="I") D  Q  
 . . . K IRESULT
 . . . S IRTNTAG=$P(L,$C(9),2)
 . . . I T="I" D  I 1
 . . . . S IRTN=RTN
 . . . . S ITAG=IRTNTAG
 . . . E  D
 . . . . S IRTN=$P(IRTNTAG," ",1)
 . . . . S ITAG=$P(IRTNTAG," ",2)
 . . . I ITAG="" D
 . . . . S ITAG=IRTN
 . . . . S IRTN=RTN
 . . . S:$E(ITAG,1,2)="$$" ITAG=$E(ITAG,3,$L(ITAG))
 . . . I $D(ITAGS(ITAG_"^"_IRTN)) K FPAR Q 
 . . . S ITAGS(ITAG_"^"_IRTN)=""
 . . . S:ITAG]"" ^AFSPATH(IRTN,ITAG,RTN,TAG)="" I 1
 . . . E  S:ITAG]"" ^AFSPATH(IRTN,IRTN,RTN,TAG)="" I 1
 . . . D NDXLOCAL(IRTN,ITAG,GL,.IRESULT,.ITAGS)
 . . . D MRGLOCAL(.RESULT,.IRESULT,.FPAR,$$SAMEPKG(RTN,IRTN,GL))
 . . . K FPAR
 . . . S:$P(L,$C(9),3)="G0" Q=1
 . . I T="L" D
 . . . S LOC=$P(L,$C(9),2),LT=$P(L,$C(9),3)
 . . . I LOC["(" S LOC=$P(LOC,"(",1),RESULT("(",LOC)=""
 . . . I LT="" Q:$D(RESULT("S",LOC))  S RESULT("I",LOC)="" Q
 . . . I LT="n" S RESULT("F",LOC)=FACNT,FACNT=FACNT+1 Q
 . . . I LT="~",$D(RESULT("F",LOC)) Q
 . . . I $P(LT,";",1)="p" S FPAR($P(LT,";",2))=LOC
 . . . I LT="~" S RESULT("N",LOC)="" I 1
 . . . E  I '$D(RESULT("N",LOC)) S RESULT("O",LOC)=""
 . . . S RESULT("S",LOC)=""
 . . I T="G" D
 . . . S GLOB=$P(L,$C(9),2)
 . . . S RESULT("G",GLOB)=""
 . . . S RESULT("GL",GLOB)="" 
 Q:Q
 I $D(@GL@(2,RTN,TAG,"N")) D
 . S ITAG=@GL@(2,RTN,TAG,"N")
 . I $D(ITAGS(ITAG_"^"_RTN)) Q  
 . S TAGLEVEL=@GL@(2,RTN,ITAG,"L")
 . Q:TAGLEVEL'=LEVEL
 . K IRESULT
 . S ^AFSPATH(RTN,ITAG,RTN,TAG)=""
 . D NDXLOCAL(RTN,ITAG,GL,.IRESULT,.ITAGS)
 . D MRGLOCAL(.RESULT,.IRESULT,"",1)
 Q
 ;
TRANS2GL(GL,PKG,RTN,TAG,SCRIPT,RESULT) ;
 N LOC,V
 S LOC="" 
 F  S LOC=$O(RESULT(SCRIPT,LOC)) Q:LOC=""  D
 . S V=""
 . S:$D(RESULT("(",LOC)) V="*"
 . S @GL@(7,PKG,RTN,TAG,SCRIPT,LOC)=V
 Q
 ;
NDXPKGIN(GL,PKG,CLEAN) ;
 N RTN,TAG,CALLERS,TPKG,FT,FTP,TRTN,TTAG,TLINE,RESULT,ITAGS,LOC,V,CMD
 N ACTTAG
 K:CLEAN @GL@(7,PKG)
 S RTN=""
 F  S RTN=$O(@GL@(9,PKG,RTN)) Q:RTN=""  D
 . S TAG=""
 . F  S TAG=$O(@GL@(9,PKG,RTN,TAG)) Q:TAG=""  D NDXTAG(GL,PKG,RTN,TAG,.RESULT)
 S RTN=""
 F  S RTN=$O(@GL@(13,PKG,RTN)) Q:RTN=""  D
 . S TAG=""
 . F  S TAG=$O(@GL@(13,PKG,RTN,TAG)) Q:TAG=""  D NDXTAG(GL,PKG,RTN,TAG,.RESULT)
 S RTN=""
 F  S RTN=$O(@GL@(14,PKG,RTN)) Q:RTN=""  D
 . S TAG=""
 . F  S TAG=$O(@GL@(14,PKG,RTN,TAG)) Q:TAG=""  D NDXTAG(GL,PKG,RTN,TAG,.RESULT)
 Q
 ;
NDXTAG(GL,PKG,RTN,TAG,RESULT)
 K RESULT
 I $E(TAG,1,2)="$$" S ACTTAG=$E(TAG,3,$L(TAG)) I 1
 E  S ACTTAG=TAG
 D NDXLOCAL(RTN,ACTTAG,GL,.RESULT)
 D NDXFMAN(GL,RTN,ACTTAG,.RESULT,0)
 D TRANS2GL(GL,PKG,RTN,TAG,"I",.RESULT)
 D TRANS2GL(GL,PKG,RTN,TAG,"O",.RESULT)
 S CMD=""
 F  S CMD=$O(RESULT("CMD",CMD)) Q:CMD=""  D
 . S @GL@(7,PKG,RTN,TAG,"CMD",CMD)=RESULT("CMD",CMD)
 M:$D(RESULT("F")) @GL@(7,PKG,RTN,TAG,"F")=RESULT("F")
 M:$D(RESULT("G")) @GL@(7,PKG,RTN,TAG,"G")=RESULT("G")
 M:$D(RESULT("GL")) @GL@(7,PKG,RTN,TAG,"GL")=RESULT("GL")
 M:$D(RESULT("FMG")) @GL@(7,PKG,RTN,TAG,"FMG")=RESULT("FMG")
 Q
 ;
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
 . . . . I $$GETPKG(IRTN,"",GL)="DI",VAL'="" D
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
NDXIN(GL,CLEAN) ;
 N PKG
 K:CLEAN @GL@(7)
 K @GL@(15)
 S PKG=""
 F  S PKG=$O(@GL@(9,PKG)) Q:PKG=""  D
 . S @GL@(15,PKG)=""
 S PKG=""
 F  S PKG=$O(@GL@(13,PKG)) Q:PKG=""  D
 . S @GL@(15,PKG)=""
 S PKG=""
 F  S PKG=$O(@GL@(14,PKG)) Q:PKG=""  D
 . S @GL@(15,PKG)=""
 S PKG=""
 F  S PKG=$O(@GL@(15,PKG)) Q:PKG=""  D
 . D NDXPKGIN(GL,PKG,0)
 Q
 ;
REPORTGL(TITLE,GL,ADDVALUE) ;
 N X,OUT,EXISTS,SPC
 S X="",OUT=1,SPC=""
 F I=1:1:$L(TITLE) S SPC=SPC_" " 
 S OUT(1)=TITLE
 F  S X=$O(@GL@(X)) Q:X=""  D
 . S:ADDVALUE X=X_@GL@(X)
 . I $L(OUT(OUT))+$L(X)>75 D
 . . S OUT=OUT+1
 . . S OUT(OUT)=SPC 
 . S OUT(OUT)=OUT(OUT)_X_","
 S:$L(OUT(1))=$L(TITLE) OUT=0
 D REPORT(TITLE,.OUT)
 Q
 ;
REPORTCMD(TITLE,GL,CMD) ;
 N X
 S X=+$G(@GL@(CMD))
 W !,TITLE,X
 Q
 ;
REPORT(TITLE,OUT) ;
 N I
 I 'OUT W !,TITLE,"--" Q
 F I=1:1:OUT W !,$E(OUT(I),1,$L(OUT(I))-1)
 Q
 ;
REPORTSR(GL,TITLE,GLB) ; 
 N X,OUT,SPC,NAME
 S X="",OUT=1,SPC=""
 F I=1:1:$L(TITLE) S SPC=SPC_" " 
 S OUT(1)=TITLE
 F  S X=$O(@GL@(X)) Q:X=""  D
 . S NAME=$$PKGNAME(X,GLB) 
 . I $L(OUT(OUT))+$L(NAME)>75 D
 . . S OUT=OUT+1
 . . S OUT(OUT)=SPC 
 . S OUT(OUT)=OUT(OUT)_NAME_","
 S:$L(OUT(1))=$L(TITLE) OUT=0
 D REPORT(TITLE,.OUT)
 Q 
 ;
WPKGAPI(GL,PKG,I,RTN,TAG) ;
 N P,SRCS
 S TAG=$G(TAG)
 I RTN="" D  Q
 . F  S RTN=$O(@GL@(7,PKG,RTN)) Q:RTN=""  D
 . . S TAG=""
 . . F  S TAG=$O(@GL@(7,PKG,RTN,TAG)) Q:TAG=""  D WRTNAPI(GL,PKG,RTN,TAG)
 I TAG'="" D WRTNAPI(GL,PKG,RTN,TAG) Q
 F  S TAG=$O(@GL@(7,PKG,RTN,TAG)) Q:TAG=""  D WRTNAPI(GL,PKG,RTN,TAG)
 Q
 ;
WRTNAPI(GL,PKG,RTN,TAG)
 W !!," "_TAG_"^"_RTN
 D REPORTSR(GL_"(9,"""_PKG_""","""_RTN_""","""_TAG_""")","  CALLING PACKAGES : ",GL)
 D REPORTRP(TAG_"^"_RTN,"  CALLING RPC's    : ")
 D REPOROPT(TAG_"^"_RTN,"  CALLING OPTIONS  : ")
 D REPORTGL("    FORMAL: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""F"")",0)
 D REPORTGL("     INPUT: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""I"")",1)
 D REPORTGL("    OUTPUT: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""O"")",1)
 D REPORTGL("      GLBS: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""G"")",0)
 D REPGLPKG("  GLS MDEP: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""G"")",PKG,GL)
 D REPORTGL("   FM GLBS: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""FMG"")",0)
 D REPGLPKG(" FM G MDEP: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""FMG"")",PKG,GL)
 D REPORTCMD("      READ: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""CMD"")","R")
 D REPORTCMD("     WRITE: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""CMD"")","W")
 W !
 D REPORTGL("  PKG GLBS: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""GL"")",0)
 D REPORTCMD("  PKG EXEC: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""CMD"")","X")
 D REPORTCMD("   PKG IND: ",GL_"(7,"""_PKG_""","""_RTN_""","""_TAG_""",""CMD"")","@")
 Q
 ;
WAPI(GL) ; 
 N PKG,I,NAME,RTN,TAG
 S PKG="",I=0,RTN="",TAG=""
 R !,"ROUTINE TO BE INDEXED //All: ",RTN
 R:RTN'="" !,"TAG TO BE INDEXED //All: ",TAG
 I RTN="" D  Q
 . F  S PKG=$O(@GL@(4,PKG)) Q:PKG=""  S I=I+1 D 
 . . W !,"--------------------------------------------------------------",!
 . . S NAME=$$PKGNAME(PKG,GL)
 . . I @GL@(4,PKG)>40 W !,I,". COMMON SERVICE NAME: "_NAME I 1
 . . E  W !,I,". PACKAGE NAME: "_NAME
 . . I '$D(@GL@(7,PKG)) W !!,"Not used by other packages",! I 1
 . . E  D RPCETAG^ZZRGND16(PKG,GL),WPKGAPI(GL,PKG,I,"","")
 . . W !,"--------------------------------------------------------------",!
 . W !
 S PKG=$$GETPKG(RTN,"",GL)
 D WPKGAPI(GL,PKG,1,RTN,TAG)
 Q
 ;
REPORTAPI(GLB) ;
 S:$G(GLB)="" GLB="^ZZRG"
 D READPKGS(GLB)
 D RGDEP(GLB)
 D WAPI(GLB)
 Q
 ;
WPKGAPIO(GL,SPKG,I) ;
 N RTN,TAG,P,SRCS,PKG,ATAG
 S PKG=""
 F  S PKG=$O(@GL@(8,SPKG,PKG)) Q:PKG=""  D
 . S RTN=""
 . F  S RTN=$O(@GL@(8,SPKG,PKG,RTN)) Q:RTN=""  D
 . . S ATAG=""
 . . F  S ATAG=$O(@GL@(8,SPKG,PKG,RTN,ATAG)) Q:ATAG=""  D
 . . . W !," "_ATAG_"^"_RTN_" ("_$$PKGNAME(PKG,GL)_")"
 Q
 ;
WAPIO(GL) ; 
 N PKG,I,NAME,RTN,TAG
 S PKG="",I=0,RTN=""
 F  S PKG=$O(@GL@(8,PKG)) Q:PKG=""  S I=I+1 D 
 . W !,"--------------------------------------------------------------",!
 . S NAME=$$PKGNAME(PKG,GL)
 . I @GL@(4,PKG)>40 W !,I,". COMMON SERVICE NAME: "_NAME,! I 1
 . E  W !,I,". PACKAGE NAME: "_NAME,!
 . D WPKGAPIO(GL,PKG,I)
 . W !,"--------------------------------------------------------------",!
 W !
 Q
 ;
REPORTAPIO(GLB) ;
 S:$G(GLB)="" GLB="^ZZRG"
 D READPKGS(GLB)
 D WAPIO(GLB)
 Q
 ;
REPORTRP(RTN,TITLE)
 N X,OUT,SPC,NAME,PROC,RPC
 S X="",OUT=1,SPC=""
 F I=1:1:$L(TITLE) S SPC=SPC_" " 
 S OUT(1)=TITLE
 F  S X=$O(^XWB(8994,X)) Q:X=""  D
 . Q:X=0  Q:$D(^XWB(8994,X,0))<1  Q:^XWB(8994,X,0)'["^"
 . S PROC=^XWB(8994,X,0),RPC=$P(PROC,"^",2)_"^"_$P(PROC,"^",3)
 . I RTN'[RPC Q
 . S NAME=$P(^XWB(8994,X,0),"^",1) 
 . I $L(OUT(OUT))+$L(NAME)>75 D
 . . S OUT=OUT+1
 . . S OUT(OUT)=SPC 
 . S OUT(OUT)=OUT(OUT)_NAME_","
 S:$L(OUT(1))=$L(TITLE) OUT=0
 D REPORT(TITLE,.OUT)
 Q
 ;
REPOROPT(RTN,TITLE)
 N X,OUT,SPC,NAME,PROC,OPTION,OPTSE,OPTDO,OPTEX
 S X="",OUT=1,SPC="",OPTSE="^M^E^P^I^S^X^C^B^",OPTDO="^A^O^",OPTEX="^R^"
 F I=1:1:$L(TITLE) S SPC=SPC_" " 
 S OUT(1)=TITLE
 Q:$D(^DIC(19,0))'>0  S LENGTH=$P(^DIC(19,0),"^",3)
 F  S X=$I(X) Q:X>LENGTH  D:$D(^DIC(19,X,0))>0
 . S OPTION=^DIC(19,X,0)
 . I OPTSE[$P(OPTION,"^",4) Q
 . S NAME=$P(OPTION,"^",1)
 . I OPTDO[$P(OPTION,"^",4) I $S($D(^DIC(19,X,20)):^(20),1:"")'[RTN Q
 . I OPTEX[$P(OPTION,"^",4) I $S($D(^DIC(19,X,25)):^(25),1:"")'[RTN Q
 . I $L(OUT(OUT))+$L(NAME)>75 D
 . . S OUT=OUT+1
 . . S OUT(OUT)=SPC 
 . S OUT(OUT)=OUT(OUT)_NAME_","
 S:$L(OUT(1))=$L(TITLE) OUT=0
 D REPORT(TITLE,.OUT)
 Q
 ;
REPORTRPC(GLB)
 S:$G(GLB)="" GLB="^ZZRG"
 D WRPC(GLB)
 Q
 ;	
WRPC(GL)
 N IDX,IDX2,IDXP,LEVL1,LEVL2,LEVL3,PROC,RVTYPES,PARTYPES
 S IDX="",LEVL1=" ",LEVL2="  ",LEVL3="   "
 S RVTYPES="SINGLE VALUE^ARRAY^WORD PROCESSING^GLOBAL ARRAY^GLOBAL INSTANCE"
 S PARTYPES="LITERAL^LIST^WORD PROCESSING^REFERENCE"
 F  S IDX=$O(^XWB(8994,IDX)) Q:IDX=""  D
 . Q:IDX=0  Q:$D(^XWB(8994,IDX,0))<1 
 . S PROC=^XWB(8994,IDX,0)
 . W !,$P(PROC,"^",1)_" - "_$P(PROC,"^",2)_"^"_$P(PROC,"^",3)
 . S IDX2="" ;
 . F  S IDX2=$O(^XWB(8994,IDX,1,IDX2)) Q:IDX2=""  D
 . . I $D(^XWB(8994,IDX,1,IDX2,0))'<1 W !,LEVL1_$G(^XWB(8994,IDX,1,IDX2,0)) 
 . W !,LEVL1,"PARAMETERS"
 . S IDXP=""
 . F  S IDXP=$O(^XWB(8994,IDX,2,IDXP)) Q:IDXP=""  D
 . . I $D(^XWB(8994,IDX,2,IDXP,0))'<1 D
 . . . W !,LEVL2,$P(^XWB(8994,IDX,2,IDXP,0),"^",1) 
 . . . W !,LEVL3,"TYPE:",$P(PARTYPES,"^",+$P(^XWB(8994,IDX,2,IDXP,0),"^",2))
 . . . W:$D(^XWB(8994,IDX,2,IDXP,1,1,0))>0 !,LEVL3,$P(^XWB(8994,IDX,2,IDXP,1,1,0),"^",1)
 . W !,LEVL1,"RETURN VALUE"
 . S RVTYPE=$P(PROC,"^",4)
 . W !,LEVL2,"TYPE: ",$P(RVTYPES,"^",+$P(PROC,"^",4))
 . I $D(^XWB(8994,IDX,3,0))'<1 D
 . . W:$D(^XWB(8994,IDX,3,1,0))>0 !,LEVL2,$G(^XWB(8994,IDX,3,1,0))
 . D:$P(PROC,"^",3)'="" RPCRTN($P(PROC,"^",3),$P(PROC,"^",2),GL)
 . W !
 Q
 ;
RPCRTN(RTN,PROC,GL)
 N X,Y,LEVL1,LEVL2,SEP,TMP
 S LEVL1=" ",LEVL2="  ",SEP="",TMP=""
 Q:RTN=""  Q:PROC="" 
 S X=GL_"(1,"""_RTN_""",""X"")"
 S Y=GL_"(1,"""_RTN_""",""X"""
 W !,LEVL1_"EXTERNAL ROUTINES:",!,LEVL2
 F  Q:X=""  D
 . S X=$Q(@X) 
 . I X'[Y S X="" Q
 . I @X[PROC D
 . . S TMP=$E($P(X,",",4),2,$L($P(X,",",4))-1)
 . . W SEP_$P(TMP," ",2)_"^"_$P(TMP," ",1) S SEP="," 
 Q
 ;
GETPKG(RTN,P,GL) ;
 N I,PREFIX,PKG,STOP,FILE
 S PKG=""
 S STOP=0
 I $D(@GL@(10))=0 D READPKGS(GL)
 F I=1:1 Q:$L(RTN)<I  D  Q:PKG]""&STOP
 . S PREFIX=$E(RTN,1,I)
 . I $D(@GL@(10,PREFIX)) S PKG=PREFIX,P=PREFIX I 1
 . E  S:PKG]"" STOP=1
 Q:PKG]"" PKG
 S STOP=0
 I $E(RTN)="%" S PKG="XU",P="%"
 S:PKG="" PKG="UNKNOWN",P="UNKNOWN"
 Q PKG
 ; 
PKGNAME(PKG,GL) ;
 Q:PKG="UNKNOWN" PKG
 N NAME
 I $D(@GL@(10,PKG))'>0 S PKG="UNKNOWN" Q PKG
 S NAME=@GL@(10,PKG)
 Q NAME
 ;
ANS()
 N FILE
 S FILE=""
 W !,"Enter Packages.csv path: "
 R FILE Q FILE
 ;
READPKGS(GL) 
 N OLDIO,LINE,LASTPN,LASTPRFX,EOF,FILE
 S OLDIO=$IO,EOF=0
 I $D(@GL@(10))>0 Q
 S FILE=$$ANS()
 S $ET="S EOF=$$EOF1END(FILE,OLDIO)"
 O FILE:"@ET":5 U FILE
 S @GL@(10)=""
 F  Q:EOF  D
 . R LINE
 . S:$P(LINE,",",1)'="" LASTPN=$P(LINE,",",1) 
 . S:$P(LINE,",",3)'="" LASTPRFX=$P(LINE,",",3) 
 . S @GL@(10,LASTPRFX)=LASTPN
 Q
 ;
EOF1END(FILE,OLDIO)
 C FILE
 I $ZE["<ENDOFFILE>" S $EC=""
 I $ZE["<NOTOPEN>" S $EC="" W !,"Path not found!"
 U OLDIO
 Q 1
 ;
REPOPTAG
 N IDX,X,LENGTH,OPTION
 S IDX=0
 Q:$D(^DIC(19,0))'>0  S LENGTH=$P(^DIC(19,0),"^",3)
 F  S IDX=$I(IDX) Q:IDX>LENGTH  D:$D(^DIC(19,IDX,0))>0
 . S OPTION=^DIC(19,IDX,0),X=""
 . I $P(OPTION,"^",4)="M" Q
 . W !,$P(OPTION,"^",2)
 . I $P(OPTION,"^",4)="A" S X="Action: "_$S($D(^DIC(19,IDX,20)):^(20),1:"") W !?3,X,! Q
 . I $P(OPTION,"^",4)="R" S X="Run routine: "_$S($D(^DIC(19,IDX,25)):^(25),1:"") W !?3,X,! Q
 . I $P(OPTION,"^",4)="E" S X="Edit file: "_$S($D(^DIC(19,IDX,50)):^(50),1:"") W !?3,X,! Q
 . I $P(OPTION,"^",4)="P" S X="Print file: "_$S($D(^DIC(19,IDX,60)):^(60),1:"") W !?3,X,! Q
 . I X="" W !
 Q
 ;
NDXOPT(GL)
 N PKG,RTN,TAG,OPT,OPTNAME,CMD,I
 F I=1:1:+$P(^DIC(19,0),"^",3) D:$D(^DIC(19,I,0))
 . S OPT=^DIC(19,I,0)
 . Q:$P(OPT,"^",4)'="R"  Q:'$D(^DIC(19,I,25))
 . S OPTNAME=$P(OPT,"^",1)
 . S RTN=$P(^DIC(19,I,25),"^",2)
 . Q:$G(RTN)=""
 . S TAG=$P(^DIC(19,I,25),"^",1)
 . S PKG=$$GETPKG(RTN,"",GL)
 . S @GL@(13,PKG,RTN,TAG,OPTNAME)=""  
 Q
 ;
NDXRPC(GL)
 N I,ENTRY,RPC,PKG,RTN,TAG
 S I=""
 F  S I=$O(^XWB(8994,I)) Q:I=""  D
 . Q:+I=0
 . S ENTRY=^XWB(8994,I,0)
 . S RPC=$P(ENTRY,"^",1)
 . S RTN=$P(ENTRY,"^",3)
 . Q:RTN=""
 . S TAG=$P(ENTRY,"^",2)
 . S PKG=$$GETPKG(RTN,"",GL)
 . S @GL@(14,PKG,RTN,TAG)=I
 . S @GL@(14,PKG,RTN,TAG,RPC)=""
 Q
 ;
GMFLDS(GL)
 N X,X1,FIELD,GLB,FX
 S X="",X1=""
 F  S X=$O(^DIC(X)) Q:X=""  D
 . S X1="" S:$D(^DIC(X,0,"GL")) GLB=^DIC(X,0,"GL") Q:X=0  
 . S FX=0
 . F  S X1=$O(^DD(X,X1)) Q:X1=""  D
 . . Q:$D(^DD(X,X1,0))=10  Q:$D(^DD(X,X1,0))=0  
 . . S FIELD=^DD(X,X1,0) Q:FIELD="" 
 . . I $P(FIELD,"^",2)["K" D
 . . . S @GL@(11,$P(GLB,"(",1),GLB,FX)=X1,FX=$I(FX) 
 Q
 ;
EXMCODE(GL)
 N X,X1,FLDS,FX,I,RTN,IR
 S X="",IR=0
 F  S X=$O(@GL@(11,X)) Q:X=""  D
 . K FLDS S X1="",FLDS=""
 . F  S X1=$O(@GL@(11,X,X1)) Q:X1=""  D
 . . S FX="",I=0
 . . F  S FX=$O(@GL@(11,X,X1,FX)) Q:FX=""  D
 . . . S FLDS(I)=$G(@GL@(11,X,X1,FX)),I=$I(I)
 . . S RTN=$P(X1,"(",1) S:RTN["%" RTN="^"_$P(RTN,"%",2)
 . . S IR=$I(IR),RTN=RTN_"^"_IR,@GL@(11,X,X1,"RTN")=RTN
 . . D PROCLNS(X1,.FLDS,RTN)
 . . D WRTDEP(GL,X,X1,RTN)
 Q
 ;
WRTDEP(GL,GLB,SGLB,RTN)
 N X,I,RTNAME,PKG,PKGNAME
 S X="",I=0
 S:$D(@GL@(11,GLB,"RTNS",0))>0 I=@GL@(11,GLB,"RTNS",0)
 F  S X=$O(^UTILITY($J,1,RTN,"X",X)) Q:X=""  D
 . S I=$I(I),@GL@(11,GLB,"RTNS",I)=X
 . S @GL@(11,GLB,"RTNS",0)=I,RTNAME=$P(X," ",1)
 . S PKG=$$GETPKG(RTNAME,"",GL),PKGNAME=$$PKGNAME(PKG,GL)
 . S @GL@(11,GLB,"PKGS",PKG)=PKGNAME S:$D(@GL@(11,GLB,SGLB,"RTNS",0))=0 @GL@(11,GLB,SGLB,"RTNS",0)=0
 . S @GL@(11,GLB,SGLB,"RTNS",0)=$I(@GL@(11,GLB,SGLB,"RTNS",0))
 . S @GL@(11,GLB,SGLB,"PKGS",PKG)=PKGNAME
 Q
 ;
PROCLNS(GLB,FLDS,RTN) ;
 N X,X1,F,LIN,LNS,GLBNAME
 S X="",LNS="" Q:$D(GLB)=0
 D BUILD^ZZRGND7
 S (IND("DO"),IND("SZT"),IND("SZC"),LABO)=0,INDLC=0,Q=$C(34),U=""
 F  S X=$O(@(GLB_""""_X_""")")) Q:X=""  D
 . S X1="",LC=0
 . F  S X1=$O(FLDS(X1)) Q:X1=""  D
 . . S F=FLDS(X1) Q:$D(@(GLB_""""_X_""","""_F_""")"))=10
 . . Q:$D(@(GLB_""""_X_""","""_F_""")"))=0 
 . . S LIN=@(GLB_""""_X_""","""_F_""")") Q:$L(LIN)<2!LIN=""
 . . I LC=0 D RTNHEAD(RTN) S LC=3
 . . S LC=$I(LC),^UTILITY($J,1,RTN,0,LC,0)=" "_LIN
 . I LC>0  D
 . . S ^UTILITY($J,1,RTN,0,0)=LC,^UTILITY($J,1,RTN,1,0)=GLB
 . . D PRCSSLNS^ZZRGNDX(RTN)
 Q 
 ;
RTNHEAD(GL)
 S ^UTILITY($J,1,GL,0,1,0)=GL_" ;"
 S ^UTILITY($J,1,GL,0,2,0)=" ;;"
 S ^UTILITY($J,1,GL,0,3,0)=GL_"1 ;"
 Q 
 ;
RGDEP(GL)
 K @GL@("11")
 D GMFLDS(GL)
 D EXMCODE(GL)
 K ^UTILITY($J)
 Q
 ;
REPGLPKG(TITLE,GLBS,PKG,GL)
 N X,X1,OUT,SPC,NAME,OUTN,RTN,TAG,TMPGLB
 S X="",X1="",OUT=1,SPC=""
 F I=1:1:$L(TITLE) S SPC=SPC_" " 
 S OUT(1)=TITLE
 F  S X1=$O(@GLBS@(X1)) Q:X1=""  D
 . S RTN=$P(GLBS,",",3),RTN=$E(RTN,2,$L(RTN)-1)
 . S TAG=$P(GLBS,",",4),TAG=$E(TAG,2,$L(TAG)-1)
 . D FNDMCODE(RTN,TAG,GL,.MGL) S X2=""
 . F  S X2=$O(MGL(X2)) Q:X2=""  D
 . . I X2["""" S X2=$P(X2,"""",1)_$P(X2,"""",2)
 . . Q:X2'[X1
 . . I $L($P(X2,"(",2))'>0 D
 . . . S TMPGLB=GL_"(11,"""_X1_""",""PKGS"")"
 . . E  S TMPGLB=GL_"(11,"""_X1_""","""_X2_","",""PKGS"")"
 . . F  S X=$O(@TMPGLB@(X)) Q:X=""  D
 . . . Q:PKG=X  S NAME=$$PKGNAME(X,GL)  Q:$D(OUTN(NAME))>0
 . . . I $L(OUT(OUT))+$L(NAME)>75 D
 . . . . S OUT=OUT+1
 . . . . S OUT(OUT)=SPC 
 . . . S OUT(OUT)=OUT(OUT)_NAME_","
 . . . S OUTN(NAME)=""
 S:$L(OUT(1))=$L(TITLE) OUT=0
 D REPORT(TITLE,.OUT)
 Q
 ;
FNDMCODE(RTN,TAG,GL,MGL)
 N X,X1,XCMD,TCMD
 S X="",MGL=""
 F  S X=$O(@GL@(2,RTN,TAG,X)) Q:X=""  D
 . S X1="",XCMD=""
 . F  S X1=$O(@GL@(2,RTN,TAG,X,X1)) Q:'X1  D
 . . S TCMD=@GL@(2,RTN,TAG,X,X1)
 . . I ($P(TCMD,$C(9),1)="G")&(XCMD="X") D
 . . . S MGL($P(TCMD,$C(9),2))="" 
 . . . I ($P(TCMD,$C(9),2)'["^DD")&($P(TCMD,$C(9),2)'["ZOSF") D
 . . . . W !,TAG_"^"_RTN_" - "_$P(TCMD,$C(9),2)
 . . I TCMD["XECUTE" S XCMD="X" 
 . . E  S XCMD="" 
 Q
 ;