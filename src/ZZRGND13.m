ZZRGND13 ;CBR/ - REPORTS ;02/07/12
 ;;7.3;TOOLKIT;**to be determined**;
REPORTAPI(GLB) ;
 S:$G(GLB)="" GLB="^ZZRG"
 D READPKGS^ZZRGND19(GLB,0)
 D RGDEP(GLB)
 D WAPI(GLB)
 Q
 ;
REPORTAPIO(GLB) ;
 S:$G(GLB)="" GLB="^ZZRG"
 D READPKGS^ZZRGND19(GLB,0)
 D WAPIO(GLB)
 Q
 ;
REPORTRPC(GLB) ; RPC calls
 S:$G(GLB)="" GLB="^ZZRG"
 D WRPC(GLB)
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
 . I PKG]"",$$GETPKG^ZZRGND19(RTN)'=PKG Q 
 . S CNT=CNT+1
 . S ^UTILITY($J,RTN)=""
 S ^UTILITY($J,0)=CNT_";ROU",NRO=CNT
 D MAIN^ZZRGNDX(1)
 I TARGET]"" D
 . M @TARGET=^UTILITY($J)
 . K ^UTILITY($J) 
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
 . S NAME=$$GPKGNAME^ZZRGND19(X,GLB) 
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
 . . S NAME=$$GPKGNAME^ZZRGND19(PKG,GL)
 . . I @GL@(4,PKG)>40 W !,I,". COMMON SERVICE NAME: "_NAME I 1
 . . E  W !,I,". PACKAGE NAME: "_NAME
 . . I '$D(@GL@(7,PKG)) W !!,"Not used by other packages",! I 1
 . . E  D RPCETAG^ZZRGND16(PKG,GL),WPKGAPI(GL,PKG,I,"","")
 . . W !,"--------------------------------------------------------------",!
 . W !
 S PKG=$$GETPKG^ZZRGND19(RTN,GL)
 D WPKGAPI(GL,PKG,1,RTN,TAG)
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
 . . . W !," "_ATAG_"^"_RTN_" ("_$$GPKGNAME^ZZRGND19(PKG,GL)_")"
 Q
 ;
WAPIO(GL) ; 
 N PKG,I,NAME,RTN,TAG
 S PKG="",I=0,RTN=""
 F  S PKG=$O(@GL@(8,PKG)) Q:PKG=""  S I=I+1 D 
 . W !,"--------------------------------------------------------------",!
 . S NAME=$$GPKGNAME^ZZRGND19(PKG,GL)
 . I @GL@(4,PKG)>40 W !,I,". COMMON SERVICE NAME: "_NAME,! I 1
 . E  W !,I,". PACKAGE NAME: "_NAME,!
 . D WPKGAPIO(GL,PKG,I)
 . W !,"--------------------------------------------------------------",!
 W !
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
 . S PKG=$$GETPKG^ZZRGND19(RTNAME,GL),PKGNAME=$$GPKGNAME^ZZRGND19(PKG,GL)
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
 . . . Q:PKG=X  S NAME=$$GPKGNAME^ZZRGND19(X,GL)  Q:$D(OUTN(NAME))>0
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