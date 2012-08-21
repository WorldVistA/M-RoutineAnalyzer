ZZRGND15 ;;CBR/AU -  - REPORTS PACKAGE DEPENDENCIES INTRODUCED BY GLOBALS USED ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
 Q ; Use tags
 ;
 ; Print directly used globals by a package and their owner
 ; Assumes @GLB@(1 - created in CRTBASE^ZZRGND14
USES(GLB,PKG,OWNPATH,FILEPATH,FILENAME)
 I $G(PKG)="" W "Specify a package",! Q
 S:$G(GLB)="" GLB="^ZZRG"
 D:$G(OWNPATH)]"" RDOWNER^ZZRGND19(GLB,1,OWNPATH)
 I $G(FILENAME)]"",$G(FILEPATH)]"" D TEXTOPEN^ZZRGND23("OUTFILE",FILEPATH,FILENAME)
 N GPKG,SUB,RTN
 W "Directly Used Globals By ",$$GPKGNAME^ZZRGND19(PKG,GLB),!!
 S RTN=""
 F  S RTN=$O(@GLB@(1,RTN)) Q:RTN=""  D
 . Q:$$GETPKG^ZZRGND19(RTN,GLB)'=PKG
 . S G=""
 . F  S G=$O(@GLB@(1,RTN,"G",G)) Q:G=""  D USESGLB(GLB,PKG,RTN,G)
 W ! 
 I $G(FILENAME)]"",$G(FILEPATH)]"" D TEXTCLS^ZZRGND23("OUTFILE",FILENAME)
 Q
 ;
USESGLB(GL,PKG,RTN,G)
 N GINFO,SUB,TAG,UPKG
 Q:$E(G,1,4)="^TMP"
 Q:$E(G,1,5)="^XTMP"
 Q:$E(G,1,8)="^UTILITY"
 Q:$E(G,1,6)="^%ZOSF"
 S GINFO=$$GLBINFO(GL,G)
 W "Routine ",RTN,": ",G," "
 S UPKG=$P(GINFO,"^",1)
 I UPKG="" W "(Dependency Unknown)" I 1 
 E  W:PKG'=UPKG "(Dependency to ",$P(GINFO,"^",2),", ",$P(GINFO,"^",3)," File)"
 W !
 Q
 ;
 ; Prints packages that use globals owned by PKG
 ; Assumes @GLB@(1 - created in CRTBASE^ZZRGND14
USED(GLB,PKG,OWNPATH,FILEPATH,FILENAME)
 I $G(PKG)="" W "Specify a package",! Q
 N GINFO
 S:$G(GLB)="" GLB="^ZZRG"
 D:$G(OWNPATH)]"" RDOWNER^ZZRGND19(GLB,1,OWNPATH)
 I $G(FILENAME)]"",$G(FILEPATH)]"" D TEXTOPEN^ZZRGND23("OUTFILE",FILEPATH,FILENAME)
 S RTN="",CNT=0
 F  S RTN=$O(@GLB@(1,RTN)) Q:RTN=""  D
 . Q:$E(RTN,1,2)="ZZ"
 . Q:$$GETPKG^ZZRGND19(RTN,GLB)=PKG
 . S G=""
 . F  S G=$O(@GLB@(1,RTN,"G",G)) Q:G=""  D
 . . S GINFO=$$GLBINFO(GLB,G)
 . . Q:$P(GINFO,"^",1)'=PKG
 . . W "Routine ",RTN,": ",G," (",$$GPKGNAME^ZZRGND19($$GETPKG^ZZRGND19(RTN,GLB),GLB),")",!
 I $G(FILENAME)]"",$G(FILEPATH)]"" D TEXTCLS^ZZRGND23("OUTFILE",FILENAME)
 Q
 ;
GLBINFO(GL,G)
 N GINFO,GLB,SUB
 S GLB=$P(G,"(",1)
 S SUB=$P($P(G,"(",2),",",1)
 S GINFO=""
 S:SUB'="" GINFO=$G(@GL@(11,GLB,SUB))
 S:GINFO="" GINFO=$G(@GL@(11,GLB))
 Q GINFO
 ;
