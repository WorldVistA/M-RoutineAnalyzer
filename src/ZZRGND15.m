ZZRGND15 ;CBR/ - REPORTS PACKAGE DEPENDENCIES INTRODUCED BY GLOBALS USED ;02/01/12
 ;;7.3;TOOLKIT;**to be determined**;
 Q ; Use tags
 ;
USES(GLB,PKG)  ; Prints globals used by a package and their owner
 S:$G(GLB)="" GLB="^ZZRG"
 N GPKG,SUB,RTN
 D RDOWNER^ZZRGND19(GLB,0)
 W !,"Directly Used Globals By ",$$GPKGNAME^ZZRGND19(PKG,GLB),!
 S RTN=""
 F  S RTN=$O(@GLB@(1,RTN)) Q:RTN=""  D
 . Q:$$GETPKG^ZZRGND19(RTN,GLB)'=PKG
 . S G=""
 . F  S G=$O(@GLB@(1,RTN,"G",G)) Q:G=""  D USESGLB(GLB,PKG,RTN,G) 
 W !!,"Globals used through FileMan calls by ",$$GPKGNAME^ZZRGND19(PKG,GLB),!
 S RTN=""
 F  S RTN=$O(@GLB@(7,PKG,RTN)) Q:RTN=""  D
 . S TAG=""
 . F  S TAG=$O(@GLB@(7,PKG,RTN,TAG)) Q:TAG=""  D
 . . Q:'$D(@GLB@(7,PKG,RTN,TAG,"FMG"))
 . . S G=""
 . . F  S G=$O(@GLB@(7,PKG,RTN,TAG,"FMG",G)) Q:G=""  D USESGLB(GLB,PKG,RTN,G)
 Q
 ;
USED(GLB,PKG) ; Prints packages that use globals owned by PKG
 N GINFO
 S:$G(GLB)="" GLB="^ZZRG"
 S RTN="",CNT=0
 F  S RTN=$O(@GLB@(1,RTN)) Q:RTN=""  D
 . Q:$$GETPKG^ZZRGND19(RTN,GLB)=PKG
 . S G=""
 . F  S G=$O(@GLB@(1,RTN,"G",G)) Q:G=""  D
 . . S GINFO=$$GLBINFO(GLB,G)
 . . Q:$P(GINFO,"^",1)'=PKG
 . . W !,"Routine ",RTN,": ",G," (",$$GPKGNAME^ZZRGND19($$GETPKG^ZZRGND19(RTN,GLB),GLB),")"
 Q
 ;
USESGLB(GL,PKG,RTN,G)
 N GINFO,SUB,TAG
 Q:$E(G,1,4)="^TMP"
 Q:$E(G,1,8)="^UTILITY"
 Q:$E(G,1,6)="^%ZOSF"
 S GINFO=$$GLBINFO(GL,G)
 W !,"Routine ",RTN,": ",G," "
 W:PKG'=$P(GINFO,"^",1) "(Dependency to ",$P(GINFO,"^",2),", ",$P(GINFO,"^",3)," File)"
 Q
 ;
GLBINFO(GL,G)
 N GINFO,GLB,SUB
 S GLB=$P(G,"(",1)
 S SUB=$P($P(G,"(",2),",",1)
 S GINFO=""
 S:SUB'="" GINFO=$G(@GL@(20,GLB,SUB))
 S:GINFO="" GINFO=$G(@GL@(20,GLB))
 Q GINFO
 ;
