ZZRGND15
RDOWNER(GL,CLEAN) 
 N OLDIO,LINE,LASTPN,LASTPRFX,EOF,FILE
 N FLDS,FN,NAME,GLB,NS,PKG,SUB,PKG
 S OLDIO=$IO,EOF=0
 K:CLEAN @GL@(20)
 I $D(@GL@(20))>0 Q
 R !,"Enter Ownership.csv path: ",FILE
 S $ET="S EOF=$$EOF1END(FILE,OLDIO)"
 O FILE:"@ET":5 U FILE
 F  Q:EOF  D
 . R LINE
 . D GETFLDS(LINE,.FLDS)
 . S GLB=$$LTRIM($P(FLDS(3),"(",1)," ")
 . S SUB=$P($P(FLDS(3),"(",2),",",1)
 . S FN=FLDS(1)
 . S NAME=FLDS(2)
 . S NS=FLDS(4)
 . S PKG=FLDS(5)
 . S @GL@(20,GLB)=NS_"^"_PKG_"^"_NAME_"^"_FN
 . S:SUB'="" @GL@(20,GLB,SUB)=NS_"^"_PKG_"^"_NAME_"^"_FN
 Q
 ;
EOF1END(FILE,OLDIO)
 C FILE
 I $ZE["<ENDOFFILE>" S $EC=""
 I $ZE["<NOTOPEN>" S $EC="" W !,"Path not found!"
 U OLDIO
 Q 1
 ;
GETFLDS(LINE,FIELDS)
 N QUOTE,CNT,STOP,VAL
 K FIELDS
 S QUOTE=0,CNT=0,STOP=0,VAL=""
 F I=1:1:$L(LINE) D
 . S CH=$E(LINE,I)
 . I CH=",",'QUOTE D  Q
 . . S CNT=CNT+1
 . . S:$E(VAL,$L(VAL))="""" VAL=$E(VAL,1,$L(VAL)-1)
 . . S FIELDS=CNT
 . . S FIELDS(CNT)=VAL
 . . S VAL=""
 . I CH="""" S QUOTE='QUOTE
 . Q:QUOTE&(CH="""")
 . S VAL=VAL_CH
 S:$E(VAL,$L(VAL))="""" VAL=$E(VAL,1,$L(VAL)-1)
 Q
 ;
USES(GLB,PKG)  ;
 S:$G(GLB)="" GLB="^ZZRG"
 N GPKG,SUB,RTN
 D RDOWNER(GLB,0)
 W !,"Directly Used Globals By ",$$GPKGNAME^ZZRGND19(PKG,GLB),!
 S RTN=""
 F  S RTN=$O(@GLB@(1,RTN)) Q:RTN=""  D
 . Q:$$GETPKG^ZZRGND19(RTN,GLB)'=PKG
 . S G=""
 . F  S G=$O(@GLB@(1,RTN,"G",G)) Q:G=""  D USESGLB(GLB,PKG,RTN,G) 
 W !,"Globals used through FileMan calls by ",$$GPKGNAME^ZZRGND19(PKG,GLB),!
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
LTRIM(STR,CH)
 N I,CX,TO
 S TO=0,CH=$E(CH)
 F I=1:1:$L(STR,CH) D
 . S CX=$E(STR,I)
 . Q:CX'=CH
 . S TO=I
 S:TO>0 STR=$E(STR,I,$L(STR))
 Q STR
 ;
USED(GL,PKG)  ;
 N GINFO
 S RTN="",CNT=0
 F  S RTN=$O(@GL@(1,RTN)) Q:RTN=""  D
 . Q:$$GETPKG^ZZRGND19(RTN,GL)=PKG
 . S G=""
 . F  S G=$O(@GL@(1,RTN,"G",G)) Q:G=""  D
 . . S GINFO=$$GLBINFO(GL,G)
 . . Q:$P(GINFO,"^",1)'=PKG
 . . W !,"Routine ",RTN,": ",G, " (",$$GPKGNAME^ZZRGND19($$GETPKG^ZZRGND19(RTN,GL),GL),")"
 Q
 ;
