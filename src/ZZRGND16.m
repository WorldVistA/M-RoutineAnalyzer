ZZRGND16 ;
 ;;
RPCETAG(PKG,GL)
 D RPCPATH(PKG,GL)
 D TT
 D TTX(GL)
 Q
 ;
UPDPATH(GLB,OTHRTN,OTHTAG,GL)
 N PKG,NXTRTN,NXTTAG,CNT,ID  
 S PKG=$$GETPKG^ZZRGND13(OTHRTN,"",GL)
 I GLB[(","""_OTHRTN_""","""_OTHTAG_"""") Q
 I $L(GLB)+$L(","""_OTHRTN_""","""_OTHTAG_""")")>1023 Q
 I $D(@GL@(14,PKG,OTHRTN,OTHTAG)) D  Q
 . S ID=@GL@(14,PKG,OTHRTN,OTHTAG)
 . S @GLB@(OTHRTN,OTHTAG)=ID
 S CNT=0
 S GLB=$E(GLB,1,$L(GLB)-1)
 S GLB=GLB_","""_OTHRTN_""","""_OTHTAG_""")"
 S NXTRTN=""
 F  S NXTRTN=$O(^AFSPATH(OTHRTN,OTHTAG,NXTRTN)) Q:NXTRTN=""  D
 . S NXTTAG=""
 . F  S NXTTAG=$O(^AFSPATH(OTHRTN,OTHTAG,NXTRTN,NXTTAG)) Q:NXTTAG=""  D
 . . S CNT=CNT+1
 . . D UPDPATH(GLB,NXTRTN,NXTTAG,GL)
 ;I 'CNT W !,"ERROR"
 Q
 ;
RPCPATH(PKG,GL)  ;
 N GMPLRTN,GMPLTAG,OTHRTN,OTHTAG,GLB
 K ^AFSALLPATH
 S GMPLRTN=""
 F  S GMPLRTN=$O(^AFSPATH(GMPLRTN)) Q:GMPLRTN=""  D
 . Q:$E(GMPLRTN,1,4)'=PKG
 . S GMPLTAG=""
 . F  S GMPLTAG=$O(^AFSPATH(GMPLRTN,GMPLTAG)) Q:GMPLTAG=""  D
 . . S OTHRTN=""
 . . F  S OTHRTN=$O(^AFSPATH(GMPLRTN,GMPLTAG,OTHRTN)) Q:OTHRTN=""  D
 . . . Q:$E(OTHRTN,1,4)=PKG
 . . . S GLB="^AFSALLPATH("""_GMPLRTN_""","""_GMPLTAG_""")"
 . . . S OTHTAG=""
 . . . F  S OTHTAG=$O(^AFSPATH(GMPLRTN,GMPLTAG,OTHRTN,OTHTAG)) Q:OTHTAG=""  D
 . . . . D UPDPATH(GLB,OTHRTN,OTHTAG,GL)
 Q 
 ;
TT  ;
 N RTN,TAG,ARTN,ATAG,CNT
 K ^AFSFPATH
 S CUR="^AFSALLPATH"
 F  S CUR=$Q(@CUR) Q:CUR=""  D
 . S CNT=$QL(CUR)
 . S RTN=$QS(CUR,1),ARTN=$QS(CUR,CNT-1)
 . S TAG=$QS(CUR,2),ATAG=$QS(CUR,CNT) 
 . S ^AFSFPATH(RTN,TAG,ARTN,ATAG)=@CUR
 Q
 ;
TTX(GL)  ;
 N RTN,TAG,ARTN,ATAG,CNT,ID
 S RTN=""
 S CNT=0
 W !!,"RPC Entry Tags",!
 F  S RTN=$O(^AFSFPATH(RTN)) Q:RTN=""  D
 . S TAG=""
 . F  S TAG=$O(^AFSFPATH(RTN,TAG)) Q:TAG=""  S CNT=CNT+1 D
 . . W !,CNT,". ",TAG,"^",RTN
 . . S ARTN=""
 . . F  S ARTN=$O(^AFSFPATH(RTN,TAG,ARTN)) Q:ARTN=""  D
 . . . S ATAG=""
 . . . F  S ATAG=$O(^AFSFPATH(RTN,TAG,ARTN,ATAG)) Q:ATAG=""  D
 . . . . S ID=^AFSFPATH(RTN,TAG,ARTN,ATAG)
 . . . . S XRTN=$P(^XWB(8994,ID,0),"^",3)
 . . . . S XTAG=$P(^XWB(8994,ID,0),"^",2)
 . . . . W:XRTN'=ARTN !,"ERROR"
 . . . . W:XTAG'=ATAG !,"ERROR"
 . . . . W !!,"RPC TAG: ",ATAG,"^",ARTN," (",ID,") (",$$PKGNAME^ZZRGND13($$GETPKG^ZZRGND13(ARTN,"",GL),GL),")",!
 . . . . F IDX=1:1 Q:'$D(^XWB(8994,ID,1,IDX))  W ^XWB(8994,ID,1,IDX,0)," "
 . . . . W !!
 Q
 ;