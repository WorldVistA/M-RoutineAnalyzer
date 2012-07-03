ZZRGND14 ;CBR/ - Used to extract and index data for the reports ;02/01/12
 ;;7.3;TOOLKIT;**to be determined**;
MAIN(GLB) ; Parses VistA codebase and extracts relevant information
 S:$G(GLB)="" GLB="^ZZRG"
 D CRTBASE(GLB)
 D NDXFI(GLB)
 D NDXOPT(GLB)
 D NDXRPC(GLB)
 D NDXIN(GLB,1)
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
CRTBASE(GLB) ; parses codebase & extracts relevant info
 N CNT,RTN,INP,NRO,DA,INDDA 
 S:$G(GLB)="" GLB="^ZZRG"
 K @GLB
 D NEWINP(.INP)
 S INP(10)=9.4,INP(1)=1,DA=-1,INDDA=DA
 S CNT=0,RTN=""
 K ^UTILITY($J)
 I '$D(^UTILITY($J)),$D(^%ZOSF("RSEL")) X ^("RSEL")
 S NRO=0,X=0 F I=0:0 S X=$O(^UTILITY($J,X)) Q:X=""  S NRO=NRO+1
 D MAIN^ZZRGNDX(1)
 M @GLB=^UTILITY($J)
 K ^UTILITY($J)
 Q
 ;
NDXFI(GL) ;
 N RTN,TAGS,TAG,PKG,T,TRTN,TTAG,TPKG,I,J
 S RTN=""
 F  S RTN=$O(@GL@(1,RTN)) Q:RTN=""  D 
 . S PKG=$$GETPKG^ZZRGND19(RTN,GL)
 . S:'$D(@GL@(4,PKG)) @GL@(4,PKG)=0
 . S T=""
 . F  S T=$O(@GL@(1,RTN,"X",T)) Q:T=""  D
 . . S TRTN=$P(T," ",1)
 . . S TTAG=$P(T," ",2)
 . . S:TTAG="" TTAG=" "
 . . S TPKG=$$GETPKG^ZZRGND19(TRTN,GL)
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
NDXOPT(GL) ; Indexes dependencies gathered from OPTION file
 N PKG,RTN,TAG,OPT,OPTNAME,CMD,I
 F I=1:1:+$P(^DIC(19,0),"^",3) D:$D(^DIC(19,I,0))
 . S OPT=^DIC(19,I,0)
 . Q:$P(OPT,"^",4)'="R"  Q:'$D(^DIC(19,I,25))
 . S OPTNAME=$P(OPT,"^",1)
 . S RTN=$P(^DIC(19,I,25),"^",2)
 . Q:$G(RTN)=""
 . S TAG=$P(^DIC(19,I,25),"^",1)
 . S PKG=$$GETPKG^ZZRGND19(RTN,GL)
 . S @GL@(13,PKG,RTN,TAG,OPTNAME)=""  
 Q
 ;
NDXRPC(GL) ; Indexes tags called by RPC Broker
 N I,ENTRY,RPC,PKG,RTN,TAG
 S I=""
 F  S I=$O(^XWB(8994,I)) Q:I=""  D
 . Q:+I=0
 . S ENTRY=^XWB(8994,I,0)
 . S RPC=$P(ENTRY,"^",1)
 . S RTN=$P(ENTRY,"^",3)
 . Q:RTN=""
 . S TAG=$P(ENTRY,"^",2)
 . S PKG=$$GETPKG^ZZRGND19(RTN,GL)
 . S @GL@(14,PKG,RTN,TAG)=I
 . S @GL@(14,PKG,RTN,TAG,RPC)=""
 Q
 ;
NDXIN(GLB,CLEAN) ;
 N PKG
 S:$G(GLB)="" GLB="^ZZRG"
 K:+$G(CLEAN) @GLB@(7)
 K @GLB@(15)
 S PKG=""
 F  S PKG=$O(@GLB@(9,PKG)) Q:PKG=""  D
 . S @GLB@(15,PKG)=""
 S PKG=""
 F  S PKG=$O(@GLB@(13,PKG)) Q:PKG=""  D
 . S @GLB@(15,PKG)=""
 S PKG=""
 F  S PKG=$O(@GLB@(14,PKG)) Q:PKG=""  D
 . S @GLB@(15,PKG)=""
 S PKG=""
 F  S PKG=$O(@GLB@(15,PKG)) Q:PKG=""  D
 . D NDXPKGIN(GLB,PKG,0)
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
 D NDXFMAN^ZZRGND17(GL,RTN,ACTTAG,.RESULT,0)
 D TRANS2GL(GL,PKG,RTN,TAG,"I",.RESULT)
 D TRANS2GL(GL,PKG,RTN,TAG,"O",.RESULT)
 S CMD=""
 F  S CMD=$O(RESULT("CMD",CMD)) Q:CMD=""  D
 . S @GL@(7,PKG,RTN,TAG,"CMD",CMD)=RESULT("CMD",CMD)
 M:$D(RESULT("F")) @GL@(7,PKG,RTN,TAG,"F")=RESULT("F")
 M:$D(RESULT("G")) @GL@(7,PKG,RTN,TAG,"G")=RESULT("G")
 M:$D(RESULT("GL")) @GL@(7,PKG,RTN,TAG,"GL")=RESULT("GL")
 M:$D(RESULT("FMG")) @GL@(7,PKG,RTN,TAG,"FMG")=RESULT("FMG")
 M:$D(RESULT("FMGC")) @GL@(7,PKG,RTN,TAG,"FMGC")=RESULT("FMGC")
 Q
 ;
NDXLOCAL(RTN,TAG,GL,RESULT,ITAGS,START,LEVEL) ; Index locals
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
 . . . D MRGLOCAL(.RESULT,.IRESULT,.FPAR,$$SAMEPKG^ZZRGND19(RTN,IRTN,GL))
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
SETLOCAL(RESULT,PAR,LOC,SCRIPT) ;
 S RESULT(SCRIPT,LOC)=""
 S:PAR RESULT("(",LOC)=""
 Q
 ;  
TRANS2GL(GL,PKG,RTN,TAG,SCRIPT,RESULT) ; Index globals
 N LOC,V
 S LOC="" 
 F  S LOC=$O(RESULT(SCRIPT,LOC)) Q:LOC=""  D
 . S V=""
 . S:$D(RESULT("(",LOC)) V="*"
 . S @GL@(7,PKG,RTN,TAG,SCRIPT,LOC)=V
 Q
 ;
