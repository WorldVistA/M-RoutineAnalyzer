ZZRGND14 ;;CBR/AU - Used to extract and index data for the reports ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
 Q ; Call tags
 ;
 ; Call main global update tags.  See called tags.
MAIN(GLB,PKGSPATH) 
 S:$G(GLB)="" GLB="^ZZRG"
 D CRTBASE(GLB)
 D:$D(PKGSPATH) READPKGS^ZZRGND19(GLB,1,PKGSPATH)
 D NDXFI(GLB)
 D NDXFMAN^ZZRGND17(GLB)
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
 ; Parses codebase and stores relevant info in a global.
 ;  GLB: Global where information is stored.
CRTBASE(GLB) ; Create base data
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
 ; Updates the global for a single routine
CRTONE(GLB,IRTN) ; Create base data
 N CNT,RTN,INP,NRO,DA,INDDA 
 S:$G(GLB)="" GLB="^ZZRG"
 D NEWINP(.INP)
 S INP(10)=9.4,INP(1)=1,DA=-1,INDDA=DA
 S CNT=0,RTN=""
 K ^UTILITY($J)
 S NRO=1,^UTILITY($J,0)="1;ROU",^UTILITY($J,IRTN)=""
 D MAIN^ZZRGNDX(1)
 M @GLB=^UTILITY($J)
 K ^UTILITY($J)
 Q
 ;
 ; Indexes for package to package entry points
 ;  GLB (INPUT): Global where information is stored.
NDXFI(GLB) ; Index fanin
 N RTN,TAGS,TAG,PKG,T,TRTN,TTAG,TPKG,I,J
 S:$G(GLB)="" GLB="^ZZRG"
 K @GLB@(4),@GLB@(8),@GLB@(9)
 S RTN=""
 F  S RTN=$O(@GLB@(1,RTN)) Q:RTN=""  D 
 . Q:$E(RTN)="%"
 . Q:$E(RTN,1,2)="ZZ"
 . S PKG=$$GETPKG^ZZRGND19(RTN,GLB)
 . S:'$D(@GLB@(4,PKG)) @GLB@(4,PKG)=0
 . S T=""
 . F  S T=$O(@GLB@(1,RTN,"X",T)) Q:T=""  D
 . . S TRTN=$P(T," ",1)
 . . S TTAG=$P(T," ",2)
 . . Q:$E(TRTN)="@"  Q:$E(TRTN)="$"  Q:$E(TRTN)="|"
 . . Q:$L(TRTN,".")>1
 . . S:$E(TTAG,1,2)="$$" TTAG=$E(TTAG,3,999)
 . . S TTAG=" "_TTAG
 . . S TPKG=$$GETPKG^ZZRGND19(TRTN,GLB)
 . . Q:PKG=TPKG
 . . I '$D(@GLB@(4,TPKG,PKG)) S @GLB@(4,TPKG)=+$G(@GLB@(4,TPKG))+1,@GLB@(4,TPKG,PKG)="" 
 . . S I=""
 . . F  S I=$O(@GLB@(1,RTN,"X",T,I)) Q:I=""  D
 . . . S TAGS=@GLB@(1,RTN,"X",T,I)
 . . . F J=1:1:$L(TAGS,",") D
 . . . . S TAG=$P(TAGS,",",J)
 . . . . S:$E(TAG,1,2)="$$" TAG=$E(TAG,3,999)
 . . . . S @GLB@(8,PKG,TPKG,TRTN,TTAG)=""  
 . . . . S @GLB@(9,TPKG,TRTN,TTAG,PKG,TAG_"^"_RTN)=""  
 Q
 ;
 ; Indexes option entry points
 ;  GLB (INPUT): Global where information is stored.
NDXOPT(GL) 
 N PKG,RTN,TAG,BOTH,OPT,OPTNAME,CMD,I
 S:$G(GLB)="" GLB="^ZZRG"
 F I=1:1:+$P(^DIC(19,0),"^",3) D:$D(^DIC(19,I,0))
 . S OPT=^DIC(19,I,0)
 . Q:$P(OPT,"^",4)'="R"  Q:'$D(^DIC(19,I,25))
 . S OPTNAME=$P(OPT,"^",1)
 . S BOTH=^DIC(19,I,25)
 . S TAG=" "
 . I $L(BOTH,"^") > 1 S RTN=$P(BOTH,"^",2),TAG=TAG_$P(BOTH,"^",1) I 1
 . E  S RTN=BOTH
 . S RTN=$P(RTN,"(",1)
 . Q:$G(RTN)=""
 . S PKG=$$GETPKG^ZZRGND19(RTN,GL)
 . S @GL@(13,PKG,OPTNAME,RTN,TAG)=""  
 Q
 ;
 ; Indexes RPC entry points
 ;  GLB (INPUT): Global where information is stored.
NDXRPC(GL) 
 N I,ENTRY,RPC,PKG,RTN,TAG
 S:$G(GLB)="" GLB="^ZZRG"
 S I=""
 F  S I=$O(^XWB(8994,I)) Q:I=""  D
 . Q:+I=0
 . S ENTRY=^XWB(8994,I,0)
 . S RPC=$P(ENTRY,"^",1)
 . S RTN=$P(ENTRY,"^",3)
 . Q:RTN=""
 . S TAG=$P(ENTRY,"^",2)
 . S PKG=$$GETPKG^ZZRGND19(RTN,GL)
 . S @GL@(14.5,PKG,RTN,TAG)=I
 . S @GL@(14,PKG,RPC,RTN,TAG)=""
 Q
 ;
NDXIN(GLB,CLEAN) ;
 N PKG
 S:$G(GLB)="" GLB="^ZZRG"
 K:+$G(CLEAN) @GLB@(7)
 K @GLB@(20),@GLB@(21)
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
 N RTN,TAG,CALLERS,TPKG,FT,FTP,TRTN,TTAG,TLINE,LOC,V,CMD,ONAME
 N ACTTAG
 S RTN=""
 F  S RTN=$O(@GL@(9,PKG,RTN)) Q:RTN=""  D
 . S TAG=""
 . F  S TAG=$O(@GL@(9,PKG,RTN,TAG)) Q:TAG=""  D NDXTAG(GL,PKG,RTN,TAG)
 S ONAME=""
 F  S ONAME=$O(@GL@(13,PKG,ONAME)) Q:ONAME=""  D
 . S RTN=""
 . F  S RTN=$O(@GL@(13,PKG,ONAME,RTN)) Q:RTN=""  D
 . . S TAG=""
 . . F  S TAG=$O(@GL@(13,PKG,ONAME,RTN,TAG)) Q:TAG=""  D NDXTAG(GL,PKG,RTN,TAG)
 S ONAME=""
 F  S ONAME=$O(@GL@(14,PKG,ONAME)) Q:ONAME=""  D
 . S RTN=""
 . F  S RTN=$O(@GL@(14,PKG,ONAME,RTN)) Q:RTN=""  D
 . . S TAG=""
 . . F  S TAG=$O(@GL@(14,PKG,ONAME,RTN,TAG)) Q:TAG=""  D NDXTAG(GL,PKG,RTN,TAG)
 Q
 ;
NDXTAG(GL,PKG,RTN,TAG,FLAG)
 N RESULT,ACTTAG,I,KEY,CMD
 K @GL@(7,PKG,RTN,TAG)
 S TAG=$$LTRIM^ZZRGND20(TAG," ")
 S:TAG="" TAG=RTN
 I $E(TAG,1,2)="$$" S ACTTAG=$E(TAG,3,$L(TAG)) I 1
 E  S ACTTAG=TAG
 ;
 S KEY=RTN_"^"_ACTTAG_"^"_0
 S FLAG=+$G(FLAG)
 D:'$D(@GL@(20,KEY,"RESULT")) BUILDRES(GL,PKG,RTN,ACTTAG,FLAG)
 M RESULT=@GL@(20,KEY,"RESULT")
 D MERGADDL(GL,PKG,FLAG,KEY,.RESULT)
 ;
 M:$D(RESULT("D")) @GL@(7,PKG,RTN,TAG,"D")=RESULT("D")
 S CMD=""
 F  S CMD=$O(RESULT("CMD",CMD)) Q:CMD=""  D
 . S @GL@(7,PKG,RTN,TAG,"CMD",CMD)=RESULT("CMD",CMD)
 M:$D(RESULT("F")) @GL@(7,PKG,RTN,TAG,"F")=RESULT("F")
 M:$D(RESULT("G")) @GL@(7,PKG,RTN,TAG,"G")=RESULT("G")
 M:$D(RESULT("FMG")) @GL@(7,PKG,RTN,TAG,"FMG")=RESULT("FMG")
 M:$D(RESULT("FMGC")) @GL@(7,PKG,RTN,TAG,"FMGC")=RESULT("FMGC")
 Q
 ;
BUILDRES(GL,PKG,RTN,ACTTAG,FLAG)
 N FANOUTS,IDX,KEY
 S KEY=RTN_"^"_ACTTAG_"^"_0
 S FANOUTS=0,IDX=0,FANOUTS("FINAL")=-1
 S FANOUTS(0)=KEY
 S FANOUTS("KEYS",FANOUTS(0))=0
 S FLAG=+$G(FLAG)
 K @GL@(21)
 F  D  S IDX=IDX+1 Q:IDX>FANOUTS 
 . N ITAG,IRTN,ISTART,ILVL,ALL
 . S ALL=FANOUTS(IDX)
 . S ITAG=$P(ALL,"^",2)
 . S IRTN=$P(ALL,"^",1)
 . S ISTART=$P(ALL,"^",3)
 . S ILVL=$P(ALL,"^",4)
 . I IRTN'=RTN Q:$$OMIT(GL,PKG,IRTN,FLAG) 
 . D NDXLOCAL(IRTN,ITAG,GL,.FANOUTS,ISTART,ILVL)
 F IDX=1:1:FANOUTS D
 . S IRTN=$P(FANOUTS(IDX),"^",1)
 . I IRTN'=RTN,$$OMIT(GL,PKG,IRTN,FLAG) S FANOUTS(IDX)=""  
 F IDX=0:1:FANOUTS("FINAL") D
 . S IRTN=$P(FANOUTS("FINAL",IDX),"^",1)
 . I IRTN'=RTN,$$OMIT(GL,PKG,IRTN,FLAG) S FANOUTS("FINAL",IDX)=""
 D MERGEEX(GL,.FANOUTS)
 D MRGASSUM^ZZRGND21(GL,.FANOUTS)
 Q  
 ;
OMIT(GLB,PKG,IRTN,FLAG) ;
 N IPKG
 S IPKG=$$GETPKG^ZZRGND19(IRTN,GLB)
 Q:FLAG=0 0
 Q:FLAG>2 1
 I FLAG=2 Q IPKG'=PKG
 I PKG'="DI",IPKG="DI" Q 1
 I PKG'="XU",IPKG="XU" Q 1
 Q 0 
 ; 
MERGEEX(GL,FANOUTS) ;
 N IDX,KEY,IKEY,FINDEX,NINDEX,LOC
 F IDX=0:1:FANOUTS("FINAL") D
 . S KEY=FANOUTS("FINAL",IDX) Q:KEY=""
 . S IKEY=""
 . F  S IKEY=$O(@GL@(21,KEY,"FI",IKEY)) Q:IKEY=""  D
 . . S FINDEX=@GL@(20,IKEY,"FO",KEY)
 . . S LOC=""
 . . F  S LOC=$O(@GL@(20,KEY,"RESULT","D",LOC)) Q:LOC=""  D
 . . . S NINDEX=$G(@GL@(20,IKEY,"RESULT","N",LOC))
 . . . I NINDEX]"" Q:+NINDEX<FINDEX
 . . . S @GL@(20,IKEY,"RESULT","A",LOC)=FINDEX
 Q
 ;
MERGADDL(GL,PKG,FLAG,KEY,RESULT) ;
 N FO,IDX,IKEY,FOKEY,CMD
 S FO=0,IDX=0,FOKEY="",FO(0)=KEY,FO("KEY",KEY)=""
 F  D  S IDX=IDX+1 Q:IDX>FO
 . S IKEY=FO(IDX)
 . S FOKEY=""
 . F  S FOKEY=$O(@GL@(20,IKEY,"FO",FOKEY)) Q:FOKEY=""  D
 . . Q:IKEY=FOKEY
 . . I RTN'=$P(FOKEY,"^",1),$$OMIT(GL,PKG,$P(FOKEY,"^",1),FLAG) Q
 . . Q:$D(FO("KEY",FOKEY))
 . . S FO=FO+1
 . . S FO(FO)=FOKEY
 . . S FO("KEY",FOKEY)=""
 F IDX=1:1:FO D
 . S IKEY=FO(IDX)
 . M RESULT("G")=@GL@(20,IKEY,"RESULT","G")
 . M:$D(@GL@(20,IKEY,"RESULT","FMG")) RESULT("FMG")=@GL@(20,IKEY,"RESULT","FMG")
 . M:$D(@GL@(20,IKEY,"RESULT","FMGC")) RESULT("FMGC")=@GL@(20,IKEY,"RESULT","FMGC")
 . F CMD="R","W","X","@" D
 . . N CNT
 . . S CNT=+$G(@GL@(20,IKEY,"RESULT","CMD",CMD))
 . . Q:CNT=0
 . . S RESULT("CMD",CMD)=+$G(RESULT("CMD",CMD))+CNT
 . . Q:CMD'="R"
 Q 
 ;
MERGEONE(IRESULT,RESULT,INDEX) ;
 N LOC
 S LOC=""
 F  S LOC=$O(@IRESULT@("D",LOC)) Q:LOC=""  D
 . I $D(@RESULT@("N",LOC)),(@RESULT@("N",LOC)<INDEX) Q
 . S @RESULT@("A",LOC)=INDEX
 Q 
 ;
NDXLOCAL(RTN,TAG,GL,FANOUTS,START,LEVEL) ; Index locals
 N I,J,L,T,CNT,FACNT,Q,TAGLEVEL,LOCALLVL,GLOB,RESULT
 N IRTNTAG,IRTN,ITAG,CMD,PRSDONE
 N LOC,LT,NODEIDX
 I TAG="" Q
 I '$D(@GL@(2,RTN,TAG)) Q
 S Q=0,FACNT=1,NODEIDX=1
 S J=+START-1
 S TAGLEVEL=@GL@(2,RTN,TAG,"L")
 S:LEVEL="" LEVEL=TAGLEVEL
 F  S J=$O(@GL@(2,RTN,TAG,J)) Q:J=""  Q:Q  D 
 . Q:J="N"  Q:J="P"  Q:J="L"
 . S PRSDONE=0
 . S CNT=+@GL@(2,RTN,TAG,J)
 . F I=1:1:CNT D  Q:Q
 . . S NODEIDX=NODEIDX+1
 . . S L=@GL@(2,RTN,TAG,J,I) Q:L=""
 . . S LOCALLVL=@GL@(2,RTN,TAG,J,I,"LVL")
 . . I LOCALLVL'=LEVEL S:LOCALLVL<LEVEL Q=1 Q
 . . I 'PRSDONE S PRSDONE=1 D NDXFMANG^ZZRGND17(GL,RTN,TAG,.RESULT,J)
 . . S T=$P(L,$C(9),1)
 . . I T="@" D  Q
 . . . S RESULT("CMD","@")=+$GET(RESULT("CMD","@"))+1
 . . I T="CMD" D  Q
 . . . S CMD=$P(L,$C(9),2)
 . . . I CMD="XECUTE" S RESULT("CMD","X")=+$GET(RESULT("CMD","X"))+1
 . . . I CMD="READ" S RESULT("CMD","R")=+$GET(RESULT("CMD","R"))+1
 . . . I CMD="WRITE" S RESULT("CMD","W")=+$GET(RESULT("CMD","W"))+1
 . . I T="LVL" D  Q
 . . . N NTAG S NTAG=+$O(@GL@(2,RTN,TAG,J)) 
 . . . I NTAG=0 D:$D(@GL@(2,RTN,TAG,"N"))  Q
 . . . . N ITAG 
 . . . . S ITAG=@GL@(2,RTN,TAG,"N")
 . . . . D SETFOFI(GL,RTN,TAG,+START,RTN,ITAG,0,.FANOUTS,NODEIDX,LOCALLVL+1)
 . . . D SETFOFI(GL,RTN,TAG,+START,RTN,TAG,J+1,.FANOUTS,NODEIDX,LOCALLVL+1)
 . . I T="Q" S Q=1 Q
 . . I T="X"!(T="I") D  Q  
 . . . I $P(L,$C(9),3)="G0",+$P(L,$C(9),4)=0 S Q=1
 . . . S IRTNTAG=$P(L,$C(9),2)
 . . . I T="I" D  Q:ITAG=TAG  I 1
 . . . . S IRTN=RTN
 . . . . S ITAG=IRTNTAG
 . . . E  D
 . . . . S IRTN=$P(IRTNTAG," ",1)
 . . . . S ITAG=$P(IRTNTAG," ",2)
 . . . . S:ITAG="" ITAG=IRTN
 . . . S:$E(ITAG,1,2)="$$" ITAG=$E(ITAG,3,$L(ITAG))
 . . . Q:ITAG=""   ; XINDEX bug
 . . . D SETFOFI(GL,RTN,TAG,+START,IRTN,ITAG,0,.FANOUTS,NODEIDX)
 . . I T="L" D  Q
 . . . S LOC=$P(L,$C(9),2),LT=$P(L,$C(9),3)
 . . . I LOC["(" S LOC=$P(LOC,"(",1)
 . . . I LT="n" S RESULT("F")=+$G(RESULT("F"))+1,RESULT("F",RESULT("F"))=LOC
 . . . Q:$D(RESULT("N",LOC))  Q:$D(RESULT("A",LOC))
 . . . I LT="n" S RESULT("N",LOC)=0 Q
 . . . I LT="~" S RESULT("N",LOC)=NODEIDX Q
 . . . S RESULT("A",LOC)=NODEIDX
 . . I T="G" D  Q
 . . . S GLOB=$P(L,$C(9),2)
 . . . S RESULT("G",GLOB)=""
 ;
 S NODEIDX=NODEIDX+1 
 I 'Q,$D(@GL@(2,RTN,TAG,"N")) D
 . S ITAG=@GL@(2,RTN,TAG,"N")
 . S TAGLEVEL=@GL@(2,RTN,ITAG,"L")
 . I TAGLEVEL=LEVEL D  Q
 . . D SETFOFI(GL,RTN,TAG,+START,RTN,ITAG,0,.FANOUTS,NODEIDX)
 . I TAGLEVEL>LEVEL D  Q
 . . D FINDLVL(GL,RTN,TAG,START,ITAG,.FANOUTS,NODEIDX,LEVEL)
 ;
 M @GL@(20,RTN_"^"_TAG_"^"_+START,"RESULT")=RESULT
 Q
 ;
FINDLVL(GL,RTN,TAG,START,ITAG,FANOUTS,NODEIDX,LVL) ;
 N J,I,CNT,FOUND,LLVL
 S J=""
 F  S J=$O(@GL@(2,RTN,ITAG,J)) Q:J=""  D  Q:LLVL'>LVL
 . Q:J="N"  Q:J="P"  Q:J="L" 
 . S CNT=+@GL@(2,RTN,ITAG,J)
 .  F I=1:1:CNT D  Q:LLVL'>LVL
 . . S LLVL=@GL@(2,RTN,ITAG,J,I,"LVL")
 I LVL=LLVL D  Q
 . S NODEIDX=NODEIDX+1
 . D SETFOFI(GL,RTN,TAG,+START,RTN,ITAG,J,.FANOUTS,NODEIDX,LVL) 
 I LVL<LLVL D  Q
 . I $D(@GL@(2,RTN,ITAG,"N")) D
 . . S ITAG=@GL@(2,RTN,ITAG,"N")
 . . S LLVL=@GL@(2,RTN,ITAG,"L")
 . . I LVL=LLVL D  Q
 . . . S NODEIDX=NODEIDX+1
 . . . D SETFOFI(GL,RTN,TAG,+START,RTN,ITAG,0,.FANOUTS,NODEIDX,LVL) 
 . . D FINDLVL(GL,RTN,TAG,START,ITAG,.FANOUTS,NODEIDX,LVL)
 Q
 ; 
SETFOFI(GL,RTN,TAG,START,TRTN,TTAG,TSTART,FANOUTS,NODEIDX,LEVEL) ;
 N KEY,TKEY
 S KEY=RTN_"^"_TAG_"^"_+START
 S TKEY=TRTN_"^"_TTAG_"^"_TSTART
 Q:$D(@GL@(20,KEY,"FO",TKEY))
 S @GL@(20,KEY,"FO",TKEY)=NODEIDX
 S @GL@(21,TKEY,"FI",KEY)=""
 N V
 S V=TRTN_"^"_TTAG_"^"_TSTART   
 I $D(LEVEL),LEVEL]"" S V=V_"^"_LEVEL
 Q:$D(FANOUTS("KEYS",V))
 S FANOUTS("KEYS",V)=FANOUTS
 I $D(@GL@(20,TKEY,"RESULT")) D  Q
 . S FANOUTS("FINAL")=FANOUTS("FINAL")+1 
 . S FANOUTS("FINAL",FANOUTS("FINAL"))=TKEY 
 S FANOUTS=FANOUTS+1
 S FANOUTS(FANOUTS)=V
 Q
 ;
