ZZRGND22 ;;CBR/AU - UTILITY TAGS ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
 ;
 Q  ; Call tags
 ;
ADDERR(RETURN,CODE,MSG) ; Adds an error message
 S RETURN=+$G(RETURN)+1
 S RETURN(RETURN)=CODE_"^"_$G(MSG) 
 Q
 ;
GLCHECK(RETURN,GL) ; Checks if relevant information available in GL.    
 S RETURN=0
 I '$D(@GL@(2)) S RETURN=RETURN+1,RETURN(RETURN)="NOBASE"
 I '$D(@GL@(10)) S RETURN=RETURN+1,RETURN(RETURN)="NOPKG" 
 Q RETURN=0
 ;
GETENTRS(RETURN,GL,PATTERN) ; Return entry points for specified routines.
 ; RETURN   : Number of entry points
 ; RETURN(I): I'th entry point, 1 based.
 ; GL       : Global that holds routine data (see CRTBASE^ZZRGND14)
 ; PATTERN  : M PATTERN for Routines
 ;  Returns 1 if routines found, 0 otherwise
 N RTN,TAG
 S RTN="",TAG="",RETURN=0
 F  S RTN=$O(@GL@(2,RTN)) Q:RTN=""  D
 . Q:RTN'?@PATTERN  
 . S TAG="" 
 . F  S TAG=$O(@GL@(2,RTN,TAG)) Q:TAG=""  D
 . . S RETURN=RETURN+1
 . . S RETURN(RETURN)=TAG_"^"_RTN
 Q RETURN>0
 ;
UPDEINFO(RESULT,GLB,ENTRIES,RST,FLAG)  ; Updates result global @GLB@(7) for entries
 ; RESULT    : Number of results
 ; RESULT(I) : Error detail (when 0 returned)
 ; RESULT(I) : 1^PKG^RTN^TAG if global updated or error detail (when 1 returned)
 ; GLB        : Source/target global
 ; ENTRIES   : Number of routines to update
 ; ENTRIES(I): Entry points for which result global is updated
 ; RST       : Kill result global (removes all previously updated results)
 S:$G(GLB)="" GLB="^ZZRG"
 N %
 K RESULT
 I +$G(ENTRIES)=0 D ADDERR(.RESULT,"NOENTRY","No entries is found")
 S %=$$GLCHECK(.RESULT,GLB)
 Q:'% 0
 ;
 N ENTRY,IX,PKG,RTN,TAG
 S RESULT=0
 K:+$G(RST) @GLB@(7)
 K @GLB@(20)
 S RESULT=0,ENTRY="" 
 F IX=1:1:ENTRIES D
 . I '$D(ENTRIES(IX)) D ADDERR(.RESULT,"EMPTYENTRY","Empty parameter") Q
 . S ENTRY=ENTRIES(IX)    
 . I ENTRY="" D ADDERR(.RESULT,"EMPTYENTRY","Empty parameter") Q
 . S TAG=$P(ENTRY,"^",1)
 . S RTN=$P(ENTRY,"^",2)
 . I RTN="" D ADDERR(.RESULT,"INVALIDENTRY","Invalid entry point") Q 
 . S TAG=$$LTRIM^ZZRGND20(TAG," ")
 . S:TAG="" TAG=RTN
 . S PKG=$$GETPKG^ZZRGND19(RTN,GLB)
 . D NDXTAG^ZZRGND14(GLB,PKG,RTN,TAG,+$G(FLAG))
 . I '$D(@GLB@(2,RTN,TAG)) D ADDERR(.RESULT,"INVALIDENTRY","Invalid entry point") Q
 . ; I '$D(@GLB@(20,RTN_"^"_TAG_"^0")) D ADDERR(.RESULT,"INVALIDENTRY","Invalid entry point") Q
 . S:TAG=RTN TAG=""
 . S RESULT=RESULT+1,RESULT(RESULT)=1_"^"_PKG_"^"_RTN_"^"_TAG  
 Q 1
 ;
