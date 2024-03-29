ZZRGND23 ;;CBR/AU - UTILITY TAGS ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
 ;
 Q  ; Call tags
 ;
TEXTOPEN(HANDLE,PATH,NAME,MODE)  ;
 S:'$D(MODE) MODE="W"
 N POP
 D OPEN^%ZISH(NAME,PATH,NAME,MODE)
 Q:POP
 U IO
 Q
 ;
TEXTCLS(HANDLE,NAME)  ;
 D CLOSE^%ZISH(NAME)
 Q
 ;
WRENTRS(GL,PATTERN) ; User interface for GETENTRS^ZZRGND22
 N RETURN,%,I
 S %=$$GETENTRS^ZZRGND22(.RETURN,GL,PATTERN)
 I '% W "No routines is matched.",! Q
 F I=1:1:RETURN W RETURN(I),!
 Q
 ;
WRESINFP(GLB,PATTERN,FILEPATH,FILENAME,FLAG) ; User interface for UPDRINFO^ZZRGND22 
 I $G(FILENAME)]"",$G(FILEPATH)]"" D TEXTOPEN^ZZRGND23("OUTFILE",FILEPATH,FILENAME)
 N ENTRIES,CNT
 S CNT=$$GETENTRS^ZZRGND22(.ENTRIES,GLB,PATTERN)
 I CNT=0 W "No routine matched",! Q
 D WRESINFO(GLB,.ENTRIES,,,+$G(FLAG))
 I $G(FILENAME)]"",$G(FILEPATH)]"" D TEXTCLS^ZZRGND23("OUTFILE",FILENAME)
 Q
 ;
WRESINFO(GLB,ENTRIES,FILEPATH,FILENAME,FLAG) ; User interface for UPDRINFO^ZZRGND22 
 I $G(FILENAME)]"",$G(FILEPATH)]"" D TEXTOPEN^ZZRGND23("OUTFILE",FILEPATH,FILENAME)
 N RESULT,%,IX,PKG,RTN,TAG
 S %=$$UPDEINFO^ZZRGND22(.RESULT,GLB,.ENTRIES,1,+$G(FLAG))
 I '% D  I 1
 . W "Cannot display due to errors",!
 . F IX=1:1:RESULT W " ",IX,". ",$P(RESULT(IX),"^",2),!
 E  F IX=1:1:RESULT D
 . I +RESULT(IX) D  Q
 . . S PKG=$P(RESULT(IX),"^",2)
 . . S RTN=$P(RESULT(IX),"^",3)
 . . S TAG=$P(RESULT(IX),"^",4)
 . . D WRTNAPI^ZZRGND13(GLB,PKG,RTN,TAG)
 . . W !
 . W " ",$$LTRIM^ZZRGND20(ENTRIES(IX)," "),!
 . W "  ERROR: "_$P(RESULT(IX),"^",2),!!
 I $G(FILENAME)]"",$G(FILEPATH)]"" D TEXTCLS^ZZRGND23("OUTFILE",FILENAME)
 Q
 ;
WREINFO(GLB,ENTRY,FILEPATH,FILENAME,FLAG) ;
 N ENTRIES
 S ENTRIES=1
 S ENTRIES(1)=ENTRY
 D WRESINFO(GLB,.ENTRIES,$G(FILEPATH),$G(FILENAME),$G(FLAG))
 Q
 ;
