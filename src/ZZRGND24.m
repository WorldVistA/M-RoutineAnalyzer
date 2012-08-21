ZZRGND24 ;;CBR/AU - UTILITY TAGS ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
 ;
 Q  ; Call tags
 ;
PLINFO(GLB,FILEPATH,FILENAME,FLAG)  ; Creates code report for all tags in GMPL API routine tags
 N PATTERN
 S PATTERN="1""GMPL""1(1""API"",1""SITE"",1""DAL"",1""EXT"").UN"
 D WRESINFP^ZZRGND23(GLB,PATTERN,FILEPATH,FILENAME,+$G(FLAG))
 Q
 ;
 ; Reports packages for all routines
 ; Assumes @GLB@(2 ans @GLB@(10 - created in MAIN^ZZRGND14
PKGINFO(GLB) 
 N RTN,NS
 S RTN=""
 D TEXTOPEN^ZZRGND23("OUTFILE","C:\Sandbox\","m_rpkgs.dat")
 F  S RTN=$O(@GLB@(2,RTN)) Q:RTN=""  D
 . S NS=$$GETPKG^ZZRGND19(RTN,GLB)
 . W RTN,":",NS,":",$$GPKGNAME^ZZRGND19(NS,GLB),!
 D TEXTCLS^ZZRGND23("OUTFILE","m_rpkgs.dat")
 Q
 ;
 ; Common tag from OPTRTNS and RPCRTNS
COMRTNS(GLB,PKG,NODE,FILEPATH,FILENAME)
 D TEXTOPEN^ZZRGND23("OUTFILE",FILEPATH,FILENAME)
 N PKGNDX
 S PKG=$G(PKG)
 S PKGNDX=0
 I PKG="" F  S PKG=$O(@GLB@(10,PKG)) Q:PKG=""  D  I 1
 . Q:PKG'=@GLB@(10,PKG,"D")
 . S PKGNDX=PKGNDX+1
 . D COMRTNSPK(GLB,NODE,FILEPATH,FILENAME,PKG,PKGNDX)
 E  D COMRTNSPK(GLB,NODE,FILEPATH,FILENAME,PKG)  
 D TEXTCLS^ZZRGND23("OUTFILE",FILENAME)
 Q
 ;
 ; Common tag from OPTRTNS and RPCRTNS
COMRTNSPK(GLB,NODE,FILEPATH,FILENAME,PKG,PKGNDX)
 N RTN,TAG,NAME,HAS
 S PKGNDX=$G(PKGNDX)
 S:PKGNDX]"" PKGNDX=PKGNDX_". "
 W PKGNDX,$$GPKGNAME^ZZRGND19(PKG,GLB),!!
 S NAME="",HAS=0
 F  S NAME=$O(@GLB@(NODE,PKG,NAME)) Q:NAME=""  D
 . S RTN="" 
 . F  S RTN=$O(@GLB@(NODE,PKG,NAME,RTN)) Q:RTN=""  D
 . . S TAG=""
 . . F  S TAG=$O(@GLB@(NODE,PKG,NAME,RTN,TAG)) Q:TAG=""  D
 . . . S HAS=1
 . . . W " ","NAME: ",NAME,!
 . . . W " "," TAG: ",$$LTRIM^ZZRGND20(TAG," ")_"^"_RTN,!!
 W:'HAS "  No entries",!! 
 Q
 ;
 ; Reports all the option tags per package
 ; Assumes @GLB@(10 - created in MAIN^ZZRGND14 or READPKGS^ZZRGND19
OPTRTNS(GLB,PKG,FILEPATH,FILENAME)
 S:$G(GLB)="" GLB="^ZZRG"
 K @GLB@(13)
 D NDXOPT^ZZRGND14(GLB)
 S PKG=$G(PKG)
 D COMRTNS(GLB,PKG,13,FILEPATH,FILENAME)
 Q
 ;
 ; Reports all the rpc tags per package
 ; Assumes @GLB@(10 - created in MAIN^ZZRGND14 or READPKGS^ZZRGND19
RPCRTNS(GLB,PKG,FILEPATH,FILENAME)
 S:$G(GLB)="" GLB="^ZZRG"
 K @GLB@(14)
 D NDXRPC^ZZRGND14(GLB)
 S PKG=$G(PKG)
 D COMRTNS(GLB,PKG,14,FILEPATH,FILENAME)
 Q
 ;
 ; Common tag from OPTTINFO and RPCTINFO
COMTINFO(GLB,NODE,FILEPATH,FILENAME)
 D TEXTOPEN^ZZRGND23("OUTFILE",FILEPATH,FILENAME)
 N PKG,RTN,TAG,NAME,ENTRIES,RTNS
 S PKG="",ENTRIES=0
 F  S PKG=$O(@GLB@(NODE,PKG)) Q:PKG=""  D
 . S NAME=""
 . F  S NAME=$O(@GLB@(NODE,PKG,NAME)) Q:NAME=""  D
 . . S RTN="" 
 . . F  S RTN=$O(@GLB@(NODE,PKG,NAME,RTN)) Q:RTN=""  D
 . . . S TAG=""
 . . . F  S TAG=$O(@GLB@(NODE,PKG,NAME,RTN,TAG)) Q:TAG=""  D
 . . . . Q:$D(RTNS(TAG_"^"_RTN))
 . . . . S ENTRIES=ENTRIES+1
 . . . . S ENTRIES(ENTRIES)=TAG_"^"_RTN
 D WRESINFO^ZZRGND23("^ZZRG",.ENTRIES)
 D TEXTCLS^ZZRGND23("OUTFILE",FILENAME)
 Q
 ;
 ; Reports code analysis results for all the tags that are used as option entry point
 ; Assumes @GLB(2 and @GLB@(10 - created in MAIN^ZZRGND14
OPTTINFO(GLB,FILEPATH,FILENAME)
 K @GLB@(13)
 D NDXOPT^ZZRGND14(GLB)
 D COMTINFO(GLB,13,FILEPATH,FILENAME)
 Q
 ;
 ; Reports code analysis results for all the tags that are used as rpc entry points
 ; Assumes @GLB(2 and @GLB@(10 - created in MAIN^ZZRGND14
RPCTINFO(GLB,FILEPATH,FILENAME)
 K @GLB@(13)
 D NDXOPT^ZZRGND14(GLB)
 D COMTINFO(GLB,13,FILEPATH,FILENAME)
 Q
 ;
FISINFO(GLB,FLAG,FILEPATH,FILENAME)
 D TEXTOPEN^ZZRGND23("OUTFILE",FILEPATH,FILENAME)
 N PKG,RTN,TAG,ENTRIES,RTNS
 S PKG="",ENTRIES=0
 F  S PKG=$O(@GLB@(9,PKG)) Q:PKG=""
 . S RTN="" 
 . F  S RTN=$O(@GLB@(9,PKG,RTN)) Q:RTN=""  D
 . . S TAG=""
 . . F  S TAG=$O(@GLB@(9,PKG,RTN,TAG)) Q:TAG=""  D
 . . . Q:$D(RTNS(TAG_"^"_RTN))
 . . . S ENTRIES=ENTRIES+1
 . . . S ENTRIES(ENTRIES)=$$LTRIM^ZZRGND20(TAG," ")_"^"_RTN
 D WRESINFO^ZZRGND23(GLB,.ENTRIES,FILEPATH,FILENAME,FLAG)
 D TEXTCLS^ZZRGND23("OUTFILE",FILENAME)
 Q
 ; 
PFISINFO(GLB,PKG,FLAG,FILEPATH,FILENAME)
 D TEXTOPEN^ZZRGND23("OUTFILE",FILEPATH,FILENAME)
 N RTN,TAG,ENTRIES,RTNS
 S ENTRIES=0
 S RTN="" 
 F  S RTN=$O(@GLB@(9,PKG,RTN)) Q:RTN=""  D
 . S TAG=""
 . F  S TAG=$O(@GLB@(9,PKG,RTN,TAG)) Q:TAG=""  D
 . . Q:$D(RTNS(TAG_"^"_RTN))
 . . S ENTRIES=ENTRIES+1
 . . S ENTRIES(ENTRIES)=$$LTRIM^ZZRGND20(TAG," ")_"^"_RTN
 D WRESINFO^ZZRGND23(GLB,.ENTRIES,FILEPATH,FILENAME,FLAG)
 D TEXTCLS^ZZRGND23("OUTFILE",FILENAME)
 Q
 ; 
 ; Generates Full suit of reports as example and testing purposes
 ; Packages.csv and Ownership.csv are assumed to be C:\Sandbox\
 ; Information is stored in ZZRGI
 ; GMPL = Problem List Package
 ; SD = Scheduling Package
GENRALL()
 N DIR,GLB
 S DIR="C:\Sandbox\",GLB="^ZZRGI" 
 D MAIN^ZZRGND14(GLB,DIR)
 D RDOWNER^ZZRGND19(GLB,1,DIR)
 ;
 D REPORTFO^ZZRGND13(GLB,,"C:\Sandbox\","m_fo_all.txt") 
 D REPORTFO^ZZRGND13(GLB,"GMPL","C:\Sandbox\","m_fo_gmpl.txt") 
 D REPORTFO^ZZRGND13(GLB,"SD","C:\Sandbox\","m_fo_sd.txt") 
 ;
 D REPORTFI^ZZRGND13(GLB,,"C:\Sandbox\","m_fi_all.txt") 
 D REPORTFI^ZZRGND13(GLB,"GMPL","C:\Sandbox\","m_fi_gmpl.txt") 
 D REPORTFI^ZZRGND13(GLB,"SD","C:\Sandbox\","m_fi_sd.txt")
 ;
 D OPTRTNS^ZZRGND24(GLB,,"C:\Sandbox\","m_opt_all.txt") 
 D OPTRTNS^ZZRGND24(GLB,"GMPL","C:\Sandbox\","m_opt_gmpl.txt") 
 D OPTRTNS^ZZRGND24(GLB,"SD","C:\Sandbox\","m_opt_sd.txt") 
 ;
 D RPCRTNS^ZZRGND24(GLB,,"C:\Sandbox\","m_rpc_all.txt") 
 D RPCRTNS^ZZRGND24(GLB,"GMPL","C:\Sandbox\","m_rpc_gmpl.txt") 
 D RPCRTNS^ZZRGND24(GLB,"SD","C:\Sandbox\","m_rpc_sd.txt") 
 ;
 D USES^ZZRGND15(GLB,"GMPL","C:\Sandbox\","C:\Sandbox\","m_uses_gmpl.txt") 
 D USES^ZZRGND15(GLB,"SD","C:\Sandbox\","C:\Sandbox\","m_uses_sd.txt") 
 ; 
 D USED^ZZRGND15(GLB,"GMPL","C:\Sandbox\","C:\Sandbox\","m_used_gmpl.txt") 
 D USED^ZZRGND15(GLB,"SD","C:\Sandbox\","C:\Sandbox\","m_used_sd.txt") 
 ;
 D RFMCALLS^ZZRGND17(GLB,"C:\Sandbox\","m_fmc_all.txt") 
 ; 
 D PFISINFO(GLB,"GMPL",0,"C:\Sandbox\","m_einfo_gmplfi_0.txt")
 D PFISINFO(GLB,"GMPL",1,"C:\Sandbox\","m_einfo_gmplfi_1.txt")
 D PFISINFO(GLB,"GMPL",2,"C:\Sandbox\","m_einfo_gmplfi_2.txt")
 ;
 D PFISINFO(GLB,"SD",0,"C:\Sandbox\","m_einfo_sdfi_0.txt")
 D PFISINFO(GLB,"SD",1,"C:\Sandbox\","m_einfo_sdfi_1.txt")
 D PFISINFO(GLB,"SD",2,"C:\Sandbox\","m_einfo_sdfi_2.txt")
 ;
 N PATTERN
 S PATTERN="1""ORQQPL"".UN"
 D WRESINFP^ZZRGND23(GLB,PATTERN,"C:\Sandbox\","m_einfo_cprspl_0.txt",0)
 D WRESINFP^ZZRGND23(GLB,PATTERN,"C:\Sandbox\","m_einfo_cprspl_1.txt",1)
 D WRESINFP^ZZRGND23(GLB,PATTERN,"C:\Sandbox\","m_einfo_cprspl_2.txt",2)
 D WRESINFP^ZZRGND23(GLB,PATTERN,"C:\Sandbox\","m_einfo_cprspl_3.txt",3)
 Q
 ;
