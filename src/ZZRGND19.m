ZZRGND19 ;CBR/ - PROCESS LIST OF PACKAGES AND GLOBAL OWNERSHIP FILES ;02/01/12
 ;;7.3;TOOLKIT;**to be determined**;
 Q ; Use tags
 ;
GETPKG(RTN,GLB) ; Extracts the namespace prefix from a routine label
 N I,PREFIX,STOP
 S PREFIX=""
 S STOP=0
 S:$G(GLB)="" GLB="^ZZRG"
 I $D(@GLB@(10))=0 D READPKGS(GLB,0)
 F I=1:1:$L(RTN) D  Q:STOP
 . I $D(@GLB@(10,$E(RTN,1,I))) S PREFIX=$E(RTN,1,I),STOP=1 Q
 Q:PREFIX]"" PREFIX
 Q:$E(RTN)="%" "XU"
 Q "UNKNOWN"
 ; 
GPKGNAME(NS,GLB) ; Returns the package name based on namespace
 Q $G(@GLB@(10,NS),"UNKNOWN")
 ;
SAMEPKG(RTN,CMPRTN,GLB) ; True if the two routines belong to the same package
 Q $$GETPKG(RTN,GLB)=$$GETPKG(CMPRTN,GLB) 
 ;
READPKGS(GLB,CLEAN)
 N PKGNAME
 K:CLEAN @GLB@(10)
 S PKGNAME=""
 S @GLB@(10)=""
 D READFILE^ZZRGND20("Packages.csv","RDPKGLIN^ZZRGND19",$G(PKGSPATH))
 Q
 ; 
RDPKGLIN(LINE)
 N NAMESPC,FIELDS
 D GCSVFLDS^ZZRGND20(LINE,.FIELDS)
 S:FIELDS(1)'="" PKGNAME=FIELDS(1)
 S:(FIELDS(3)'="")&(PKGNAME'="") @GLB@(10,FIELDS(3))=PKGNAME
 Q
 ;