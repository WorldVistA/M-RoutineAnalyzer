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
 S:PREFIX]"" PREFIX=@GLB@(10,PREFIX,"D")
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
READPKGS(GLB,CLEAN) ; Reads Packages.csv file and stores info in @GLB@(10)
 N PKGNAME,DEFAULT
 S:$G(GLB)="" GLB="^ZZRG"
 K:+$G(CLEAN) @GLB@(10)
 Q:$D(@GLB@(10))
 S PKGNAME="",DEFAULT=""
 D READFILE^ZZRGND20("Packages.csv","RDPKGLIN^ZZRGND19",$G(PKGSPATH))
 Q
 ; 
RDPKGLIN(LINE) ; Process one csv line from Packages.csv
 N NAMESPC,FIELDS
 D GCSVFLDS^ZZRGND20(LINE,.FIELDS)
 I FIELDS(1)'="" D
 . S PKGNAME=FIELDS(1)
 . S DEFAULT=$G(FIELDS(3))
 I (FIELDS(3)'="")&(PKGNAME'="") D
 . S @GLB@(10,FIELDS(3))=PKGNAME
 . S @GLB@(10,FIELDS(3),"D")=DEFAULT
 Q
 ;
RDOWNER(GLB,CLEAN) ; Reads Ownership.csv file and stores the info in @GLB@(20)
 S:$G(GLB)="" GLB="^ZZRG"
 K:+$G(CLEAN) @GLB@(20)
 Q:$D(@GLB@(20))
 D READFILE^ZZRGND20("Ownership.csv","RDOWNLIN^ZZRGND19",$G(PKGSPATH))
 Q
 ;
RDOWNLIN(LINE)
 Q:$G(LINE)=""
 N FLDS,GLOBAL,SUB,FN,NAME,NS,PKG
 D GCSVFLDS^ZZRGND20(LINE,.FLDS)
 S GLOBAL=$$LTRIM^ZZRGND20($P(FLDS(3),"(",1)," ")
 S SUB=$P($P(FLDS(3),"(",2),",",1)
 S FN=FLDS(1)
 S NAME=FLDS(2)
 S NS=FLDS(4)
 S PKG=FLDS(5)
 S @GLB@(20,GLOBAL)=NS_"^"_PKG_"^"_NAME_"^"_FN
 S:SUB'="" @GLB@(20,GLOBAL,SUB)=NS_"^"_PKG_"^"_NAME_"^"_FN
 Q
 ;