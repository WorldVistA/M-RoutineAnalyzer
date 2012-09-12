ZZRGND19 ;;CBR/AU - PROCESS LIST OF PACKAGES AND GLOBAL OWNERSHIP FILES ;08/01/12
 ;;1.0;RGI Dependency Tool;**260004**;08/01/2012
 Q  ; Use tags
 ;
GETPKG(RTN,GLB) ; Extracts the namespace prefix from a routine label
 N I,PREFIX,NUMCHAR,CAND
 S PREFIX=""
 S STOP=0
 S:$G(GLB)="" GLB="^ZZRG"
 I $D(@GLB@(10))=0 Q "UNCATEGORIZED"
 S NUMCHAR=$L(RTN)
 S:NUMCHAR>4 NUMCHAR=4
 F I=1:1:NUMCHAR D
 . S CAND=$E(RTN,1,I)
 . S:$D(@GLB@(10,CAND)) PREFIX=CAND
 S:PREFIX]"" PREFIX=@GLB@(10,PREFIX,"D")
 Q:PREFIX]"" PREFIX
 Q:$E(RTN)="%" "XU"
 Q "UNCATEGORIZED"
 ; 
GPKGNAME(NS,GLB) ; Returns the package name based on namespace
 Q $G(@GLB@(10,NS),"UNCATEGORIZED")
 ;
SAMEPKG(RTN,CMPRTN,GLB) ; True if the two routines belong to the same package
 Q $$GETPKG(RTN,GLB)=$$GETPKG(CMPRTN,GLB)
 ;
 ; Reads Packages.csv file and stores info in @GLB@(10)
 ;  GLB (INPUT): Global where information is stored.
 ;  CLEAN (INPUT): K @GLB@(10) even when it exists
 ;  PKGSPATH (INPUT): Directory where Packages.csv us
READPKGS(GLB,CLEAN,PKGSPATH) ; 
 N PKGNAME,DEFAULT
 S:$G(GLB)="" GLB="^ZZRG"
 K:+$G(CLEAN) @GLB@(10)
 Q:$D(@GLB@(10))
 S @GLB@(10,"UNCATEGORIZED")="UNCATEGORIZED"
 S @GLB@(10,"UNCATEGORIZED","D")="UNCATEGORIZED"
 D READFILE^ZZRGND20("Packages.csv","RDPKGLIN^ZZRGND19",$G(PKGSPATH))
 Q
 ; 
RDPKGLIN(LINE) ; Process one csv line from Packages.csv
 N NAMESPC,FIELDS
 D GCSVFLDS^ZZRGND20(LINE,.FIELDS)
 I $G(FIELDS(1))'="" D  Q:PKGNAME="Package Name"
 . S PKGNAME=FIELDS(1)
 . S DEFAULT=$G(FIELDS(3))
 I ($G(FIELDS(3))'="")&(PKGNAME'="") D
 . S @GLB@(10,FIELDS(3))=PKGNAME
 . S @GLB@(10,FIELDS(3),"D")=DEFAULT
 Q
 ;
 ; Reads Ownership.csv file and stores the info in @GLB@(11
 ;  GLB: Global where information is stored.
 ;  CLEAN: Kill global first even when it is previously populated
 ;  OWNPATH: Path to Ownership.csv
RDOWNER(GLB,CLEAN,OWNPATH) ;
 S:$G(GLB)="" GLB="^ZZRG"
 K:+$G(CLEAN) @GLB@(11)
 Q:$D(@GLB@(11))
 D READFILE^ZZRGND20("Ownership.csv","RDOWNLIN^ZZRGND19",OWNPATH)
 Q
 ;
RDOWNLIN(LINE) ;
 Q:$G(LINE)=""
 N FLDS,GLOBAL,SUB,FN,NAME,NS,PKG
 D GCSVFLDS^ZZRGND20(LINE,.FLDS)
 S GLOBAL=$$LTRIM^ZZRGND20($P(FLDS(3),"(",1)," ")
 S SUB=$P($P(FLDS(3),"(",2),",",1)
 S FN=FLDS(1)
 S NAME=FLDS(2)
 S NS=FLDS(4)
 S PKG=FLDS(5)
 S @GLB@(11,GLOBAL)=NS_"^"_PKG_"^"_NAME_"^"_FN
 S:SUB'="" @GLB@(11,GLOBAL,SUB)=NS_"^"_PKG_"^"_NAME_"^"_FN
 Q
 ;
