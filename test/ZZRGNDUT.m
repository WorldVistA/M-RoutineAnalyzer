ZZRGNDUT
 I $T(EN^XTMUNIT)'="" D EN^XTMUNIT("ZZRGNDUT")
 Q
 ;
PKGUTIL
 N PKG
 S PKGSPATH="D:\"
 D READPKGS^ZZRGND19("^ZZRG",1)
 S PKG=$$GETPKG^ZZRGND19("ZUGTM","^ZZRGT")
 D CHKEQ^XTMUNIT("ZU",PKG,"$$GETPKG^ZZRGND19: Wrong namespace for GMPLSAVE routine.")
 S PKG=$$GPKGNAME^ZZRGND19(PKG,"^ZZRGT")
 D CHKEQ^XTMUNIT("KERNEL",PKG,"$$GPKGNAME^ZZRGND19: Wrong package name for ZUGTM routine.")
 Q
 ;
XTENT
 ;;PKGUTIL
 Q
 ;