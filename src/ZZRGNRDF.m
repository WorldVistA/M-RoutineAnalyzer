ZZRGNRDF ;CBR/ - RDF/OWL generator ;02/14/12
 ;;7.3;TOOLKIT;**to be determined**;
WRITE(GLB,PATH)
 N PKG,PKGN,RTN,GLOB,CALL
 S:$G(GLB)="" GLB="^ZZRG"
 I $G(PATH)="" R !,"ENTER PATH: ",PATH
 D INDEX(GLB,0)
 W !,"<?xml version=""1.0""?>"
 W !,"<rdf:RDF"
 W !," xmlns:rdf=""http://www.w3.org/1999/02/22-rdf-syntax-ns#"""
 W !," xmlns:rdfs=""http://www.w3.org/2000/01/rdf-schema#"""
 W !," xmlns:owl=""http://www.w3.org/2002/07/owl#"""
 W !," xmlns:mv=""http://metavista.name/foundation#"""
 W !," xmlns:vista=""http://osehra.org/ns#"">"
 W !,"  <owl:Ontology rdf:about=""""/>"
 W !,"  <rdfs:Class rdf:about=""http://metavista.name/foundation#Routine"">"
 W !,"    <rdfs:comment>An ANS MUMPS Routine</rdfs:comment>"
 W !,"    <rdfs:label>Routine</rdfs:label>"
 W !,"  </rdfs:Class>"
 W !,"  <rdfs:Class rdf:about=""http://metavista.name/foundation#Package"">"
 W !,"    <rdfs:comment>A collection of routines to perform some application process</rdfs:comment>"
 W !,"    <rdfs:label>Package</rdfs:label>"
 W !,"  </rdfs:Class>"
 W !,"  <rdfs:Class rdf:about=""http://metavista.name/foundation#Global"">"
 W !,"    <rdfs:comment>An ANS MUMPS global</rdfs:comment>"
 W !,"    <rdfs:label>Global</rdfs:label>"
 W !,"  </rdfs:Class>"
 W !,"  <owl:ObjectProperty rdf:about=""http://metavista.name/foundation#contains"">"
 W !,"    <rdfs:comment>Defines a routine contained in a Package</rdfs:comment>"
 W !,"    <rdfs:domain rdf:resource=""http://metavista.name/foundation#Package""/>"
 W !,"    <rdfs:range rdf:resource=""http://metavista.name/foundation#Routine""/>"
 W !,"    <rdfs:label>contains</rdfs:label>"
 W !,"  </owl:ObjectProperty>"
 W !,"  <owl:ObjectProperty rdf:about=""http://metavista.name/foundation#calls"">"
 W !,"    <rdfs:comment>Defines a routine called by another routine</rdfs:comment>"
 W !,"	  <rdfs:domain rdf:resource=""http://metavista.name/foundation#Routine""/>"
 W !,"	  <rdfs:range rdf:resource=""http://metavista.name/foundation#Routine""/>"
 W !,"	  <rdfs:label>calls</rdfs:label>"
 W !,"  </owl:ObjectProperty>"
 W !,"  <owl:ObjectProperty rdf:about=""http://metavista.name/foundation#reads_global"">"
 W !,"    <rdfs:comment>A reference to code that directly reads a MUMPS Global</rdfs:comment>"
 W !,"	  <rdfs:domain rdf:resource=""http://metavista.name/foundation#Routine""/>"
 W !,"	  <rdfs:range rdf:resource=""http://metavista.name/foundation#Global""/>"
 W !,"	  <rdfs:label>reads_global</rdfs:label>"
 W !,"  </owl:ObjectProperty>"
 S PKG=0
 F  S PKG=$O(@GLB@(30,PKG)) Q:PKG=""  D
 . S RTN=""
 . W !," <rdf:Description rdf:about=""http://osehra.org/ns#"_$$RPLCURL(PKG)_""">"
 . F  S RTN=$O(@GLB@(30,PKG,RTN)) Q:RTN=""  D
 . . W !,"   <mv:contains rdf:resource=""http://osehra.org/ns#"_$$RPLCURL(RTN)_"""/>"
 . W !,"   <rdfs:label>"_$$RPLCXML(@GLB@(30,PKG))_"</rdfs:label>"
 . W !,"   <rdf:type rdf:resource=""http://metavista.name/foundation#Package""/>"
 . W !," </rdf:Description>"
 S PKG=0
 F  S PKG=$O(@GLB@(30,PKG)) Q:PKG=""  D
 . S RTN="",GLOB="",CALL=""
 . F  S RTN=$O(@GLB@(30,PKG,RTN)) Q:RTN=""  D
 . . W !," <rdf:Description rdf:about=""http://osehra.org/ns#"_$$RPLCURL(RTN)_""">"
 . . W !,"   <rdf:type rdf:resource=""http://metavista.name/foundation#Routine""/>"
 . . W !,"   <rdfs:label>"_$$RPLCXML(RTN)_"</rdfs:label>"
 . . S GLOB=""
 . . F  S GLOB=$O(@GLB@(30,PKG,RTN,"G",GLOB)) Q:GLOB=""  D
 . . . W !,"   <mv:reads_global rdf:resource=""http://osehra.org/ns#"_$$RPLCURL(GLOB)_"""/>"
 . . S CALL=""
 . . F  S CALL=$O(@GLB@(30,PKG,RTN,"X",CALL)) Q:CALL=""  D
 . . . Q:CALL["|"
 . . . W !,"   <mv:calls rdf:resource=""http://osehra.org/ns#"_$$RPLCURL(CALL)_"""/>"
  . . W !," </rdf:Description>"
 S GLOB=""
 F  S GLOB=$O(@GLB@(30,0,"G",GLOB)) Q:GLOB=""  D
 . W !," <rdf:Description rdf:about=""http://osehra.org/ns#"_$$RPLCURL(GLOB)_""">"
 . W !,"   <rdf:type rdf:resource=""http://metavista.name/foundation#Global""/>"
 . W !,"   <rdfs:label>"_$$RPLCXML(GLOB)_"</rdfs:label>"
 . W !," </rdf:Description>"
 W !,"</rdf:RDF>"
 Q
 ;
INDEX(GLB,CLEAN)
 N PKG,PKGN,RTN,GLOB,CALL
 S:$G(GLB)="" GLB="^ZZRG"
 K:+$G(CLEAN) @GLB@(30)
 S PKG=""
 F  S PKG=$O(@GLB@(4,PKG)) Q:PKG=""  D
 . S PKGN=$$GPKGNAME^ZZRGND19(PKG,GLB)
 . S @GLB@(30,PKG)=PKGN
 S RTN=""
 F  S RTN=$O(@GLB@(1,RTN)) Q:RTN=""  D
 . S PKG=$$GETPKG^ZZRGND19(RTN,GLB)
 . S @GLB@(30,PKG,RTN)=""
 . S GLOB=""
 . F  S GLOB=$O(@GLB@(1,RTN,"G",GLOB)) Q:GLOB=""  D
 . . S @GLB@(30,PKG,RTN,"G",$P(GLOB,"(",1))=""
 . . S @GLB@(30,0,"G",$P(GLOB,"(",1))=""
 . S CALL=""
 . F  S CALL=$O(@GLB@(1,RTN,"X",CALL)) Q:CALL=""  D
 . . S @GLB@(30,PKG,RTN,"X",$P(CALL," ",1))=""
 Q
 ;
RPLCXML(STR)
 N RETVAL
 S RETVAL=$$REPLACE^ZZRGND20(STR,"&","&amp;")
 S RETVAL=$$REPLACE^ZZRGND20(RETVAL,"""","&quot;")
 S RETVAL=$$REPLACE^ZZRGND20(RETVAL,"'","&apos;")
 S RETVAL=$$REPLACE^ZZRGND20(RETVAL,"<","&lt;")
 S RETVAL=$$REPLACE^ZZRGND20(RETVAL,">","&gt;")
 Q RETVAL
 ;
RPLCURL(STR)
 N RETVAL
 S RETVAL=$$REPLACE^ZZRGND20(STR,"&","~")
 S RETVAL=$$REPLACE^ZZRGND20(RETVAL,"$","!")
 S RETVAL=$$REPLACE^ZZRGND20(RETVAL,"%","*")
 S RETVAL=$$REPLACE^ZZRGND20(RETVAL,"^",".")
 Q RETVAL
 ;