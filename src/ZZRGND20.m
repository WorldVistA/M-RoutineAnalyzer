ZZRGND20 ;;CBR/AU - UTILITY TAGS ;08/15/12
 ;;1.0;RGI Dependency Tool;**260004**;08/15/2012
READFILE(FILENAME,COMMAND,PATH)
 ; Opens a file, and for each line of text calls the TAG specified in COMMAND 
 ;   FILENAME - (required) complete file name without path
 ;   COMMAND  - (required) tag to be called. It should accept the line of text as first parameter.
 ;   PATH     - (optional) file path ending in /
 Q:$G(FILENAME)=""
 Q:$G(COMMAND)=""
 D TEXTOPEN^ZZRGND23("INFILE",PATH,FILENAME,"R")  ;
 F  Q:$$STATUS^%ZISH  D
 . R LINE 
 . S TAG=COMMAND_"("""_$$DBLQUOT(LINE)_""")"
 . D @TAG
 D TEXTCLS^ZZRGND23("INFILE",FILENAME)  ; 
 Q
 ;
GCSVFLDS(LINE,FIELDS,SEP)
 ; Returns fields from a .CSV file
 ;   LINE = line of text from the CSV file
 ;   FIELDS = array passed by reference where the field values will be stored
 ;   SEP (optional) = separator, default ','
 Q:$G(LINE)=""
 N QUOTE,CNT,STOP,VAL
 S:$G(SEP)="" SEP=","
 S SEP=$E(SEP)
 K FIELDS
 S QUOTE=0,CNT=0,STOP=0,VAL=""
 F I=1:1:$L(LINE) D
 . S CH=$E(LINE,I)
 . I CH=SEP,'QUOTE D  Q
 . . S CNT=CNT+1
 . . S:$E(VAL,$L(VAL))="""" VAL=$E(VAL,1,$L(VAL)-1)
 . . S FIELDS=CNT
 . . S FIELDS(CNT)=VAL
 . . S VAL=""
 . I CH="""" S QUOTE='QUOTE
 . Q:QUOTE&(CH="""")
 . S VAL=VAL_CH
 S:$E(VAL,$L(VAL))="""" VAL=$E(VAL,1,$L(VAL)-1)
 Q
 ;
LTRIM(STR,CH)
 ; Trims characters passed in CH from the left side of a string
 N I,CX,TO
 S TO=0,CH=$E(CH)
 F I=1:1:$L(STR,CH) D
 . S CX=$E(STR,I)
 . Q:CX'=CH
 . S TO=I
 S:TO>0 STR=$E(STR,I,$L(STR))
 Q STR
 ;
DBLQUOT(STR)
 ; Doubles quotes in a string
 N I,CH,RETVAL
 S RETVAL=""
 F I=1:1:$L(STR) D
 . S CH=$E(STR,I)
 . S RETVAL=RETVAL_CH
 . S:CH="""" RETVAL=RETVAL_CH
 Q RETVAL
 ;
REPLACE(STR,CH1,CH2)
 N I,CH,RETVAL
 S RETVAL=""
 F I=1:1:$L(STR) D
 . S CH=$E(STR,I)
 . S:CH=CH1 CH=CH2
 . S RETVAL=RETVAL_CH
 Q RETVAL
 ;
