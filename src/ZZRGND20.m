ZZRGND20 ;CBR/ - UTILITY TAGS ;02/01/12
 ;;7.3;TOOLKIT;**to be determined**;
READFILE(FILENAME,COMMAND,PATH)
 ; Opens a file, and for each line of text calls the TAG specified in COMMAND 
 ;   FILENAME - (required) complete file name without path
 ;   COMMAND  - (required) tag to be called. It should accept the line of text as first parameter.
 ;   PATH     - (optional) file path ending in /
 Q:$G(FILENAME)=""
 Q:$G(COMMAND)=""
 N OLDIO,LINE,EOF,FILE,TAG
 S OLDIO=$IO,EOF=0
 I $G(PATH)'="" S FILE=PATH_FILENAME
 E  W !,"Enter "_FILENAME_" path: " R FILE
 S $ET="S EOF=$$EOF1END^ZZRGND20(FILE)"
 O FILE:"R":5 U FILE
 F  Q:EOF  D
 . R LINE 
 . S TAG=COMMAND_"("""_$$DBLQUOT(LINE)_""")"
 . D @TAG
 Q
 ;
EOF1END(FILE)
 S $ET=""
 C FILE
 I $ZE["<ENDOFFILE>" S $EC=""
 I $ZE["<NOTOPEN>" S $EC="" W !,"Path not found!"
 U OLDIO
 Q 1
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