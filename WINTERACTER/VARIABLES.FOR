        SUBROUTINE VARIABLES(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        REAL*8 DW1,DW2,DW3,DW4,DW5
        INTEGER ISET,JSET,KSET,IOPTION,ENDIT,I
        CHARACTER NAME8*8,VIN1*80,VIN2*80,NAME80*80
        LOGICAL DINC,WT,HIGH,LOW
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'RESOURCE.INC'
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE OPERATION OF THE VARIABLES GUI
C
        CALL WDIALOGSELECT(IDD_VARED)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDF_DELALL)
        WRITE(INPUT,*) 'VARI'
        CALL PROCES
        WRITE(INPUT,*) 'EOS'
        CALL PROCES
        CALL VARIFRESH
C
        CASE(IDF_DEL)
        CALL WDIALOGGETSTRING(IDF_VNAMEDEL,NAME8)
        CALL WDIALOGGETINTEGER(IDF_VSURFDEL,ISET)
        CALL WDIALOGGETINTEGER(IDF_VCFGDEL,JSET)
        WRITE(INPUT,*) 'U VB'
        CALL PROCES
        WRITE(INPUT,*) 'DEL ',TRIM(NAME8),',',ISET,',',JSET
        CALL PROCES
        WRITE(INPUT,*) 'EOS'
        CALL PROCES
        CALL VARIFRESH
        CALL WDIALOGCLEARFIELD(IDF_VNAMEDEL)
        CALL WDIALOGCLEARFIELD(IDF_VSURFDEL)
        CALL WDIALOGCLEARFIELD(IDF_VCFGDEL)
        CALL WDIALOGPUTINTEGER(IDF_VCFGDEL,1)
C
        CASE(IDF_VADD)
        VIN1=AA//AA//AA//AA
        VIN2=AA//AA//AA//AA
        DW1=0.0D0
        DW2=0.0D0
        DW3=0.0D0
        DW4=0.0D0
        ISET=-1
        JSET=-1
        NAME8='        '
        WT=.FALSE.
        LOW=.FALSE.
        HIGH=.FALSE.
        DINC=.FALSE.
        CALL WDIALOGGETMENU(IDF_NAMEMENU,IOPTION,NAME80)
        ENDIT=8
        NAME8='        '
                        DO I=1,8
                IF(NAME80(I:I).EQ.'_') THEN
                        ENDIT=I
                        GO TO 1
                        END IF
                        END DO
 1      IF(ENDIT.GT.8) ENDIT=8
        NAME8(1:ENDIT-1)=NAME80(1:ENDIT-1)
        CALL WDIALOGGETINTEGER(IDF_VSURFADD,ISET)
        CALL WDIALOGGETINTEGER(IDF_VCFGADD,JSET)
        CALL WDIALOGGETDOUBLE(IDF_VLOWERADD,DW1)
        CALL WDIALOGGETDOUBLE(IDF_VUPPERADD,DW2)
        CALL WDIALOGGETDOUBLE(IDF_VWTADD,DW3)
        CALL WDIALOGGETDOUBLE(IDF_VDINCRADD,DW4)
        IF(ISET.GE.0.AND.ISET.LE.499
     1  .AND.NAME8.NE.'        ') THEN
        IF(DW1.EQ.0.0D0) THEN
C       DEFAULT LOW
        LOW=.FALSE.
                        ELSE
C       USER SPECIFIES LOW
        LOW=.TRUE.
                        END IF
        IF(DW2.EQ.0.0D0) THEN
C       DEFAULT HIGH
        HIGH=.FALSE.
                        ELSE
C       USER SPECIFIES HIGH
        HIGH=.TRUE.
                        END IF
        IF(DW3.EQ.0.0D0) THEN
C       DEFAULT WT
        WT=.FALSE.
                        ELSE
C       USER SPECIFIES WT
        WT=.TRUE.
                        END IF
        IF(DW4.EQ.0.0D0) THEN
C       DEFAULT DINCR
        DINC=.FALSE.
                        ELSE
C       USER SPECIFIES DINCR
        DINC=.TRUE.
                        END IF
        INPUT='U VB'
        CALL PROCES
        WRITE(INPUT,*) 'CFG,',JSET
        CALL PROCES
        IF(WT) THEN
        WRITE(VIN1,*) NAME8,',',ISET,',',DW3
                        ELSE
        WRITE(VIN1,*) NAME8,',',ISET,',,'
                        END IF
        IF(LOW) THEN
        WRITE(VIN2,*) ',',DW1
                        ELSE
        WRITE(VIN2,*) ',,'
                        END IF
        VIN1=TRIM(VIN1)//TRIM(VIN2)
        IF(HIGH) THEN
        WRITE(VIN2,*) ',',DW2
                        ELSE
        WRITE(VIN2,*) ',,'
                        END IF
        VIN1=TRIM(VIN1)//TRIM(VIN2)
        INPUT=TRIM(VIN1)
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL VARIFRESH
        CALL WDIALOGGETMENU(IDF_NAMEMENU,IOPTION,NAME8)
        CALL WDIALOGCLEARFIELD(IDF_VSURFADD)
        CALL WDIALOGPUTINTEGER(IDF_VCFGADD,1)
        CALL WDIALOGCLEARFIELD(IDF_VLOWERADD)
        CALL WDIALOGCLEARFIELD(IDF_VUPPERADD)
        CALL WDIALOGCLEARFIELD(IDF_VWTADD)
        CALL WDIALOGCLEARFIELD(IDF_VDINCRADD)
        CALL WDIALOGPUTINTEGER(IDF_VSURFADD,0)
        CALL WDIALOGPUTINTEGER(IDF_VCFGADD,0)
        CALL WDIALOGPUTDOUBLE(IDF_VLOWERADD,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_VUPPERADD,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_VWTADD,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_VDINCRADD,0.0D0)
                END IF
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
