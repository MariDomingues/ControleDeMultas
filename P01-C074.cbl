      *{Bench}prg-comment
      * P01-C074.cbl
      * P01-C074.cbl is generated from T:\Transport\FONTES\TRABALHO\CTRC\P01-C074.Psf
      *{Bench}end
       IDENTIFICATION              DIVISION.
      *{Bench}prgid
       PROGRAM-ID. P01-C074.
       AUTHOR. mariane.aparecida.
       DATE-WRITTEN. terça-feira, 9 de outubro de 2018 16:01:35.
       REMARKS. 
      *{Bench}end
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SPECIAL-NAMES.
      *{Bench}activex-def
      *{Bench}end
      *{Bench}decimal-point
           DECIMAL-POINT IS COMMA.
      *{Bench}end
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *{Bench}file-control
       COPY "arq-usu.sl".
       COPY "arq-usu-prg.sl".
       COPY "arq-prg.sl".
       COPY "ARQ-LOG123.sl".
       COPY "ARQ-LOG23.sl".
       COPY "arq-emp00.sl".
       COPY "ARQ-VEI01.sl".
       COPY "ARQ-MOT01.sl".
       COPY "arq-cid01.sl".
       COPY "ARQ-PRO01.sl".
       COPY "ARQ-TIP01.sl".
       COPY "T01-C074-3.sl".
       COPY "T01-C074-4.sl".
       COPY "T01-C074-5.sl".
       COPY "ARQ-IFC01.sl".
       COPY "ARQ-IFS01.sl".
       COPY "ARQ-IFM01.sl".
       COPY "ARQ-IFI01.sl".
       COPY "ARQ-IFC99.sl".
       COPY "arq-for15.sl".
       COPY "ARQ-IFP01.sl".
       COPY "arq-uni15.sl".
       COPY "ARQ-IFR01.sl".
       COPY "ARQ-BAI09.sl".
       COPY "ARQ-PAG09.sl".
      * print sl
       SELECT PRINTF
              ASSIGN TO PRINT PTR-DEV-NAME
              FILE   STATUS   IS STAT-PRINTF.
      *{Bench}end
       DATA                        DIVISION.
       FILE                        SECTION.
      *{Bench}file
       COPY "arq-usu.fd".
       COPY "arq-usu-prg.fd".
       COPY "arq-prg.fd".
       COPY "ARQ-LOG123.fd".
       COPY "ARQ-LOG23.fd".
       COPY "arq-emp00.fd".
       COPY "ARQ-VEI01.fd".
       COPY "ARQ-MOT01.fd".
       COPY "arq-cid01.fd".
       COPY "ARQ-PRO01.fd".
       COPY "ARQ-TIP01.fd".
       COPY "T01-C074-3.fd".
       COPY "T01-C074-4.fd".
       COPY "T01-C074-5.fd".
       COPY "ARQ-IFC01.fd".
       COPY "ARQ-IFS01.fd".
       COPY "ARQ-IFM01.fd".
       COPY "ARQ-IFI01.fd".
       COPY "ARQ-IFC99.fd".
       COPY "arq-for15.fd".
       COPY "ARQ-IFP01.fd".
       COPY "arq-uni15.fd".
       COPY "ARQ-IFR01.fd".
       COPY "ARQ-BAI09.fd".
       COPY "ARQ-PAG09.fd".
      * print fd
       FD PRINTF    LABEL   RECORD  OMITTED.
       01 PRINTF-R.
          05 PRINTF-01              PIC X OCCURS 1024 TIMES.
      *{Bench}end
       WORKING-STORAGE             SECTION.
      *{Bench}acu-def
       COPY "acugui.def".
       COPY "acucobol.def".
       COPY "crtvars.def".
       COPY "fonts.def".
       COPY "acureport.def".
       COPY "showmsg.def".
      *{Bench}end

      *{Bench}copy-working
       COPY "P01-C074.wrk".
      *{Bench}end
       LINKAGE                     SECTION.
      *{Bench}linkage
       COPY "P01-C074.lks".
      *{Bench}end
       SCREEN                      SECTION.
      *{Bench}copy-screen
       COPY "P01-C074.scr".
      *{Bench}end

      *{Bench}linkpara
       PROCEDURE DIVISION USING lnk-padrao, LNK-IFC01-CODIGO.
      *{Bench}end
      *{Bench}declarative
       DECLARATIVES.
       INPUT-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON INPUT.
       0100-DECL.
           EXIT.
       I-O-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON I-O.
       0200-DECL.
           EXIT.
       OUTPUT-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON OUTPUT.
       0300-DECL.
           EXIT.
       arq-usu-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-usu.
       arq-usu-prg-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-usu-prg.
       arq-prg-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-prg.
       ARQ-LOG123-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-LOG123.
       ARQ-LOG23-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-LOG23.
       arq-emp00-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-emp00.
       ARQ-VEI01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-VEI01.
       ARQ-MOT01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-MOT01.
       arq-cid01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-cid01.
       ARQ-PRO01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-PRO01.
       ARQ-TIP01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-TIP01.
       T01-C074-3-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON T01-C074-3.
       T01-C074-4-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON T01-C074-4.
       T01-C074-5-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON T01-C074-5.
       ARQ-IFC01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFC01.
       ARQ-IFS01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFS01.
       ARQ-IFM01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFM01.
       ARQ-IFI01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFI01.
       ARQ-IFC99-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFC99.
       arq-for15-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-for15.
       ARQ-IFP01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFP01.
       arq-uni15-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-uni15.
       ARQ-IFR01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFR01.
       ARQ-BAI09-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-BAI09.
       ARQ-PAG09-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-PAG09.
       END DECLARATIVES.
      *{Bench}end

       Acu-Main-Logic.
      *{Bench}entry-befprg
      *    Before-Program
      *{Bench}end
           PERFORM Acu-Initial-Routine
      * run main screen
      *{Bench}run-mainscr
           PERFORM Acu-t1-Routine
      *{Bench}end
           PERFORM Acu-Exit-Rtn
           .

      *{Bench}copy-procedure
       COPY "showmsg.cpy".
       COPY "P01-C074.prd".
       COPY "P01-C074.evt".
       COPY "P01-C074.rpt".
      *{Bench}end
       REPORT-COMPOSER SECTION.
      *{Bench}r1-masterprintpara
       Acu-RPT-r1-MASTER-PRINT-LOOP.
           .
      *{Bench}end
            
