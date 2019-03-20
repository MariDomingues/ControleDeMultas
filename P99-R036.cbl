      *{Bench}prg-comment
      * P99-R036.cbl
      * P99-R036.cbl is generated from T:\Transport\FONTES\TRABALHO\SISTEMA\P99-R036.Psf
      *{Bench}end
       IDENTIFICATION              DIVISION.
      *{Bench}prgid
       PROGRAM-ID. P99-R036.
       AUTHOR. mariane.aparecida.
       DATE-WRITTEN. terça-feira, 9 de outubro de 2018 15:56:53.
       REMARKS. 
      *{Bench}end
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SPECIAL-NAMES.
      *{Bench}activex-def
      *{Bench}end
       COPY "\Transport\FONTES\TRABALHO\COPYLIB\Excel.def".
      *{Bench}decimal-point
           DECIMAL-POINT IS COMMA.
      *{Bench}end
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *{Bench}file-control
       COPY "arq-usu.sl".
       COPY "arq-emp00.sl".
       COPY "ARQ-IFC01.sl".
       COPY "ARQ-IFC99.sl".
       COPY "ARQ-IFM01.sl".
       COPY "ARQ-IFS01.sl".
       COPY "ARQ-IFT01.sl".
       COPY "ARQ-IFI01.sl".
       COPY "ARQ-IFR01.sl".
       COPY "T99-R036.sl".
       COPY "arq-txt.sl".
       COPY "ARQ-EMP01.sl".
       COPY "ARQ-MOT01.sl".
       COPY "arq-cid01.sl".
       COPY "arq-par01.sl".
       COPY "arq-pro01.sl".
       COPY "T99-R036-2.sl".
       COPY "ARQ-IFP01.sl".
       COPY "ARQ-BAI09.sl".
       COPY "ARQ-PAG09.sl".
       COPY "arq-vei01.sl".
       COPY "arq-uni15.sl".
      * print sl
       SELECT PRINTF
              ASSIGN TO PRINT PTR-DEV-NAME
              FILE   STATUS   IS STAT-PRINTF.
      *{Bench}end
       DATA                        DIVISION.
       FILE                        SECTION.
      *{Bench}file
       COPY "arq-usu.fd".
       COPY "arq-emp00.fd".
       COPY "ARQ-IFC01.fd".
       COPY "ARQ-IFC99.fd".
       COPY "ARQ-IFM01.fd".
       COPY "ARQ-IFS01.fd".
       COPY "ARQ-IFT01.fd".
       COPY "ARQ-IFI01.fd".
       COPY "ARQ-IFR01.fd".
       COPY "T99-R036.fd".
       COPY "arq-txt.fd".
       COPY "ARQ-EMP01.fd".
       COPY "ARQ-MOT01.fd".
       COPY "arq-cid01.fd".
       COPY "arq-par01.fd".
       COPY "arq-pro01.fd".
       COPY "T99-R036-2.fd".
       COPY "ARQ-IFP01.fd".
       COPY "ARQ-BAI09.fd".
       COPY "ARQ-PAG09.fd".
       COPY "arq-vei01.fd".
       COPY "arq-uni15.fd".
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
       COPY "acureport.def".
       COPY "showmsg.def".
      *{Bench}end

      *{Bench}copy-working
       COPY "P99-R036.wrk".
      *{Bench}end
       01 Excel-Objects.
           05 Hexcelapp         handle of application of excel.
           05 Hexcelwkb         handle of workbook    of excel.
           05 Hexcelwks         handle of worksheet   of excel.
           05 Hrange            handle of range       of excel.
           05 Hcharts           handle of charts      of excel.
       LINKAGE                     SECTION.
      *{Bench}linkage
       COPY "P99-R036.lks".
      *{Bench}end
       SCREEN                      SECTION.
      *{Bench}copy-screen
       COPY "P99-R036.scr".
      *{Bench}end

      *{Bench}linkpara
       PROCEDURE DIVISION USING 
              lnk-padrao, lnk-saida, lnk-tipo, lnk-lancada,
              lnk-em-recurso, lnk-cancelada, lnk-aguardando,
              lnk-baixada, lnk-periodo, lnk-dt-inicial, lnk-dt-final,
              lnk-mot, lnk-pro, lnk-vei, lnk-assinada, lnk-nao-assinada,
              lnk-sem-mot, lnk-codigo, lnk-notificacao, lnk-multa.
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
       arq-emp00-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-emp00.
       ARQ-IFC01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFC01.
       ARQ-IFC99-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFC99.
       ARQ-IFM01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFM01.
       ARQ-IFS01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFS01.
       ARQ-IFT01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFT01.
       ARQ-IFI01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFI01.
       ARQ-IFR01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFR01.
       T99-R036-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON T99-R036.
       arq-txt-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-txt.
       ARQ-EMP01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-EMP01.
       ARQ-MOT01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-MOT01.
       arq-cid01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-cid01.
       arq-par01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-par01.
       arq-pro01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-pro01.
       T99-R036-2-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON T99-R036-2.
       ARQ-IFP01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-IFP01.
       ARQ-BAI09-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-BAI09.
       ARQ-PAG09-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-PAG09.
       arq-vei01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-vei01.
       arq-uni15-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-uni15.
       END DECLARATIVES.
      *{Bench}end

       Acu-Main-Logic.
      *{Bench}entry-befprg
      *    Before-Program
      *{Bench}end
           PERFORM Acu-Initial-Routine
      * run main screen
      *{Bench}run-mainscr
      *{Bench}end
           PERFORM Inicio
           PERFORM Acu-Exit-Rtn
           .

      *{Bench}copy-procedure
       COPY "showmsg.cpy".
       COPY "P99-R036.prd".
       COPY "P99-R036.evt".
       COPY "P99-R036.rpt".
      *{Bench}end
       REPORT-COMPOSER SECTION.
      *{Bench}r1-masterprintpara
       Acu-RPT-r1-MASTER-PRINT-LOOP.
           .
      *{Bench}end
            
