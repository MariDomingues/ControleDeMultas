      *{Bench}prg-comment
      * P99-R035.cbl
      * P99-R035.cbl is generated from T:\Transport\FONTES\TRABALHO\SISTEMA\P99-R035.Psf
      *{Bench}end
       IDENTIFICATION              DIVISION.
      *{Bench}prgid
       PROGRAM-ID. P99-R035.
       AUTHOR. mariane.aparecida.
       DATE-WRITTEN. terça-feira, 8 de maio de 2018 15:23:01.
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
       COPY "ARQ-MOT01.sl".
       COPY "arq-pro01.sl".
       COPY "arq-vei01.sl".
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
       COPY "ARQ-MOT01.fd".
       COPY "arq-pro01.fd".
       COPY "arq-vei01.fd".
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
       COPY "P99-R035.wrk".
      *{Bench}end
       LINKAGE                     SECTION.
      *{Bench}linkage
       COPY "P99-R035.lks".
      *{Bench}end
       SCREEN                      SECTION.
      *{Bench}copy-screen
       COPY "P99-R035.scr".
      *{Bench}end

      *{Bench}linkpara
       PROCEDURE DIVISION USING lnk-padrao.
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
       ARQ-MOT01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON ARQ-MOT01.
       arq-pro01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-pro01.
       arq-vei01-ERROR SECTION.
           USE AFTER STANDARD EXCEPTION PROCEDURE ON arq-vei01.
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
       COPY "P99-R035.prd".
       COPY "P99-R035.evt".
       COPY "P99-R035.rpt".
      *{Bench}end
       REPORT-COMPOSER SECTION.
      *{Bench}r1-masterprintpara
       Acu-RPT-r1-MASTER-PRINT-LOOP.
           .
      *{Bench}end
            
