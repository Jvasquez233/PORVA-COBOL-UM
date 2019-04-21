      ******************************************************************
      * Author:    Jose Angel Vasquez Lopez
      * Date:      17-04-2019
      * Purpose:   Menu Principal
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm00000.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  AREA-DE-TRABALHO.

           05 FILE-STATUS                  PIC 99       VALUE ZEROES.
           05 OPC-W                        PIC 9        VALUE ZEROES.
           05 SW-AGREGAR                   PIC X        VALUE SPACES.
           05 SW-MODIFIC                   PIC X        VALUE SPACES.
           05 SW-EXCLUC                    PIC X        VALUE SPACES.
           05 SW-IMPORT                    PIC X        VALUE SPACES.
           05 VALID-Codigo                 PIC X        VALUE "N".
           05 VALID-CNPJ                   PIC X        VALUE "N".
           05 VALID-Razão                  PIC X        VALUE "N".
           05 VALID-Latitude               PIC X        VALUE "N".
           05 VALID-Longitude              PIC X        VALUE "N".

           05 SW-PROCESO                   PIC X        VALUE SPACES.
           05 SW-M                         PIC X        VALUE SPACES.
           05 MENSAGEM                     PIC X(60)    VALUE SPACES.
           05 MENSAGEM2                    PIC X(60)    VALUE SPACES.
           05 SWITCHES-FLAGS               PIC X.
              88 FIN-CLIENTES                           VALUE "F".
              88 NO-FIN-CLIENTES                        VALUE "N".

           05 CONSTANT-LITERALES.
              10 LT-PROGRAMA               PIC X(008)  VALUE "PGM00001".
              10 WRK-SAL                   PIC ZZ9,V99999999-.
              10 SW-TABLA                  PIC X       VALUE SPACES.
           05 CONSTANTS-NUMERICS.
              10 DATE-AUXILIAR.
                 15 AA-AUX                 PIC X(002)  VALUE SPACES.
                 15 MM-AUX                 PIC X(002)  VALUE SPACES.
                 15 DD-AUX                 PIC X(002)  VALUE SPACES.
              10 HORA-AUXILIAR.
                 15 HH-AUX                 PIC X(002)  VALUE SPACES.
                 15 MIN-AUX                PIC X(002)  VALUE SPACES.
                 15 SEG-AUX                PIC X(002)  VALUE SPACES.
                 15 MILI-AUX               PIC X(002)  VALUE SPACES.

           05 REGISTRO-INFORME1.
              10 NOME-PROGRAMA.
                 15 FILLER                 PIC X(015) VALUE "PROGRAMA:".
                 15 PROGRAMA               PIC X(008) VALUE "PGM00000".
                 15 FILLER                 PIC X(057) VALUE SPACES.

              10 DATE-PROGRAMA.
                 15 FILLER                 PIC X(015)  VALUE "DATE: ".
                 15 DATE-RESUMEN.
                    20 DIA                 PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE "-".
                    20 MES                 PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE "-".
                    20 SIGLO               PIC X(002)  VALUE SPACES.
                    20 ANNO                PIC X(002)  VALUE SPACES.
                 15 FILLER                 PIC X(055)  VALUE SPACES.

              10 HORA-PROGRAMA.
                 15 FILLER                 PIC X(015)  VALUE "HORA INICI
      -    "O:".
                 15 HORA-RESUMEN.
                    20 HORA                PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE ":".
                    20 MINUTOS             PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE ":".
                    20 SEGUNDOS            PIC X(002)  VALUE SPACES.
                 15 FILLER                 PIC X(057)  VALUE SPACES.
       PROCEDURE DIVISION.

           PERFORM INICIO
           PERFORM PROCESO
           PERFORM FIN.

       INICIO.
           ACCEPT DATE-AUXILIAR FROM DATE
           MOVE "20"        TO SIGLO
           MOVE DD-AUX      TO DIA
           MOVE MM-AUX      TO MES
           MOVE AA-AUX      TO ANNO
           ACCEPT HORA-AUXILIAR FROM TIME
           MOVE HH-AUX      TO HORA
           MOVE MIN-AUX     TO MINUTOS
           MOVE SEG-AUX     TO SEGUNDOS.

       PROCESO.
           MOVE "N"          TO SW-PROCESO
           MOVE SPACES       TO MENSAGEM
           PERFORM UNTIL SW-PROCESO = "S"
           PERFORM LIMPIAR-PANTALLA
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display HORA-PROGRAMA
           display "    MENU PRINCIPAL DE OPÇÕES              "
           display "                                          "
           display "      C A D A S T R O S                   "
           display "       1 => CLIENTES                      "
           display "       2 => VENDEDORES                    "
           display "                                          "
           display "     R E L A T O R I O S                  "
           display "   3 => CLIENTES ASCENDENTES              "
           display "   4 => CLIENTES DESCENDENTES             "
           display "   5 => VENDEDORES ASCENDENTES            "
           display "   6 => VENDEDORES DESCENDENTES           "
           display "                                          "
           display "       E X E C U T A R                    "
           display "  7 => DISTRIBUIÇÃO DE CLIENTES           "
           display "                                          "
           display "  9 => Salir                              "
           display MENSAGEM
           display "INSIRA OPCAO DESADA"
                      display " "with no advancing
           accept OPC-W
           MOVE SPACES   TO MENSAGEM
           EVALUATE OPC-W
                   WHEN = 1
                   CALL 'SYSTEM' USING 'PGM00001.exe'
                   WHEN = 2
                   CALL "SYSTEM" USING "PGM00002.exe"
                   WHEN = 3
                   CALL "SYSTEM" USING "Programa2.exe"
                   WHEN = 4
                   CALL "SYSTEM" USING "Prosorcli.exe"
                   WHEN = 5
                   CALL "SYSTEM" USING "Programa3.exe"
                   WHEN = 6
                   CALL "SYSTEM" USING "Prosorven.exe"
                   WHEN = 7
                   CALL "SYSTEM" USING "Programa1.exe"
                   WHEN = 9
                   MOVE "S" TO SW-PROCESO
                   WHEN OTHER
                   MOVE "N" TO SW-PROCESO
                   MOVE FUNCTION CONCATENATE("Opção " OPC-W " INVALIDA")
                   TO MENSAGEM
           END-EVALUATE
           END-PERFORM.

       LIMPIAR-PANTALLA.
           CALL "SYSTEM" USING "cls".

       FIN.
           STOP RUN.

       END PROGRAM Pgm00000.
