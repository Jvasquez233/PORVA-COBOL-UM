      ******************************************************************
      * Author:  JOSE ANGEL VASQUEZ LOPEZ
      * Date:    16-04-2019
      * Purpose: IMPRIMIR CLIENTES DESCENDENTES POR CODIGO DE CLIENTES
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMA4.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CLIOUT ASSIGN   "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\cliedecs.txt"
           ORGANISATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL.

           SELECT INFORME4 ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\informe4.txt"
           ORGANISATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CLIOUT.

       01  REG-CLIOUT.
           05 Codigo-Cliente               PIC 9(007).
           05 CNPJ                         PIC 9(014).
           05 Razão-Social                 PIC X(040).
           05 Latitude-c                   PIC s9(003)v9(008).
           05 Longitude-c                  PIC s9(003)v9(008).

       FD  INFORME4.
      *
       01  REG-INFORME4                    PIC X(100).

       WORKING-STORAGE SECTION.

       01  AREA-DE-TRABALHO.
           05 FIN-FICHERO                  PIC X        VALUE "N".
           05 SWITCHES-FLAGS               PIC X.
              88 FIN-CLIENTES                           VALUE "F".
              88 NO-FIN-CLIENTES                        VALUE "N".

           05 CONSTANT-LITERALES.
              10 LT-PROGRAMA               PIC X(009)  VALUE "PROGRAMA4"
              .
           05 CONSTANTS-NUMERICS.

              10 FILE-STATUS               PIC 99      VALUE ZEROES.
              10 LINE-DET                  PIC 99      VALUE ZEROES.
              10 CODE-CLI2                 PIC 9(7)    VALUE ZEROES.
              10 DATE-AUXILIAR.
                 15 AA-AUX                 PIC X(002)  VALUE SPACES.
                 15 MM-AUX                 PIC X(002)  VALUE SPACES.
                 15 DD-AUX                 PIC X(002)  VALUE SPACES.
              10 HORA-AUXILIAR.
                 15 HH-AUX                 PIC X(002)  VALUE SPACES.
                 15 MIN-AUX                PIC X(002)  VALUE SPACES.
                 15 SEG-AUX                PIC X(002)  VALUE SPACES.
                 15 MILI-AUX               PIC X(002)  VALUE SPACES.

           05 REGISTRO-INFORME4.
              10 NOME-PROGRAMA.
                 15 FILLER                 PIC X(015) VALUE "PROGRAMA:".
                 15 PROGRAMA               PIC X(009) VALUE SPACES.
                 15 FILLER                 PIC X(076) VALUE SPACES.

              10 DATE-PROGRAMA.
                 15 FILLER                 PIC X(015)  VALUE "DATE: ".
                 15 DATE-RESUMEN.
                    20 DIA                 PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE "-".
                    20 MES                 PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE "-".
                    20 SIGLO               PIC X(002)  VALUE SPACES.
                    20 ANNO                PIC X(002)  VALUE SPACES.
                 15 FILLER                 PIC X(005)  VALUE SPACES.
                 15 FILLER                 PIC X(070)  VALUE "RELATORIO
      -    " DE CLIENTES DESCENDENTE POR CODIGO DE CLIENTE".

              10 LINEA-ENCAB-CLI.
                 15 CODE-CAB               PIC X(96)  VALUE
           "Cod Cliente       CNPJ             R a z a o   S o c i a l
      -    "           Latitude       Longitude ".
                 15 FILLER                 PIC X(004)  VALUE SPACES.
              10 LINEA-BLNKS.
                 15 FILLER                 PIC X(100)  VALUE SPACES.

              10 DETALLE-CLI.
                 15 CODE-CLI               PIC Z.ZZZ.ZZ9.
                 15 FILLER                 PIC X(002)  VALUE SPACES.
                 15 CNPJ-CLI               PIC X(014)  VALUE SPACES.
                 15 FILLER                 PIC X(002)  VALUE SPACES.
                 15 NAME-CLI               PIC X(040)  VALUE SPACES.
                 15 FILLER                 PIC X(002)  VALUE SPACES.
                 15 LATI-CLI               PIC ZZ9,99999999-.
                 15 FILLER                 PIC X(002)  VALUE SPACES.
                 15 LONGI-CLI              PIC ZZ9,99999999-.

              10 HORA-PROGRAMA.
                 15 FILLER                 PIC X(015)  VALUE "HORA INICI
      -    "O:".
                 15 HORA-RESUMEN.
                    20 HORA                PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE ":".
                    20 MINUTOS             PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE ":".
                    20 SEGUNDOS            PIC X(002)  VALUE SPACES.
                 15 FILLER                 PIC X(077)  VALUE SPACES.

       PROCEDURE DIVISION.

           PERFORM INICIO
           PERFORM PROCESO UNTIL FIN-FICHERO = "S"
           PERFORM FIN
           STOP RUN.

       INICIO.
           OPEN INPUT  CLIOUT
                OUTPUT INFORME4

           MOVE"N"          TO FIN-FICHERO
           MOVE LT-PROGRAMA TO PROGRAMA

           ACCEPT DATE-AUXILIAR FROM DATE
           MOVE "20"        TO SIGLO
           MOVE DD-AUX      TO DIA
           MOVE MM-AUX      TO MES
           MOVE AA-AUX      TO ANNO

           ACCEPT HORA-AUXILIAR FROM TIME
           MOVE HH-AUX      TO HORA
           MOVE MIN-AUX     TO MINUTOS
           MOVE SEG-AUX     TO SEGUNDOS

           PERFORM IMPRIMIR-CAB.

       PROCESO.

           IF LINE-DET = 60
           PERFORM IMPRIMIR-CAB
           END-IF

           PERFORM LEER-CLIOUT

           IF FIN-FICHERO = "N"
           PERFORM IMPRIMIR-DET
           END-IF.

       LEER-CLIOUT.

           READ CLIOUT
                AT END
                MOVE "S"      TO   FIN-FICHERO
           END-READ.

       IMPRIMIR-CAB.
           MOVE  ZEROES       TO   LINE-DET
           WRITE REG-INFORME4 FROM NOME-PROGRAMA
           AFTER ADVANCING 1 LINES
           WRITE REG-INFORME4 FROM DATE-PROGRAMA
           AFTER ADVANCING 1 LINES
           WRITE REG-INFORME4 FROM HORA-PROGRAMA
           AFTER ADVANCING 1 LINES
           WRITE REG-INFORME4 FROM LINEA-ENCAB-CLI
           AFTER ADVANCING 1 LINES
           WRITE REG-INFORME4 FROM LINEA-BLNKS
           AFTER ADVANCING 1 LINES.

       IMPRIMIR-DET.
           INITIALIZE DETALLE-CLI
           ADD  1              TO LINE-DET
           MOVE Codigo-Cliente TO CODE-CLI
           MOVE CNPJ           TO CNPJ-CLI
           MOVE Razão-Social   TO NAME-CLI
           MOVE Latitude-c     TO LATI-CLI
           MOVE Longitude-c    TO LONGI-CLI
           WRITE REG-INFORME4 FROM DETALLE-CLI
           AFTER ADVANCING 1 LINES.

       FIN.
           CLOSE CLIOUT
                 INFORME4
           STOP RUN.

       END PROGRAM PROGRAMA4.
