      ******************************************************************
      * Author:  JOSE ANGEL VASQUEZ LOPEZ
      * Date:    16-04-2019
      * Purpose: IMPRIMIR VENDEDORES ASCENDENTES POR CODIGO DE VENDEDOR
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMA3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT VENDEDOR ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\vendedor.dat"
           ORGANISATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS CPF
           ALTERNATE RECORD KEY IS Codigo-Vendedor
           FILE STATUS IS FILE-STATUS.

           SELECT INFORME3 ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\informe3.txt"
           ORGANISATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  VENDEDOR.
      *
       01  REG-VENDEDOR.
           05 Codigo-Vendedor              PIC 9(003).
           05 CPF                          PIC 9(011).
           05 Nome-Vendedor                PIC X(040).
           05 Latitude-v                   PIC s9(003)v9(008).
           05 Longitude-v                  PIC s9(003)v9(008).

       FD  INFORME3.
      *
       01  REG-INFORME3                    PIC X(100).

       WORKING-STORAGE SECTION.

       01  AREA-DE-TRABALHO.
           05 FIN-FICHERO                  PIC X        VALUE "N".
           05 SWITCHES-FLAGS               PIC X.
              88 FIN-CLIENTES                           VALUE "F".
              88 NO-FIN-CLIENTES                        VALUE "N".

           05 CONSTANT-LITERALES.
              10 LT-PROGRAMA               PIC X(009)  VALUE "PROGRAMA3"
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

           05 REGISTRO-INFORME3.
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
                    20 ANNO                PIC X(004)  VALUE SPACES.
                    15 FILLER              PIC X(005)  VALUE SPACES.
                 15 FILLER                 PIC X(070)  VALUE "RELATORIO
      -    " DE VENDEDORES ASCENDENTES POR NUMERO DE CPF ".
              10 LINEA-ENCAB-VEN.
                 15 CODE-CAB               PIC X(87)  VALUE
           "Cven     CPF              R a z a o   S o c i a l
      -    "  Latitude       Longitude ".
                 15 FILLER                 PIC X(013)  VALUE SPACES.
              10 LINEA-BLNKS.
                 15 FILLER                 PIC X(100)  VALUE SPACES.

              10 DETALLE-VEN.
                 15 CODE-VEN               PIC ZZ9.
                 15 FILLER                 PIC X(002)  VALUE SPACES.
                 15 CPF-VEN                PIC X(011)  VALUE SPACES.
                 15 FILLER                 PIC X(002)  VALUE SPACES.
                 15 NAME-VEN               PIC X(040)  VALUE SPACES.
                 15 FILLER                 PIC X(002)  VALUE SPACES.
                 15 LATI-VEN               PIC ZZ9,99999999-.
                 15 FILLER                 PIC X(002)  VALUE SPACES.
                 15 LONGI-VEN              PIC ZZ9,99999999-.

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
           OPEN I-O  VENDEDOR
                OUTPUT INFORME3

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

           PERFORM IMPRIMIR-CAB

           PERFORM LEER-VENDEDOR

           IF FIN-FICHERO = "N"
           PERFORM IMPRIMIR-DET
           END-IF.

       PROCESO.

           IF LINE-DET = 60
           PERFORM IMPRIMIR-CAB
           END-IF
           PERFORM LEER-VENDEDOR

           IF FIN-FICHERO = "N"
           PERFORM IMPRIMIR-DET
           END-IF.

       LEER-VENDEDOR.

           READ VENDEDOR
                AT END
                MOVE "S"      TO   FIN-FICHERO
           END-READ.

       IMPRIMIR-CAB.
           MOVE  ZEROES       TO   LINE-DET
           WRITE REG-INFORME3 FROM NOME-PROGRAMA
           AFTER ADVANCING 1 LINES
           WRITE REG-INFORME3 FROM DATE-PROGRAMA
           AFTER ADVANCING 1 LINES
           WRITE REG-INFORME3 FROM HORA-PROGRAMA
           AFTER ADVANCING 1 LINES
           WRITE REG-INFORME3 FROM LINEA-ENCAB-VEN
           AFTER ADVANCING 1 LINES
           WRITE REG-INFORME3 FROM LINEA-BLNKS
           AFTER ADVANCING 1 LINES.

       IMPRIMIR-DET.
           INITIALIZE DETALLE-VEN
           ADD  1               TO LINE-DET
           MOVE Codigo-Vendedor TO CODE-VEN
           MOVE CPF             TO CPF-VEN
           MOVE Nome-Vendedor   TO NAME-VEN
           MOVE Latitude-v      TO LATI-VEN
           MOVE Longitude-v     TO LONGI-VEN
           WRITE REG-INFORME3 FROM DETALLE-VEN
           AFTER ADVANCING 1 LINES.

       FIN.
           CLOSE VENDEDOR
                 INFORME3.

       END PROGRAM PROGRAMA3.
