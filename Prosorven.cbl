      ******************************************************************
      * Author:  JOSE ANGEL VASQUEZ LOPEZ
      * Date:    16-04-2019
      * Purpose: ORDENA VENDEDOR DE FORMA DESCENDENTE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROSORVEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT VENDEDOR ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\vendedor.dat"
           ORGANISATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS CPF-Ven
           ALTERNATE RECORD KEY IS Codigo-Ven
           FILE STATUS IS FILE-STATUS.

           SELECT VENOUT ASSIGN   "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\venddecs.txt"
           ORGANISATION IS SEQUENTIAL.

           SELECT WORK ASSIGN     "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\vendtemp.txt"
           ORGANISATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  VENDEDOR.

       01  INPUT-VEN.
           05 Codigo-ven                   PIC 9(003).
           05 CPF-ven                      PIC 9(011).
           05 Nome-Ven                     PIC X(040).
           05 Latitude-ven                 PIC s9(003)v9(008).
           05 Longitude-ven                PIC s9(003)v9(008).

       FD  VENOUT.

       01  OUTPUT-VEN.
           05 Codigo-out                   PIC 9(003).
           05 CPF-out                      PIC 9(011).
           05 Nome-out                     PIC X(040).
           05 Latitude-out                 PIC s9(003)v9(008).
           05 Longitude-out                PIC s9(003)v9(008).

       SD  WORK.

       01  WORK-VEN.
           05 Codigo-wor                   PIC 9(003).
           05 CPF-wor                      PIC 9(011).
           05 Nome-wor                     PIC X(040).
           05 Latitude-wor                 PIC s9(003)v9(008).
           05 Longitude-wor                PIC s9(003)v9(008).

       WORKING-STORAGE SECTION.

       01  AREA-DE-TRABALHO.
           05 FIN-FICHERO                  PIC X        VALUE "N".
           05 SWITCHES-FLAGS               PIC X.
              88 FIN-CLIENTES                           VALUE "F".
              88 NO-FIN-CLIENTES                        VALUE "N".

           05 CONSTANT-LITERALES.
              10 LT-PROGRAMA               PIC X(009)  VALUE "PROGRAMA2"
              .
           05 CONSTANTS-NUMERICS.
              10 LINE-DET                  PIC 99      VALUE ZEROES.
              10 FILE-STATUS               PIC 99      VALUE ZEROES.
              10 CODE-CLI2                 PIC 9(7)    VALUE ZEROES.

           PROCEDURE DIVISION.

           SORT WORK ON DESCENDING KEY Codigo-out
           USING VENDEDOR GIVING VENOUT

           CALL "SYSTEM" USING "Programa5.exe".

       FIN.

       END PROGRAM PROSORVEN.
