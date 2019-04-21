      ******************************************************************
      * Author:  JOSE ANGEL VASQUEZ LOPEZ
      * Date:    16-04-2019
      * Purpose: ORDENA CLIENTES DE FORMA DESCENDENTE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROSORCLI.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CLIENTE  ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\clientes.dat"
           ORGANISATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS CNPJ
           ALTERNATE RECORD KEY IS Codigo-Cliente
           FILE STATUS IS FILE-STATUS.


           SELECT CLIOUT ASSIGN   "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\cliedecs.txt"
           ORGANISATION IS SEQUENTIAL.

           SELECT WORK ASSIGN     "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\clietemp.txt"
           ORGANISATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CLIENTE.

       01  INPUT-CLI.
           05 Codigo-Cliente               PIC 9(007).
           05 CNPJ                         PIC 9(014).
           05 Razão-Social                 PIC X(040).
           05 Latitude-c                   PIC s9(003)v9(008).
           05 Longitude-c                  PIC s9(003)v9(008).

       FD  CLIOUT.

       01  OUTPUT-CLI.
           05 Codigo-out                   PIC 9(007).
           05 CNPJ-out                     PIC 9(014).
           05 Razão-out                    PIC X(040).
           05 Latitude-out                 PIC s9(003)v9(008).
           05 Longitude-out                PIC s9(003)v9(008).

       SD  WORK.

       01  WORK-CLI.
           05 Codigo-wor                   PIC 9(007).
           05 CNPJ-wor                     PIC 9(014).
           05 Razão-wor                    PIC X(040).
           05 Latitude-wor                 PIC s9(003)v9(008).
           05 Longitude-wor                PIC s9(003)v9(008).

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
              10 LINE-DET                  PIC 99      VALUE ZEROES.
              10 FILE-STATUS               PIC 99      VALUE ZEROES.
              10 CODE-CLI2                 PIC 9(7)    VALUE ZEROES.

           PROCEDURE DIVISION.

           SORT WORK ON DESCENDING KEY Codigo-out
           USING CLIENTE GIVING CLIOUT

           CALL "SYSTEM" USING "Programa4.exe".

       FIN.

       END PROGRAM PROSORCLI.
