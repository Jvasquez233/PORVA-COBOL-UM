      ******************************************************************
      * Author: Jose Angel Vasquez Lopez
      * Date:   17-04-2019
      * Purpose: CREACION DE FICHEROS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMA0.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CLIENTES ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\clientes.dat"
           ORGANISATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS CNPJ
           ALTERNATE RECORD KEY IS Codigo-Cliente
           FILE STATUS IS FILE-STATUS.

           SELECT VENDEDOR ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\vendedor.dat"
           ORGANISATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS CPF
           ALTERNATE RECORD KEY IS Codigo-Vendedor
           FILE STATUS IS FILE-STATUS.

           SELECT TABLA    ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\tabla.dat"
           ORGANISATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS CRLTAB
           FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLIENTES.

       01  REG-CLIENTES.
           05 Codigo-Cliente               PIC 9(007).
           05 CNPJ                         PIC 9(014).
           05 Razão-Social                 PIC X(040).
           05 Latitude-c                   PIC s9(003)v9(008).
           05 Longitude-c                  PIC s9(003)v9(008).

       FD  VENDEDOR.

       01  REG-VENDEDOR.
           05 Codigo-Vendedor              PIC 9(003).
           05 CPF                          PIC 9(011).
           05 Nombre-Vendedor              PIC X(040).
           05 Latitude-v                   PIC s9(003)v9(008).
           05 Longitude-v                  PIC s9(003)v9(008).

       FD  TABLA.

       01  REG-TABLA.
           05 CRLTAB                       PIC 9(003).
           05 Codigo-Tabclie               PIC 9(007).
           05 Codigo-Tabvend               PIC 9(003).

       WORKING-STORAGE SECTION.

       01  AREA-DE-TRABALHO.
           05 FILE-STATUS                  PIC 99       VALUE ZEROES.
           05 SWITCHES-FLAGS               PIC X.
              88 FIN-CLIENTES                           VALUE "F".
              88 NO-FIN-CLIENTES                        VALUE "N".
              88 FIN-VENDEDOR                           VALUE "F".
              88 NO-FIN-VENDEDOR                        VALUE "N".

           PROCEDURE DIVISION.

           PERFORM INICIO
           PERFORM PROCESO
           PERFORM FIN
           STOP RUN.

       INICIO.
           OPEN OUTPUT CLIENTES
                       VENDEDOR
                       TABLA.

       PROCESO.
           INITIALIZE REG-CLIENTES
                      REG-VENDEDOR
                      REG-TABLA

           WRITE REG-CLIENTES

           WRITE REG-VENDEDOR

           MOVE 001     TO CRLTAB
           MOVE  ZEROES TO Codigo-Tabclie
           MOVE  ZEROES TO Codigo-Tabvend
           WRITE REG-TABLA

           CLOSE CLIENTES
                 VENDEDOR

           OPEN I-O CLIENTES
                    VENDEDOR

           MOVE ZEROES   TO CNPJ
           MOVE ZEROES   TO CPF
           READ CLIENTES KEY CNPJ
           IF FILE-STATUS = 00
           DELETE CLIENTES
           END-IF
           READ VENDEDOR KEY CPF
           IF FILE-STATUS = 00
           DELETE VENDEDOR
           END-IF.

       FIN.
           CLOSE CLIENTES
                 VENDEDOR
                 TABLA
           STOP RUN.

       END PROGRAM PROGRAMA0.
