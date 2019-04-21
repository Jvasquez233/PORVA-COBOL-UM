      ******************************************************************
      * Author:  JOSE ANGEL VASQUEZ LOPEZ
      * Date:    17-04-2019
      * Purpose: DISTRIBUIÇÃO DE CLIENTES
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMA1.
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

           SELECT RELVENCL ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\relvencli.csv"
           ORGANISATION IS LINE SEQUENTIAL
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
      *
       01  REG-VENDEDOR.
           05 Codigo-Vendedor              PIC 9(003).
           05 CPF                          PIC 9(011).
           05 Nombre-Vendedor              PIC X(040).
           05 Latitude-v                   PIC s9(003)v9(008).
           05 Longitude-v                  PIC s9(003)v9(008).

       FD  RELVENCL BLOCK 107 CHARACTERS.

       01  REG-RELVENCL.
           05 Codigo-Cliente-i             PIC ZZZZZZZ.
           05 FIL-1                        PIC X           VALUE ";".
           05 Razão-Social-i               PIC X(040).
           05 FIL-2                        PIC X           VALUE ";".
           05 Codigo-Vendedor-i            PIC ZZZ.
           05 FIL-3                        PIC X           VALUE ";".
           05 Nombre-Vendedor-i            PIC X(040).
           05 FIL-4                        PIC X           VALUE ";".
           05 Distancia-i                  PIC ZZZ.ZZ9,99-.
           05 FIL-5                        PIC X           VALUE ";".

       WORKING-STORAGE SECTION.

       01  AREA-DE-TRABALHO.
           05 SWITCHES-FLAGS               PIC X.
              88 FIN-CLIENTES                           VALUE "S".
              88 NO-FIN-CLIENTES                        VALUE "N".
              88 FIN-VENDEDOR                           VALUE "S".
              88 NO-FIN-VENDEDOR                        VALUE "N".

           05 Codven-ant                   PIC 9(003)   VALUE ZEROES.
           05 Nomven-ant                   PIC X(040)   VALUE SPACES.

           05 VARIABLES.
              10 FILE-STATUS           PIC 9(002)         VALUE ZEROES.
              10 Dif-Latit             PIC s9(010)v9(008) VALUE ZEROES.
              10 Dif-Longid            PIC s9(010)v9(008) VALUE ZEROES.
              10 Sum-Cuad              PIC s9(010)v9(008) VALUE ZEROES.
              10 Sum-Dif               PIC s9(010)v9(008) VALUE ZEROES.
              10 Metros-Dis            PIC s9(010)v9(002) VALUE ZEROES.
              10 Metros-Sal            PIC s9(010)v9(002) VALUE ZEROES.

       PROCEDURE DIVISION.

           PERFORM INICIO
           PERFORM PROCESO UNTIL FIN-CLIENTES
           PERFORM FIN.

       INICIO.

           OPEN INPUT  CLIENTES
                INPUT  VENDEDOR
                OUTPUT RELVENCL

           SET NO-FIN-CLIENTES TO TRUE
           SET NO-FIN-VENDEDOR TO TRUE

           PERFORM LEER-CLIENTES

           IF NO-FIN-CLIENTES
           PERFORM LEER-VENDEDOR
           IF FIN-VENDEDOR
              SET FIN-CLIENTES TO TRUE
           END-IF
           END-IF.

       PROCESO.

           PERFORM LEER-CALCULAR UNTIL FIN-VENDEDOR

           PERFORM GENERA-FICHERO

           IF FIN-VENDEDOR
           CLOSE VENDEDOR
           OPEN INPUT VENDEDOR
           SET NO-FIN-VENDEDOR TO TRUE
           PERFORM LEER-VENDEDOR
           END-IF
           PERFORM LEER-CLIENTES.

       LEER-CLIENTES.
           MOVE 999999,99        TO Metros-Sal
           READ CLIENTES
                AT END
                SET FIN-CLIENTES TO TRUE
           END-READ.

       LEER-VENDEDOR.
           READ VENDEDOR
                AT END
                SET FIN-VENDEDOR TO TRUE
           END-READ.

       LEER-CALCULAR.

           COMPUTE Dif-Latit  = Latitude-c  - Latitude-v
           IF Dif-Latit < 0
              COMPUTE Dif-Latit = Dif-Latit * -1
           END-IF
           COMPUTE Dif-Longid = Longitude-c - Longitude-v
           IF Dif-Longid < 0
              COMPUTE Dif-Longid = Dif-Longid * -1
           END-IF
           COMPUTE Sum-Dif  = Dif-Latit + Dif-Longid
           COMPUTE Sum-Cuad = Sum-Dif   * Sum-Dif
           COMPUTE Metros-Dis = Sum-Cuad**(1/2)

           IF Metros-Dis < 0
              COMPUTE Metros-Dis = Metros-Dis * -1
           END-IF

           IF Metros-Dis < Metros-Sal
              MOVE Codigo-Vendedor  TO Codven-ant
              MOVE Nombre-Vendedor  TO Nomven-ant
              MOVE Metros-Dis       TO Metros-Sal
           END-IF

           READ VENDEDOR
                AT END
                SET FIN-VENDEDOR TO TRUE
           END-READ.

       GENERA-FICHERO.
           MOVE Codigo-Cliente         TO Codigo-Cliente-i
           MOVE Razão-Social           TO Razão-Social-i
           MOVE Codven-ant             TO Codigo-Vendedor-i
           MOVE Nomven-ant             TO Nombre-Vendedor-i
           MOVE Metros-Sal             TO Distancia-i
           MOVE ";"                    TO FIL-1 FIL-2 FIL-3 FIL-4 FIL-5
           WRITE REG-RELVENCL.

       FIN.
           CLOSE CLIENTES
                 VENDEDOR
                 RELVENCL
           STOP RUN.

       END PROGRAM PROGRAMA1.
