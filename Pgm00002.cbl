      ******************************************************************
      * Author:    Jose Angel Vasquez Lopez
      * Date:      16-04-2019
      * Purpose:   Cadastro de Vendedor
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm00002.
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
           RECORD KEY IS CPF
           ALTERNATE RECORD KEY IS Codigo-Vendedor
           FILE STATUS IS FILE-STATUS.

           SELECT TABLA    ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\tabla.dat"
           ORGANISATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS CRLTAB
           FILE STATUS IS FILE-STATUS.

           SELECT VENIMPOR ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\venimpor.csv"
           ORGANISATION IS SEQUENTIAL
           FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  VENDEDOR.

       01  REG-VENDEDOR.
           05 Codigo-Vendedor              PIC 9(003).
           05 CPF                          PIC 9(011).
           05 Nome-Vendedor                PIC X(040).
           05 Latitude-c                   PIC s9(003)v9(008).
           05 Longitude-c                  PIC s9(003)v9(008).

       FD  VENIMPOR.

       01  REG-VENIMPOR.
           05 Codigo-Vendedor-i            PIC 9(003).
           05 FIL-1                        PIC X           VALUE ";".
           05 CPF-i                        PIC 9(011).
           05 FIL-2                        PIC X           VALUE ";".
           05 Nome-Vendedor-i              PIC X(040).
           05 FIL-3                        PIC X           VALUE ";".
           05 Latitude-i                   PIC ZZZ,ZZZZZZZZ-.
           05 FIL-4                        PIC X           VALUE ";".
           05 Longitude-i                  PIC ZZZ,ZZZZZZZZ-.

       FD  TABLA.

       01  REG-TABLA.
           05 CRLTAB                       PIC 9(003).
           05 Codigo-Tabclie               PIC 9(007).
           05 Codigo-Tabvend               PIC 9(003).

       WORKING-STORAGE SECTION.

       01  REG-WORK.
           05 Codigo-W                     PIC 9(003).
           05 CPF-W                        PIC 9(011).
           05 Nome-W                       PIC X(040).
           05 Latitude-W                   PIC s9(003)v9(008).
           05 Longitude-W                  PIC s9(003)v9(008).
       01  AREA-DE-TRABALHO.

           05 FILE-STATUS                  PIC 99       VALUE ZEROES.
           05 OPC-W                        PIC 9        VALUE ZEROES.
           05 SW-AGREGAR                   PIC X        VALUE SPACES.
           05 SW-MODIFIC                   PIC X        VALUE SPACES.
           05 SW-EXCLUC                    PIC X        VALUE SPACES.
           05 SW-IMPORT                    PIC X        VALUE SPACES.
           05 VALID-Codigo                 PIC X        VALUE "N".
           05 VALID-CPF                    PIC X        VALUE "N".
           05 VALID-Nome                   PIC X        VALUE "N".
           05 VALID-Latitude               PIC X        VALUE "N".
           05 VALID-Longitude              PIC X        VALUE "N".

           05 SW-PROCESO                   PIC X        VALUE SPACES.
           05 SW-M                         PIC X        VALUE SPACES.
           05 MENSAGEM                     PIC X(60)    VALUE SPACES.
           05 MENSAGEM2                    PIC X(60)    VALUE SPACES.
           05 SWITCHES-FLAGS               PIC X.
              88 FIN-VENDEDOR                           VALUE "F".
              88 NO-FIN-VENDEDOR                        VALUE "N".

           05 CONSTANT-LITERALES.
              10 LT-PROGRAMA               PIC X(008)  VALUE "PGM00002".
              10 WRK-SAL                   PIC ZZ9,99999999-.
              10 SW-TABLA                PIC X       VALUE SPACES.
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
                 15 FILLER                 PIC X(009) VALUE "PROGRAMA:".
                 15 FILLER                 PIC X(004) VALUE SPACES.
                 15 PROGRAMA               PIC X(008) VALUE "PGM00002".

              10 DATE-PROGRAMA.
                 15 FILLER                 PIC X(006) VALUE "DATE: ".
                 15 FILLER                 PIC X(007)  VALUE SPACES.
                 15 DATE-RESUMEN.
                    20 DIA                 PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE "-".
                    20 MES                 PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE "-".
                    20 SIGLO               PIC X(002)  VALUE SPACES.
                    20 ANNO                PIC X(002)  VALUE SPACES.

              10 HORA-PROGRAMA.
                 15 FILLER                 PIC X(015)  VALUE "HORA INICI
      -    "O:".
                 15 HORA-RESUMEN.
                    20 HORA                PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE ":".
                    20 MINUTOS             PIC X(002)  VALUE SPACES.
                    20 FILLER              PIC X(001)  VALUE ":".
                    20 SEGUNDOS            PIC X(002)  VALUE SPACES.

       PROCEDURE DIVISION.

           PERFORM INICIO
           PERFORM PROCESO
           PERFORM FIN
           STOP RUN.

       INICIO.
             OPEN I-O VENDEDOR
                      TABLA
      *         OPEN    OUTPUT TABLA
      *         OPEN    OUTPUT VENDEDOR
      *         MOVE 001    TO CRLTAB
      *         MOVE ZEROES TO Codigo-Tabclie
      *         MOVE ZEROES TO Codigo-Tabvend
      *         WRITE REG-TABLA

           ACCEPT DATE-AUXILIAR FROM DATE
           MOVE "20"        TO SIGLO
           MOVE DD-AUX      TO DIA
           MOVE MM-AUX      TO MES
           MOVE AA-AUX      TO ANNO

           ACCEPT HORA-AUXILIAR FROM TIME
           MOVE HH-AUX      TO HORA
           MOVE MIN-AUX     TO MINUTOS
           MOVE SEG-AUX     TO SEGUNDOS

           MOVE SPACES   TO MENSAGEM

           INITIALIZE REG-WORK

           MOVE "N"            TO VALID-Codigo
           MOVE "N"            TO VALID-CPF
           MOVE "N"            TO VALID-Nome
           MOVE "N"            TO VALID-Latitude
           MOVE "N"            TO VALID-Longitude
           MOVE "N"            TO SW-PROCESO
           SET NO-FIN-VENDEDOR TO TRUE.

       PROCESO.

           PERFORM UNTIL SW-PROCESO = "S"
           PERFORM LIMPIAR-PANTALLA
           MOVE "N"          TO SW-AGREGAR
           MOVE "N"          TO SW-MODIFIC
           MOVE "N"          TO SW-EXCLUC
           MOVE "N"          TO SW-IMPORT
           MOVE "N"          TO VALID-Codigo
           MOVE "N"          TO VALID-CPF
           MOVE "N"          TO VALID-Nome
           MOVE "N"          TO VALID-Latitude
           MOVE "N"          TO VALID-Longitude
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display HORA-PROGRAMA
           display "                                          "
           display "       CADASTRO DE VENDEDORES             "
           display "         1 => Inclusão                    "
           display "         2 => Alterações                  "
           display "         3 => Exclusão                    "
           display "         4 => Importaçao                  "
           display "                                          "
           display "         9 => Salir                       "
           display "                                          "
           display MENSAGEM
           display "INSIRA OPCAO DESADA"
           display " "with no advancing
           accept OPC-W
           MOVE SPACES   TO MENSAGEM
           EVALUATE OPC-W
                   WHEN = 1
                   PERFORM AGREGAR
                   WHEN = 2
                   PERFORM MODIFICA
                   WHEN = 3
                   PERFORM ELIMINA
                   WHEN = 4
                   PERFORM IMPORTA
                   WHEN = 9
                   MOVE "S" TO SW-PROCESO
                   WHEN OTHER
                   MOVE "N" TO SW-PROCESO
                   MOVE FUNCTION CONCATENATE("Opção " OPC-W " INVALIDA")
                   TO MENSAGEM
                   END-EVALUATE
           END-PERFORM.

       AGREGAR.
           PERFORM UNTIL SW-AGREGAR = "S"
           PERFORM LIMPIAR-PANTALLA
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display "INCLUSAO DE VENDEDORES"
           display MENSAGEM
      ********** VALIDACION DE CPF   *****
           IF VALID-CPF = "N"
           display "INSIRA CPF: "
           display " "with no advancing
           accept CPF-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-CPF
           END-IF
      ********** VALIDACION DE CODIGO   *****
           MOVE "S"                    TO VALID-Codigo
           MOVE "A"                    TO SW-TABLA

           IF VALID-Nome = "N" AND
               (VALID-Codigo = "S" AND VALID-CPF = "S")
           display "INSIRA Nome Vendedor: "
           display " "with no advancing
           accept Nome-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Nome
           END-IF
      ********** VALIDACION DE Latitud   *****
           IF VALID-Latitude = "N" AND
               (VALID-Nome = "S" AND VALID-Codigo = "S"
                AND VALID-CPF = "S")
           display "INSIRA Latitud formato NNN,NNNNNNNN: "
           display " "with no advancing
           accept Latitude-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Latitud
           END-IF
      ********** VALIDACION DE longitud   *****
           IF VALID-Longitude = "N" AND
               (VALID-Latitude = "S" AND VALID-Nome = "S"
                AND VALID-Codigo = "S" AND VALID-CPF = "S")
           display "INSIRA Longitud formato NNN,NNNNNNNN: "
           display " "with no advancing
           accept Longitude-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Longitud
           END-IF

           IF (VALID-Longitude  = "S" AND
               VALID-Latitude   = "S" AND VALID-Nome = "S" AND
               VALID-Codigo = "S" AND VALID-CPF = "S")
               display "Va a MOSTRAR-REGISTRO"
               PERFORM MOSTRAR-REGISTRO
               display "Salio de  MOSTRAR-REGISTRO " OPC-W
               IF OPC-W = 1
                  MOVE REG-WORK     TO REG-VENDEDOR
                  WRITE REG-VENDEDOR
                  MOVE Codigo-W     TO Codigo-Tabvend
                  REWRITE REG-TABLA
                  MOVE "S"          TO SW-AGREGAR
                  INITIALIZE REG-WORK
               END-IF
                  INITIALIZE REG-WORK
                  MOVE "S"          TO SW-AGREGAR
                  MOVE "N"          TO VALID-CPF
                  MOVE "N"          TO VALID-Nome
                  MOVE "N"          TO VALID-Latitude
                  MOVE "N"          TO VALID-Longitude
           END-IF
           END-PERFORM.

       MODIFICA.
           MOVE SPACES                 TO MENSAGEM
           PERFORM UNTIL SW-MODIFIC = "S"
           PERFORM LIMPIAR-PANTALLA
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display "ALTERACAO DE VENDEDORES"
           display MENSAGEM
      ********** VALIDACION DE CPF   *****
           IF VALID-CPF = "N"
           display "INSIRA CPF: "
           display " "with no advancing
           accept CPF-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-CPF-MOD
           END-IF
      ********** VALIDACION DE CODIGO   *****
           MOVE "S"                    TO VALID-Codigo
           MOVE "M"                    TO SW-TABLA

           IF VALID-CPF = "S"

           PERFORM MOSTRAR-REGISTRO-MOD

           IF VALID-Nome = "N" AND
               (VALID-Codigo = "S" AND VALID-CPF = "S")
           display "Alterar Nome Vendedor: "
           display " "with no advancing
           accept Nome-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Nome
           END-IF
      ********** VALIDACION DE Latitud   *****
           IF VALID-Latitude = "N" AND
               (VALID-Nome = "S" AND VALID-Codigo = "S"
                AND VALID-CPF = "S")
           display "Alterar Latitud formato NNN,NNNNNNNN: "
           display " "with no advancing
           accept Latitude-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Latitud
           END-IF
      ********** VALIDACION DE longitud   *****
           IF VALID-Longitude = "N" AND
               (VALID-Latitude = "S" AND VALID-Nome = "S"
                AND VALID-Codigo = "S" AND VALID-CPF = "S")
           display "Alterar Longitud formato NNN,NNNNNNNN: "
           display " "with no advancing
           accept Longitude-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Longitud
           END-IF
           END-IF

           IF (VALID-Longitude  = "S" AND
               VALID-Latitude   = "S" AND VALID-Nome = "S" AND
               VALID-Codigo = "S" AND VALID-CPF = "S")
      *
               MOVE "M"   TO SW-TABLA
               PERFORM MOSTRAR-REGISTRO
               IF OPC-W = 1
                  MOVE REG-WORK     TO REG-VENDEDOR
                  REWRITE REG-VENDEDOR
                  MOVE "S"          TO SW-MODIFIC
               END-IF
                  INITIALIZE REG-WORK
                  MOVE "S"          TO SW-MODIFIC
                  MOVE "N"          TO VALID-CPF
                  MOVE "N"          TO VALID-Nome
                  MOVE "N"          TO VALID-Latitude
                  MOVE "N"          TO VALID-Longitude
           END-IF

           END-PERFORM.

       ELIMINA.
           MOVE SPACES   TO MENSAGEM
           PERFORM UNTIL SW-EXCLUC = "S"
           PERFORM LIMPIAR-PANTALLA
           MOVE "E"                    TO SW-TABLA
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display "EXCLUSAO DE VENDEDORES"
           display MENSAGEM
           display "INSIRA CPF: "
           display " "with no advancing
           accept CPF-W
           MOVE SPACES   TO MENSAGEM
           PERFORM VALIDA-CPF-MOD
           IF VALID-CPF = "S"
           MOVE "E"                 TO SW-TABLA
           PERFORM MOSTRAR-REGISTRO
           IF OPC-W = 1
                  DELETE VENDEDOR
                  MOVE "S"          TO SW-EXCLUC
                  INITIALIZE REG-WORK
               ELSE
                  INITIALIZE REG-WORK
                  MOVE "S"          TO SW-EXCLUC
                  MOVE "N"          TO VALID-CPF
                  MOVE "N"          TO VALID-Nome
                  MOVE "N"          TO VALID-Latitude
                  MOVE "N"          TO VALID-Longitude
           END-IF
           END-IF
           END-PERFORM.

       IMPORTA.
           MOVE SPACES   TO MENSAGEM
           PERFORM UNTIL SW-IMPORT = "S"
           PERFORM LIMPIAR-PANTALLA
           MOVE "I"                    TO SW-TABLA
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display "IMPORTACAO DE VENDEDORES"
           display MENSAGEM
           display "INSIRA CPF: "
           display " "with no advancing
           accept CPF-W
           MOVE SPACES   TO MENSAGEM
           PERFORM VALIDA-CPF-MOD
           IF VALID-CPF = "S"
           MOVE "I"                      TO SW-TABLA
           PERFORM MOSTRAR-REGISTRO
           IF OPC-W = 1
                  OPEN OUTPUT VENIMPOR
                  MOVE ";"               TO FIL-1
                  MOVE ";"               TO FIL-2
                  MOVE ";"               TO FIL-3
                  MOVE ";"               TO FIL-4
                  MOVE Codigo-Vendedor   TO Codigo-Vendedor-i
                  MOVE CPF               TO CPF-i
                  MOVE Nome-Vendedor     TO Nome-Vendedor-i
                  MOVE Latitude-c        TO Latitude-i
                  MOVE Longitude-c       TO Longitude-i
                  WRITE REG-VENIMPOR
                  CLOSE VENIMPOR
                  MOVE "S"               TO SW-IMPORT
                  INITIALIZE REG-WORK
               ELSE
                  INITIALIZE REG-WORK
                  MOVE "S"               TO SW-IMPORT
                  MOVE "N"               TO VALID-CPF
                  MOVE "N"               TO VALID-Nome
                  MOVE "N"               TO VALID-Latitude
                  MOVE "N"               TO VALID-Longitude
           END-IF
           END-IF
           END-PERFORM.

       LIMPIAR-PANTALLA.
           CALL "SYSTEM" USING "cls".


       VALIDA-CPF.
           IF CPF-W <= 0 OR CPF-W IS NOT NUMERIC
           MOVE FUNCTION CONCATENATE("CPF-W " CPF-W " NAO INVALIDO")
                      TO MENSAGEM
           MOVE "N"                   TO VALID-CPF
           ELSE
           MOVE CPF-W                TO CPF
           READ VENDEDOR KEY CPF
           IF FILE-STATUS = 00
           MOVE FUNCTION CONCATENATE("CPF-W " CPF-W " JA EXISTE")
                      TO MENSAGEM
           MOVE "N"                   TO VALID-CPF
           ELSE
           MOVE "S"                   TO VALID-CPF
           END-IF
           END-IF.

       VALIDA-CPF-MOD.
           IF CPF-W <= 0 OR CPF-W IS NOT NUMERIC
           MOVE FUNCTION CONCATENATE("CPF-W " CPF-W " NAO INVALIDO")
                      TO MENSAGEM
           MOVE "N"                   TO VALID-CPF
           ELSE
           MOVE CPF-W                TO CPF
           READ VENDEDOR KEY CPF
           IF FILE-STATUS <> 00
           MOVE FUNCTION CONCATENATE("CPF-W " CPF-W " NO EXISTE")
                      TO MENSAGEM
           MOVE "N"                   TO VALID-CPF
           ELSE
           MOVE "M"                   TO SW-TABLA
           MOVE "S"                   TO VALID-CPF
           END-IF
           END-IF.

       VALIDA-Nome.
           IF Nome-W IS EQUAL SPACES
           MOVE FUNCTION CONCATENATE("Nome " Nome-W
           " NAO INVALIDA")  TO MENSAGEM
           MOVE "N"                   TO VALID-Nome
           ELSE
           MOVE "S"                   TO VALID-Nome
           END-IF.

       VALIDA-Latitud.
           IF Latitude-W <= 0 OR Latitude-W IS NOT NUMERIC
           MOVE ZEROES      TO WRK-SAL
           MOVE Latitude-W TO WRK-SAL
           MOVE FUNCTION CONCATENATE("Latitud " WRK-SAL
           " NAO INVALIDA")  TO MENSAGEM
           MOVE "N"                   TO VALID-Latitude
           ELSE
           MOVE "S"                   TO VALID-Latitude
           END-IF.

       VALIDA-Longitud.
           IF Longitude-W <= 0 OR Longitude-W IS NOT NUMERIC
           MOVE ZEROES      TO WRK-SAL
           MOVE Longitude-W TO WRK-SAL
           MOVE FUNCTION CONCATENATE("Longitud " WRK-SAL
           " NAO INVALIDA")  TO MENSAGEM
           MOVE "N"                   TO VALID-Longitude
           ELSE
           MOVE "S"                   TO VALID-Longitude
           END-IF.

       MOSTRAR-REGISTRO.
           MOVE SPACES TO MENSAGEM2
           EVALUATE (SW-TABLA)
               WHEN = "A"
                MOVE "OPCAO 1 GRAVAR    OPCAO  2 SAIR" TO MENSAGEM2
                MOVE 001        TO CRLTAB
                READ TABLA KEY CRLTAB
                IF FILE-STATUS = 00
                MOVE ZEROES     TO Codigo-W
                COMPUTE Codigo-W = Codigo-Tabvend + 1
                MOVE REG-WORK   TO REG-VENDEDOR
                END-IF
               WHEN = "E"
                MOVE "OPCAO 1 Exclusão  OPCAO  2 SAIR" TO MENSAGEM2
                MOVE REG-VENDEDOR TO REG-WORK
               WHEN = "M"
                MOVE "OPCAO 1 Alterações OPCAO  2 SAIR" TO MENSAGEM2
               WHEN = "I"
                MOVE REG-VENDEDOR TO REG-WORK
                MOVE "OPCAO 1 Importação OPCAO  2 SAIR" TO MENSAGEM2
           END-EVALUATE

           MOVE "N"    TO SW-M
           PERFORM UNTIL SW-M = "S"
           PERFORM LIMPIAR-PANTALLA
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display "VERIFQUE A INFORMACAO"
           display " "
           display "Código Vendedor: " Codigo-W
           display "CPF          : " CPF-W
           display "Nome Vendedor  : " Nome-W
           MOVE ZEROES       TO WRK-SAL
           MOVE Latitude-W   TO WRK-SAL
           display "Latitude       : " WRK-SAL
           MOVE ZEROES       TO WRK-SAL
           MOVE Longitude-W  TO WRK-SAL
           display "Longitude     : " WRK-SAL
           display " "
           display MENSAGEM2
           display MENSAGEM
           display "INSIRA OPCAO: "
           display " "with no advancing
           accept OPC-W
           MOVE SPACES   TO MENSAGEM
           IF OPC-W <> 1 AND OPC-W <> 2
                MOVE "N" TO SW-M
                MOVE FUNCTION CONCATENATE("Opção " OPC-W " INVALIDA")
                TO MENSAGEM
              ELSE
                MOVE "S" TO SW-M
           END-IF
           END-PERFORM.

       MOSTRAR-REGISTRO-MOD.
           PERFORM LIMPIAR-PANTALLA
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display "ALTERE A INFORMACAO"
           display " "
           MOVE Codigo-Vendedor    TO Codigo-W
           MOVE Nome-Vendedor      TO Nome-W
           MOVE Latitude-c         TO Latitude-W
           MOVE Longitude-c        TO Longitude-W
           display "Código Vendedor: " Codigo-W
           display "CPF           : " CPF-W
           display "Nome Vendedor  : " Nome-W
           MOVE ZEROES       TO WRK-SAL
           MOVE Latitude-W   TO WRK-SAL
           display "Latitude       : " WRK-SAL
           MOVE ZEROES       TO WRK-SAL
           MOVE Longitude-W  TO WRK-SAL
           display "Longitude     : " WRK-SAL
           display " ".

       FIN.
           CLOSE VENDEDOR TABLA
           STOP RUN.

       END PROGRAM Pgm00002.
