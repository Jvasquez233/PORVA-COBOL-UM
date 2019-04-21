      ******************************************************************
      * Author:    Jose Angel Vasquez Lopez
      * Date:      16-04-2019
      * Purpose:   Cadastro de Clientes
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm00001.
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

           SELECT TABLA    ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\tabla.dat"
           ORGANISATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS CRLTAB
           FILE STATUS IS FILE-STATUS.

           SELECT CLIIMPOR ASSIGN "C:\Users\Jose Angel\Documents\cobol\f
      -    "iles\cliimpor.csv"
           ORGANISATION IS SEQUENTIAL
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

       FD  CLIIMPOR.

       01  REG-CLIIMPOR.
           05 Codigo-Cliente-i             PIC 9(007).
           05 FIL-1                        PIC X           VALUE ";".
           05 CNPJ-i                       PIC 9(014).
           05 FIL-2                        PIC X           VALUE ";".
           05 Razão-Social-i               PIC X(040).
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
           05 Codigo-W                     PIC 9(007).
           05 CNPJ-W                       PIC 9(014).
           05 Razão-W                      PIC X(040).
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
              10 WRK-SAL                   PIC ZZ9,99999999-.
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
                 15 FILLER                 PIC X(009) VALUE "PROGRAMA:".
                 15 FILLER                 PIC X(004) VALUE SPACES.
                 15 PROGRAMA               PIC X(008) VALUE "PGM00001".

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
              OPEN I-O CLIENTES
                      TABLA

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
           MOVE "N"            TO VALID-CNPJ
           MOVE "N"            TO VALID-Razão
           MOVE "N"            TO VALID-Latitude
           MOVE "N"            TO VALID-Longitude
           MOVE "N"            TO SW-PROCESO
           SET NO-FIN-CLIENTES TO TRUE.

       PROCESO.

           PERFORM UNTIL SW-PROCESO = "S"
           PERFORM LIMPIAR-PANTALLA
           MOVE "N"          TO SW-AGREGAR
           MOVE "N"          TO SW-MODIFIC
           MOVE "N"          TO SW-EXCLUC
           MOVE "N"          TO SW-IMPORT
           MOVE "N"          TO VALID-Codigo
           MOVE "N"          TO VALID-CNPJ
           MOVE "N"          TO VALID-Razão
           MOVE "N"          TO VALID-Latitude
           MOVE "N"          TO VALID-Longitude
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display HORA-PROGRAMA
           display "                                          "
           display "       CADASTRO CLIENTES                  "
           display "         1 => Inclusão                    "
           display "         2 => Alteração                   "
           display "         3 => Exclusão                    "
           display "         4 => Importação                  "
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
           display HORA-PROGRAMA
           display "INCLUSAO DE CLIENTES"
           display MENSAGEM
      ********** VALIDACION DE CNPJ   *****
           IF VALID-CNPJ = "N"
           display "INSIRA CNPJ: "
           display " "with no advancing
           accept CNPJ-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-CNPJ
           END-IF
      ********** VALIDACION DE CODIGO   *****
           MOVE "S"                    TO VALID-Codigo
           MOVE "A"                    TO SW-TABLA

           IF VALID-Razão = "N" AND
               (VALID-Codigo = "S" AND VALID-CNPJ = "S")
           display "INSIRA Razão Social: "
           display " "with no advancing
           accept Razão-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Razão
           END-IF
      ********** VALIDACION DE Latitud   *****
           IF VALID-Latitude = "N" AND
               (VALID-Razão = "S" AND VALID-Codigo = "S"
                AND VALID-CNPJ = "S")
           display "INSIRA Latitud formato NNN,NNNNNNNN: "
           display " "with no advancing
           accept Latitude-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Latitud
           END-IF
      ********** VALIDACION DE longitud   *****
           IF VALID-Longitude = "N" AND
               (VALID-Latitude = "S" AND VALID-Razão = "S"
                AND VALID-Codigo = "S" AND VALID-CNPJ = "S")
           display "INSIRA Longitud formato NNN,NNNNNNNN: "
           display " "with no advancing
           accept Longitude-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Longitud
           END-IF

           IF (VALID-Longitude  = "S" AND
               VALID-Latitude   = "S" AND VALID-Razão = "S" AND
               VALID-Codigo = "S" AND VALID-CNPJ = "S")

               PERFORM MOSTRAR-REGISTRO
               IF OPC-W = 1
                  MOVE REG-WORK     TO REG-CLIENTES
                  WRITE REG-CLIENTES
                  MOVE Codigo-W     TO Codigo-Tabclie
                  REWRITE REG-TABLA
                  MOVE "S"          TO SW-AGREGAR
                  INITIALIZE REG-WORK
               END-IF
                  INITIALIZE REG-WORK
                  MOVE "S"          TO SW-AGREGAR
                  MOVE "N"          TO VALID-CNPJ
                  MOVE "N"          TO VALID-Razão
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
           display HORA-PROGRAMA
           display "ALTERACAO DE CLIENTES"
           display MENSAGEM
      ********** VALIDACION DE CNPJ   *****
           IF VALID-CNPJ = "N"
           display "INSIRA CNPJ: "
           display " "with no advancing
           accept CNPJ-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-CNPJ-MOD
           END-IF
      ********** VALIDACION DE CODIGO   *****
           MOVE "S"                    TO VALID-Codigo
           MOVE "M"                    TO SW-TABLA

           IF VALID-CNPJ = "S"

           PERFORM MOSTRAR-REGISTRO-MOD

           IF VALID-Razão = "N" AND
               (VALID-Codigo = "S" AND VALID-CNPJ = "S")
           display "Alterar Razão Social: "
           display " "with no advancing
           accept Razão-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Razão
           END-IF
      ********** VALIDACION DE Latitud   *****
           IF VALID-Latitude = "N" AND
               (VALID-Razão = "S" AND VALID-Codigo = "S"
                AND VALID-CNPJ = "S")
           display "Alterar Latitud formato NNN,NNNNNNNN: "
           display " "with no advancing
           accept Latitude-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Latitud
           END-IF
      ********** VALIDACION DE longitud   *****
           IF VALID-Longitude = "N" AND
               (VALID-Latitude = "S" AND VALID-Razão = "S"
                AND VALID-Codigo = "S" AND VALID-CNPJ = "S")
           display "Alterar Longitud formato NNN,NNNNNNNN: "
           display " "with no advancing
           accept Longitude-W
           MOVE SPACES                 TO MENSAGEM
           PERFORM VALIDA-Longitud
           END-IF
           END-IF

           IF (VALID-Longitude  = "S" AND
               VALID-Latitude   = "S" AND VALID-Razão = "S" AND
               VALID-Codigo = "S" AND VALID-CNPJ = "S")
      *
               MOVE "M"   TO SW-TABLA
               PERFORM MOSTRAR-REGISTRO
               IF OPC-W = 1
                  MOVE REG-WORK     TO REG-CLIENTES
                  REWRITE REG-CLIENTES
                  MOVE "S"          TO SW-MODIFIC
               END-IF
                  INITIALIZE REG-WORK
                  MOVE "S"          TO SW-MODIFIC
                  MOVE "N"          TO VALID-CNPJ
                  MOVE "N"          TO VALID-Razão
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
           display HORA-PROGRAMA
           display "EXCLUSAO DE CLIENTES"
           display MENSAGEM
           display "INSIRA CNPJ: "
           display " "with no advancing
           accept CNPJ-W
           MOVE SPACES   TO MENSAGEM
           PERFORM VALIDA-CNPJ-MOD
           IF VALID-CNPJ = "S"
           MOVE "E"                 TO SW-TABLA
           PERFORM MOSTRAR-REGISTRO
           IF OPC-W = 1
                  DELETE CLIENTES
                  MOVE "S"          TO SW-EXCLUC
                  INITIALIZE REG-WORK
               ELSE
                  INITIALIZE REG-WORK
                  MOVE "S"          TO SW-EXCLUC
                  MOVE "N"          TO VALID-CNPJ
                  MOVE "N"          TO VALID-Razão
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
           display HORA-PROGRAMA
           display "IMPORTACAO DE CLIENTES"
           display MENSAGEM
           display "INSIRA CNPJ: "
           display " "with no advancing
           accept CNPJ-W
           MOVE SPACES   TO MENSAGEM
           PERFORM VALIDA-CNPJ-MOD
           IF VALID-CNPJ = "S"
           MOVE "I"                 TO SW-TABLA
           PERFORM MOSTRAR-REGISTRO
           IF OPC-W = 1
                  OPEN OUTPUT CLIIMPOR
      *           MOVE REG-CLIENTES   TO REG-CLIIMPOR
                  MOVE Codigo-Cliente TO Codigo-Cliente-i
                  MOVE ";"            TO FIL-1
                  MOVE ";"            TO FIL-2
                  MOVE ";"            TO FIL-3
                  MOVE ";"            TO FIL-4
                  MOVE CNPJ           TO CNPJ-i
                  MOVE Razão-Social   TO Razão-Social-i
                  MOVE Latitude-c     TO Latitude-i
                  MOVE Longitude-c    TO Longitude-i
                  WRITE REG-CLIIMPOR
                  CLOSE CLIIMPOR
                  MOVE "S"          TO SW-IMPORT
                  INITIALIZE REG-WORK
               ELSE
                  INITIALIZE REG-WORK
                  MOVE "S"          TO SW-IMPORT
                  MOVE "N"          TO VALID-CNPJ
                  MOVE "N"          TO VALID-Razão
                  MOVE "N"          TO VALID-Latitude
                  MOVE "N"          TO VALID-Longitude
           END-IF
           END-IF
           END-PERFORM.

       LIMPIAR-PANTALLA.
           CALL "SYSTEM" USING "cls".


       VALIDA-CNPJ.
           IF CNPJ-W <= 0 OR CNPJ-W IS NOT NUMERIC
           MOVE FUNCTION CONCATENATE("CNPJ-W " CNPJ-W " NAO INVALIDO")
                      TO MENSAGEM
           MOVE "N"                   TO VALID-CNPJ
           ELSE
           MOVE CNPJ-W                TO CNPJ
           READ CLIENTES KEY CNPJ
           IF FILE-STATUS = 00
           MOVE FUNCTION CONCATENATE("CNPJ-W " CNPJ-W " JA EXISTE")
                      TO MENSAGEM
           MOVE "N"                   TO VALID-CNPJ
           ELSE
           MOVE "S"                   TO VALID-CNPJ
           END-IF
           END-IF.

       VALIDA-CNPJ-MOD.
           IF CNPJ-W <= 0 OR CNPJ-W IS NOT NUMERIC
           MOVE FUNCTION CONCATENATE("CNPJ-W " CNPJ-W " NAO INVALIDO")
                      TO MENSAGEM
           MOVE "N"                   TO VALID-CNPJ
           ELSE
           MOVE CNPJ-W                TO CNPJ
           READ CLIENTES KEY CNPJ
           IF FILE-STATUS <> 00
           MOVE FUNCTION CONCATENATE("CNPJ-W " CNPJ-W " NO EXISTE")
                      TO MENSAGEM
           MOVE "N"                   TO VALID-CNPJ
           ELSE
           MOVE "M"                   TO SW-TABLA
           MOVE "S"                   TO VALID-CNPJ
           END-IF
           END-IF.

       VALIDA-Razão.
           IF Razão-W IS EQUAL SPACES
           MOVE FUNCTION CONCATENATE("Razão " Razão-W
           " NAO INVALIDA")  TO MENSAGEM
           MOVE "N"                   TO VALID-Razão
           ELSE
           MOVE "S"                   TO VALID-Razão
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
                MOVE 001    TO CRLTAB
                READ TABLA KEY CRLTAB
                IF FILE-STATUS = 00
                MOVE ZEROES     TO Codigo-W
                COMPUTE Codigo-W = Codigo-Tabclie + 1
                MOVE REG-WORK   TO REG-CLIENTES
                END-IF
               WHEN = "E"
                MOVE "OPCAO 1 Exclusao  OPCAO  2 SAIR" TO MENSAGEM2
                MOVE REG-CLIENTES TO REG-WORK
               WHEN = "M"
                MOVE "OPCAO 1 Alteracao OPCAO  2 SAIR" TO MENSAGEM2
               WHEN = "I"
                MOVE REG-CLIENTES TO REG-WORK
                MOVE "OPCAO 1 Importacao OPCAO  2 SAIR" TO MENSAGEM2
           END-EVALUATE

           MOVE "N"    TO SW-M
           PERFORM UNTIL SW-M = "S"
           PERFORM LIMPIAR-PANTALLA
           display NOME-PROGRAMA
           display DATE-PROGRAMA
           display HORA-PROGRAMA
           display "VERIFQUE A INFORMACAO"
           display " "
           display "Código Cliente: " Codigo-W
           display "CNPJ          : " CNPJ-W
           display "Razão Social  : " Razão-W
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
           display HORA-PROGRAMA
           display "ALTERE A INFORMACAO"
           display " "
           MOVE Codigo-Cliente     TO Codigo-W
           MOVE Razão-Social       TO Razão-W
           MOVE Latitude-c         TO Latitude-W
           MOVE Longitude-c        TO Longitude-W
           display "Código Cliente: " Codigo-W
           display "CNPJ          : " CNPJ-W
           display "Razão Social  : " Razão-W
           MOVE ZEROES       TO WRK-SAL
           MOVE Latitude-W   TO WRK-SAL
           display "Latitude       : " WRK-SAL
           MOVE ZEROES       TO WRK-SAL
           MOVE Longitude-W  TO WRK-SAL
           display "Longitude     : " WRK-SAL
           display " ".

       FIN.
           CLOSE CLIENTES TABLA
           STOP RUN.

       END PROGRAM Pgm00001.
