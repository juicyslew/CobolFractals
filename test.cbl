       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIERPINSKI-GAME.
       AUTHOR. WILLIAM DERKSEN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INIT-FILE
               ASSIGN TO "./Sierpinski-Initial.dat".
           SELECT OUT-FILE
               ASSIGN TO "./Sierpinski-Done.dat".

       DATA DIVISION.
       FILE SECTION.
       FD  INIT-FILE
           DATA RECORD IS IN-RECORD.
       01  IN-RECORD.
           05  I-LINE-INFO     PIC X(201).
       FD  OUT-FILE
           DATA RECORD IS OUT-RECORD.
       01  OUT-RECORD.
           05  O-LINE-INFO     PIC X(201).

       WORKING-STORAGE SECTION.
       01  INDICATORS.
          05  FILE-EMPTY       PIC XXX       VALUE "NO ".
       
       01  MAXITER             PIC 9999       VALUE 9999.
       01  CURRITER            PIC 9999       VALUE 000.
       01  CURRLINE            PIC X(201).
       01  LASTLINE            PIC X(201).
       01  SEARCH-STRING       PIC X         VALUE "X".
       01  MATCH-COUNT         PIC 9999.
       01  SEARCH-INDEX        PIC 9999.
       01  WRITE-INDEX         PIC S9999.
       01  WRITE-OFFSET        PIC S9.
       01  MATCH-POSITIONS.
           05  MATCH-POS       PIC 999 OCCURS 201 TIMES.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM A-100-INITIALIZATION.
           PERFORM B-100-PROCESS-FILE

      *     MOVE TEST-STRING-2 TO TEST-STRING
      *     PERFORM FIND-MATCHES

           PERFORM C-100-CLEAN-UP.

           STOP RUN
           .

       A-100-INITIALIZATION.
           OPEN INPUT INIT-FILE
                OUTPUT OUT-FILE.

       B-100-PROCESS-FILE.
           READ INIT-FILE
               AT END
                   MOVE "YES" TO FILE-EMPTY.
                   DISPLAY "INIT FILE WAS EMPTY."
           IF FILE-EMPTY = "NO " THEN
               MOVE I-LINE-INFO TO LASTLINE
               MOVE I-LINE-INFO TO O-LINE-INFO
               WRITE OUT-RECORD
                   AFTER ADVANCING 0 LINES

               PERFORM B-150-LINELOOPING
                   UNTIL CURRITER = MAXITER
           END-IF
           .


       B-150-LINELOOPING.
           PERFORM B-200-MATCHING.

           COMPUTE CURRITER = CURRITER + 1.
           MOVE CURRLINE TO LASTLINE.
           MOVE SPACES TO CURRLINE.


       B-200-MATCHING.
           PERFORM B-XXX-MATCHPRINT.
           PERFORM VARYING SEARCH-INDEX FROM 1 BY 1
               UNTIL SEARCH-INDEX = 202
               IF LASTLINE (SEARCH-INDEX:1) = SEARCH-STRING THEN
                   MOVE 1 TO WRITE-OFFSET
                   PERFORM B-300-WRITELOGIC
                   MOVE -1 TO WRITE-OFFSET
                   PERFORM B-300-WRITELOGIC
                   MOVE 0 TO WRITE-OFFSET
                   PERFORM B-300-WRITELOGIC
      *             DISPLAY SEARCH-INDEX ' ' WITH NO ADVANCING
               END-IF
           END-PERFORM
           MOVE CURRLINE TO O-LINE-INFO
           WRITE OUT-RECORD
               AFTER ADVANCING 1 LINES
           .

       B-300-WRITELOGIC.
           COMPUTE WRITE-INDEX = SEARCH-INDEX + WRITE-OFFSET
           IF WRITE-INDEX > 0 AND WRITE-INDEX < 202 THEN
               IF CURRLINE(WRITE-INDEX:1) = " " THEN
                   MOVE "X" TO CURRLINE(WRITE-INDEX:1)
               ELSE
                   MOVE " " TO CURRLINE(WRITE-INDEX:1)
               END-IF
           END-IF
           .

       C-100-CLEAN-UP.
           CLOSE INIT-FILE
                 OUT-FILE.

      * THIS FUNCTION IS ENTIRELY UNNECESSARY
       B-XXX-MATCHPRINT.
           MOVE ZERO TO MATCH-COUNT 
           INSPECT LASTLINE TALLYING MATCH-COUNT
               FOR ALL SEARCH-STRING.
      *     DISPLAY 'FOUND ' MATCH-COUNT ' OCCURRENCE(S) OF '
      *         SEARCH-STRING ' IN:'
           DISPLAY LASTLINE
      *     DISPLAY ' '
           .
