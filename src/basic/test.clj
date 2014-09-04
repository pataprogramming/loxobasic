(ns basic.test)

(def oo "10  PRINT \"ABS(-5)=\";ABS(-5)
         20  PRINT \"ASC(FOO)=\";ASC(\"FOO\")
         30  PRINT \"ATN(25)=\";ATN(2.5)
         40  PRINT \"CHR$(69)=\";CHR$(69)
         50  PRINT \"COS(1)=\";COS(1)
         60  PRINT \"EXP(2)=\";EXP(2)
         70  PRINT \"INT(4.5)=\";INT(4.5)
         80  PRINT \"LEFT$('BAR',2)=\";LEFT$(\"BAR\",2)
         90  PRINT \"LEN('WEASEL')=\";LEN(\"WEASEL\")
         100 PRINT \"LOG(2.178)=\";LOG(2.178)
         110 PRINT \"MID$('FOOBARBAZ',3,4)=\";MID$(\"FOOBARBAZ\",3,4)
         120 PRINT \"RND(1)=\";RND(1)
         130 PRINT \"RIGHT$('FOOBARBAZ',3)=\";RIGHT$(\"FOOBARBAZ\",3)
         140 PRINT \"SGN(-5)=\";SGN(-5)
         150 PRINT \"STR$(8.45)=\";STR$(8.45)
         160 PRINT \"SQR(100)=\";SQR(100)
         170 PRINT \"FOO\";TAB(10);\"FOO\"
         180 PRINT \"TAN(10)=\";TAN(10)
         190 PRINT \"VAL('500.2')=\";VAL(\"500.2\")")

(def pp "10 A=2.2
         20 B=0.1
         30 C=1
         40 PRINT A+B+C")

(def qq "10 DIM A(5)
         15 X=2
         20 DIM B(X,X,3)
         25 B(1,1,2)=99
         30 FOR N=1 TO 5
         40 A(N)=10-N
         50 NEXT N
         60 FOR N=1 TO 5
         70 PRINT A(N);\" \";
         80 NEXT N
         90 PRINT")

(def rr "5  P=-1
         10 DEF F(P,Q)=(P+Q)*G(P)
         15 DEF G(Q)=P*Q
         20 PRINT F(5,4)
         30 REM OUTPUT SHOULD BE 45")

(def ss "5  PRINT ;
         10 INPUT \"LET'S HAVE IT: \";D
         20 PRINT 3")

(def tt "10 A=1
         20 B=2
         30 C=3
         34 GOTO 40
         35 C=4
         40 D=4
         50 D=5
         60 GOSUB 100
         70 PRINT A;\" \";B;\" \";C;\" \";D;\" \";E
         99 END
         100 E=5
         110 RETURN")

(def uu "10 K=3
         20 FOR I=0 TO K
         30 FOR J=5 TO I+6
         31 PRINT I
         32 PRINT J
         40 NEXT J,I")

(def vv "10  A=2:B=1
         20  ON A GOSUB 100,110,120
         30  ON B GOTO 50,60,70
         50  PRINT \"FOO\":STOP
         60  PRINT \"BAR\":STOP
         70  PRINT \"BAZ\":STOP
         100 B=1:A=1:RETURN
         110 B=2:A=1:RETURN
         120 B=3:A=1:RETURN")

(def ww "4  C=1
         5  B=C*2
         6  PRINT B
         10 PRINT \"ENTER A NUMBER:\"
         20 INPUT A
         30 PRINT \"YOU ENTERED:\"
         40 PRINT A")

(def xx "10 DATA \"A\",5
         15 DATA \"B\",10
         20 READ FOO,BAR,BAZ
         30 PRINT FOO
         40 PRINT BAR
         50 PRINT BAZ
         60 RESTORE
         70 READ WOMBLE
         80 PRINT WOMBLE
         90 END")

(def yy "5 REM HOWDY
         10 DATA 1,2,3,4")
