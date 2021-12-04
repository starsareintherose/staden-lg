C  nipmen
C  9-7-90 Reorganised lists
      SUBROUTINE MENU(OPT,KOPT,MOPT,MAXOPT,MINMEN,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH)
C   AUTHOR: RODGER STADEN
      INTEGER IHELPS(0:MAXOPT),IHELPE(0:MAXOPT)
      INTEGER OPT
      CHARACTER HELPF*(*)
1     CONTINUE
      CALL CLEARV
      IF(MOPT.EQ.0)THEN
      WRITE(KBOUT,5000)
5000  FORMAT(' Menus and their numbers are '/
     +' m0 = This menu'/
     +' m1 = General'/
     +' m2 = Screen control'/
     +' m3 = Statistical analysis of content'/
     +' m4 = Structures and repeats'/
     +' m5 = Translation and codons'/
     +' m6 = Gene search by content'/
     +' m7 = General signals'/
     +' m8 = Specific signals'/
     +'  ? = Help'/
     +'  ! = Quit')
1001  FORMAT(
     +' ? Menu or option number=',$)
      ELSE IF(MOPT.EQ.-1)THEN
              WRITE(KBOUT,5001)
5001            FORMAT(' General menu'/
     +          '  0 = List of menus'/
     +          '  ? = Help'/
     +          '  ! = Quit'/
     1          '  3 = Read a new sequence'/
     1          '  4 = Define active region'/
     +          '  5 = List the sequence'/
     +          '  6 = List a text file'/
     +          '  7 = Direct output to disk'/
     +          '  8 = Write active sequence to disk'/
     +          '  9 = Edit the sequence'/
     +          ' 17 = Search for restriction enzymes'/
     +          ' 29 = Reverse and complement the active sequence'/
     +          ' 66 = Exchange t and u')
      ELSE IF(MOPT.EQ.-2)THEN
                WRITE(KBOUT,5002)
5002            FORMAT(' Screen control menu'/
     +          '  0 = List of menus'/
     +          ' 10 = Clear graphics'/
     +          ' 11 = Clear text'/
     +          ' 12 = Draw a ruler'/
     +          ' 13 = Use crosshair'/
     +          ' 14 = Reposition plots'/
     +          ' 15 = Label diagram'/
     +          ' 16 = Display a map')
      ELSE IF(MOPT.EQ.-3)THEN
                WRITE(KBOUT,5005)
5005            FORMAT(' Statistical analysis of content'/
     +          '  0 = List of menus'/
     +          ' 21 = Count bases'/
     +          ' 22 = Count dinucleotides'/
     +          ' 24 = Plot base composition'/
     2          ' 25 = Plot local deviations in base composition'/
     3          ' 26 = Plot local deviations in dinucleotide',
     +          ' composition'/
     4          ' 27 = Plot local deviations in trinucleotide',
     +          ' composition'/
     3          ' 38 = Examine repeats'/
     5          ' 59 = Plot negentropy'/
     6          ' 64 = Plot observed-expected word frequencies')
      ELSE IF(MOPT.EQ.-4)THEN
                WRITE(KBOUT,5007)
5007            FORMAT(' Structures and repeats menu'/
     +          '  0 = List of menus'/
     5          ' 30 = Search for hairpin loops'/
     1          ' 31 = Search for long range inverted repeats'/
     1          ' 32 = Search for repeats'/
     1          ' 33 = Search for z dna (total ry,yr)'/
     1          ' 34 = Search for z dna (runs of ry or yr)'/
     1          ' 35 = Search for z dna (best phased value)'/
     1          ' 36 = Search for local similarity or complementarity'/
     3          ' 38 = Examine repeats')
      ELSE IF(MOPT.EQ.-5)THEN
                WRITE(KBOUT,5006)
5006            FORMAT(' Translation and codon menu'/
     +          '  0 = List of menus'/
     +          ' 23 = Count codons, amino acids and bases',/,
     +          ' 28 = Calculate codon constraint',/,
     +          ' 37 = Set genetic code'/
     1          ' 39 = Translate and list in up to six phases'/
     5          ' 40 = Translate and write protein sequence to disk'/
     5          ' 41 = Write codon table to disk',/,
     +          ' 54 = Search for open reading frames')
      ELSE IF(MOPT.EQ.-6)THEN
                WRITE(KBOUT,5003)
5003            FORMAT(' Gene search by content'/
     +          '  0 = List of menus'/
     1          ' 42 = Codon usage method'/
     5          ' 43 = Positional base preferences method'/
     5          ' 44 = Uneven positional base frequencies method'/
     4          ' 45 = Codon improbability on base composition'/
     4          ' 46 = Codon improbability on amino acid composition'/
     2          ' 47 = Shepherd RNY preference method'/
     3          ' 48 = Ficketts method'/
     5          ' 49 = tRNA gene search')
      ELSE IF(MOPT.EQ.-7)THEN
                WRITE(KBOUT,5004)
5004            FORMAT(' General signals menu'/
     +          '  0 = List of menus'/
     +          ' 18 = Compare a short sequence'/
     +          ' 19 = Compare a short sequence using a score matrix'/
     +          ' 20 = Search using a weight matrix'/
     1          ' 50 = Plot start codons'/
     2          ' 51 = Plot stop codons'/
     3          ' 52 = Plot stop codons on the complementary strand'/
     3          ' 53 = Plot stop codons on both strands'/
     +          ' 60 = Search using a dinucleotide weight matrix'/
     +          ' 63 = Search the complementary strand using a ',
     +                'weight matrix'/
     +          ' 67 = Search for patterns of motifs')
      ELSE IF(MOPT.EQ.-8)THEN
                WRITE(KBOUT,5008)
5008            FORMAT(' Eukaryotic signal search'/
     +          '  0 = List of menus'/
     3          ' 55 = Search for E. coli promoters'/
     3          ' 56 = Search for E. coli promoters',
     +                ' on the complementary strand'/
     3          ' 57 = Search for E. coli promoters (-35 and -10)'/
     4          ' 58 = Search for prokaryotic ribosome binding sites'/
     5          ' 61 = Search for eukaryotic ribosome binding sites'/
     6          ' 62 = Search for splice junctions'/
     6          ' 65 = Search for  polya sites')
      END IF
3     CONTINUE
      WRITE(KBOUT,1001)
      CALL GETOPT(KBIN,KOPT,OPT)
      IF((OPT.LT.MINMEN).OR.(OPT.GT.MAXOPT).OR.(KOPT.GT.2)) THEN
        MOPT = 0
        GO TO 1
      END IF
      IF((KOPT.EQ.2).AND.(OPT.GE.0))THEN
        CALL HELP2(IHELPS(OPT),IHELPE(OPT),HELPF,IDEVH,KBIN,KBOUT)
        GO TO 1
      END IF
      IF(OPT.EQ.0) THEN
        MOPT = 0
        GO TO 1
      END IF
      IF(OPT.LT.0) THEN
        MOPT = OPT
        GO TO 1
      END IF
      END
