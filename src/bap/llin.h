/* A PACKAGE FOR SEQUENCE COMPARISON WITH AFFINE WEIGHTS:
     Gene Myers, Dept. of Computer Science, U. of Arizona 85721 (10/3/87)

#define NMAX <integer>

    NMAX is a compilation constant giving the maximum input sequence length.
    It is to be adjusted according to available memory.


int DIFF(A,B,M,N,W,G,H,S) int M,N; char A[],B[]; int W[][128],G,H; int S[];

    DIFF compares sequence A[1..M] with sequence B[1..N] and returns the
    minimum conversion cost.  Costs are determined by the parameters W, G,
    and H.  W[128][128] is an array giving replacement costs for each pair of
    ASCII characters, e.g. W['a']['b'] is the cost of replacing 'a' by 'b'.
    Be sure to set W['a']['a'] to zero if exact matches are to accrue no cost.
    The cost of a k-symbol indel is the affine function G+Hk.

    DIFF also has the side-effect of placing an encoding of an optimal
    conversion in an integer array S[0..M+N-1] supplied by the caller.
    The sequence of integers S[0], S[1], S[2], ... gives the editing
    operations in a left-to-right conversion where integers encode
    operations as follows:
         0 => replace
        -k => delete k symbols
        +k => insert k symbols.
    The script is guaranteed to have the properties:
      (1)  Inserts are never followed by inserts.
      (2)  Deletes are never followed by deletes or inserts.
      (3)  A replacement followed by a k-gap is always preferred
           to a k-gap followed by a replacement in the event that
           both have the same cost.

    DIFF returns -1.0 if NMAX isn't large enough.


int DISPLAY(A,B,M,N,S) int M,N; char A[],B[]; int S[];

    DISPLAY places on the standard output a display of the alignment
    implied by the conversion S computed in the call DIFF(A,B,M,N,?,?,?,S).
    For example:

              0    .    :    .    :    .    :    .    :    .    :
               ggcgtttcataccggcgagga  ctagagatcccagatgcagcctcgata
               !-!!!!||||!!!!!!!!!!|--!!!!!|!!|!!||||!!-!!!!!!!!!
               g cgttcataaccggcgaggtacctagacattcccagagc gcctcgata
          
             50    .    :    .    :    .
               taggaagaa tc agcaacgatcggcatg
               !|!||!!!!-!!-!!!!!!!!-!!|!-!!
               tggacagaaatcgagcaacga cgac tg
*/

#ifdef BIGMEM
#define NMAX 30000
#else
#define NMAX  3000
#endif

extern int   DIFF();
extern int   DISPLAY();

