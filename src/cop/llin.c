/* A PACKAGE FOR SEQUENCE COMPARISON WITH AFFINE WEIGHTS:
     See include file "linear.h" for complete interface information. */

#include "llin.h"
#define XTERNAL
#include "upam.gbl"
#include "uascii.gbl"
#define translate(A) (nascii[A])

/* Globally passed params and macros */

static int (*w)[32];				/* w = W */
static int g, h, m;				/* g = G, h = H, m = g+h */

#define gap(k)  ((k) <= 0 ? 0 : g+h*(k))	/* k-symbol indel cost */

static int *sapp;				/* Current script append ptr */
static int  last;				/* Last script op appended */

						/* Append "Delete k" op */
#define DEL(k)				{ if (last < 0)				    last = *(sapp-1) -= (k);		  else					    last = *sapp++ = -(k);		}
						/* Append "Insert k" op */
#define INS(k)				{ if (last < 0)				    { *(sapp-1) = (k); *sapp++ = last; }	  else					    last = *sapp++ = (k);		}

#define REP { last = *sapp++ = 0; }		/* Append "Replace" op */


/* diff(A,B,M,N,tb,te) returns the cost of an optimum conversion between
   A[1..M] and B[1..N] that begins(ends) with a delete if tb(te) is zero
   and appends such a conversion to the current script.                   */

static int diff(A,B,M,N,tb,te) char *A, *B; int M, N; int tb, te;

{ static int CC[NMAX+1], DD[NMAX+1];	/* Forward cost-only vectors */
  static int RR[NMAX+1], SS[NMAX+1];	/* Reverse cost-only vectors */
         int   midi, midj, type;	/* Midpoint, type, and cost */
         int midc;

{ register int   i, j;
  register int c, e, d, s;
           int t, *wa;

/* Boundary cases: M <= 1 or N == 0 */

  if (N <= 0)
    { if (M > 0) DEL(M)
      return gap(M);
    }
  if (M <= 1)
    { if (M <= 0)
        { INS(N);
          return gap(N);
        }
      if (tb > te) tb = te;
      midc = (tb+h) + gap(N);
      midj = 0;
      wa = w[translate(A[1])];
      for (j = 1; j <= N; j++)
        { c = gap(j-1) + wa[translate(B[j])] + gap(N-j);
          if (c < midc)
            { midc = c;
              midj = j;
            }
        }
      if (midj == 0)
        { INS(N) DEL(1) }
      else
        { if (midj > 1) INS(midj-1)
          REP
          if (midj < N) INS(N-midj)
        }
      return midc;
    }

/* Divide: Find optimum midpoint (midi,midj) of cost midc */

  midi = M/2;			/* Forward phase:                          */
  CC[0] = 0;			/*   Compute C(M/2,k) & D(M/2,k) for all k */
  t = g;
  for (j = 1; j <= N; j++)
    { CC[j] = t = t+h;
      DD[j] = t+g;
    }
  t = tb;
  for (i = 1; i <= midi; i++)
    { s = CC[0];
      CC[0] = c = t = t+h;
      e = t+g;
      wa = w[translate(A[i])];
      for (j = 1; j <= N; j++)
        { if ((c =   c   + m) < (e =   e   + h)) e = c;
          if ((c = CC[j] + m) < (d = DD[j] + h)) d = c;
          c = s + wa[translate(B[j])];
          if (e < c) c = e;
          if (d < c) c = d;
          s = CC[j];
          CC[j] = c;
          DD[j] = d;
        }
    }
  DD[0] = CC[0];

  RR[N] = 0;			/* Reverse phase:                          */
  t = g;			/*   Compute R(M/2,k) & S(M/2,k) for all k */
  for (j = N-1; j >= 0; j--)
    { RR[j] = t = t+h;
      SS[j] = t+g;
    }
  t = te;
  for (i = M-1; i >= midi; i--)
    { s = RR[N];
      RR[N] = c = t = t+h;
      e = t+g;
      wa = w[translate(A[i+1])];
      for (j = N-1; j >= 0; j--)
        { if ((c =   c   + m) < (e =   e   + h)) e = c;
          if ((c = RR[j] + m) < (d = SS[j] + h)) d = c;
          c = s + wa[translate(B[j+1])];
          if (e < c) c = e;
          if (d < c) c = d;
          s = RR[j];
          RR[j] = c;
          SS[j] = d;
        }
    }
  SS[N] = RR[N];

  midc = CC[0]+RR[0];		/* Find optimal midpoint */
  midj = 0;
  type = 1;
  for (j = 0; j <= N; j++)
    if ((c = CC[j] + RR[j]) <= midc)
      if (c < midc || CC[j] != DD[j] && RR[j] == SS[j])
        { midc = c;
          midj = j;
        }
  for (j = N; j >= 0; j--)
    if ((c = DD[j] + SS[j] - g) < midc)
      { midc = c;
        midj = j;
        type = 2;
      }
}

/* Conquer: recursively around midpoint */

  if (type == 1)
    { diff(A,B,midi,midj,tb,g);
      diff(A+midi,B+midj,M-midi,N-midj,g,te);
    }
  else
    { diff(A,B,midi-1,midj,tb,0);
      DEL(2);
      diff(A+midi+1,B+midj,M-midi-1,N-midj,0,te);
    }
  return midc;
}


/* Interface and top level of comparator */

int DIFF(A,B,M,N,W,G,H,S) char A[],B[]; int M,N; int W[][32],G,H; int S[];

{ if (N > NMAX) return -1;	/* Error check */

  w = W;			/* Setup global parameters */
  g = G;
  h = H;
  m = g+h;
  sapp = S;
  last = 0;

  return diff(A,B,M,N,g,g);	/* OK, do it */
}


/* Alignment display routine */

static char ALINE[51], BLINE[51], CLINE[51];

int DISPLAY(A,B,M,N,S) char A[], B[]; int M, N; int S[];
{ register char *a, *b, *c;
  register int   i,  j, op;
           int   lines;

  i = j = op = lines = 0;
  a = ALINE;
  b = BLINE;
  c = CLINE;
  while (i < M || j < N)
    { if (op == 0 && *S == 0)
        { op = *S++;
          *a = nt[translate(A[++i])];
          *b = nt[translate(B[++j])];
          *c++ = (*a++ == *b++) ? ':' : ' ';
        }
      else
        { if (op == 0)
            op = *S++;
          if (op > 0)
            { *a++ = ' ';
              *b++ = nt[translate(B[++j])];
              op--;
            }
          else
            { *a++ = nt[translate(A[++i])];
              *b++ = ' ';
              op++;
            }
          *c++ = '-';
        }
      if (a >= ALINE+50 || i >= M && j >= N)
        { *a = *b = *c = '\0';
          printf("\n%5d",50*lines++);
          for (b = ALINE+10; b <= a; b += 10)
            printf("    .    :");
          if (b <= a+5)
            printf("    .");
          printf("\n     %s\n     %s\n     %s\n",ALINE,CLINE,BLINE);
          a = ALINE;
          b = BLINE;
          c = CLINE;
        }
    }

    return 0;
}

