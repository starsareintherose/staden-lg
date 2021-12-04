#ifndef _seqOutput_h
#define _seqOutput_h
/* 
    Title:       seqOutput

    File: 	 seqOutput.h
    Purpose:	 Output of sequences

*/

extern Boolean writeSeq(Seq seq, char *fn, char *seqName,Boolean includeHeader);
/*
    Write the clipped, edited part of the ABI format sequence `seq'
    into file `fn'. `seqName' is for sequence header information.
    The result indicates success.
    Currently, this just writes the bases out as text.
*/
#endif  /*_seqOutput_h*/
