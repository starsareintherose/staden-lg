/*
** Line types for experiment file
*/
static char feature_ids[MAXIMUM_EFLTS][MAXIMUM_EFLT_LENGTH+1] = {
    "CF", /*  0 cloning vector sequence file */
    "CN", /*  1 clone name */
    "CS", /*  2 cloning vector sequence present in sequence */
    "CV", /*  3 cloning vector type */
    "DR", /*  4 direction of read */
    "DT", /*  5 date of experiment */
    "EN", /*  6 experiment name */
    "EX", /*  7 experimental notes */
    "FM", /*  8 sequencing vector fragmentation method */
    "LN", /*  9 local format trace file name */
    "LT", /* 10 local format trace file type */
    "MC", /* 11 machine on which experiment ran */
    "MN", /* 12 machine generated trace file name */
    "MT", /* 13 machine generated trace file type */
    "OP", /* 14 operator */
    "PN", /* 15 primer name */
    "QR", /* 16 poor quality sequence present at right (3') end */
    "SC", /* 17 sequencing vector cloning site */
    "SF", /* 18 sequencing vector sequence file */
    "SI", /* 19 sequencing vector insertion length */
    "SL", /* 20 sequencing vector present at left (5') end */
    "SP", /* 21 sequencing vector primer site (relative to cloning site) */
    "SQ", /* 22 sequence */
    "SR", /* 23 sequencing vector present at right (3') end */
    "ST", /* 24 strands */
    "SV", /* 25 sequencing vector type */
    "TN", /* 26 template name */
    "QL", /* 27 poor quality sequence present at left (5') end */
    "PS", /* 28 processing status */
    "CC", /* 29 comments */
    "SS", /* 30 sequence to screen against */
};
