#!/bin/csh -f

# This file must be sourced after, or at the end of, staden.login

# It sets up the sequence libraries for use by the software

# Edit this file to set environment variables for the paths to the homes of 
# your sequence libraries and their associated files
#
# First choose a home for the library data - ie the division files, and the 
# index files. Then set up the paths in this file.
#
#####################################################################################
#
#         Introduction
#         ------------


# The sequence libraries are made available to the programs by several levels
# of files. At the top of the tree is a file that says which libraries are
# available locally and defines environment variables to point to the next
# level of files. It also tells the programs (with its library type flag) the
# format of the library (embl, embl-updates and swissprot are of library type A; 
# PIR, NRL3D are of type B and Genbank type C).
# The file is called sequence.libs and is distributed in $STADTABL and is given
# the environment variable SEQUENCELIBRARIES which is defined in this file.

# setenv SEQUENCELIBRARIES $STADTABL/sequence.libs

# Contents of file sequence.libs

#A EMBLFILES EMBL 30 nucleotide library Dec 91! in cdrom format
#A EMBLUFILES EMBL 30 updates library Dec 91! in cdrom format
#A SWISSFILES SWISSPROT 20 protein library Nov 91! in cdrom format
#B PIRFILES PIR 31 protein library Dec 91!
#B NRL3DFILES NRL3D 58 From Brookhaven protein library Dec 91!
#C GENBFILES GenBank example!
#! format is: library type, space, name of library description file, prompt
#! anything after ! is ignored

# end of file

# For example the first line defines a library of type A, and environment variable
# (see below) EMBLFILES, and the text "EMBL 30 nucleotide library Dec 91" would
# appear on the users screen.


# Four environment variables are required to define each library so that it can
# be used by the software:

# 1. The first to a file that contains a list of the files (such as indexes) 
#    used by the programs.
# 2. A path to the directory that contains the division_lookup file.
# 3. A path to the directory that contains the index files.
# 4. A path to the directory that contains the division (or data) files.

# For example, for EMBL

# setenv EMBLFILES $STADTABL/embl.files
# setenv EMBLDIVPATH /nfs/al/pubseq/pubseq/inhouse/al-generic/tables
# setenv EMBLINDPATH /nfs/al/pubseq/pubseq/seqlibs/embl
# setenv EMBLPATH /nfs/al/pubseq/pubseq/seqlibs/embl

# (Of course the division_lookup, indexes and data files could be in the same place)

#   Making a new library available to the programs
#   ----------------------------------------------
# 
# If making (say) embl available to the programs for the first time you would need
# to add a line to the file $STADTABL/sequence.libs (embl, embl-updates and
# swissprot are of library type A; PIR, NRL3D are of type B and Genbank type C),
# and you would need to define EMBLPATH and EMBLINDPATH. No other changes should
# be required.

#   Moving the location of indexes or data files
#   --------------------------------------------

# If you move the data files for embl you need to redefine EMBLPATH. 
# If you move the index files for embl you would need to redefine EMBLINDPATH.


# The other files (on the distribution tape stored in $STADTABL) will only
# require changes if the libraries are reorganised - eg when embl added two
# extra divisions in spring 1993 the division lookup file needed changing.
#

#     End of introdiction
#     -------------------
#####################################################################################
#  
#
# First define the file containing the names and environment variables
# for the locally available libraries.
#

setenv SEQUENCELIBRARIES $STADTABL/sequence.libs


# Now do the individual libraries

#
#   EMBL library
#

setenv EMBLFILES $STADTABL/embl.files
setenv EMBLDIVPATH /nfs/al/pubseq/pubseq/inhouse/al-generic/tables
setenv EMBLPATH /nfs/al/pubseq/pubseq/seqlibs/embl
setenv EMBLINDPATH /nfs/al/pubseq/pubseq/seqlibs/embl

#
#   EMBL updates library
#

setenv EMBLUFILES $STADTABL/emblu.files
setenv EMBLUDIVPATH /nfs/al/pubseq/pubseq/inhouse/al-generic/tables
setenv EMBLUPATH /nfs/al/pubseq/pubseq/seqlibs/embl-updates
setenv EMBLUINDPATH /nfs/al/pubseq/pubseq/seqlibs/embl-updates

#
#   SWISSPROT library
#

setenv SWISSFILES $STADTABL/swiss.files
setenv SWISSDIVPATH /nfs/al/pubseq/pubseq/inhouse/al-generic/tables
setenv SWISSPATH /nfs/al/pubseq/pubseq/seqlibs/swiss
setenv SWISSINDPATH /nfs/al/pubseq/pubseq/seqlibs/swiss

#
#   PIR library
#

setenv PIRFILES $STADTABL/pir.files
setenv PIRDIVPATH /nfs/al/pubseq/pubseq/inhouse/al-generic/tables
setenv PIRPATH /nfs/al/pubseq/pubseq/seqlibs/pir
setenv PIRINDPATH /nfs/al/pubseq/pubseq/seqlibs/pir

#
#   GenBank library
#

setenv GENBFILES $STADTABL/genbank.files
setenv GENBDIVPATH /nfs/al/pubseq/pubseq/inhouse/al-generic/tables
setenv GENBPATH /nfs/al/pubseq/pubseq/seqlibs/genbank
setenv GENBINDPATH /nfs/al/pubseq/pubseq/seqlibs/genbank

#
#   NRL3D library
#

setenv NRL3DFILES $STADTABL/nrl3d.files
setenv NRL3DDIVPATH /nfs/al/pubseq/pubseq/inhouse/al-generic/tables
setenv NRL3DPATH /nfs/al/pubseq/pubseq/seqlibs/pir
setenv NRL3DINDPATH /nfs/al/pubseq/pubseq/seqlibs/pir

