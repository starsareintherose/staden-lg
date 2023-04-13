# STADEN-LG

STADEN Last Generation

## Introduction

<div  align="center"> 
<img src="./desktop/staden-lg.png" width = "300" alt="logo" align=center />
</div>

This pkg is a fork of STADEN 1993 v12.1. Currently, I fixed the bug for compiling, but not completely.

## Build

You should clone this repo and cd to the repo get the `$pkgname` via `pwd` 

```
# build easy
for pkgalone in ted newted abi alf Misc cop convert expGetSeq frog getMCH indexseqlibs bap/osp-bits
do
cd $pkgname/src/$pkgalone
make
done

# compile x related
cd $pkgname/src/staden
bash gen_a.sh
make mep dap sap splitp1 splitp2 splitp3 gip sethelp convert_project sapf nipf xmep xdap xsap
cd $pkgname/src/bap
make bap xbap
for pkgalone in vepe update_subclones scf
do
cd $pkgname/src/$pkgalone
make
done
```

Or you can get this pkg from BioArchLinux repo.

## Todo

Following pkgs need your hands.

- [ ] nip

- [ ] sip

- [ ] rep

- [ ] lip

- [ ] vep

- [ ] nipl

- [ ] pipl

- [ ] sipl

- [ ] xnip

- [ ] xpip

- [ ] xsip


## License

- Copyright (c) 1993 MEDICAL RESEARCH COUNCIL (MRC), UK. BSD License.

- Copyright (c) 2023~ Everyone. GPL License.
