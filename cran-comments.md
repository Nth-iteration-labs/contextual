New submission.

## Test environments

* local Windows 10 (x64) release, R 3.5.1
* OS X install (on travis-ci) R-release
* Ubuntu 12.04 (on travis-ci) R-release
* Windows Server 2012 R2 x64 install (on appveyor), R 3.5.1
* Rhub:
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* win-builder (devel, oldrelease and release)

## R CMD check results

### Generally no errors, no warnings, no notes

```
0 ERRORs | 0 WARNINGs | 0 NOTES.
```

### Oldrelease and Ubuntu Linux 16.04: 1 NOTE

```
  Author field differs from that derived from Authors@R
    Author:    'Robin van Emden [aut, cre] (<https://orcid.org/0000-0001-5820-8638>), Maurits Kaptein [ctb]       (<https://orcid.org/0000-0002-6316-7524>)'   
    Authors@R: 'Robin van Emden [aut, cre] (0000-0001-5820-8638), Maurits Kaptein [ctb] (0000-0002-6316-7524)'
```
The only way to get rid of this is by removing the ORCID from the Authors@R comment field - which is processed correctly in R versions later than oldrelease/Ununtu 16.04. Presume this can safely be ignored.

## Downstream dependencies

No ERRORs or WARNINGs found 
