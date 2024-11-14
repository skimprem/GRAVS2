# The GRAVS2 package to process the relative gravity data

## About

The open-source software package GRAVS2 [1] helps to process the data of relative
gravimeters. The processing includes pre-processing (e.g. data conversion of raw
data), corrections (tides, calibration, height, atmosphere, and polar motion),
the weighted least squares adjustment and post-processing (e.g. the modeling of
local vertical gradient). A detailed PDF manual was compiled to explain the
methods, options, input parameters with several examples. The installation file
contains source codes (in Fortran 77), precompiled binaries (e.g.
for windows and linux) and example data. This is free software, distributed in
the hope that it will be useful, but without any warranty. The link to the
homepage of original GRAVS2 is [here](
https://docs.google.com/document/d/13dG5Lp3x99OuriIaFnCpHbgse8to9pDzh3ju3fj1qbM).

## Upgraded by Roman Sermiagin (2024-11)

### Done

1. Command line argument interface for GRREDU3 and GRADJ3
2. Short help for GRREDU3 and GRADJ3

### ToDo

1. Command line arguments for all utilities
2. Converter for CG-6 meter
3. Long help

### Main Utilities

- **GRREDU3**: A program for converting raw gravimeter observations into reduced observations.
- **GRADJ3**: A program for adjusting and analyzing gravimetric measurement data.

### Installation

To install and compile the GRAVS2 utilities, follow these steps:

1. Clone the repository:

```bash
git clone https://github.com/skimprem/GRAVS2
cd GRAVS2
```

2. Compile the source code using the provided Makefile:

```bash
make
```

3. Installing

```
sudo make install
```

### Usage

To run the GRREDU3 utility, use the following command:

```bash
Grredu304 [OPTION]... [PARAMETER]...

    ...

```
To run the GRADJ3 utility, use the following command:

```bash
Grredu304 [OPTION]... [PARAMETER]...

    ...

```

<!-- For a brief help on the utilities, use the -h or --help option. -->

### Examples

## Contact

If you have any questions or suggestions, please contact us at:
[roman.sermiagin@gmail.com](mailto:roman.sermiagin@gmail.com)

## References

1. Oja, Tõnis. (2022). GRAVS2 - processing software of relative gravity data [doi:10.13140/RG.2.2.25386.49603](http://dx.doi.org/10.13140/RG.2.2.25386.49603). 

