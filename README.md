# Replication package for Dube and Zipperer (2024) "Own-Wage Elasticity: Quantifying the Impact of Minimum Wages on Employment"

This is the replication package that produces the tables, figures, statistics, and appendix source list in [Dube and Zipperer (2024)](https::/someurl.somewhere) "Own-Wage Elasticity: Quantifying the Impact of Minimum Wages on Employment".

## Contents

The files in this repository needed for replication are

-   `mw_owe_repository.csv` : the OWE repository [data](https://economic.github.io/owe/download.html)
-   `ns_dz_match.csv`: hand entered matches between the OWE repository data and [Neumark and Shirley (2022)](https://www.nber.org/papers/w28388)
-   `_targets.R`: the [targets](https://docs.ropensci.org/targets/) pipeline
-   `packages.R`: necessary R packages
-   `R/`: directory of R scripts
-   `docs/`: output directory

## Instructions

All of the output is already in the `docs/` folder.

To reproduce this output, simply run the targets pipeline in R with `tar_make()`.
