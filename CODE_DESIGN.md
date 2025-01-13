# Code Design

Here are details regarding code design choices/decisions, 
particularly where things are bit convoluted, as well as style decisions or 
naming conventions.

## Data storage

Data used for preparing data sets for the package. 

- Smaller data to be accessed by the user are stored in `inst/extdata`. 
- Large data or data which should not be shared directly, are stored in the 
  `misc` folder which is Git-ignored.This ensures the same path for all developers
  and that data is kept within the project.

## Workflow style

Workflows (i.e. not functions, but for example scripts to create internal data)
use sections (# Header -------) to create the TOC used in RStudio.
