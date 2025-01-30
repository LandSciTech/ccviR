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

## Naming
- Snake case is used wherever possible
- Test files are named `test-XX_DESCRIPTION.R`, where `XX` is the order they 
  should be run (try to test lower order functions first).

## Documentation
- `aa_common_docs.R` holds documentation that is common to multiple functions.
  Use `@inheritParams` to pull out the documentation for function parameters 
  from the common docs. If you include your own documentation for that parameter
  it will take precedence.
  
## Shiny Modules

### Namespacing
Namespacing can be a bit tricky in modules. In the simplest way, you just need
to use the `NS()` function in the UI or the `ns` variable in the server (if
using `renderUI()`, not required for anything going to/from `output` or `input`)

- UI: Use `ns <- NS(id)` at the start, wrap all UI ids in `ns(my_id)`
- Server: Use `ns <- serssion$ns` at the start, then wrap ids in `ns(my_id)`. 

**Notes**

- We only need to use `ns(my_id)` in the server if creating our own UI elements
  i.e. with `renderUI()`
- In functions which dynamically create UIs, use `NS(id, ui_id)` *inside* the 
  function, cf `get_file_ui2()`
- `conditionalPanel()` has a `ns` argument we should use (https://stackoverflow.com/a/76905697)
- `NS()` (or `ns`) is *not* required for shinyjs
  - e.g., `spat_vuln_hide2()`

### Passing variables
- We return reactives from a module like returning outputs from a function,
  put them in a list calling them by name (i.e. `spatial_data`,
  not`spatial_data()`)
- Then pass them as arguments to other module functions
- Remember that we want a list of reactives, not a reactive that returns a list
  (otherwise anytime any list item is updated, the whole reactive invalidates)

## Notes

- `spat_vuln_hide2()` gets no `req()` because depends on present/absence to hide

## Testing
- Where possible, testing is performed with the `testServer()` (Rather than 
shinytest2) https://mastering-shiny.org/scaling-testing.html#testing-reactivity
