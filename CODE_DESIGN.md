# Code Design

Here are details regarding code design choices/decisions, particularly
where things are bit convoluted, as well as style decisions or naming
conventions.

## Data storage

Data used for preparing data sets for the package.

- Smaller data to be accessed by the user are stored in `inst/extdata`.
- Large data or data which should not be shared directly, are stored in
  the `misc` folder which is Git-ignored. This ensures the same path for
  all developers and that data is kept within the project.
- However, for reasons, R CMD Build actually copies all files to a temp
  folder and *then* deletes the ones in .Rbuildignore, which can create
  problems. The fix to this (if using RStudio) is to add
  `PKG_BUILD_COPY_METHOD=link` to your .Renviron file, which will link
  to those files rather than copying.
  (<https://github.com/r-lib/pkgbuild/issues/59#issuecomment-1327752199>)

## Workflow style

Workflows (i.e. not functions, but for example scripts to create
internal data) use sections (# Header ——-) to create the TOC used in
RStudio.

## Naming

- Snake case is used wherever possible
- Test files are named `test-XX_DESCRIPTION.R`, where `XX` is the order
  they should be run (try to test lower order functions first).

## Documentation

- `aa_common_docs.R` holds documentation that is common to multiple
  functions. Use `@inheritParams` to pull out the documentation for
  function parameters from the common docs. If you include your own
  documentation for that parameter it will take precedence.

## Random code bits

- `!!!` Triple bang (or bang-bang-bang) -
  <https://adv-r.hadley.nz/quasiquotation.html?q=>!!!#unquoting-many-arguments
  When you have a list, but you need the items to be arguments in a
  function one at a time. i.e., `tagList()` takes … so you need to do
  `tagList(div1, div2, div3)` So if you programatically create the divs,
  you have a list, then you can use `tagList(!!!list_of_divs)` to
  ‘explode’ them out (this is used a lot in the report).

## Spatial Data

When creating our own spatial data, we use - Equal Area Albers
projection for North America (i.e. ESRI:102008 for protected areas) -
CRS 4326 (i.e. no projection) for CCEI raster - In
[`analyze_spatial()`](https://landscitech.github.io/ccviR/reference/analyze_spatial.md)
data are transformed to match their overlapping datasets - Polygons are
transformed to match the rasters - Most are matched to Mean Annual
Temperature - Protected areas are matched to the projected range change
rasters (hrasts) - Non-breeding polygon is transformed to CCEI
projection - We transform to CRS 3857 for mapping (after simplifying, so
it’s pretty speedy)

Sometimes raster data get’s weird when it’s transformed for maps
(<https://github.com/rspatial/terra/issues/1356>), where the NA values
become super large. This creates a warning, but can otherwise be ignored
*or* this can be fixed by loading the raster is loaded into memory. See
`prep_raster_map()`.

**Speed** - projecting and checking geometries in lat/lon coordinates
(as opposed to projected data) is realllllly slow - try to clip all data
before transforming - when matching CRS, try to transform non-complex
data to match more complex data

For example, protected areas is a complicated polygon. So we first clip
it to the assessment area (transform assessment area to match protected
areas). Then we transform protected areas to match the hs raster.

## Inputs

Spatial inputs are created dynamically and update by using
`updateInputsXX()` functions when re-loading data. This means that the
inputs need to be available when loading the saved data, *before* the
spatial data is available. Therefore, questions that *require* spatial
data are created, but hidden until the spatial data is available. This
way they can have the comments and evidence restored when re-loading
data, and then it will be show after the spatial analyses rerun.

## Adding new inputs

This is a bit complicated with the saving and restoring process. - Make
sure the the new input is included in the output of the module it is
part of. - Add the new input name to the
column_definitions_results.csv - Add the new input and the input_fun to
shell.exec(here::here(“data-raw/ui_build_table.csv”)) - If the input_fun
is a custom function create an update function of the form
updateInput_fun. See updateCheck_comment_ui as an example. - Run all of
data-raw/lookup_tbls.R - Test that the input is saved in the csv and
updated on reloading

## Shiny Modules

### Namespacing

Namespacing can be a bit tricky in modules. In the simplest way, you
just need to use the `NS()` function in the UI or the `ns` variable in
the server (if using `renderUI()`, not required for anything else going
to/from `output` or `input`)

- UI: Use `ns <- NS(id)` at the start, wrap all UI ids in `ns(my_id)`
- Server: Use `ns <- session$ns` at the start, then wrap ids in
  `ns(my_id)`.

**Notes**

- We only need to use `ns(my_id)` in the server if creating our own UI
  elements i.e. with `renderUI()`
- In functions which dynamically create UIs, use `NS(id, ui_id)`
  *inside* the function, cf `get_file_ui()`
- `conditionalPanel()` has a `ns` argument we should use
  (<https://stackoverflow.com/a/76905697>)
- `NS()` (or `ns`) is *not* required for shinyjs (mainly because always
  called within an observer, I believe)

### Passing variables among modules

- We return reactives from a module like returning outputs from a
  function, put them in a list calling them by name
  (i.e. `spatial_data`, not `spatial_data()`)
- Then pass them as arguments to other module functions
- Remember that we want a list of reactives, not a reactive that returns
  a list (otherwise anytime any list item is updated, the whole reactive
  invalidates)

### Circular modules

- The results and the save module are somewhat circular in that the
  index from results is passed to save and the output of save is used by
  results to create the report. This isn’t ideal, but allows us to keep
  both ui and server of the report module fully contained in the results
  module.

### Checking inputs and messages

- Cannot use shinyFeedback for shinyFiles inputs
- But can use validate(need()) on the *text outputs* to provide messages
  about file paths
- To ensure that the spatial files are loaded early and to catch
  specific messages related to each file, we use ‘error’ verbatim
  textboxes that pass through any validate(need()) failures.

## Testing

- It makes testing easier if, as much as possible, all code is moved
  into functions which can be tested individually, as well as used to
  create inputs for other parts of tests.
- `server_setup()` checks `is_testing()` to set the relative path
  correctly when running tests.
- Consider running `devtools::test(filter = "test-FILE")` to test a
  single file in a ‘clean’ session. Helps find out why getting warnings
  which are once per session, for example, without have to re-run the
  entire set of tests.

### Test data

- Most test data is stored in `inst/extdata` with the demo data
- Most test data can be created on the fly by the `test_xxx()` functions
  (e.g., `test_files()`, `test_data()`, `test_spatial()`, depending on
  what you need for testing.
- Some test files which are private are stored in
  `misc/external_test_files`. Tests using these files are skipped if the
  file doesn’t exist (e.g., test-01_checks.R tests for the M/Z
  dimensions)
- Because terra rasters point to a C++ object
  (<https://github.com/rspatial/terra/issues/161>) and shinytest2 does
  *things*, we can’t actually pass rasters to an app for testing with
  shinytest2 (but no problem with interactive testing). This is why
  `mod_c_test()` accepts output of `test_files()` and then passes it to
  `test_data()` and then to `test_spatial()` internally (which ends up
  preserving the pointer location)
- There are several saved progress files for testing different things
  (stored in `inst/extdata/test_files`)
  - `test_full_run.csv` - Full run of a plant
  - `test_full_run_migration.csv` - Full run for a migratory species
  - `test_questions_only.csv` - Run with spatial and questions but no
    index
  - `test_sp_changes.csv` - Full run of a plant, but some spatial
    questions changed
  - `test_empty.csv` - An empty run file

### Interactive testing

- Most functions have an examples section in the roxygen2 docs which can
  be used for interactive testing.
- Most Shiny modules have an explicit test function that can be used
  interactively
  - These generally have multiple settings for using pre-filled spatial
    file paths, or simulating loading data, or a vanilla run.
- Some examples use
  [`withr::with_options()`](https://withr.r-lib.org/reference/with_options.html)
  to simulate certain conditions (e.g., `mod_report_test()`). In these
  cases the example must use `runApp()` or it doesn’t work
  (<https://stackoverflow.com/a/78200567>).

### testServer()

- Where possible, testing is performed with the `testServer()` (Rather
  than shinytest2)
  <https://mastering-shiny.org/scaling-testing.html#testing-reactivity>
- See `?MockShinySession` for some of what you can access in the
  testServer
- Can’t test input recovery from previous data because doesn’t work with
  `update*()` family of functions
  (<https://mastering-shiny.org/scaling-testing.html#limitations>)
- Can use [`browser()`](https://rdrr.io/r/base/browser.html) inside
  `testServer({...})` to interactively test and set up expectations.
- Errors implying that cannot find bottom of stack (“Can’t find `bottom`
  on the call tree”) usually just mean that the snapshot has changed, so
  use `snapshot_review()`

### shinytest2

- **You MUST rebuild the package before running Shiny tests!**
- see
  [`?shinytest2::AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
  for some of what you can access form the `AppDriver`
- shinytest2 can be a bit finicky, but once you get the details right it
  works well
- Careful: if you have a general error that won’t allow the app to work,
  running that tests can delete the snapshots. If that happens, revert
  that change in git, fix the app problem then retry.
- Errors in the test pane that imply the app isn’t working (not that
  specific tests are failing) are best troubleshooted by calling the app
  interactively, or by using `record_test()` (don’t actually record it,
  but the errors messages are much better in this mode, and may be
  something as simple as misspelling a function name, for example)
- Errors like “Unable request data from server” when trying to get
  snapshots may indicate that there is something wrong with the “Save
  progress” step. Run the app interactively and see what happens when
  you click “Save progress” at that step in the application run.
- Because the app is running from a function, `record_test()` can be
  used to create the details of the test, but not the whole test file
  (you need to copy the code and add it to an existing test).
- shinyFiles is a pain to test (because it uses actionButtons, which
  shinytest2 insists can only be ‘clicked’ not set to file values), so
  we don’t test the actual loading directly.
- Clicking on modal buttons:
  <https://github.com/rstudio/shinytest/issues/227>
- comments about “shiny.testmode” being set are almost always red
  herrings and point out a general error in the app.
- If snapshots keep changing by a small amount, add a Sys.sleep(1) to
  force the test to wait until it’s ready

## Random Notes

### shinytest2 error: “Target position can only be set for new windows”

- This happened after a Google Chrome update
  (<https://stackoverflow.com/questions/79488849/target-position-can-only-be-set-for-new-windows-in-chromote-in-r/79489622#79489622>)
- Solution is to update chromote package to \>v0.5.0
