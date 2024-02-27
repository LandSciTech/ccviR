# create a lookup table for help text from guidelines
devtools::load_all(".")
library(stringr)
# Get raw text from guidelines.R that was previously copied from the excel
# sheets and made into html

# Section B #===================================================================
sectionB <- "
<style>
h1   {font-size:16px;margin-top:0px}
h2   {font-size:16px;font-weight:bold;}
h3   {font-size:16px;font-weight:bold;}
h4   {font-size:16px;font-weight:bold;}
</style>

<h1>B. Indirect Exposure to Climate Change</h1>

<h2>1) Exposure to Sea Level Rise</h2>

<p>This factor comes into play only in the case that all or a portion of the
range within the assessment area may be subject to the effects of a 0.5-1 m or
greater sea level rise and the consequent influence of storm surges and
intrusion of salt water. Most climate model scenarios predict at least a 0.5 m
sea level rise. Because projected sea level rise (0.5-2 m by 2100) is great
compared to historical sea level changes, the negative impact on habitats for
most affected species is expected to be high.</p>

<p><b>Greatly Increase Vulnerability</b>: &gt;90% of range occurs in area
subject to sea level rise (on low-lying island(s) or in coastal zone).</p>

<p><b>Increase Vulnerability</b>: 50-90% of range occurs in area subject to sea
level rise (on low-lying island(s) or in coastal zone).</p>

<p><b>Somewhat Increase Vulnerability</b>: 10-49% of range occurs in area
subject to sea level rise (on low-lying island(s) or in coastal zone).</p>

<p><b>Neutral</b>: &lt;10% of range occur in area subject to sea level rise (on
low-lying island(s) or in coastal zone). Includes inland areas not subject to
sea level rise. Also, species that occur in an intertidal habitat that is
expected to increase in extent with a rising sea level.</p>

<p><i>Tools</i>: <a href=\"\" target=\"_blank\"></a></p>


<h2>2) Distribution Relative to Barriers</h2>

<p>This factor assesses the degree to which natural (e.g., topographic,
geographic, ecological) or anthropogenic barriers limit a species' ability to
shift its range in response to climate change. Barriers are defined here as
features or areas that completely or almost completely prevent movement or
dispersal of the species (currently and for the foreseeable future). Species for
which barriers would inhibit distributional shifts with climate change-caused
shifts in climate envelopes likely are more vulnerable to climate change than
are species whose movements are not affected by barriers. Barriers must be
identified for each species (but often are the same for a group of closely
related species). Natural and anthropogenic barriers are defined for many
species and taxonomic groups in NatureServe's Element Occurrence Specifications
(viewable in the Population/Occurrence Delineation section of species accounts
on <a href=\"http://www.natureserve.org/explorer\" target = \"_blank\">
Natureserve Explorer</a>, but usually these readily can be determined by
considering a species' basic movement capacity and ecological tolerances.</p>

<p>The distinction between a barrier and unsuitable habitat sometimes may be
unclear; in these cases assume the feature or area is unsuitable habitat
(habitat through which the species can disperse or move but that does not
support reproduction or long-term survival) and score the species here and/or in
factor C1 as appropriate. Note that caves are considered under factor C3:
Restriction to Uncommon Landscape/Geological Features, and not here where the
focus is on barriers that affect the wide array of nonsubterranean species.</p>

<p>Note that no barriers exist for most temperate-zone bird species that simply
fly over or around potential obstructions. Species restricted to habitats that
are believed to persist unchanged in spite of climate change are scored as
Neutral (because in these situations barriers do not contribute to vulnerability
even if climate changes). If a feature or area does not completely or almost
completely prevent dispersal or movement then it is categorized here as
unsuitable or suitable habitat, and the dispersal/movement of individuals across
that feature or area is assessed under factor C1 (Dispersal and Movements).
In most cases, unsuitable habitat is habitat through which propagules or
individuals may move but that does not support reproduction or long-term
survival.</p>

<p>The degree to which a barrier may affect a species' ability to shift its
range in response to climate change depends in part on the distance of the
barrier from the species' current distribution. Barriers that are separated from
a species' range by a long distance of relatively flat topography can
nevertheless affect range shifts because in gentle terrain relatively small
changes in climate can result in large shifts in the location of a particular
climate envelope. If a species changed its range accordingly (to track a
particular climate envelope), it might encounter barriers that were far from its
original range. In contrast, in landscapes in which climatic conditions change
rapidly over small horizontal distances (e.g., mountainous areas, steep slopes,
or other topographically diverse landscapes) a species' distribution would have
to shift a relatively small distance in order to track a particular climate
envelope, so the species is less likely to encounter distant barriers.</p>

<p>To count as a barrier for the purposes of this factor, a feature can be up to
50 km from the species' current range when measured across areas where climate
changes gradually over latitude or longitude (e.g., relatively flat terrain) and
up to 10 km when measured across areas where climate changes abruptly over
latitude or longitude (e.g.mountainous or steep terrain). Use 25 km for species
that occur in intermediate topography, such as moderate hill country. These
distances apply to both terrestrial and aquatic species. These distances are
derived from Loarie et al.(2009, Nature 462:1052).</p>

<p>The following categories and criteria apply to both natural and anthropogenic
barriers, but the two types of barriers are scored separately. Note that it is
illogical for natural and anthropogenic barriers to both cause greatly increased
vulnerability to climate change for a single species (only one or the other can
completely surround a species' range). If both barriers occur, estimate the
relative portions of the circumference of the range blocked by each and then
score accordingly.</p>


<h3>a) Natural barriers</h3>

<p>Examples of features that may function as natural barriers for various
species: upland habitat (i.e., absence of aquatic stream, lake, or pond habitat)
is a barrier for fishes (but not for semiaquatic or amphibious species that may
occupy the same body of water); high mountain ranges (especially those that
extend west-east) are a barrier for many lowland plants and nonvolant lowland
animals; warm lowlands are a barrier for some alpine species such as American
pika but not for elk or American pipit; large expanses of water are barriers for
pocket gophers and many other small terrestrial animals (but not for many volant
species, or for plant species that are dispersed by wide-ranging birds, or for
species that readily swim between land areas if the distance is not too great);
a high waterfall is a barrier for fishes (but not for American dippers or garter
snakes that occur along the same stream).</p>

<p><b>Greatly Increase Vulnerability</b>: Barriers completely OR almost
completely surround the current distribution such that the species' range in the
assessment area is unlikely to be able to shift significantly with climate
change, or the direction of climate change-caused shift in the species'
favorable climate envelope is fairly well understood and barriers prevent a
range shift in that direction. See Neutral for species in habitats not
vulnerable to climate change.</p>

<p><b>Increase Vulnerability </b>: Barriers border the current distribution such
that climate change-caused distributional shifts in the assessment area are
likely to be greatly but not completely or almost completely impaired.</p>

<p><b>Somewhat Increase Vulnerability</b>: Barriers border the current
distribution such that climate change-caused distributional shifts in the
assessment area are likely to be significantly but not greatly or completely
impaired. </p>

<p><b>Neutral</b>: Significant barriers do not exist for this species, OR small
barriers exist in the assessment area but likely would not significantly impair
distributional shifts with climate change, OR substantial barriers exist but are
not likely to contribute significantly to a reduction or loss of the species'
habitat or area of occupancy with projected climate change in the assessment
area.</p>


<h3>b) Anthropogenic barriers</h3>

<p>Examples of features that may function as anthropogenic barriers: large areas
of intensive urban or agricultural development are barriers for many animals
and plants; waters subject to chronic chemical pollution (e.g., acid mine
drainage) can be a barrier for fishes and other strictly aquatic species;
waters subject to thermal pollution (e.g., from power plants) may be a barrier
for some strictly aquatic species but not for others (note thermal alterations
associated with reservoirs often produce unsuitable habitat rather than impose
a barrier); dams without fish passage facilities and improperly installed
culverts can be barriers for fishes and certain other strictly aquatic species;
tortoise-proof fencing may be barrier for small reptiles and certain other
nonvolant animals (but not for most plants, large mammals, or large snakes).</p>

<p><b>Greatly Increase Vulnerability</b>: Barriers completely OR almost
completely surround the current distribution such that the species' range in the
assessment area is unlikely to be able to shift significantly with climate
change, or the direction of climate change-caused shift in the species'
favorable climate envelope is fairly well understood and barriers prevent a
range shift in that direction. See Neutral for species in habitats not
vulnerable to climate change.</p>

<p><b>Increase Vulnerability</b>: Barriers border the current distribution such
that climate change-caused distributional shifts in the assessment area are
likely to be greatly but not completely or almost completely impaired.</p>

<p><b>Somewhat Increase Vulnerability</b>: Barriers border the current
distribution such that climate change-caused distributional shifts in the
assessment area are likely to be significantly but not greatly or completely
impaired.</p>

<p><b>Neutral</b>: Significant barriers do not exist for this species, OR small
barriers exist in the assessment area but likely would not significantly impair
distributional shifts with climate change, OR substantial barriers exist but are
not likely to contribute significantly to a reduction or loss of the species'
habitat or area of occupancy with projected climate change in the assessment
area.</p>

<p><i>Tools</i>: <a href=\"\" target=\"_blank\"></a></p>


<h2>3) Predicted Impact of Land Use Changes Resulting from Human Responses to
Climate Change</h2>

<p>(e.g., plantations for carbon offsets, new seawalls in response to sea level
rise, and renewable energy projects such as wind-farms, solar arrays, or
biofuels production)</p>

<p>Strategies designed to mitigate or adapt to climate change have the potential
to affect very large areas of land, and the species that depend on these areas,
in both positive and negative ways. This factor arguably should be considered in
conservation status assessments, but considering that for most species this
factor has not yet been considered in these assessments, we include it here.
If the land use changes for alternative energy projects have already been
considered in the conservation status assessment for the species, consider not
scoring this factor, especially if the vulnerability assessment results will be
used to revise status ranks.</p>

<p>This factor is NOT intended to
include habitat loss or destruction due to on-going human activities, as these
should already be reflected in existing conservation status ranks. Include only
new activities related directly to climate change mitigation here. There is much
uncertainty about the types of mitigation action that are likely to threaten
habitats and species. Remember that multiple categories can be checked for each
factor to capture uncertainty. As federal and state climate change legislation
is enacted, some of the mitigation directions (and associated threats or
benefits to species) will become clearer. </p>

<p><b>Increase Vulnerability</b>: The natural history/requirements of the
species are known to be incompatible with mitigation-related land use changes
that are likely to very likely to occur within its current and/or potential
future range. This includes (but is not limited to) the following:</p>

<ul>
  <li>Species requiring open habitats within landscapes likely to be reforested
  or afforested. If the species requires openings within forests that are
  created/maintained by natural processes (e.g., fire), and if those processes
  have a reasonable likelihood of continuing to operate within its range, a
  lesser impact category may be appropriate.</li>

  <li>Bird and bat species whose migratory routes, foraging territory, or
  lekking sites include existing and/or suitable wind farm sites and for which
  studies indicate substantial negative impact (e.g., mortality from or
  avoidance of turbines). If such studies indicate a relatively low impact from
  wind energy development, a lesser impact category may be appropriate.</li>

  <li>Greater than 20% of the species' range within the assessment area occurs
  on marginal agricultural land, such as CRP land or other open areas with
  suitable soils for agriculture (&quot;prime farmland&quot;, etc.) that are not
  currently in agricultural production OR &gt; 50% of the species' range within
  the assessment area occurs on any non-urbanized land with suitable soils,
  where there is a reasonable expectation that such land may be converted to
  biofuel production.</li>

  <li>The species occurs in one or more river/stream reaches not yet developed
  for hydropower, but with the potential to be so developed.</li>

  <li>Species of deserts or other permanently open, flat lands with potential
  for placement of solar arrays.</li>

  <li>Species dependent on dynamic shoreline habitats (e.g., active dunes or
  salt marshes) likely to be destroyed by human fortifications against rising
  sea levels.</li>
</ul>

<p><b>Somewhat Increase Vulnerability</b>: The natural history/requirements of
the species are known to be incompatible with mitigation-related land use
changes that may possibly occur within its current and/or potential future
range, including any of the above (under Increase).</p>

<p><b>Neutral</b>: The
species is unlikely to be significantly affected by mitigation-related land use
changes that may occur within its current and/or potential future range,
including any of the above; OR it is unlikely that any mitigation-related land
use changes will occur within the species' current and/or potential future
range; OR it may benefit from mitigation-related land use changes. </p>

<p><i>Tools</i>: <a href=\"\" target=\"_blank\"></a></p>
"

# split by question numbers or letters

datB <- data.frame(section = "B",
                  guide_text = sectionB %>%
                   str_split_1("(?=<h2)|(?=<h3)")) %>%
  mutate(question = str_extract(guide_text, "(<h2>)(\\d)(\\))", group = 2),
         sub_question = str_extract(guide_text, "(<h3>)([a-c])(\\))", group = 2),
         .before = guide_text) %>%
  tidyr::fill(question, .direction = "down") %>%
  mutate(sub_question = str_to_lower(sub_question))

# Section C #===================================================================
sectionC <- "
<style>
h1   {font-size:16px;margin-top:0px}
h2   {font-size:16px;font-weight:bold;}
h3   {font-size:16px;font-weight:bold;}
h4   {font-size:16px;font-weight:bold;}
</style>

<h1>C. Sensitivity and Adaptive Capacity</h1>

<p>Note that these factors relate to
characteristics of the species only. Anthropogenic effects, such as on the
availability of dispersal corridors, should not be considered in this
section.</p>


<h2>1) Dispersal and Movements</h2>

<p>This factor pertains to known or predicted dispersal or movement capacities
and characteristics and ability to shift location in the absence of barriers as
conditions change over time as a result of climate change. Species in which
individuals exhibit substantial dispersal, readily move long distances as adults
or juveniles, or exhibit flexible movement patterns should be better able to
track shifting climate envelopes than are species in which dispersal and
movements are more limited or inflexible. This factor pertains specifically to
dispersal through unsuitable habitat, which, in most cases, is habitat through
which propagules or individuals may move but that does not support reproduction
or long-term survival. If all habitat is regarded as suitable (i.e., species can
reproduce and persist in every habitat in which it occurs), then dispersal
ability is assessed for suitable habitat. If appropriate, scoring of species
whose dispersal capacity is not known can be based on characteristics of closely
related species (or species of similar body size in the same major group) with
similar and relevant morphological features.</p>

<p>Barriers, which are here defined as features or areas that completely or
almost completely block dispersal, are treated in factor B2. If a species
requires other species for propagule dispersal, please also complete factor C4d.
The following categorization for plants is loosely based on Vittoz and Engler
(2007 Botanica Helvetica 117:109-124).</p>

<p>A small number of species are confined by barriers to areas that are
smaller than the species' potential dispersal distance (fishes in small
isolated springs or plants that only occur in vernal pools are classic
examples). Most if not all of the fish species that occur in the smallest
such habitat patches could disperse farther than the greatest extent of the
occupied patch if a larger extent of habitat were available to them. For
the purposes of this factor, the dispersal ability of these species is
scored as if the species occurred in a large patch of habitat (longer than
the dispersal distance), based on dispersal or movement patterns or
capabilities of closely related species (or species of similar body size in
the same major group of animals).</p>

<p>Migratory species should be scored according to their ability to shift
their distribution within the assessment area during the period of
occupation or from one year to the next (whichever is larger).</p>

<p>Species in which propagule dispersal is both synchronous among all members
of the population in the assessment area and infrequent (average of several
years between successful reproduction events) should be scored as one
category more vulnerable than the category that would otherwise apply. An
example is the monocarpic giant cane (Arundinaria gigantea), a bamboo
species that reproduces synchronously every 25-50 years and then dies.</p>

<p><b>Greatly Increase Vulnerability</b>: Species is characterized by severely
restricted dispersal or movement capability. Species is represented by
sessile organisms that almost never disperse more than 10 meters per
dispersal event.</p>

<p><b>Increase Vulnerability</b>: Species is characterized by highly restricted
dispersal or movement capability. Species rarely disperses through
unsuitable habitat more than about 10-100 meters per dispersal event; OR
dispersal beyond a very limited distance (or outside a small isolated patch
of suitable habitat) periodically or irregularly occurs but is dependent on
highly fortuitous or rare events; OR species has substantial movement
capability but exhibits a very high degree of site fidelity.</p>

<p><b>Somewhat Increase Vulnerability</b>: Species is characterized by limited
or moderate but not highly or severely restricted dispersal or movement
capability. A significant percentage (at least approximately 50%) of
propagules or individuals disperse approximately 100-1,000 meters per
dispersal event (rarely farther); OR species has substantial movement
capability but exhibits a moderate to high degree of site fidelity and has
very limited existing or potential habitat within the assessment area; OR
dispersal likely is consistent with one of the following examples.</p>

<p><b>Neutral</b>: Species is characterized by good to excellent dispersal or
movement capability. Species has propagules or dispersing individuals that
commonly move more than 1 kilometer from natal or source areas; OR species
tends to occupy all or most areas of suitable habitat, or readily or
predictably moves more than 1 kilometer to colonize newly available habitat
(e.g., recently restored areas, areas that become suitable as a result of
fire, insect infestations, or other environmental changes, etc.); OR
dispersal capability likely is consistent with one of the following
examples. Note that species in the Neutral category are not necessarily
'early successional' or 'r-selected' species but also may include
certain 'late successional' or equilibrium ('K-selected') species that
have excellent innate or vector-aided dispersal capability.</p>

<p><i>Tools</i>: <a href=\"\" target=\"_blank\"></a></p>


<h2>2) Predicted Sensitivity to Temperature and Moisture Changes</h2>

<p>This factor pertains to the breadth of temperature and precipitation
conditions, at both broad and local scales, within which a species is known
to be capable of reproducing, feeding, growing, or otherwise existing.
Species with narrow environmental tolerances/requirements may be more
vulnerable to habitat loss from climate change than are species that thrive
under diverse conditions.</p>


<h3>a) Predicted sensitivity to changes in temperature, based on current/recent
past temperature tolerance</h3>


<h4>i) Historical thermal niche (exposure to past variations in temperature)
</h4>

<p>This factor measures large-scale temperature variation that a
species has experienced in recent historical times (i.e., the past 50
years), as approximated by mean seasonal temperature variation (difference
between highest mean monthly maximum temperature and lowest mean monthly
minimum temperature) for occupied cells within the assessment area. It is a
proxy for species' temperature tolerance at a broad scale. This factor may
be evaluated by comparing the species range with the Annual Temperature
Variation map 1951-2006 (see first image at bottom of this page) or
calculated using GIS data downloaded from NatureServe
(http://www.natureserve.org/ccvi). For aquatic species, follow the same
procedure as for terrestrial species, since this factor measures broad
regional patterns.</p>

<p>Use the annual map for both resident and migratory species. Although
migratory species are not physically present to experience temperature
variations, they nonetheless are affected by these variations through
effects on food supply and habitat availability.</p>

<p><b>Greatly Increase Vulnerability</b>: Considering the mean seasonal
temperature variation for occupied cells, the species has experienced very small
(&lt; 37&deg; F/20.8&deg; C) temperature variation in the past 50 years.
Includes cave obligates and species occurring in thermally stable groundwater
habitats.</p>

<p><b>Increase Vulnerability</b>: Considering the mean seasonal temperature
variation for occupied cells, the species has experienced small (37 - 47&deg;
F/20.8 - 26.3&deg; C) temperature variation in the past 50 years. Includes
facultative cave invertebrates.</p>

<p><b>Somewhat Increase Vulnerability</b>: Considering the mean seasonal
temperature variation for occupied cells, the species has experienced slightly
lower than average (47.1 - 57&deg; F/26.3 - 31.8&deg; C) temperature variation
in the past 50 years.</p>

<p><b>Neutral</b>: Considering the mean seasonal temperature variation for
occupied cells, the species has experienced average or greater than average
(&gt;57.1&deg; F/43.0&deg; C) temperature variation in the past 50 years.</p>


<h4>ii) Physiological thermal niche</h4>

<p>Current projections indicate that climate warming will be nearly
pervasive in North America over the next several decades. Species
associated with cool or cold conditions likely will experience a reduction
in habitat extent or quality and may experience declines in distribution or
abundance within a given assessment area. This factor assesses the degree
to which a species is restricted to relatively cool or cold above-ground
terrestrial or aquatic environments that are thought to be vulnerable to
loss or significant reduction as a result of climate change. Species that
depend on these cool/cold environments include (but may not be limited to)
those that occur in the assessment area's highest elevational zones,
northernmost areas, or the coldest waters. The restriction to these
relatively cool environments may be permanent or seasonal.</p>

<p>Species that occur in frost pockets, on north-facing slopes, in shady
ravines, in alpine areas, or similar cool sites are scored here if those
areas represent or are among the coldest environments in the assessment
area; lacking this stipulation, species occurring in such sites may not be
vulnerable to climate change because favorable sites may simply shift in
location without reduction or loss. Species that are associated
specifically with snow or ice are assessed separately in factor C2d. Note
that temperature conditions and hydrological regimes often covary and often
are not neatly separable; these situations should be scored here if
temperature per se appears to be the overriding factor; otherwise they
should be scored under factor C2bii: Physiological Hydrological Niche.</p>

<p><b>Greatly Increase Vulnerability</b>: Species is completely or almost
completely (&gt; 90% of occurrences or range) restricted to relatively cool or
cold environments that may be lost or reduced in the assessment area as a result
of climate change.</p>

<p><b>Increase Vulnerability</b>: Species is moderately (50-90% of occurrences
or range) restricted to relatively cool or cold environments that may be lost
or reduced in the assessment area as a result of climate change.</p>

<p><b>Somewhat Increase Vulnerability</b>: Species is somewhat (10-50% of
occurrences or range) restricted to relatively cool or cold environments that
may be lost or reduced in the assessment area as a result of climate change.</p>

<p><b>Neutral</b>: Species distribution is not significantly affected by thermal
characteristics of the environment in the assessment area, species occupies
habitats that are thought to be not vulnerable to projected climate change, or
species shows a preference for environments at the warmer end of the spectrum.
</p>


<h3>b) Predicted sensitivity to changes in precipitation, hydrology, or
moisture regime.</h3>


<h4>i) Historical hydrological niche (exposure to past variations in
precipitation)</h4>

<p>This factor measures large-scale precipitation variation that a
species has experienced in recent historical times (i.e., the past 50
years), as approximated by mean annual precipitation variation across
occupied cells within the assessment area. Overlay the species' range on
the Climate Wizard mean annual precipitation map 1951-2006. Subtract the
lowest pixel value from the highest value to assess this factor. Use the
extreme pixel values for this calculation. Use annual data for migratory
species, as this measure reflects the precipitation regime of the ecosystem
as a whole.</p>

<p><b>Greatly Increase Vulnerability</b>: Considering the range of mean annual
precipitation across occupied cells, the species has experienced very small
(&lt; 4 inches/100 mm) precipitation variation in the past 50 years.</p>

<p><b>Increase Vulnerability</b>: Considering the range of mean annual
precipitation across occupied cells, the species has experienced small (4 - 10
inches/100 - 254 mm) precipitation variation in the past 50 years.</p>

<p><b>Somewhat Increase Vulnerability</b>: Considering the range of mean annual
precipitation across occupied cells, the species has experienced slightly
lower than average (11 - 20 inches/255 - 508 mm) precipitation variation in
the past 50 years.</p>

<p><b>Neutral</b>: Considering the range of mean annual precipitation across
occupied cells, the species has experienced average or greater than average
&gt;20 inches/508 mm) precipitation variation in the past 50 years.</p>


<h4>ii) Physiological hydrological niche</h4>

<p>This factor pertains to a species' dependence on a narrowly defined
precipitation/hydrologic regime, including strongly seasonal precipitation
patterns and/or specific aquatic/wetland habitats (e.g., certain springs,
vernal pools, seeps, seasonal standing or flowing water) or localized
moisture conditions that may be highly vulnerable to loss or reduction with
climate change. Dependence may be permanent or seasonal, and for migratory
species may include staging areas, migration stops, and wintering areas
outside the assessment area. Aquatic cave obligate species are considered
here according to their hydrological needs and habitat vulnerability.
Species nesting on islands in lakes, reservoirs, and/or wetlands that
prevent predator access can be scored here to the extent that a changed
hydrological regime may influence the availability of these predator-free
breeding sites (for example, birds nesting on islands to avoid predation by
mammals). If a species is dependent on aquatic/wetland habitats that are
actively managed to maintain a particular hydrology, consider whether this
management would be sufficient to ameliorate projected climate change
impacts (and, if so, score as Neutral).</p>

<p>For plant species, the advantage of the C4 photosynthetic pathway for water
use efficiency will likely enable C4 plants to be less vulnerable to
decline under drying conditions than C3 plants (Taylor et al. 2010, New
Phytologist 185:780). The predicted vulnerability of these plants with
respect to this factor has been adjusted accordingly.</p>

<p>For nonmigratory species, 'range' refers to the range within the assessment
area. For migratory species, 'range' encompasses the assessment area and
additional areas (e.g., migration stops, staging areas, wintering areas)
that are used to a significant extent by the populations being assessed.
For example, a migratory bird species for which 95% of the significant
migration stops are in shallow inland/interior wetlands should be assigned
to the Greatly Increase Vulnerability category, even if the species is not
dependent on such habitats within the assessment area.</p>

<p>Note that temperature conditions and hydrological regimes often covary and
often are not neatly separable. These situations should be scored under
factor C2aii (Physiological Thermal Niche) if temperature per se appears to
be the overriding factor; otherwise they should be scored here.</p>

<p><b>Greatly Increase Vulnerability</b>: Completely or almost completely
(&gt;90% of occurrences or range) dependent on a specific aquatic/wetland
habitat or localized moisture regime that is likely to be highly vulnerable to
loss or reduction with climate change.</p>

<p><b>Increase Vulnerability</b>: Moderately (50-90% of occurrences or range)
dependent on a strongly seasonal hydrologic regime and/or a specific
aquatic/wetland habitat or localized moisture regime that is likely to be
highly vulnerable to loss or reduction with climate change.</p>

<p><b>Somewhat Increase Vulnerability</b>: Somewhat (10-50%) dependent on a
strongly seasonal hydrologic regime and/or a specific aquatic/wetland habitat or
localized moisture regime that is highly vulnerable to loss or reduction
with climate change.</p>

<p><b>Neutral</b>: Species has little or no dependence on a strongly seasonal
hydrologic regime and/or a specific aquatic/wetland habitat or localized
moisture regime that is highly vulnerable to loss or reduction with climate
change; OR hydrological requirements are not likely to be significantly
disrupted in major portion of the range; OR species tolerates a very wide
range of moisture conditions.</p>


<h3>c) Dependence on a specific disturbance regime likely to be impacted by
climate change</h3>

<p>This factor pertains to a species' response to specific disturbance
regimes such as fires, floods, severe winds, pathogen outbreaks, or similar
events. It includes disturbances that impact species directly as well as
those that impact species via abiotic aspects of habitat quality. For
example, changes in flood and fire frequency/intensity may cause changes in
water turbidity, silt levels, and chemistry, thus impacting aquatic species
sensitive to these aspects of water quality. The potential impacts of
altered disturbance regimes on species that require specific river features
created by peak flows should also be considered here; for example, some
fish require floodplain wetlands for larval/juvenile development or high
peak flows to renew suitable spawning habitat. Use care when estimating the
most likely effects of increased fires; in many ecosystems, while a small
increase in fire frequency might be beneficial, a greatly increased fire
frequency could result in complete habitat destruction.</p>

<p>Be sure to also consider species that benefit from a lack of disturbance
and may suffer due to disturbance increases when scoring this factor.</p>

<p><b>Increase Vulnerability</b>: Strongly affected by specific disturbance
regime, and climate change is likely to change the frequency, severity, or
extent of that disturbance regime in a way that reduces the species'
distribution, abundance, or habitat quality. For example, many
sagebrush-associated species in regions predicted to experience increased fire
frequency/intensity would be scored here due to the anticipated deleterious
effects of increased fire on their habitat.</p>

<p><b>Somewhat Increase Vulnerability</b>: Moderately affected by specific
disturbance regime, and climate change is likely to change the frequency,
severity, or extent of that disturbance regime in a way that reduces the
species' distribution, abundance, or habitat quality, OR strongly affected
by specific disturbance regime, and climate change is likely to change that
regime in a way that causes minor disruption to the species' distribution,
abundance, or habitat quality. For example, plants in a riverscour
community that are strongly tied to natural erosion and deposition flood
cycles, which may shift position within the channel rather than disappear
as a result of climate change.</p>

<p><b>Neutral</b>: Little or no response to a specific disturbance regime, OR
climate change is unlikely to change the frequency, severity, or extent of that
disturbance regime in a way that affects the range or abundance of the
species, OR climate change is likely to change the characteristics of the
disturbance regime in a way that increases the species' distribution.</p>

<p><i>Tools</i>: <a href=\"\" target=\"_blank\"></a></p>


<h3>d) Dependence on ice, ice-edge, or snow cover habitats</h3>
<p>This factor pertains to a species' dependence on habitats associated
with ice (e.g., sea ice, glaciers) or snow (e.g., long-lasting snow beds,
avalanche chutes) throughout the year or seasonally during an essential
period of the life cycle. For aquatic species, the importance of snowpack
for maintaining downstream water temperatures should be considered here.
'Range' refers to the range within the assessment area.</p>

<p><b>Greatly Increase Vulnerability</b>: Highly dependent (&gt;80% of
subpopulations or range) on ice- or snow-associated habitats; or found almost
exclusively on or near ice or snow during at least one stage of the life cycle.
For example, polar bear (Ursus maritimus) is strongly dependent on sea ice
throughout its range.</p>

<p><b>Increase Vulnerability</b>: Moderately dependent (50-80% of subpopulations
or range) on ice- or snow-associated habitats; or often found most abundantly
on or near ice or snow but also regularly occurs away from such areas. For
example, Kittlitz's murrelet (Brachyramphus brevirostris) feeding habitat
is moderately to strongly associated with tidewater glaciers.</p>

<p><b>Somewhat Increase Vulnerability</b>: Somewhat (10-49% of subpopulations or
range) dependent on ice- or snow-associated habitats, or may respond
positively to snow or ice but is not dependent on it. For example, certain
alpine plants are often associated with long-lasting snowbeds but also
commonly occur away from such areas; certain small mammals experience
increased survival and may develop relatively large populations under
winter snow cover but do not depend on snow cover. Species that benefit
from a minimum thickness of ice or snowpack for winter insulation should
also be scored here.</p>

<p><b>Neutral</b>: Little dependence on ice- or snow-associated habitats (may be
highly dependent in up to 10% of the range).</p>


<h2>3) Restriction to Uncommon Landscape/Geological Features or Derivatives</h3>

<p>This factor pertains to a species' need for a particular
soil/substrate, geology, water chemistry, or specific physical or landscape
feature (e.g., caves, cliffs, active sand dunes, islands) for reproduction,
feeding, growth, shelter, or other aspects of the life cycle. It focuses on
the commonness of suitable conditions for the species on the landscape, as
indicated by the commonness of the features themselves combined with the
degree of the species' restriction to them. Climate envelopes may shift
away from the locations of fixed (within at least a 50 year timeframe)
landscape or geological features or their derivatives, making species tied
to these uncommon features potentially more vulnerable to habitat loss from
climate change than are species that thrive under diverse conditions.</p>

<p>This factor does NOT include habitat preferences based on temperature,
hydrology, or disturbance regime, as these are covered elsewhere in the
Index. For example, species dependent on springs or ephemeral pools should
not be scored as more vulnerable for this factor solely on that basis
(addressed under factor C2bii: Physiological Hydrological Niche). However,
restriction to aquatic features with regionally uncommon water chemistry
should be considered here. This factor also does NOT include habitat
features such as stream riffles or basking rocks. Finally, this factor does
NOT include biotic habitat components; for example, species that require
features such as tree snags or a particular type/condition of plant
community (e.g., old growth forest) should not be scored as more vulnerable
for this factor.</p>

<p>If the idea of specificity to soil/substrate, geology, or specific physical
or landscape features is not relevant to the species (e.g., many birds and
mammals), choose Neutral.</p>

<p><b>Increase Vulnerability</b>: Highly dependent upon (i.e., more or less
endemic to, or &gt; 85% of occurrences found on) a particular highly uncommon
landscape or geological feature or derivative (e.g., soil, water chemistry).</p>

<p><b>Somewhat Increase Vulnerability</b>: Moderately dependent upon a
particular uncommon landscape or geological feature or derivative, i.e., (1) an
indicator of but not an endemic to (65-85% of occurrences found on) the
types of features described under Increase, OR (2) more or less restricted
to a landscape or geological feature or derivative that is not highly
uncommon within the species' range, but is not one of the dominant types.</p>

<p><b>Neutral</b>: Having a clear preference for (&gt; 85% of occurrences found
on) a particular landscape or geological feature or derivative, but the
feature/derivative is among the dominant types within the species' range;
OR somewhat flexible in dependence upon geological features or derivatives
(i.e., found on a subset of the dominant substrate/water chemistry types
within its range); OR highly generalized relative to dependence upon
geological features or derivatives; species is described as a generalist
and/or occurrences have been documented on widely varied substrates or
water chemistries.</p>

<p><i>Tools</i>: <a href=\"\" target=\"_blank\"></a></p>


<h2>4) Interspecific Interactions</h2>

<p>The primary impact of climate change on many species may occur via
effects on synchrony with other species on which they depend (Parmesan
2006, ARES 37:637), rather than through direct physiological stress.</p>


<h3>a) Dependence on other species to generate habitat</h3>

<p>This factor pertains to a species' dependence on uncommon/restricted
habitats that are generated or maintained by other species. Species that
are dependent on a small number of other species likely are more vulnerable
to climate change than are species that have more flexibility or that do
not have specialized habitat requirements.</p>

<p>Habitat refers to any habitat (e.g., for reproduction, feeding,
hibernation, seedling establishment) necessary for completion of the life
cycle, including those used only on a seasonal basis. This includes
specific (often structural) features within a more generalized habitat type
(e.g., burrows created by other species in a grassland habitat;
woodpecker-created cavities in a forest habitat). These habitats must be
required for completion of the life cycle (e.g., reproduction, feeding,
hibernation, seedling establishment, etc.) and may include habitats used
only on a seasonal basis. For plants, species-specific relationships
involved in creating specific habitat conditions necessary for seedling
establishment should be considered here; nutritional relationships
necessary for seedling establishment (e.g., parasitic or obligately
myco-heterotrophic plants) should be considered under C4g. The relationship
between freshwater mussels and their larval hosts should be scored only
under factor C4d (Dependence on other species for propagule dispersal).</p>

<p>This factor is concerned specifically with habitats generated or maintained
by particular species and does NOT include ecological dependencies based
primarily on disturbance regime, geological features, or diet, as these are
covered elsewhere in the Index. Required habitats involving
temperature/hydrological conditions that are generated by a small number of
particular species are included in this factor, but
temperature/hydrology-related habitats that are not primarily species
dependent are considered under C2aii and C2bii (thermal and hydrological
niches); if in doubt, score under C2aii or C2bii.</p>

<p><b>Increase Vulnerability</b>: Required habitat is generated primarily by one
species.</p>

<p><b>Somewhat Increase Vulnerability</b>: Required habitat is generated by only
a few species.</p>

<p><b>Neutral</b>: Required habitat is generated by more than a few species; or
species does not require any uncommon/restricted habitats; or habitat
requirements do not involve species-specific processes.</p>


<h3>b) Dietary versatility (animals only)</h3>

<p>This factor pertains to the diversity of food types consumed by
animal species. Dietary specialists are more likely to be negatively
affected by climate change than are species that readily switch among
different food types.</p>

<p>Note that the relationship between freshwater mussels and their larval
hosts should be scored only under factor C5d (Dependence on other species
for propagule dispersal).</p>

<p><b>Increase Vulnerability</b>: Completely or almost completely (&gt;90%)
dependent on one species during any part of the year; equivalent
alternatives to this single-species food resource are not readily
available.</p>

<p><b>Somewhat Increase Vulnerability</b>: 'Completely or almost completely
(&gt;90%) dependent during any part of the year on (1) a few species from a
restricted taxonomic group or (2) a narrow guild the members of which are
thought to respond similarly to climate change.</p>

<p><b>Neutral</b>: 'Diet flexible; during any season species readily switches
among multiple food resources according to availability; not strongly dependent
on one or a few species; omnivorous, with diet including numerous species
of both plants and animals.</p>


<h3>c) Pollinator versatility (plants only)</h3>

<p>Quantitative thresholds loosely follow data in Waser et al. (1996,
Ecology 77:1043). If appropriate, scoring of species whose pollinators is
not known can be based on characteristics of closely related species that
have similar and relevant morphological floral features that may have
similar pollination syndromes (Willmer 2011, Pollination and Floral
Ecology, Princeton Press).</p>

<p>In some cases, sympatric, sequentially flowering species may enhance
reproductive success for each other through maintenance of pollinator
populations. Species that are documented to benefit from such interactions
with one or only a few sympatric species should be scored one category
higher (Hegland et al. 2009, Ecology Letters 12:184).</p>

<p><b>Increase Vulnerability</b>: Completely or almost completely dependent on
one species for pollination (&gt; 90% of effective pollination accomplished by
1 species) or, if no observations exist, morphology suggests very significant
limitation of potential pollinators (e.g., very long corolla tube).</p>

<p><b>Somewhat Increase Vulnerability</b>: Completely or almost completely
dependent on 2-4 species for pollination (&gt; 90% of effective pollination
accomplished by 2-4 species) or, if no observations exist, morphology
suggests conformation to a specific 'pollination syndrome' (e.g., van der
Pijl 1961, Evolution 15: 44-59,
http://www.fs.fed.us/wildflowers/pollinators/syndromes.shtml).</p>

<p><b>Neutral</b>: Pollination apparently flexible; five or more species make
significant contributions to pollination or, if no observations exist,
morphology does not suggest pollinator limitation or pollination syndrome.
Score wind-pollinated species as Neutral.</p>

<p><i>Tools</i>: <a href=\"\" target=\"_blank\"></a></p>

<h3>d) Dependence on other species for propagule dispersal</h3>

<p>Can be applied to plants or animals. Examples: Different species of
freshwater mussels can be dispersed by one to many fish species; fruit
dispersal by animals.</p>

<p><b>Increase Vulnerability</b>: Completely or almost completely (roughly &gt;
90%) dependent on a single species for propagule dispersal.For example,
whitebark pine would fit here because Clark's nutcracker is the primary
dispersal agent.</p>

<p><b>Somewhat Increase Vulnerability</b>: Completely or almost completely
(roughly &gt; 90%) dependent on a small number of species for propagule
dispersal. For example, a freshwater mussel for which only a few species of fish
can disperse larvae.</p>

<p><b>Neutral</b>: Disperses on its own (most animals, wind-dispersed plants) OR
propagules can be dispersed by more than a few species (many plants).</p>


<h3>e) Sensitivity to pathogens or natural enemies</h3>

<p>This factor refers to pathogens and natural enemies (e.g.,
predators, parasitoids, or herbivores) that can increase or become more
pathogenic due to climate change, or vectors of disease when they expand
their distributions due to changes in climate and therefore become more
harmful or influence a greater portion of the distribution of the species
being evaluated. Examples include the chytrid fungal pathogen that can
become more harmful to frogs because of climate change (Pounds et al. 2006
Nature 439:161) or sudden oak death in California caused by a pathogen that
is invasive under favorable climate conditions (Meentenmeyer et al. 2011
Ecosphere 2:1).</p>

<p><b>Increase Vulnerability</b>: Species is negatively affected to a high
degree by a pathogen or natural enemy that is likely to increase in
distribution, abundance, or impact as a result of climate change. Example: The
cold-sensitive non-native hemlock woolly adelgid commonly causes a high
level of mortality in eastern hemlock, and the distribution/abundance/impact of
the adelgid may increase in areas where winter temperatures become milder.</p>

<p><b>Somewhat Increase Vulnerability</b>: Species is negatively affected to a
moderate degree by a pathogen or natural enemy that is likely to increase
in distribution, abundance, or impact as a result of climate change.</p>

<p><b>Neutral</b>: There is no indication that the species is currently or in
the foreseeable future likely to be significantly affected by a pathogen or
natural enemy that is likely to increase in distribution, abundance, or
impact as a result of climate change; OR the negative impact of pathogens
or natural enemies is likely to decrease with climate change. Example: A
warmer/drier climate may reduce the negative impact of certain fungal
pathogens that depend/thrive on relatively cold/moist conditions.</p>


<h3>f) Sensitivity to competition from native or non-native species</h3>

<p>Species may suffer when competitors are favored by both changing
climates and the effects these climates have on disturbance regimes
(Abatzoglou and Kolden 2011 Rangeland Ecology &amp; Management 64:471,
Dukes et al. 2011 Ecological Applications 21:1887, Pint&#x00F3-Marijuan and
Munn&#x00E9-Bosch 2013, Grossman and Rice 2014 Ecology Letters 17:710). However,
in some cases climate change will decrease the spread of particular
invasive species (Bradley et al. 2010 Trends in Ecology &amp; Evolution
25:310). To score this factor, some indication is needed that a potential
competitor is favored by projected future climates.</p>

<p><b>Increase Vulnerability</b>: Strongly affected by a native or non-native
competing species that is likely to be favored by climate change.</p>

<p><b>Somewhat Increase Vulnerability</b>: Moderately affected to a moderate
degree by a native or non-native competing species that is likely to be favored
by climate change.</p>

<p><b>Neutral</b>: Little or no response to a native or non-native species that
is likely to shift its distribution or abundance due to climate change OR
climate change is likely to decrease or have no effect on the spread or
abundance of a native or non-native species that negatively impacts the
species.</p>


<h3>g) Forms part of an interspecific interaction not covered by C5a-f</h3>

<p>Can be applied to plants or animals. Here an interspecific
interaction can include mutualism, parasitism, or commensalism. Refers to
interactions unrelated to habitat, seedling establishment, diet,
pollination, or propagule dispersal. For example, an acacia bush requiring
an ant colony for protection against herbivores.</p>

<p><b>Increase Vulnerability</b>: Requires an interaction with a single other
species for persistence.</p>

<p><b>Somewhat Increase Vulnerability</b>: Requires an interaction with a one
member of a small group of taxonomically related species for persistence. Could
also include cases where specificity is not known for certain, but is
suspected. Many Orchidaceae will be in this category because of their
requirement for a specific fungal partner for germination (Tupac Otero and
Flanagan 2006, TREE 21: 64-65).</p>

<p><b>Neutral</b>: Does not require an interspecific interaction or, if it does,
many potential candidates for partners are available.</p>


<h2>5) Genetic Factors</h2>


<h3>a) Measured genetic variation</h3>

<p>Species with less standing genetic variation will be less able to
adapt because the appearance of beneficial mutations is not expected to
keep pace with the rate of 21st century climate change. Throughout this
question, 'genetic variation' may refer neutral marker variation,
quantitative genetic variation, or both. To answer the question, genetic
variation should have been assessed over a substantial proportion of a
species' range.</p>

<p>Because measures of genetic variability vary across taxonomic groups, there
cannot be specific threshold numbers to distinguish among the categories.
The assessor should interpret genetic variation in a species relative to
that measured in related species to determine if it is low, high, or in
between.</p>

<p><b>Increase Vulnerability</b>: Genetic variation reported as 'very low'
compared to findings using similar techniques on related taxa, i.e., lack of
genetic variation has been identified as a conservation issue for the species.
</p>

<p><b>Somewhat Increase Vulnerability</b>: Genetic variation reported as 'low'
compared to findings using similar techniques on related taxa.</p>

<p><b>Neutral</b>: Genetic variation reported as 'average' or 'high' compared to
findings using similar techniques on related taxa.</p>


<h3>b) Occurrence of bottlenecks in recent evolutionary history (use only if
C5a is 'unknown')</h3>

<p>In the absence of rangewide genetic variation information (C5a),
this factor can be used to infer whether reductions in species-level
genetic variation that would potentially impede its adaptation to climate
change may have occurred. Only species that suffered population reductions
and then subsequently rebounded qualify for the Somewhat Increase or
Increase Vulnerability categories.</p>

<p><b>Increase Vulnerability</b>: Evidence that total population was reduced to
&leq; 250 mature individuals, to one occurrence, and/or that occupied area was
reduced by &gt;70% at some point in the past 500 years.</p>

<p><b>Somewhat Increase Vulnerability</b>: Evidence that total population was
reduced to 251-1000 mature individuals, to less than 10 occurrences, and/or that
occupied area was reduced by 30-70% at some point in the past 500 years.</p>

<p><b>Neutral</b>: No evidence that total population was reduced to &leq; 1000
mature individuals and/or that occupied area was reduced by &gt; 30% at some
point in the past 500 years.</p>


<h3>c) Reproductive system (plants only; use only if C5a and C5b are 'unknown')
</h3>

<p>In plants, genetic variation is strongly linked to reproductive
mode. Therefore, in the absence of measured genetic variation and knowledge
of recent genetic bottlenecks, a plant's reproductive system may serve as a
proxy for a species' genetic variation or capacity to adapt to novel
climatic conditions. For example, species that can outcross may be better
able to adapt to novel environments (Morran et al. 2009, Morran et al.
2011). Species with mixed mating systems, which make up 42% of the world's
flora, appear to favor selfing as a buffering mechanism to climate change
(Jones et al. 2013).</p>

<p><b>Increase Vulnerability</b>: Genetic variation of the species is assumed to
be 'very low' in the assessment area because the species is restricted to
asexual reproduction (vegetatively or apomicticly). These species are
expected to be negatively impacted because rapid climate change can
strongly impact genetic variation, ultimately reducing fitness (Jump and
Penuelas 2005).</p>

<p><b>Somewhat Increase Vulnerability</b>: Genetic variation assumed to be 'low'
in the assessment area due to known disruptions or barriers to gene flow among
subpopulations, range disjunctions, or documented outbreeding depression
(Franks et al. 2014). Reproductive system may be either mixed or obligate
outcrossing.</p>

<p><b>Neutral</b>: Genetic variation is assumed to be 'average' in the
assessment area based on reproductive system. Includes species that have either
mixed mating systems or are obligate outcrossers AND there are no known major
disruptions to gene flow.</p>


<h2>6) Phenological Response to Changing Seasonal Temperature or Precipitation
Dynamics</h2>

<p>Recent research suggests that some phylogenetic groups are declining
due to lack of response to changing annual temperature dynamics (e.g.,
earlier onset of spring, longer growing season), including European bird
species that have not advanced their migration times (Moller et al. 2008),
and some temperate zone plants that are not moving their flowering times
(Willis et al. 2008) to correspond to earlier spring onset. This may be
assessed using either published multi-species studies such as those cited
above or large databases such as that of the U.S. National Phenology
Network.</p>

<p><b>Increase Vulnerability</b>: Seasonal temperature or precipitation dynamics
within the species' range show detectable change, but phenological
variables measured for the species show no detectable change.</p>

<p><b>Somewhat Increase Vulnerability</b>: Seasonal temperature or precipitation
dynamics within the species' range show detectable change, and phenological
variables measured for the species show some detectable change, but the
change is significantly less than that of other species in similar habitats
or taxonomic groups.</p>

<p><b>Neutral</b>: Seasonal temperature or precipitation dynamics within the
species' range show detectable change, and phenological variables measured for
the species show detectable change which is average compared to other species
in similar habitats or taxonomic groups; OR seasonal dynamics within the
species' range show no detectable change.</p>

<p><i>Tools</i>: <a href=\"\" target=\"_blank\"></a></p>
"

# split by question numbers or letters

datC <- data.frame(section = "C",
                   guide_text = sectionC %>%
                     str_split_1("(?=<h2)|(?=<h3)|(?=<h4)")) %>%
  mutate(question = str_extract(guide_text, "(<h2>)(\\d)(\\))", group = 2),
         sub_question = str_extract(guide_text, "(<h3>)([a-g])(\\))", group = 2),
         sub2_question = str_extract(guide_text, "(<h4>)([i]+)(\\))", group = 2),
         .before = guide_text) %>%
  tidyr::fill(question, .direction = "down") %>%
  group_by(question) %>%
  tidyr::fill(sub_question, .direction = "down")

# Section D #===================================================================

sectionD <- "
<style>
h1   {font-size:16px;margin-top:0px}
h2   {font-size:16px;font-weight:bold;}
h3   {font-size:16px;font-weight:bold;}
h4   {font-size:16px;font-weight:bold;}
</style>

<h1>D. Documented or Modeled Response to Climate Change (optional)</h1>


<h2>1) Documented Response to Recent Climate Change (e.g., range contraction or
phenology mismatch with critical resources)</h2>

<p>This factor pertains to the degree to which a species is known to
have responded to recent climate change based on published accounts in the
peer-reviewed literature. Time frame for the reduction or increase is 10
years or three generations, whichever is longer. Some examples include
population declines due to phenology mismatches between species and
critical food or pollinator resources, e.g., great tits (Parus major) or
pied flycatchers (Ficedula hypoleuca) with winter moth (Operophtera
brumata) caterpillars, or honey-buzzards (Pernis apivorus) with wasps.</p>

<p>Note that not all responses to climate change necessarily indicate
vulnerability. Species that respond to climate change by shifting (but not
contracting) their range, for example, show adaptability to climate change
and should be scored as Neutral for this factor. Similarly, species that
respond by changing their phenology (without a related decline in
population) should also be scored as Neutral.</p>

<p><b>Greatly Increase Vulnerability</b>: Distribution or abundance undergoing
major reduction (&gt;70% over 10 years or three generations) believed to be
associated with climate change.</p>

<p><b>Increase Vulnerability</b>: Distribution or abundance undergoing moderate
reduction (30-70% over 10 years or three generations) believed to be
associated with climate change.</p>

<p><b>Somewhat Increase Vulnerability</b>: Distribution or abundance undergoing
small but measureable (10-30% over 10 years or three generations) believed to be
associated with climate change.</p>

<p><b>Neutral</b>: Distribution and abundance not known to be decreasing with
climate change. Includes species undergoing range shifts without loss of
distributional area or species undergoing changes in phenology but no net
loss in range size or population size. Includes species in which climate
change is documented to be causing an increase in range size or abundance.</p>


<h2>2) Modeled Future (2050) Change in Range or Population Size</h2>

<p>This factor can include both distribution models and population
models. Models should be developed based on reasonably accurate locality
data (error &lt; 5km) using algorithms that are supported by peer-reviewed
literature. Areas of obvious overprediction should be removed from current
and predicted future distributions. Projections should be based on 'middle
of the road' climate scenarios for the year 2050. Range size should be
based on 'extent of occurrence' sensu IUCN Red List. Population models
should be based on known processes as described in peer-reviewed
literature. Examples include (a) phenological changes that are likely to
result in a mismatch with critical dietary, pollination, or habitat
resources (Visser and Both 2005) or (b) documented narrow temperature
tolerances and thermal safely levels, particularly in insects (Deutsch et
al. 2008, Calosi et al. 2008).</p>

<p>If necessary, check multiple boxes to reflect variation in model output.</p>

<p><b>Greatly Increase Vulnerability</b>: Predicted future range disappears
entirely from the assessment area OR predicted future abundance declines to zero
as a result of climate change processes.</p>

<p><b>Increase Vulnerability</b>: Predicted future range represents 50-99%
decrease relative to current range within the assessment area OR predicted
future abundance represents 50-99% decrease associated with climate change
processes.</p>

<p><b>Somewhat Increase Vulnerability</b>: Predicted future range represents a
20-50% decrease relative to current range within the assessment area OR
predicted future abundance represents 20-50% decrease associated with climate
change processes.</p>

<p><b>Neutral</b>: Predicted future range represents an increase, no change, or
a decrease of less than a 20% relative to current range within the assessment
area OR predicted future abundance increases, remains stable, or decreases
&lt; 20% as a result of climate change processes.</p>


<h2>3) Overlap of Modeled Future (2050) Range with Current Range</h2>

<p>Distribution models of current and projected future ranges should
meet standards described in the notes for D2. Overlap is calculated as the
percent of the current range represented by an intersection of the
predicted future and current ranges. If the range disappears or declines
&gt; 70% within the assessment area, such that factor D2 is coded as
Greatly Increase Vulnerability, this factor should be skipped to avoid
double-counting model results.</p>

<p><b>Greatly Increase Vulnerability</b>: There is no overlap between the
current and predicted future range within the assessment area.</p>

<p><b>Increase Vulnerability</b>: Predicted future range overlaps the current
range by 30% or less within the assessment area.</p>

<p><b>Somewhat Increase Vulnerability</b>: Predicted future range overlaps the
current range by 30-60% within the assessment area.</p>

<p><b>Neutral</b>: Predicted future range overlaps the current range by &gt; 60%
within the assessment area.</p>


<h2>4) Occurrence of Protected Areas in Modeled Future (2050) Distribution</h2>

<p>'Protected area' refers to existing parks, refuges, wilderness
areas, and other designated conservation areas that are relatively
invulnerable to outright habitat destruction from human activities and that
are likely to provide suitable conditions for the existence of viable
populations of the species. Models of current and projected future ranges
should meet standards described in the notes for D2. Modeled future
distribution may refer to a single season (e.g., breeding season
distribution or winter distribution) for migratory species. This factor
considers ranges and protected areas within the assessment area only.</p>

<p><b>Increase Vulnerability</b>: &lt; 5% of the modeled future distribution
within the assessment area is encompassed by one or more protected areas.</p>

<p><b>Somewhat Increase Vulnerability</b>: 5-30% of the modeled future
distribution within the assessment area is encompassed by one or more protected
areas.</p>

<p><b>Neutral</b>: &gt;30% of the modeled future distribution within the
assessment area is encompassed by one or more protected areas.</p>"

datD <- data.frame(section = "D",
                   guide_text = sectionD %>%
                     str_split_1("(?=<h2)")) %>%
  mutate(question = str_extract(guide_text, "(<h2>)(\\d)(\\))", group = 2),
         .before = guide_text)

# Exposure #====================================================================
exposure <- "<p>
    The Index treats exposure to climate change as a modifier of sensitivity and
    adaptive capacity. If the climate in a given assessment area will not change
    much, none of the sensitivity/adaptive capacity factors will weigh heavily,
    and a species is likely to score at the Less Vulnerable end of the range.
    A large change in temperature or moisture availability will amplify the effect
    of any related sensitivity/adaptive capacity factor, and will contribute
    to a score reflecting higher vulnerability to climate change. In most cases,
    changes in temperature and moisture availability will combine to modify
    sensitivity and adaptive capacity factors. However, for factors such as
    sensitivity to temperature change (factor 2a) or precipitation/moisture
    regime (2b), only the specified climate driver will have a modifying effect.
</p>"

exposure <- data.frame(section = "exposure", guide_text = exposure)

# Combine #=====================================================================
# Combine and save as csv to be edited for improved html readability
guide_dat <-  bind_rows(datB, datC, datD, exposure)

if(file.exists("data-raw/guideline_lu_tbl.csv")){
  message("The guideline look up table already exists and may contian edits")
  prompt <- readline("Are you sure you want to replace it? (y/n) ")
  if(prompt == "y"){
    write.csv(guide_dat, "data-raw/guideline_lu_tbl.csv", row.names = FALSE)
  }
}

# Need to run lookup_tbls.R to add it to internal package data
