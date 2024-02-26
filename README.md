# IndiSample
Tools for sampling various socio-environmental indicators from spatial raster/vector data onto adminstrative areas. Currently this is just for air pollution, but can be extended as needed. It also still needs a few comments, apologies!

IMPORTANT:
When running this code, set your cwd (setcwd(<PATH>)) to somewhere you're happy for it to very much make it's own, ideally a bespoke directory. The program will download datasets as needed, which can take a while, especially the OS ones.

To produce the graphs of air pollution:
1. set the cwd somewhere appropriate.
2. import the package. "devtools::install_github('Jonathan-Lee-UofG/IndiSample')".
3. run "\[IndiSample::\]makeAllAirPolPlots()" and wait. The first run will take a bit, since it'll have to download and process stuff.
