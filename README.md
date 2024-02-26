# IndiSample
Tools for sampling various socio-environmental indicators from spatial raster/vector data onto adminstrative areas. Currently this is just for air pollution, but can be extended as needed.

IMPORTANT:
When running this code, set your cwd (setcwd(<PATH>)) to somewhere you're happy for it to very much make it's own, ideally a bespoke directory. The program will download datasets as needed, which can take a while, especially the OS ones.

To produce the graphs of air pollution:
> set the cwd somewhere appropriate.
> import the package.
> run "makeAllAirPolPlots()" and wait. The first run will take a bit, since it'll have to download and process stuff.
