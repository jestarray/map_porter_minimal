# Standalone map porter

Converts only the `.dmm` files directly inside this project's `custom_maps/`
folder into Tiled JSON maps in `custom_maps_REMAKE/`.

## Run

Install Racket, then run from this folder:

```sh
racket map_porter.rkt
```

You can also pass the full path to `map_porter.rkt` from any working directory.
All input, asset, and output paths are relative to this project folder.
No additional Racket packages are required.

## Included files

- `map_porter.rkt`: converter, including the unique layer ID fix.
- `custom_maps/`: copies of the source DMM maps.
- `STANDARD/stan_new.json`: output map template.
- `tsd/`: tileset definitions used by the converter and generated maps.
- `tilesets/`: PNG images referenced by those definitions.
- `objecttypes.json`: object property defaults used during conversion.
- `ztlcpaths_default_vars.clj`: BYOND default variable lookup data.
- `MISSING_TEXTURES.sx`: existing missing-texture records; also updated by runs.
- `map_porter.tiled-project`: optional Tiled project with the original custom
  property types and only local folder references.

Open `map_porter.tiled-project` in Tiled to browse the generated maps with their
custom property types. Keep `custom_maps_REMAKE/`, `tsd/`, and `tilesets/` in their
relative locations so tileset and image references resolve.

## Generated output

- `custom_maps_REMAKE/*.json`: converted maps; matching files are overwritten.
- `all_map_stats.csv`: conversion statistics, replaced on each run.
- `CUSTOM_TILE_DATA.sx`: appended custom tile diagnostics.
- `MISSING_TEXTURES.sx`: appended newly encountered missing texture paths.

The whole project folder can be moved or copied independently of the original
repository. The original converter and original map folders are not used by this
copy. Apart from project-relative paths, `.dmm` input filtering, and restricting
the batch to `custom_maps/`, conversion behavior is preserved.

## Validation

The initial run converted all 115 maps successfully from an unrelated working
directory. Every output map has unique layer IDs and a correct `nextlayerid`;
all tileset and image references resolve inside this project.

`map_porter_log.txt` contains that run's output. Two warnings came from the
existing layer-capacity handling: removal of excess blood tiles and insufficient
space on an overhead layer. Those conversion rules have not been changed.
To capture a fresh log, run:

```sh
racket map_porter.rkt > map_porter_log.txt 2>&1
```
