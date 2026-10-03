# crease-gen

Generates origami crease patterns in Scala 3, checks that the paper can actually fold flat, and draws the folded result.

## Run

```
scala-cli run .                          # all models -> out/
scala-cli run . -- out miura-ori bird-base
scala-cli test .
```

Each model produces:

- `<name>.svg`: the crease pattern (mountains red, valleys dashed blue)
- `<name>-folded.svg`: the folded model, if it folds
- `<name>.fold`: open it in [Origami Simulator](https://origamisimulator.org)

For models labelled by search, these files show the best folding found, and `<name>-2.*` and `<name>-3.*` show the runners-up.

## How it works

1. **Pattern**: creases on a sheet, cut into a graph of vertices, edges and facets (`pattern/`).
2. **Laws**: every vertex must satisfy Kawasaki, Maekawa and big-little-big. Unlabelled creases get mountain/valley labels by search; creases marked optional may also stay flat (`laws/`).
3. **Fold**: place every facet, then find a layer order where paper never passes through paper (`folding/`).
4. **Output**: SVG and FOLD files (`output/`).

The geometry, including the seven Huzita-Hatori folding axioms, is in `geometry/`. The models are in `library/Patterns.scala`.

## Models

These fold: accordion, Miura-ori, waterbomb base, preliminary base, bird base.

The `*-lines` models give only the lines and let the search label them: `bird-lines`, `yoshimura-lines`, `waterbomb-lines`. The same lines usually fold flat in many ways, so the search keeps the distinct foldings it finds (a folding and its turned-over copy count once) and ranks them, fewest layers first. They are what these lines can do, not the named model.

These fail on purpose, each for a different reason:

- `impossible-x` breaks Maekawa.
- `dead-end` has a crease that stops mid-sheet.
- `hypar` needs the paper to bend.
- `uniform-rule` passes every vertex law, but no layer order exists.
