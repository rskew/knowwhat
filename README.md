# Workflow

Graph drawing for mindmapping, knowledge engineering and creative-tool-making.

![example graph](./exampleGraph.png)

### Ramblings on Implementation

The core data-structure is the (newly minted by moi) 'megagraph', sort of a nested graph where nodes are linked to graphs, and edges are linked to mappings between graphs. A mapping between graphs is a function from the nodes of the soruce graph to the nodes of the target graph, and a similar function for egdes. Equations between paths of a graph are also supported.
Other proposed names for this data structure:

- nolog (nested-olog)
- ultra-megagraph
- bivuac

The other core date-structure is updates to the megagraph. These come in two strata:

- the MegagraphOperation which describes a simple update to the base megagraph data
- the AppOperation which is a MegagraphUpdate (and Array MegagraphOperation) and the update to the history of a particular element.

Implementing an interpreter for MegagraphOperation and HistoryUpdate in a way that satisfies the requied laws (TBD, for now use common sense/shaman intuition) is all that's needed to share and sync megagraphs across different grounding representations. Two implemented in this repo are:

- Purescript data structures, used in the front-end
- SQLite databases, used in the back-end

## Quick start

You will either need [NPM](https://www.npmjs.com/get-npm) or [Yarn](https://yarnpkg.com/) installed.

### NPM

```sh
npm run dev     # run development server
npm run build   # build production assets
```

### Yarn

```sh
yarn dev        # run development server
yarn build      # build production assets
```

### Run the server

```sh
cd dist
node ../server/main.js
```

## Development

We use [spago](https://github.com/spacchetti/spago) to manage our Purescript dependencies.
While this is installed as a `dev-dependency` in `package.json` you may want to install it directly to make it easier to manage these dependencies directly.

## License

The project is licensed under the terms of the Apache License (Version 2.0).

See [LICENSE](./LICENSE) or http://www.apache.org/licenses/LICENSE-2.0 for details.
