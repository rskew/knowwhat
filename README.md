# Knowwhat

Graph drawing for mindmapping, knowledge engineering and creative-tool-making.

![example graph](./exampleGraph.png)


## Quick start

You will need Docker and docker-compose installed.

From the repo root, run `docker-compose -f deployment/docker-compose.yml up`, then head to `localhost:8085` in the browser. You should see a blank graph with title 'home', ready for editing `:D`

Double-click on the background to add a new node, and explore from there.

The interface is under development. The best way to find out what commands are supported is to look at the `handleKeypress` function in `src/GraphComponent/handleAction.purs`.


## Development

For development, bring up the backend with `docker-compose`. In stead of the nginx fileserver container with the built-in version of the frontend, run a local devserver for your modified frontend with [parcel](https://parceljs.org/).

We use [spago](https://github.com/spacchetti/spago) to manage our Purescript dependencies.
While this is installed as a `dev-dependency` in `package.json` you may want to install it directly to make it easier to manage these dependencies directly.

After running `spago test` to compile the purescript to javascript, run `parcel assets/index.html --no-hmr` to build the front-end and run a dev server you can point your browser at. 


## License

The project is licensed under the terms of the Apache License (Version 2.0).

See [LICENSE](./LICENSE) or http://www.apache.org/licenses/LICENSE-2.0 for details.
