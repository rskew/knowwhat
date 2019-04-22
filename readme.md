Prototype task tracking application. Record subtasks and collapse/expand groups, never losing track of the goals your tasks are heading towards.

To try it, run `pulp build` in the `purescript` directory to compile the purescript sources to javascript, then run `parcel ./index.html` in the top level directory to run the hotloading server at `localhost:1234` (hotloading needs work :/)

![example graph](https://github.com/rskew/workflow/raw/master/procrastination.png)

# Development

[Install PureScript][] and [Webpack][] globally.

Build and watch the PureScript project in one shell:

```shell
cd purescript
pulp --watch build
```

Then build the whole project in another shell:

```shell
./webpackz.sh
```

You may want to add the line `dist` to `.git/info/exclude` to avoid conflicts in the build in the repository.

 [Install Purescript]: https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md
 [Webpack]: https://webpack.js.org/guides/installation/
