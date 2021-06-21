# Zamonia

## Installation

Using `stack`:
```
stack install
```

An `AUR` package is planned.

## Usage

You can use the `-h/--help` option to print the program/command/subcommand help.

The tool has 3 main commands, and multiple subcommands :
- `init` : Initialize the Zamonia database.
- `film` : Command leading to subcommands related to films.
- `serie` : Command leading to subcommands related to series.

### Subcommands

All the subcommands are the same for `film` and `serie`. Only optional arguments are different, they are listed in each subcommand help.

There is 10 subcommands :
- `add` : Add a work to the database, takes an index and a title. All other informations are optional.
- `delete` : Delete a work from the database, takes an index.
- `show` : Print informations related to a work, takes an index.
- `modify` : Modify a work, takes an index, all modified fields are optional. Using this without any optional argument lead to no change.
- `import-csv`/`import-json` : Import a database from a headerless (to discuss) CSV/JSON file.
- `export-csv`/`export-json` : Import a database to a headerless (to discuss) CSV/JSON file.
- `list` : List all the works by index and title.
- `search` : Not implemented yet.

### Completions

You can activate completions by executing
```
$ source <(zamonia --bash-completion-script `which zamonia`)
```

## TODO

- [ ] Add Search
- [ ] Add support for books and video games
- [ ] Make a package for the AUR
- [ ] Enhance and **document** the code
