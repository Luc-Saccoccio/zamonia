# Zamonia

## Installation

Using `stack`:
```
stack install
```

If you are using Arch, you can install the AUR package `zamonia-bin` e.g. :
```
yay -S zamonia-bin
```

## Usage

You can use the `-h/--help` option to print the program/command/subcommand help.

The tool has 3 main commands, and multiple subcommands :
- `init` : Initialize the Zamonia database.
- `film` : Command leading to subcommands related to films.
- `series` : Command leading to subcommands related to series.

### Subcommands

All the subcommands are the same for `film` and `series`. Only optional arguments are different, they are listed in each subcommand help.

There is 11 subcommands :
- `add` : Add a work to the database, takes an index and a title. All other informations are optional.
- `delete` : Delete a work from the database, takes an index.
- `show` : Print informations related to a work, takes an index.
- `modify` : Modify a work, takes an index, all modified fields are optional. Using this without any optional argument lead to no change.
- `import-csv`/`import-json` : Import a database from a headerless (to discuss) CSV/JSON file.
- `export-csv`/`export-json` : Import a database to a headerless (to discuss) CSV/JSON file.
- `export-tex`: Export a table to a document. Altough the command indicate TeX, the template can be in any markup language. See the section.
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
- [ ] Write documentation for `export-tex`
