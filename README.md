# Zamonia

## Installation

Using `stack`:
```
stack install
```

If you are using Arch, you can install the AUR package `zamonia-bin` e.g.:
```
yay -S zamonia-bin
```

## Usage

You can use the `-h/--help` option to print the program/command/subcommand help.

The tool has 3 main commands, and multiple subcommands:
- `init`: Initialize the Zamonia database.
- `update`: Only required if the database was initialized before 0.2.0.0. Correct the database.
- `film`: Command leading to subcommands related to films.
- `series`: Command leading to subcommands related to series.

### Subcommands

All the subcommands are the same for `film` and `series`. Only optional arguments are different, they are listed in each subcommand help.

There is 11 subcommands:
- `add`: Add a work to the database, takes an index and a title. All other informations are optional.
- `delete`: Delete a work from the database, takes an index.
- `show`: Print informations related to a work, takes an index.
- `modify`: Modify a work, takes an index, all modified fields are optional. Using this without any optional argument lead to no change.
- `import-csv`/`import-json`: Import a database from a headerless (to discuss) CSV/JSON file.
- `export-csv`/`export-json`: Import a database to a headerless (to discuss) CSV/JSON file.
- `export-formatted`: Export a table to a document. The template can be in any markup language. See the section.
- `list`: List all the works by index and title.
- `search`: Not implemented yet.

### Formatted output

Using the subcommand `export-formatted`, you can export **the entire table** (e.g. Films or Series) to a text document by providing a template. The template is used for each entry in the table. There are keywords for each field in the table, and they can be used more than one time (or not at all).

If the update consists of the addition of a table, `zamonia init` is sufficient.

#### Example

Example in LaTeX, for films:
```tex
\item \textit{%index%}\\
	\textbf{Title:} %title%\\
	\textbf{Original Title:} %originalTitle%\\
	\textbf{Director:} %director%\\
	\textbf{Year:} %year%\\
	\textbf{Possession:} %possession%\\
	\textbf{Watched ?} %watched%
```

#### Films

Here are the keywords for films:
- `%index%`: Index of the film
- `%title%`: Title of the film
- `%originalTitle%`: Original title of the film
- `%director%`: Director of the film
- `%year%`: Year of release
- `%possession%`: Possession state
- `%watchde%`: Watching state

#### Films

Here are the keywords for series:
- `%index%`: Index of the series
- `%title%`: Title of the series
- `%originalTitle%`: Original title of the series
- `%director%`: Director of the series
- `%year%`: Year(s) of release
- `%episodeNumber%`: Number of episode
- `%seasonNumber%`: Number of seasons
- `%possession%`: Possession state
- `%watchde%`: Watching state

####

Here are the keywords for books:
- `%index%`: Index of the book
- `%isbn%`: ISBN of the book
- `%title%`: Title of the book
- `%originalTitle%`: Original title of the book
- `%author%`: Author
- `%publisher%`: Publisher
- `%year%`: Year of release
- `%possession%`: Possession state
- `%read%`: Reading state

### Updating

The structure of the database is supposed to be static. **If and only if** there is a error in the name of a column/table, or **if and only if** a field is missing should the structure change. In that case, a new command will be added to update the database accordingly.

### Completions

You can activate completions by executing
```
$ source <(zamonia --bash-completion-script `which zamonia`)
```

## TODO

- [ ] Add Search
- [ ] Write a man page (for AUR install at least)
- [ ] Add support for video games
