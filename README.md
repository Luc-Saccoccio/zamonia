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
- `export-formatted`: Export a table to a document. The template can be in any markup language. See the section.
- `list` : List all the works by index and title.
- `search` : Not implemented yet.

### Formatted output

Using the subcommand `export-formatted`, you can export **the entire table** (e.g. Films or Series) to a text document by providing a template. The template is used for each entry in the table. There are keywords for each field in the table, and they can be used more than one time (or not at all).

#### Example

Example in LaTeX, for films :
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

Here are the keywords for films :
- `%index%`: Index of the film in the table
- `%title%`: Title of the film in the table
- `%originalTitle%`: Original title of the film in the table
- `%year%`: Year of release of the film in the table
- `%possession%`: Possession of the film in the table
- `%watchde%`: Watching state of the film in the table

#### Films

Here are the keywords for series :
- `%index%`: Index of the series in the table
- `%title%`: Title of the series in the table
- `%originalTitle%`: Original title of the series in the table
- `%year%`: Year(s) of release of the series in the table
- `%episodeNumber%`: Number of episode of the series in the table
- `%seasonNumber%`: Number of seasons of the series in the table
- `%possession%`: Possession of the series in the table
- `%watchde%`: Watching state of the series in the table

### Completions

You can activate completions by executing
```
$ source <(zamonia --bash-completion-script `which zamonia`)
```

## TODO

- [ ] Add Search
- [ ] Write a man page (for AUR install at least)
- [ ] Add support for books and video games
