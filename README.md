# Zamonia

## Disclaimer

1. I have no idea what I'm doing, but I'm doing it. Suggestions about code style are of course welcome.
2. Don't expect anything from this tool, neither stability nor consistent update schedule
3. Consequently, do not expect Windows/MacOS support. Do it yourself if you want it, I'm making this tool for me.

## Installation

Using `stack`:
```
stack install
```

## Usage

You can use the `-h/--help` option to print the program/command/subcommand help.

The tool has 3 main commands, and multiple subcommands:
- `init`: Initialize the Zamonia database.
- `film`: Command leading to subcommands related to films.
- `series`: Command leading to subcommands related to series.
- `book`: Command leading to subcommands related to books.
- `tui`: Open the TUI

### Subcommands

All the subcommands are the same. Only optional arguments are different, they are listed in each subcommand help.

There is 11 subcommands:
- `add`: Add a work to the database, takes an index and a title. All other informations are optional.
- `delete`: Delete a work from the database, takes an index.
- `show`: Print informations related to a work, takes an index.
- `modify`: Modify a work, takes an index, all modified fields are optional. Using this without any optional argument lead to no change.
- `import-csv`/`import-json`: Import a database from a headerless (to discuss) CSV/JSON file.
- `export-csv`/`export-json`: Import a database to a headerless (to discuss) CSV/JSON file.
- `export-formatted`: Export a table to a document. The template can be in any markup language. See the section.
- `list`: List all the works by index and title.

### Formatted output

Using the subcommand `export-formatted`, you can export **the entire table** (e.g. Films or Series) to a text document by providing a template. The template is used for each entry in the table. There are keywords for each field in the table, and they can be used more than one time (or not at all).

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
- `%watched%`: Watching state

#### Series

Here are the keywords for series:
- `%index%`: Index of the series
- `%title%`: Title of the series
- `%originalTitle%`: Original title of the series
- `%director%`: Director of the series
- `%year%`: Year(s) of release
- `%episodeNumber%`: Number of episode
- `%seasonNumber%`: Number of seasons
- `%possession%`: Possession state
- `%watched%`: Watching state

#### Books

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

### Completions

You can activate completions by executing
```
$ source <(zamonia --bash-completion-script `which zamonia`)
```

## Remarks

- **The ID is supposed to be UNIQUE**, don't act surprised if you use the same ID and end up with problems...
- **This tool is not optimal**, it was made so I could use it. Contributions are, of course, welcome.

## TODO

- [ ] Add Search
- [ ] Add support for video games
- [ ] (in UI) Store series/movies/so on differently ?
- [ ] (in UI) insertion takes time. Too much time
- [ ] Add comments to the code
- [ ] Review **every** `case` in Zamonia.UI and see if Proxies can help
