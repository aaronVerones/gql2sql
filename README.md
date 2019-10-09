# GQL2SQL

Description: [https://wiki.ubc.ca/Course:GQL2SQL](https://wiki.ubc.ca/Course:GQL2SQL)

## Pre-requisites:
- Stack

## Setup:

```bash
git clone git@github.com:aaronVerones/gql2sql.git
cd gql2sql
stack build
```

## Usage:

run:
```bash
stack build && stack exec gql2sql-exe
```

test: (also runs `graphql-parser` tests)
```bash
stack test
```



## Notes for Developers:

- The Hasura GraphQL parser is in the `vendor` directory. There's no documentation for it, so you'll have to figure out how it works by looking at the code, specifically the `vendor/graphql-parser-hs/src/Language/GraphQL/Draft/Syntax.hs` file
- The entrypoint for the app is `app/Main.hs`
- Tests are in `test/Spec.hs`
- The schema the program parses is `./schema.graphql`
- [Dope GQL stuff](https://raw.githubusercontent.com/sogko/graphql-shorthand-notation-cheat-sheet/master/graphql-shorthand-notation-cheat-sheet.png)

# TODO

1. Modify `schema.graphql` so that it's more interesting
2. Complete the function `Main.hs: ast2sql`
3. Complete the function `Main.hs: ast2dot`
