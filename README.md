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

- The Hasura GraphQL parser is in the `vendor` directory. There's no documentation for it, so you'll have to figure out how it works by looking at the code
- The entrypoint for the app is `app/Main.hs`
- Tests are in `test/Spec.hs`
- The schema the program parses is `./schema.graphql`

# TODO

1. Modify `schema.graphql` so that it's more interesting
2. Write a function to 
