# InspriRAD: Cross platform vector and matiricies on scala 3

ALWAYS reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

This is a scala 3 project using the mill build tool that wants to explore automatic differentiation in scala. It build on `vecxt`, which should deal with the low level operations. You can proviode suggestions for things to implement in vecxt, if you think they are missing. Flag it to the operator.

Act as expert in linear algebra, autodiff and computational mathematics.

## Working Effectively

Mill may be found via it's wrapper script `./mill` in the root of the repository. For example `./mill vecxt.__.compile` will compile the JVM, JS and native targets.

Use `cellar` you have this skill, to look up type signatures, members, and docs for any dependency directly from the terminal. Cellar queries the API of any published Maven artifact and returns results as Markdown, no JAR unpacking needed. Use this skill whenever you need to call an unfamiliar library method, explore a package's types, or check a dependency's API. Prefer cellar over Metals MCP only for looking up external dependency APIs (`cellar get-external` vs Metals `inspect`/`get-docs`) — cellar needs no project import and queries any published Maven artifact. For everything else (references, rename, goto definition, diagnostics, compile), use Metals.

##  Validation

The primary form of validation is via unit testing.

Tests are writting using scala munit, see the folder structure for locations.

In general it is expected that all tests, pass on all platforms. The number 1 goal of this library is *correctness* - this goal takes precedeance above all else. Continually check and flag potentially any suspected incorrect calculation.

The second, equally important goal of this library is *performance* and *usability*. Prefer usability where forced to choose against performance.

The third goal of this library is *cross platform consistency*.

Cross platform consistency is measured by compliance with the cross platform tests.

When writing code, follow the coding guidelines in `styleguide.md` in the root of the repository.

## Code Guidelines

Use inline methods where possible to avoid dispatch overhead.

## GitHub Actions CI
The project uses GitHub Actions for CI/CD

## Gotchas
