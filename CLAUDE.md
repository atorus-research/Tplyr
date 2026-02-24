# Project overview

- Name: Tplyr
- One-line description: A traceability focused tool created to simplify
  the data manipulation necessary to create clinical summaries.
- Main users / audience: Clinical statistical programmers creating
  regulatory outputs for clinical trials

## Why this project exists

- Primary goal: Simplify the creation of clinical tables used within
  clinical regulatory reporting

## What is in this repo

- Tech stack: R package generally following the tidyverse style of
  programming
- Important directories:
  - `./R`: all packages objects and functions
  - `./tests/testthat`: package test suite
- Key services/components and what they do:
  - tplyr_table: the main container upon which a Tplyr table is
    constructed
  - tplyr_layer: Individual layer of a tables that specifies a specific
    analysis to be performed and stack
  - f_str: Formatting controller used to specify how resulting strings
    of numbers should appear

## How to work on this project

When editing this repo:

1.  Prefer small, focused changes with clear intent.
2.  Ask before large refactors unless explicitly requested.
3.  Keep existing patterns unless there is a strong reason to change
    them.

### Commands

- Run tests:
  - `devtools::test()`

## Coding standards

- Language(s): R
- Style:
  - Follow the tidyverse style guide for R package development, unless
    it conflicts with an existing pattern
- Testing:
  - Preferred test framework(s): testthat
  - Only create new tests when new functionality has been added
  - Do not create new tests against existing functionality
