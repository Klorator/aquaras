# Example runlist data frame

An example runlist for demonstration and test purposes.

## Usage

``` r
ras.Example_Runlist
```

## Format

A data frame with 672 rows and 14 variables:

- Index:

  Unique row identifier

- Plate:

  Well plate number, 3–9

- Row:

  Row letter, A-H

- Col:

  Column number, 1-12

- LC_Position:

  Well coordinate, composite of "`Plate`:`Row`,`Col`"

- Date:

  Date as character string

- Signature:

  Signature to identify owner

- Sample_name:

  Sample name, composite of "`Date`\_`Signature`\_Index.`Index`"

- Compound:

  Name of compound being tested

- Timepoint:

  Integer for plotting timeseries

- Well_Type:

  Type of well contents, one of "bead", "medium", "cell", "STD", or
  "blank"

- LC_Well_Type:

  Either "blank" or "Analyte"

- Replicate:

  Integer for numbering replicate experiments

- Sample_text:

  Sample text, composite of
  "`Compound`\_`Timepoint`\_`Well_Type`\_`Replicate`"

## Source

Created in house
