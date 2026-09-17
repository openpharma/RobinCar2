# Prepare Events Table

This function creates a data frame summarizing the number of patients
and events for each treatment arm and stratification factor.

## Usage

``` r
h_events_table(data, vars)
```

## Arguments

- data:

  (`data.frame`) The data frame containing the survival data.

- vars:

  (`list`) A list containing the treatment, time, status, and strata
  variables.

## Value

A data frame with columns for the treatment, strata, number of patients,
and number of events.
