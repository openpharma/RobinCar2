# Count Number of Events per Unique Event Time

This function counts the number of events at each unique event time
point in a survival dataset.

## Usage

``` r
h_n_events_per_time(df, time, status)
```

## Arguments

- df:

  (`data.frame`) containing the survival data.

- time:

  (`string`) name of the time variable.

- status:

  (`string`) name of the status variable, where 1 indicates an event and
  0 indicates censoring.

## Value

A `data.frame` with two columns: `time` and `n_events`, where `n_events`
is the number of events at each time point.

## Details

If there are no events in the dataset, it returns an empty `data.frame`.
