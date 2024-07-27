# time-report

KISS solution for time tracking.  Parses a text file containing a work time log
that can be used for billing purposes.  This clojure program was a learning exercise
to replace a far more complex ruby script.

## Tme Log Format

I used this text file format to track my billable hours for years when working as a consutant.

Each day in the time log starts with a `Date:` line followed by one or more
project lines.  Each project line starts with a client id, comma, and project id.
These are followed by a `:` and a series of comma separated start and stop times
in the form `hhmm-hhmm`.

```
Date: Thursday 07/04/2024
acme,cms: 0835-1155,1400-1500,1530-1810
bozon,prototype: 1205-1400,1810-2000

Date: Friday 07/05/2024
acme,cms: 0815-1415
bozon,prototype: 1515-1820
```

## Data Structures

The original ruby script parsed the file to produce instances of various classes.
Methods on those classes were used to process the data.  This clojure program
instead parses the time log into a single map/vector structure.  Processing
the time as data turned out to be far easier to understand than the OO solution was.

Each day is represented by a map like this:

```
{
    :year 2024
    :month 7
    :day 4
    :day-number: 4
    :week-number 2000
    :times [
        {
            :client "acme"
            :project "cms"
            :elapsed 200
            :times [
                {
                    :start "0835"
                    :end "1155
                    :elapsed 200
                }
            ]
        }
    ]
}
```


## Usage

The program requires the path to the time log as its sole positional argument.
An optional date tells the program what "cycle" to print time for.
A cycle is a half-month (1-15 or 16+).  The date given can be any date in the cycle.

To see complete output without a data file you can use `random` as a file name.
The program will then compute random data on the fly and report it.

Allowable modes are:

- `weekly` (the default): Report daily and weekly times for wach week in a "cycle".
- `random`: Generate a time log file using randomly generated data and print it to stdout.

```
time-tracker OPTIONS file-name
  -d, --date DATE  2024-07-25  Set base date (defaults to today).
  -m, --mode MODE  weekly      Select the execution mode.
  -h, --help                   Print help message and exit.
```

In `weekly` mode you can use `random` as a file name to have the program 
generate the report with random data.
