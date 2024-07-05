# time-report

KISS solution for time tracking.  Parses a text file containing a work time log
that can be used for bill purposes.  I used this text file format to track my
billable hours for years when working as a consutant.  This clojure program was
a learning exercise to replace a far more complex ruby script.

## Usage

The program requires the path to the time log as its sole positional argument.
An optional date tells the program what "cycle" to print time for.
A cycle is a half-month (1-15 or 16+).  The date given can be any date in the cycle.

```
usage: time-tracker OPTIONS file-name
  -d DATE, --date DATE  2024-07-05  Set base date (defaults to today).
  -h, --help                        Print help message and exit.
```

## Tme Log Format

Each day in the time log starts with a `Date:` line and followed by one or more
project lines.  Each project line starts with a client id, comma, and project id.
These are followed by a `:` and a series of comma separated start and stop times
in the form `hhmm-hhmm`.

```
Date: Thursday 07/04/2024
acme,cms: 0835-1155,1400-1500,1530-1810
bozon,prototype: 1205-1400,1810-2000

Date: Thursday 07/05/2024
acme,cms: 0815-1415
bozon,prototype: 1515-1820
```
