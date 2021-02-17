# RCowView
 
Contains methods to analyse RTLS data.


- `cows.R`: methods to work with data on cows
- `analysis.R`: analysis methods
- `data.R`: data manipulation methods
- `database.R`: database methods
- `example.R`: example code to read, process and visualise data
- `farmXXX.R`: farm-specific methods (each farm should have a separate file)
- `init.R`: specified paths for input files and other user-specific settings here (not synced on GitHub)
- `plot.R`: plotting methods
- `time.R`: example comparing running time to read input files
- `tmp.R`: sandbox for your own methods (not synced on GitHub)


## Ignored files

Certain files are ignored and not synced on GitHub:
- `data` folder and all its contents
- `init.R`: specified paths for input files and other user-specific settings here (not synced on GitHub)
- `tmp.R`: sandbox for your own methods (not synced on GitHub)
- Any file with name that contains hyphen (-), e.g. `analysis-MC.R`


## Before you start

- Make sure that you have all the necessary data files (e.g. `barn.csv`, `tags.csv`) in the `data` folder (not synced on GitHub)
- Specify paths for input files in `init.R`


## Data handling

To match CowView data with data on cows, we use the following routines. Cow data are stored in a dataframe `cowData` with at least columns `CowID, Lactation, CalvingDate`, mapping between cows and tags is tored in `cowTagMap` dataframe with columns `CowID, Tag, From` (the latter indicates the date when the tag was assigned to a cow). There is `getCowID(tag, date, cowTagMap)` function in `data.R` to determine ID of a cow with certain tag on a specific day.


### Barn data

Barn data are stored in a dataframe with columns `Unit, x1, y1, x2, y2, x3, y3, x4, y4` corresponding to unit name and four pairs of coordinates for the four points defining the rectangle (bottom-left, top-left, top-right, bottom-right).

## Different farms

Our core code should be independent of specific farms, thus we make use of "abstract"" functions (i.e. those that need to be overriden for each specific farm). Few examples are: `readBarnData, readCowData, readCowTagMap`. They need to be defined in `farmXXX.R` files along with other farm-specific functions. Currently, there are two versions for frams Lad and Wim.
