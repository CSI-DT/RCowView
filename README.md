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