# Birdbrain Disc Golf Standings System
This document and repository are the R scripts used to process scores of Birdbrain Disc Golf, 
a travelling handicap singles disc golf league in Northern Illinois. The league has been playing since 2016, and over 650 unique
members have joined for a round of disc golf. There are many different courses and layouts in the area, so a system needed to
be made in order to make handicaps more fair than simply comparing to par.

The repository contains the following:
runsheet - where everything should be sourced/called/etc.
bbsetup - to be run at the beginning of the season ONLY
bbround_udisc - to be used if the league is using UDisc scoring
bbround_pdga_hbh - to be used if the league is using PDGA Live scoring and the data export used is hole-by-hole results
bbround_pdga_col - to be used if the league is using PDGA Live scoring and the data export used is total results only
bbinput - called within the respective bbround_() function to re-load the input GoogleSheets
bbpoints - called within the respective bbround_() function to add points to the season standings and all-time leaderboard
bbsham - called within the respective bbround_() function - the Slope and Handicap Adjustment Machine
bbpools - called at the league admin's discretion from the runsheet - puts players into pools for grand slope adjustment

This document will go over each R script's inputs, process, and output. It contains, as an example to run, 20 UDisc rounds
that have been blinded for player privacy. The runsheet script will populate these standings.


# Runsheet

Everything should be sourced, owned, and operated from the runsheet. The key inputs that have to be adjusted for each season are
as follows:

1. Setting the working directory (line 13) - this is where all of the round recaps will get written.
2. Assigning the folder name to input (line 16) - where the UDisc or PDGA Live scores are saved
3. Assigning out (line 19) - the GoogleSheets link that you want the league data to write to

After that starts the true 'runsheet'. Technically, you should be able to execute lines 20 through to the end of the document at
any point to produce the current league standings. You don't want to do that, though, because requesting GoogleSheets access that 
many times in a row is rough!

The next step is to use bbsetup() to create the league season infrastructure in GoogleSheets.


At this point, the league administrator should manually input or upload the league schedule on the newly set up 'League Schedule'
tab of the Sheet. It doesn't have to be permanent or complete, but it has to be ready before any league round functions are done.

Each runsheet will have the following components:
bbsetup() called once, to start the season.
bbround_() calls numbered 1-X, corresponding to the rows in the 'League Schedule' tab of the Sheet
bbpools() called as needed

At this stage, there is no recap / finalization / carry-over process, so this is all that runsheets should contain.

# League Setup with bbsetup()

The parameters of the bbsetup() function are explained in detail within the bbsetup.R script. If this is the first EVER function
call for your league, it will look like this:

bbsetup(link = out, first = 1)

Where out is what was defined in line 19 of the runsheet. That's it! If this is not the first Birdbrain season in your league's
history, then first becomes '0' and the oldwb and year variables must be updated. Note that 'oldwb' will be in the same format
as out from the runsheet (i.e. a GoogleDocs link), and year is simply for the all-time tab update so it doens't need to be a
specific date. It only has to reflect how you want to chronicle seasons.

After setup is done, it's time to complete a round.

# Processing completed league rounds with bbround_()

Within bbround() are almost all of the other functions, so you don't need to worry about the inner workings of bbinput,
bbsham or bbpoints. The syntax for inputting a new round are within the bbround() script itself, but here is a rundown 
in pseudocode of what happens for each call of bbround():

1. bbinput() is called, which reloads all of the parts of the GoogleSheet.
2. Today's scores are added to the 'Season Scores' dataframe.
3. bbpoints() is called, which adds points to the current leaderboard as well as the all-time dataframe.
4. the program takes a look at how many rounds the league has played and determines whether or not to formally
run bbsham() for slope and handicap calculations, to run the first iteration around round 11 or 12 (which is an abbreviated
version of the function that lives within bbround functions), or to do neither and simply use course par.
5. no matter what happens in (4), the handicap dataframe is updated and the handicap component of the leaderboard is
updated, completing that dataframe. The slope and pools dataframes are obviously only updated if those options are selected
from (4).
6. The hot round of the day is checked against the current course record, and the dataframe updates if it has to.
The next two steps do not happen in bbround_pdga_col, because there is no hole-by-hole data:
7. The entire dataframe is checked for aces, and the dataframe is updated if it has to.
8. The hole-by-hole averages are calculated and the 6 hardest / easiest holes are displayed.

# Updating the player pools with bbpools()

The bbpools() function has a few steps:
1. Measures the average score per player, relative to par.
2. Ranks all players in quintiles, A through E pool.
3. Filters out players who have not played half the number of rounds yet (i.e., returns 'Pool' to null)

This is required prior to bbsham() (and thus it does not come after bbsham ever in the bbround functions). There must
be players with Pool assigned to run the slope/handicap calculations!

# Conclusion

As of March 26, 2026, this is still a work in progress as I update the intricacies of the menu calls and adapt stuff from
my local setup. But all in all, this is a passion project and I know it works for my league. If you have any questions
please don't hesitate to reach out!
