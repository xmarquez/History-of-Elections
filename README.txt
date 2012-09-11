This set of files contain the code for recreating the graphs in this post:

http://abandonedfootnotes.blogspot.co.nz/2012/09/the-great-norm-shift-and-triumph-of.html

You will also need the PIPE dataset, which is available here:
https://sites.google.com/a/nyu.edu/adam-przeworski/home/data

And some patience; the PIPE dataset needs some cleanup before the graphs can be recreated.

You will first need to use Google Refine (http://code.google.com/p/google-refine/) and the cleanup.JSON file to clean the country names and do a few other minor tidying up.
Then you will need to run the code that adds capital longitudes and latitudes (right now this is commented out in PIPE.R; you will need to uncomment it). 

I also manually cleaned up the resulting dataset using Google Refine (adding franchise information for Russia, for example) and added a few capitals that were not available in the cshapes dataset. 

Then you will need to save the resulting file as PIPE.csv and run Processing PIPE.R to get the data ready for graph creation.

If all goes well (which it probably won't) you should get something that can be used by Final Graphs.R to produce 
something much like the graphs in the post.