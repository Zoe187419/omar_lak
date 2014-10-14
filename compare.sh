#   1) translate all data sets to one which has data for all participants
#   2) idenitfy the treatment and controls (add column marker) 
#       - may shrink dataset
#       - allows for null distribution (IGNORING THIS)
#   3) create matched dataset (mark treatment and matched control) 
#       - may shrink dataset
#       - allows for pair swap null distribution
#       * matching methods 
#           all euclidian neighbors, single parameter with averaging
#           one euclidian neighbor, many parameters
#           one propensity and/or prognositc neighbor, many parameters
#           many propensity neighbors, full matching, many parameters
#   4) analyze result and create graphics
#       * ks distributions for matched variables
#       * null distribution, pair swap distribution, nieve bar + width, matched bar + width,  

#--------------------------------------------------
# JARED'S SINGLE PARAMETER MATCHING

# learning communities
# added the id column, matched on sst, and then made gender a numeric value to accomodate my matching code 
    myquick_resample.py -if input-new.csv -id_col 9 -mat 3 -comp 8 -dist 0.1 -user "[7,'eq',1]" -non "[7,'eq',0]" # sst
    myquick_resample.py -if input-new.csv -id_col 9 -mat 10 -comp 8 -dist 0.1 -user "[7,'eq',1]" -non "[7,'eq',0]" # gender

# problem roulette (acoach08)
    myquick_resample.py -if pr_lak_ecoach.csv -id_col 0 -mat 4 -comp 1 -dist 0.1 -user "[9,'eq',1]" -non "[9,'eq',0]"   # match on goal grade
    myquick_resample.py -if pr_lak_both.csv -id_col 0 -mat 3 -comp 1 -dist 0.1 -user "[10,'eq',1]" -non "[10,'eq',0]"   # match on gpa
    myquick_resample.py -if pr_lak_gpa.csv -id_col 0 -mat 3 -comp 1 -dist 0.1 -user "[5,'eq',1]" -non "[5,'eq',0]"      # match on gpa

# ecoach (mcdb matched on gpa)
    myquick_resample.py -if mcdb2.csv -id_col 16 -mat 11 -comp 0 -dist 0.1 -user "[15,'eq',1]" -non "[15,'eq',0]"

#--------------------------------------------------
# CHRIS' PAIRED MATCHING METHOD

# learning communities
    python resample.py -i ../data_lak_2015/input-new.csv -c ../data_lak_2015/definitions-new.csv -m "treat" -o chris_pairs_lc.csv

# problem roulette (acoach08)
    python resample.py -i ../data_lak_2015/pr_lak_both.csv -c ../data_lak_2015/definitions-pr_lak_both.csv -m "tried_xxx" -o chris_pairs_pr_lak_both.csv
    python resample.py -i ../data_lak_2015/pr_lak_ecoach.csv -c ../data_lak_2015/definitions-pr_lak_ecoach.csv -m "tried_xxx" -o chris_pairs_pr_lak_ecoach.csv
    python resample.py -i ../data_lak_2015/pr_lak_gpa.csv -c ../data_lak_2015/definitions-pr_lak_gpa.csv -m "tried_xxx" -o chris_pairs_pr_lak_gpa.csv

# ecoach (mcdb matched on gpa)
    python resample.py -i ../data_lak_2015/mcdb2.csv -c ../data_lak_2015/definitions-mcdb2.csv -m "usage.ratio_xxx" -o chris_pairs_mcdb2.csv


#--------------------------------------------------
# OMAR'S PAIRED MATCHING METHOD

R> source('jared.r') # edit the proper config

