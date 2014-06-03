#!/bin/bash

#myquick_resample.py -if ../data_lak_2015/stats_nts_f13.csv -id 0 -mat 1 -comp 1 -dist 0.1 -user "[5,'gt',20]" -non "[5,'le',20]"

#myquick_resample.py -if ../data_lak_2015/stats_nts_f13.csv -id 0 -mat 1 -comp 2 3 -dist 0.1 -user "[5,'gt',20]" -non "[5,'le',20]" -pre "[2,'ne','']"
myquick_resample.py -if ../data_lak_2015/stats_nts_f13.csv -id 0 -mat 1 -comp 2 3 -dist 0.1 -user "[5,'gt',100]" -non "[5,'le',100]" -pre "[2,'ne','']"
