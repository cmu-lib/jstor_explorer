#!/bin/bash

rsync -avh --progress --delete --exclude-from rsync_exclude.txt ./* mlincoln@shiny.library.cmu.edu:/vol/shiny/apps/exploravec/.
