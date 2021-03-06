#!/usr/bin/env zsh

### Job name
#BSUB -J aiolia_eval

### File / path where STDOUT & STDERR will be written
###    %J is the job ID, %I is the array ID
#BSUB -o parameters.%J.%I

### Request the time you need for execution in minutes
### The format for the parameter is: [hour:]minute,
### that means for 80 minutes you could also use this: 1:20
#BSUB -W 0:15

### Request memory you need for your job in TOTAL in MB
#BSUB -M 8096

### Request the number of compute slots you want to use
#BSUB -n 12

### Use esub for OpenMP/shared memeory jobs
#BSUB -a openmp

### Change to the work directory
cd /home/user/aiolia

### Execute your application
java -jar aiolia-assembly-0.1.jar
