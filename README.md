# Getting-and-Cleaning-Data-Project---DGJ
Repository for Coursera Class "Getting and Cleaning Data"

The run_analysis.R code is designed to retrieve a series of data files and perform a summary analysis resulting in a single “tidy data set” that summarizes averages of mean and standard deviation data.

Variables included are listed below.

1 Subject ID Number – subject identification, range from 1-30
2 Activity Number – activity number, range from 1-6
3 Activity Name – activity descriptive name corresponding to variable 2
1 tBodyAcc-mean()-X
2 tBodyAcc-mean()-Y
3 tBodyAcc-mean()-Z
4 tBodyAcc-std()-X
5 tBodyAcc-std()-Y
6 tBodyAcc-std()-Z
41 tGravityAcc-mean()-X
42 tGravityAcc-mean()-Y
43 tGravityAcc-mean()-Z
44 tGravityAcc-std()-X
45 tGravityAcc-std()-Y
46 tGravityAcc-std()-Z
81 tBodyAccJerk-mean()-X
82 tBodyAccJerk-mean()-Y
83 tBodyAccJerk-mean()-Z
84 tBodyAccJerk-std()-X
85 tBodyAccJerk-std()-Y
86 tBodyAccJerk-std()-Z
121 tBodyGyro-mean()-X
122 tBodyGyro-mean()-Y
123 tBodyGyro-mean()-Z
124 tBodyGyro-std()-X
125 tBodyGyro-std()-Y
126 tBodyGyro-std()-Z
161 tBodyGyroJerk-mean()-X
162 tBodyGyroJerk-mean()-Y
163 tBodyGyroJerk-mean()-Z
164 tBodyGyroJerk-std()-X
165 tBodyGyroJerk-std()-Y
166 tBodyGyroJerk-std()-Z
201 tBodyAccMag-mean()
202 tBodyAccMag-std()
214 tGravityAccMag-mean()
215 tGravityAccMag-std()
227 tBodyAccJerkMag-mean()
228 tBodyAccJerkMag-std()
229 tBodyAccJerkMag-mad()
240 tBodyGyroMag-mean()
241 tBodyGyroMag-std()
253 tBodyGyroJerkMag-mean()
254 tBodyGyroJerkMag-std()
266 fBodyAcc-mean()-X
267 fBodyAcc-mean()-Y
268 fBodyAcc-mean()-Z
269 fBodyAcc-std()-X
270 fBodyAcc-std()-Y
271 fBodyAcc-std()-Z
345 fBodyAccJerk-mean()-X
346 fBodyAccJerk-mean()-Y
347 fBodyAccJerk-mean()-Z
348 fBodyAccJerk-std()-X
349 fBodyAccJerk-std()-Y
350 fBodyAccJerk-std()-Z
351 fBodyAccJerk-mad()-X
352 fBodyAccJerk-mad()-Y
353 fBodyAccJerk-mad()-Z
354 fBodyAccJerk-max()-X
355 fBodyAccJerk-max()-Y
356 fBodyAccJerk-max()-Z
357 fBodyAccJerk-min()-X
358 fBodyAccJerk-min()-Y
359 fBodyAccJerk-min()-Z
360 fBodyAccJerk-sma()
361 fBodyAccJerk-energy()-X
362 fBodyAccJerk-energy()-Y
363 fBodyAccJerk-energy()-Z
364 fBodyAccJerk-iqr()-X
365 fBodyAccJerk-iqr()-Y
366 fBodyAccJerk-iqr()-Z
367 fBodyAccJerk-entropy()-X
368 fBodyAccJerk-entropy()-Y
369 fBodyAccJerk-entropy()-Z
370 fBodyAccJerk-maxInds-X
371 fBodyAccJerk-maxInds-Y
372 fBodyAccJerk-maxInds-Z
373 fBodyAccJerk-meanFreq()-X
374 fBodyAccJerk-meanFreq()-Y
375 fBodyAccJerk-meanFreq()-Z
376 fBodyAccJerk-skewness()-X
377 fBodyAccJerk-kurtosis()-X
378 fBodyAccJerk-skewness()-Y
379 fBodyAccJerk-kurtosis()-Y
380 fBodyAccJerk-skewness()-Z
381 fBodyAccJerk-kurtosis()-Z
382 fBodyAccJerk-bandsEnergy()-1,8
383 fBodyAccJerk-bandsEnergy()-9,16
384 fBodyAccJerk-bandsEnergy()-17,24
385 fBodyAccJerk-bandsEnergy()-25,32
386 fBodyAccJerk-bandsEnergy()-33,40
387 fBodyAccJerk-bandsEnergy()-41,48
388 fBodyAccJerk-bandsEnergy()-49,56
389 fBodyAccJerk-bandsEnergy()-57,64
390 fBodyAccJerk-bandsEnergy()-1,16
391 fBodyAccJerk-bandsEnergy()-17,32
392 fBodyAccJerk-bandsEnergy()-33,48
393 fBodyAccJerk-bandsEnergy()-49,64
394 fBodyAccJerk-bandsEnergy()-1,24
395 fBodyAccJerk-bandsEnergy()-25,48
396 fBodyAccJerk-bandsEnergy()-1,8
397 fBodyAccJerk-bandsEnergy()-9,16
398 fBodyAccJerk-bandsEnergy()-17,24
399 fBodyAccJerk-bandsEnergy()-25,32
400 fBodyAccJerk-bandsEnergy()-33,40
401 fBodyAccJerk-bandsEnergy()-41,48
402 fBodyAccJerk-bandsEnergy()-49,56
403 fBodyAccJerk-bandsEnergy()-57,64
404 fBodyAccJerk-bandsEnergy()-1,16
405 fBodyAccJerk-bandsEnergy()-17,32
406 fBodyAccJerk-bandsEnergy()-33,48
407 fBodyAccJerk-bandsEnergy()-49,64
408 fBodyAccJerk-bandsEnergy()-1,24
409 fBodyAccJerk-bandsEnergy()-25,48
410 fBodyAccJerk-bandsEnergy()-1,8
411 fBodyAccJerk-bandsEnergy()-9,16
412 fBodyAccJerk-bandsEnergy()-17,24
413 fBodyAccJerk-bandsEnergy()-25,32
414 fBodyAccJerk-bandsEnergy()-33,40
415 fBodyAccJerk-bandsEnergy()-41,48
416 fBodyAccJerk-bandsEnergy()-49,56
417 fBodyAccJerk-bandsEnergy()-57,64
418 fBodyAccJerk-bandsEnergy()-1,16
419 fBodyAccJerk-bandsEnergy()-17,32
420 fBodyAccJerk-bandsEnergy()-33,48
421 fBodyAccJerk-bandsEnergy()-49,64
422 fBodyAccJerk-bandsEnergy()-1,24
423 fBodyAccJerk-bandsEnergy()-25,48
424 fBodyGyro-mean()-X
425 fBodyGyro-mean()-Y
426 fBodyGyro-mean()-Z
427 fBodyGyro-std()-X
428 fBodyGyro-std()-Y
429 fBodyGyro-std()-Z
503 fBodyAccMag-mean()
504 fBodyAccMag-std()
513 fBodyAccMag-meanFreq()
516 fBodyBodyAccJerkMag-mean()
517 fBodyBodyAccJerkMag-std()
526 fBodyBodyAccJerkMag-meanFreq()
529 fBodyBodyGyroMag-mean()
530 fBodyBodyGyroMag-std()
539 fBodyBodyGyroMag-meanFreq()
542 fBodyBodyGyroJerkMag-mean()
543 fBodyBodyGyroJerkMag-std()



