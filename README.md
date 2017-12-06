# impto

A little script for imports digital photos using Racket.

This program imports digital photos from 'source dir' to 'dest dir' and stores them in a directory tree organized in the form 'yyyy/mm/dd', the date-time information is read from the photo's buile-in Exif data or file name. This program will recursively copy each JPEG file under the 'source dir' and all subdirectories. 

The 'source dir' parameter is optional, in which case program will look for JPEG images in the 'current working directory'.

The 'dest dir' parameter is optional too, in which case the program reads the value of the environment variable "PHOTO_DIR" (needs to be defined in advance) as 'dest dir' 

If the attempt to get date info from Exif or file name fails, the file will be ignored. All failed log can be found at 'impto-error.log' in current directory. All successful logs can be found at 'impto.log' too. 
 
Only files with '.jpg' '.jpeg' '.JPG' '.JPEG' extension will be copied.
 
  Usage:

    impto [-r] [-s <source-dir>] [-d <dest-dir>]
