% Example of if to test if a number is positive.
val(X) :- 
  (   X < 0 -> print('X is negative. Failing now'), fail;
     X == 0 -> print('X is zero');   
    print('X is positive')).