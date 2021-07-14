C      PROGRAM PROGRAM_CHECK
      CHARACTER(LEN=6) MSG
      MSG = 'YOWZA!'
10    FORMAT(1X, "The included message is", 1X, A)
      WRITE(6,10) MSG
      STOP
      END
