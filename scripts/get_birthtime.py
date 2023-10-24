import os
import sys
import datetime

file_path = sys.argv[1]

try:
    birth_time = os.stat(file_path).st_birthtime
    birth_time_date =datetime.datetime.fromtimestamp(birth_time)
    print(birth_time)
    print(birth_time_date)

except Exception as e:
    print(str(e))
