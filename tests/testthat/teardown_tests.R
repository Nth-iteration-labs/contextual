

delete_files <- dir(path=".", pattern="*.csv")
file.remove(delete_files)

delete_files <- dir(path=".", pattern="*.pdf")
file.remove(delete_files)

delete_files <- dir(path=".", pattern="*.svg")
file.remove(delete_files)

delete_files <- dir(path=".", pattern="*.log")
file.remove(delete_files)
