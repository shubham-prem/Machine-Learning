f <- file("stdin", "r")
on.exit(close(f))

T <- readLines(f)
T <- strsplit(T, ",")
Timecharged <- as.numeric(T[1])
if (Timecharged <= 4) {
  write(Timecharged * 2, stdout())
} else {
  write ("8.00", stdout())
}