library(microbenchmark)

benchmark <- microbenchmark(
  readCSV = utils::read.csv("GlobalLandTemperaturesByCity.csv"),
  readrCSV = readr::read_csv("GlobalLandTemperaturesByCity.csv", progress = F),
  fread = data.table::fread("GlobalLandTemperaturesByCity.csv", showProgress = F),
  times = 2
)
print(benchmark, signif = 2)

#fread() is by far the fastest way to read in csv data
