xy <- read.csv(system.file("external/points_over_perimeter.csv", package="caiman"))
calcOpticalCenter(xy[, c("X", "Y")])
