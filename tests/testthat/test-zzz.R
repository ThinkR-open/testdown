unlink(pkg, TRUE, TRUE)
remove.packages(basename(pkg))
unloadNamespace(basename(pkg))
