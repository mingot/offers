# $ cat file1
# 11002
# 10995
# 48981
# 79600
# $ cat file2
# 10993   item    0
# 11002   item    6
# 10995   item    7
# 79600   item    7
# 439481  item    5
# 272557  item    7
# 224325  item    7
# 84156   item    6
# 572546  item    7
# 693661  item    7
# $ awk 'BEGIN {FS=","}NR==FNR{a[$1]; next} $1 in a' file1 file2
# 11002   item    6
# 10995   item    7
# 79600   item    7

