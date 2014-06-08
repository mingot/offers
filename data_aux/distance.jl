using RDatasets # readtable
using Distance # distance martix

user_brand = readtable("user_brand.csv",allowcomments=true,header=true,separator=',')


R = pairwise(CosineDist(), user_brand)

#write to a file
#writecsv(filename, x)
