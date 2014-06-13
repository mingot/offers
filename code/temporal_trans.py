
# $python XXX.py train.csv trans.csv trans_90.csv 90 


# Parameter reading

train_file = sys.argv[1]
trans_file = sys.argv[2]
output_file = sys.argv[3]
days = sys.argv[4]

train = open( train_file )
trans = open( trans_file )
output = open( output_file, 'w' )


# Read TRAIN and construct mapping ID-OFFERDATE
reader = csv.reader(i)
headers = reader.next()


n = 0
for line in reader:
    label = 1
    if not check_test:
        label = line.pop(0)

    importance = 1.0
    if check_importance:
        importance = line.pop(0)
        
    new_line = construct_line( label, line, headers, importance)
    o.write( new_line )
    
    n += 1
    if n % 10000 == 0:
        print n



# Read TRAIN and construct mapping ID-OFFERDATE

# READ