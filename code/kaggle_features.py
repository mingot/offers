#Kaggle Challenge: 
#"http://www.kaggle.com/c/acquire-valued-shoppers-challenge/" 
#'Reduce the data and generate features' by Triskelion 
#After a forum post by BreakfastPirate
#Very mediocre and hacky code, single-purpose, but pretty fast

from datetime import datetime, date
from collections import defaultdict

def diff_days(s1,s2):
	date_format = "%Y-%m-%d"
	a = datetime.strptime(s1, date_format)
	b = datetime.strptime(s2, date_format)
	delta = b - a
	return delta.days

loc_offers = "../data/original/offers.csv"
loc_train = "../data/original/trainHistory.csv"
loc_test = "../data/original/testHistory.csv"
loc_transactions = "../data/original/reduced2.csv"
loc_out_train = "../data/triskelion/train.csv"
loc_out_test = "../data/triskelion/test.csv"


# header= "offer_quantity,has_bought_brand_category,has_bought_category_q_90,has_bought_company_a,has_bought_brand_180,has_bought_brand_a_180,has_bought_brand_q_180,offer_value,has_bought_brand_a_60,has_bought_category_30,has_bought_company_q,has_bought_brand_q_30,has_bought_brand,id,has_bought_company_q_30,has_bought_brand_30,has_bought_company_q_60,has_bought_category_q,label,has_bought_brand_company,has_bought_category_a_30,has_bought_brand_90,has_bought_category_a,has_bought_brand_company_category,has_bought_company_q_180,has_bought_category_90,has_bought_company_30,has_bought_brand_a,has_bought_category,has_bought_company_a_90,has_bought_brand_q_90,never_bought_category,has_bought_company_180,has_bought_brand_q,has_bought_company_a_30,has_bought_company_q_90,has_bought_category_q_60,has_bought_category_a_90,has_bought_category_a_180,has_bought_category_60,has_bought_category_180,has_bought_brand_a_30,total_spend,has_bought_company_a_60,has_bought_brand_q_60,has_bought_category_q_30,has_bought_category_a_60,has_bought_company_a_180,has_bought_company_60,has_bought_brand_60,has_bought_company_90,has_bought_brand_a_90,has_bought_company,has_bought_category_q_180\n"



def generate_features(loc_train, loc_test, loc_transactions, loc_out_train, loc_out_test):
	header = ["offer_quantity","has_bought_brand_category","has_bought_category_q_90","has_bought_company_a","has_bought_brand_180","has_bought_brand_a_180","has_bought_brand_q_180","offer_value","has_bought_brand_a_60","has_bought_category_30","has_bought_company_q","has_bought_brand_q_30","has_bought_brand","has_bought_company_q_30","has_bought_brand_30","has_bought_company_q_60","has_bought_category_q","label","has_bought_brand_company","has_bought_category_a_30","has_bought_brand_90","has_bought_category_a","has_bought_brand_company_category","has_bought_company_q_180","has_bought_category_90","has_bought_company_30","has_bought_brand_a","has_bought_category","has_bought_company_a_90","has_bought_brand_q_90","never_bought_category","has_bought_company_180","has_bought_brand_q","has_bought_company_a_30","has_bought_company_q_90","has_bought_category_q_60","has_bought_category_a_90","has_bought_category_a_180","has_bought_category_60","has_bought_category_180","has_bought_brand_a_30","total_spend","has_bought_company_a_60","has_bought_brand_q_60","has_bought_category_q_30","has_bought_category_a_60","has_bought_company_a_180","has_bought_company_60","has_bought_brand_60","has_bought_company_90","has_bought_brand_a_90","has_bought_company","has_bought_category_q_180"]
	#keep a dictionary with the offerdata
	offers = {}
	for e, line in enumerate( open(loc_offers) ):
		row = line.strip().split(",")
		offers[ row[0] ] = row

	#keep two dictionaries with the shopper id's from test and train
	train_ids = {}
	test_ids = {}
	for e, line in enumerate( open(loc_train) ):
		if e > 0:
			row = line.strip().split(",")
			train_ids[row[0]] = row
	for e, line in enumerate( open(loc_test) ):
		if e > 0:
			row = line.strip().split(",")
			test_ids[row[0]] = row

	#open two output files
	with open(loc_out_train, "wb") as out_train, open(loc_out_test, "wb") as out_test:
		#iterate through reduced dataset 
		last_id = 0

		# write headers
		header_text = ','.join(header)
		header_text = "id," + header_text
		header_text += "\n"
		out_test.write( header_text ) 
		out_train.write( header_text )	

		# dictionary initialization
		# features = defaultdict(float)
		features = {}
		for h in header:
			features[h] = 0

		for e, line in enumerate( open(loc_transactions) ):
			if e > 0: #skip header
				
				row = line.strip().split(",")
				
				#write away the features when we get to a new shopper id
				if last_id != row[0] and e != 1:
					
					#generate negative features
					if features["has_bought_company" ] == 0:
						features['never_bought_company'] = 1
					
					if features["has_bought_category"] == 0:
						features['never_bought_category'] = 1
						
					if features["has_bought_brand"] == 0:
						features['never_bought_brand'] = 1
						
					if features["has_bought_brand"] > 0 and features["has_bought_category"] > 0 and features["has_bought_company"] > 0:
						features['has_bought_brand_company_category'] = 1
					
					if features["has_bought_brand"] > 0 and features["has_bought_category"] > 0:
						features['has_bought_brand_category'] = 1
					
					if features["has_bought_brand"] > 0 and features["has_bought_company"] > 0:
						features['has_bought_brand_company'] = 1
					
					test = False
					if features['label'] == 0.5:
						test = True

					aux = []
					for h in header:
						aux.append(features[h])

					outline = ",".join(map(str, aux))					
					outline = last_id + "," + outline
					outline += "\n"
					out_test.write( outline ) if test else out_train.write( outline )
					
					#reset features
					# features = defaultdict(float)
					for k in features:
						features[k] = 0

				#generate features from transaction record
				#check if we have a test sample or train sample
				if row[0] in train_ids or row[0] in test_ids:
					#generate label and history
					if row[0] in train_ids:						
						history = train_ids[row[0]]
						if train_ids[row[0]][5] == "t":
							features['label'] = 1
						else:
							features['label'] = 0
						
					else:
						history = test_ids[row[0]]
						features['label'] = 0.5
						
					
					features['offer_value'] = offers[ history[2] ][4]
					features['offer_quantity'] = offers[ history[2] ][2]
					offervalue = offers[ history[2] ][4]
					
					features['total_spend'] += float( row[10] )
					
					if offers[ history[2] ][3] == row[4]:
						features['has_bought_company'] += 1.0
						features['has_bought_company_q'] += float( row[9] )
						features['has_bought_company_a'] += float( row[10] )
						
						date_diff_days = diff_days(row[6],history[-1])
						if date_diff_days < 30:
							features['has_bought_company_30'] += 1.0
							features['has_bought_company_q_30'] += float( row[9] )
							features['has_bought_company_a_30'] += float( row[10] )
						if date_diff_days < 60:
							features['has_bought_company_60'] += 1.0
							features['has_bought_company_q_60'] += float( row[9] )
							features['has_bought_company_a_60'] += float( row[10] )
						if date_diff_days < 90:
							features['has_bought_company_90'] += 1.0
							features['has_bought_company_q_90'] += float( row[9] )
							features['has_bought_company_a_90'] += float( row[10] )
						if date_diff_days < 180:
							features['has_bought_company_180'] += 1.0
							features['has_bought_company_q_180'] += float( row[9] )
							features['has_bought_company_a_180'] += float( row[10] )
					
					if offers[ history[2] ][1] == row[3]:
						
						features['has_bought_category'] += 1.0
						features['has_bought_category_q'] += float( row[9] )
						features['has_bought_category_a'] += float( row[10] )
						date_diff_days = diff_days(row[6],history[-1])
						if date_diff_days < 30:
							features['has_bought_category_30'] += 1.0
							features['has_bought_category_q_30'] += float( row[9] )
							features['has_bought_category_a_30'] += float( row[10] )
						if date_diff_days < 60:
							features['has_bought_category_60'] += 1.0
							features['has_bought_category_q_60'] += float( row[9] )
							features['has_bought_category_a_60'] += float( row[10] )
						if date_diff_days < 90:
							features['has_bought_category_90'] += 1.0
							features['has_bought_category_q_90'] += float( row[9] )
							features['has_bought_category_a_90'] += float( row[10] )						
						if date_diff_days < 180:
							features['has_bought_category_180'] += 1.0
							features['has_bought_category_q_180'] += float( row[9] )
							features['has_bought_category_a_180'] += float( row[10] )				
					if offers[ history[2] ][5] == row[5]:
						features['has_bought_brand'] += 1.0
						features['has_bought_brand_q'] += float( row[9] )
						features['has_bought_brand_a'] += float( row[10] )
						date_diff_days = diff_days(row[6],history[-1])
						if date_diff_days < 30:
							features['has_bought_brand_30'] += 1.0
							features['has_bought_brand_q_30'] += float( row[9] )
							features['has_bought_brand_a_30'] += float( row[10] )
						if date_diff_days < 60:
							features['has_bought_brand_60'] += 1.0
							features['has_bought_brand_q_60'] += float( row[9] )
							features['has_bought_brand_a_60'] += float( row[10] )
						if date_diff_days < 90:
							features['has_bought_brand_90'] += 1.0
							features['has_bought_brand_q_90'] += float( row[9] )
							features['has_bought_brand_a_90'] += float( row[10] )						
						if date_diff_days < 180:
							features['has_bought_brand_180'] += 1.0
							features['has_bought_brand_q_180'] += float( row[9] )
							features['has_bought_brand_a_180'] += float( row[10] )	
				last_id = row[0]
				if e % 100000 == 0:
					print e
generate_features(loc_train, loc_test, loc_transactions, loc_out_train, loc_out_test)

# loc_preds = "kaggle_shop\\shop.preds.txt"
# loc_test = "kaggle_shop\\testHistory.csv"
# loc_submission = "kaggle_shop\\kaggle.submission2.csv"

# def generate_submission(loc_preds, loc_test, loc_submission):
# 	preds = {}
# 	for e, line in enumerate( open(loc_preds) ):
# 		row = line.strip().split(" ")
# 		preds[ row[1] ] = row[0]
		
	
# 	with open(loc_submission, "wb") as outfile:
# 		for e, line in enumerate( open(loc_test) ):
# 			if e == 0:
# 				outfile.write( "id,repeatProbability\n" )
# 			else:
# 				row = line.strip().split(",")
# 				if row[0] not in preds:
# 					outfile.write(row[0]+",0\n")
# 				else:
# 					outfile.write(row[0]+","+preds[row[0]]+"\n")
# #generate_submission(loc_preds, loc_test, loc_submission)