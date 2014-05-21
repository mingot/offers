BEGIN {
	FS=","
	srand()
	selected = 1
} 
// { 
	if (selected==$1 || FNR==1){ 
		print $0
	}else if(last!=$1){ 
		if(rand() <= .15){
			selected=$1
			print $0
		}
	}
	last=$1
}
