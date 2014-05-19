NR>1{
	k = $1 + $2
	arr[k]++
}
END{
	for (a in arr) 
		print a, arr[a]
}
