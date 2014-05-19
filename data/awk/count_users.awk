BEGIN {
	FS=","
	current = 1
	users = 0
}
// {
	if($1!=current){
		current = $1
		users += 1
	}
}
END {
	print users
}