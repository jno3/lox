type envr = {
	values: (string, Literal.literal) Hashtbl.t;
	enclosing : envr option;
}

let make = {
	values = Hashtbl.create 16;
	enclosing = None;
}

let make_enclosed enclosing = {
	values =  Hashtbl.create 16;
	enclosing = Some enclosing;
}

let define name value envr = 
	Hashtbl.replace envr.values name value

let rec get name envr =
	match Hashtbl.find_opt envr.values name with
	| Some value -> value
	| None -> match envr.enclosing with
		| Some parent -> get name parent
		| None -> failwith (Printf.sprintf "Undefined variable '%s'." name)

let rec assign name value envr =
	if Hashtbl.mem envr.values name then
		Hashtbl.replace envr.values name value
	else
		match envr.enclosing with
		| Some parent -> assign name value parent
		| None -> failwith (Printf.sprintf "Undefined variable '%s'." name)