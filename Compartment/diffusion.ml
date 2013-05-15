open Mods

type rule =
	{
		dynamics : Dynamics.rule ;
		script : action list ;
		location : int ; 
	}
and action = CREATE of int | DELETE of int | DIFFUSE of int
	
let compile r ((diff_lhs,param_loc),(diff_rhs,param_loc')) env = 
	(*preserved: list of indexes in diff_lhs where the corresponding volume is preserved*)
	let preserved,added,removed = 
		let p,a,r = ref [], ref [], ref [] in
  	Array.iteri 
  	(fun i vol_id -> 
  		try if diff_rhs.(i) = vol_id then p := i::!p else (r := i::!r ; a := i::!a)
  		with _ -> r := i::!r
  	) diff_lhs ;
		(!p,!a,!r)
	in
	[] (*TODO check param_loc is consistent and generate script*)
	
	
	


	