open Mods

type t =
	{
		script : (int list) * (int list) * (int list) ;
		lhs : int array ;
		rhs : int array ;
		loc_in : int ;
		loc_out : int; 
	}
	
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
	(*generate script*)
	{lhs=diff_lhs ; rhs = diff_rhs ; script=(preserved,added,removed); loc_in=param_loc ; loc_out=param_loc'}


let loc_in df = try df.lhs.(df.loc_in) with _ -> failwith ("Diffusion.loc_in: index "^(string_of_int df.loc_in)^" out of bounds.") 