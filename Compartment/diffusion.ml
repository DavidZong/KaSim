open Mods

type t =
	{
		script : IntSet.t * IntSet.t * IntSet.t ;
		lhs : int array ;
		rhs : int array ;
		loc_in : int ;
		loc_out : int; 
	}


let compile r ((diff_lhs,param_loc),(diff_rhs,param_loc')) env = 
	(*preserved: list of indexes in diff_lhs where the corresponding volume is preserved*)
	let preserved,added,removed = 
		let p,a,r = (ref IntSet.empty, ref IntSet.empty, ref IntSet.empty) in
  	Array.iteri 
  	(fun i vol_num -> 
  		try if diff_rhs.(i) = vol_num then 
				(p := IntSet.add i !p) else 
				(r := IntSet.add i !r ; a := IntSet.add i !a)
  		with _ -> r := IntSet.add i !r
  	) diff_lhs ;
		(!p,!a,!r)
	in
	 
	(*generate script*)
	{lhs=diff_lhs ; rhs = diff_rhs ; script=(preserved,added,removed); loc_in=param_loc ; loc_out=param_loc'}


let num_in df = try df.lhs.(df.loc_in) with _ -> failwith ("Diffusion.loc_in: index "^(string_of_int df.loc_in)^" out of bounds.") 
let num_out df = try df.rhs.(df.loc_out) with _ -> failwith ("Diffusion.loc_out: index "^(string_of_int df.loc_out)^" out of bounds.") 
