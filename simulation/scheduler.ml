open Mods
open Vol

exception End_of_sim of int

type t = {
	environment:Environment.t ; 
	clock : Counter.t ; 
	compartments: Vol.CompHeap.t IntMap.t ;
	reactive_comp : IntSet.t 
	}

let c_of_id (n,id) sched = 
	let hp = IntMap.find n sched.compartments in
	try CompHeap.find id hp with _ -> failwith (Printf.sprintf "Cannot find volume %d in Heap(%s)" id (Environment.volume_of_num n sched.environment))

let random num sched = 
	let hp = IntMap.find num sched.compartments in
	CompHeap.random hp 

let create comp_map env =
	let reactive = 
		IntMap.fold 
		(fun vol_num comp_hp set ->
			let control = Environment.control_of_volume vol_num env in
				if control > 1 then set else IntSet.add vol_num set
		) comp_map IntSet.empty
	in
	{environment = env ; 
	clock = Counter.create 0.0 0 !Parameter.maxTimeValue !Parameter.maxEventValue;
	compartments = comp_map ;
	reactive_comp = reactive 
	}

let elect_leader sched =
	let env = sched.environment in
	
	let upd_leader leader_opt (vol_num,vol_id) event = 
		match leader_opt with
		| None -> Some (vol_num,vol_id,event)
		| Some (_,_,event') -> 
			if event.trigger_time < event'.trigger_time then Some (vol_num,vol_id,event) 
			else leader_opt
	in
	
	IntSet.fold
	(fun vol_num leader_opt ->
		let comp_hp = IntMap.find vol_num sched.compartments in
		CompHeap.fold
		(fun vol_id c_id leader_opt ->
			let event = c_id#getNextEvent env 
			in
				upd_leader leader_opt (vol_num,vol_id) event
		) comp_hp leader_opt
	) sched.reactive_comp None

let dump sched = 
	let env = sched.environment in
  	IntMap.iter
  	(fun vol_num vol_hp  ->
			let vol_nme = Environment.volume_of_num vol_num env in
			CompHeap.iteri
			(fun vol_id c ->
				let next_event = c#getEvent in
				let time = match next_event with None -> "n.a" | Some e -> string_of_float e.trigger_time in
				Debug.tag (Printf.sprintf 
				"<VOLUME %s_%d (next event will occur at %s t.u)>" vol_nme vol_id time
				) ;
				State.dump c#getState c#getCounter env ;
				Debug.tag "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" ;
			) vol_hp
		) sched.compartments
	

let step sched =
	let e_clock = sched.clock.Counter.events in 
	let max_time = match sched.clock.Counter.max_time with None -> infinity | Some t -> t
	in
	let max_event = sched.clock.Counter.max_events in 
  match elect_leader sched with
  	| None -> raise (End_of_sim 1)
  	| Some (vol_num,vol_id,event) ->
			if event.trigger_time > max_time then (sched.clock.Counter.time <- max_time ; raise (End_of_sim 0)) 
			else
				if Some e_clock = max_event then raise (End_of_sim 0)
				else
      		let c = c_of_id (vol_num,vol_id) sched in
      		
					(*************************************************************************)
					if !Parameter.debugModeOn then 
      			begin
      				dump sched ;
      				Debug.tag (Printf.sprintf "[[Next reaction is in volume %s_%d]]" 
      				(Environment.volume_of_num vol_num sched.environment) vol_id) ;
      			end ;
      		(*************************************************************************)
					
					let env = c#react (fun num -> random num sched) sched.environment 
      	  in
      		sched.clock.Counter.time <- event.trigger_time ; 
					Counter.inc_events sched.clock ; 
					
      		{sched with environment = env }
		
		
