open Random_tree
open Mods

open ExceptionDefn
open Mods

(**Volume management*)
type modif = Rule of (Dynamics.rule * State.embedding_t) | Diffuse of (Dynamics.rule * State.embedding_t) | Pert of IntSet.t | Nothing 
type event = Out of (float * int * bool) | Tau of float 

class compartment vol_id new_volume new_state new_counter new_causal new_plot =
	object (self)
	val mutable id:int = vol_id
	val volume:float = new_volume
	
	val mutable causal:(Compression_main.D.S.PH.B.PB.CI.Po.K.P.log_info * Compression_main.D.S.PH.B.PB.CI.Po.K.step list) = new_causal
	val mutable plot:Plot.t = new_plot
	
	val mutable compState:State.implicit_state = new_state
	val mutable compCounter:Counter.t = new_counter
	val mutable event_buffer:modif =  Nothing
		
	method getId = id
	method setId x = id <- x
	method getState () = compState
	method getCounter () = compCounter
	method get_local_clock () = compCounter.Counter.time 
	method set_local_clock t = (compCounter.Counter.time <- t)
		
	method set_event_buffer env =  
		(*Computing next dt based on rules activity*)
		let rd = Random.float 1.0 
		and activity = (*Activity.total*) Random_tree.total compState.State.activity_tree 
		in
		let dt = -. (log rd /. activity) in
		if dt = infinity || activity = 0. then 
		begin
			if !Parameter.dumpIfDeadlocked then	
				let vol_nme = try Environment.volume_of_num id env with Not_found -> failwith "Vol.next_event: Unknown compartment"
				in
				let desc = 
					if !Parameter.dotOutput then open_out ("deadlock_"^vol_nme^".dot") 
					else open_out ("deadlock_"^vol_nme^".ka") 
				in
				State.snapshot compState compCounter desc true env
			else () ;
		end ; 
		
		(*Checking if some perturbations should be applied before dt*)
		let env,pert_ids_time = State.update_dep compState (-1) Mods.TIME IntSet.empty compCounter env in
		let env,pert_ids,dt' = External.check_stopping_time dt pert_ids_time compState compCounter env in
		if not (IntSet.is_empty pert_ids) then (event_buffer <- Pert pert_ids ; Tau dt')
		else  
		
  		(*Precomputing which rule should be applied if compartment is selected*)
  		let opt_instance,upd_state = 
  			try State.draw_rule compState compCounter env with Null_event i -> (Counter.stat_null i compCounter ; (None,compState))
  		in
  		compState <- upd_state ; (*State.draw_rule may update activity*)
  		match opt_instance with
  		| None -> (event_buffer <- Nothing ; Tau dt)
  		| Some (r,embedding) -> 
  			begin
  				match r.Dynamics.diffuse with
  				| None -> (event_buffer <- Rule (r,embedding) ; Tau dt)
  				| Some diff_rule -> 
  					let (_,added,_) = diff_rule.Diffusion.script in
						let create = IntSet.mem diff_rule.Diffusion.loc_out added 
						in 
  					let vol_id = try diff_rule.Diffusion.rhs.(diff_rule.Diffusion.loc_out) with _ -> failwith "next_event(): index out of bounds"
  					in
  					(event_buffer <- Diffuse (r,embedding) ; Out (dt,vol_id,create))
  			end
	
	method channel_in (nodes:Node.t list) (env:Environment.t) = ()
	
	method react (comp_of_id:int->compartment) event env =
		
		let inc env dt is_null =
		Plot.fill compState compCounter plot env dt ; (*updating plot*) 
		Counter.inc_time compCounter dt ; (*incrementing time*)
		Counter.inc_events compCounter ; (*incrementing event counter*)
		if is_null then (Counter.inc_null_events compCounter ; Counter.inc_consecutive_null_events compCounter) 
		else (compCounter.Counter.cons_null_events <- 0) 
		(*set_event_buffer env*)  (*recomputing next event*)
		in
		
		(*no time perturbation should be applied since it would have been in the event horizon*)
		let env,pert_ids =
  		match event_buffer with
    		| Nothing -> (*Buffer contains a null event*)
  				begin
  					match event with
  						Tau dt -> (inc env dt true ; (env,IntSet.empty)) 
  					| _ -> failwith "Vol.react: Invalid event"
  				end
  			| Diffuse (r,embedding) -> (*Buffer contains a diffusion event*)
  				begin
  					match event with 
  					  Out (dt,vol_id,is_new) ->
  							begin 
  								try
										let c_out = comp_of_id vol_id in
  									inc env dt false ; 
  									let nodes,pert_ids_from_diffusion,obs_from_diffusion = 
  										Diffusion.apply_and_extract compState r embedding env in
  									c_out#channel_in nodes env ; (*diffusing connected component of nodes to c_out*)
  									(env,pert_ids_from_diffusion) (*TODO add story tracking of diffusion*)
  								with
  								| Not_found -> failwith "Vol.react: No compartment of the desired type is present"
  							end
  							| _ ->  failwith "Vol.react: Invalid event"
  					end  
  			| Rule (r,embedding) -> (*Buffer contains a internal rule*)
					begin
  					match event with
  						Tau dt ->  
  							begin
        				try
									let (env,state,side_effect,embedding_t,psi,pert_ids_rule) = 
										State.apply compState r embedding compCounter env
									in
									compState <- state ;
									inc env dt false ; (*rule did not result in a null event*)
																		
									let env,pert_ids = (*pert_ids: event clock based perturnbations*)
										State.update_dep compState (-1) Mods.EVENT pert_ids_rule compCounter env 
									in
									
									(*Local positive update: adding new partial injection*)
          				let env,state,pert_ids',new_injs,obs_from_rule_app = (*pert_ids': positive update based perturbations*)
          					State.positive_update compState r (State.map_of embedding_t,psi) (side_effect,Int2Set.empty) compCounter env
          				in
          				
          				(*Non local positive update: adding new possible intras*)
          				let state = 
          					if env.Environment.has_intra then 
          						NonLocal.positive_update r embedding_t new_injs compState compCounter env 
          					else state 
          				in
									
									compState <- state ;
									
									(****************recording rule based event for story production********************)
          				let phi = State.map_of embedding_t in
          				
									let story_profiling,event_list = causal in
          				let story_profiling,event_list = 
          					if Environment.tracking_enabled env then (*if logging events is required*) 
          					  begin
                      let story_profiling,event_list = 
          					  	Compression_main.D.S.PH.B.PB.CI.Po.K.store_event story_profiling (Compression_main.D.S.PH.B.PB.CI.Po.K.import_event ((r,phi,psi),(obs_from_rule_app,r,Counter.event compCounter,side_effect))) event_list 
          	          in 
                      (story_profiling,event_list) 
                      end
                    else
          						(story_profiling,event_list)
                  in 
                  let story_profiling,event_list =
                  	if Environment.tracking_enabled env && !Parameter.causalModeOn then (*if tracking the observable is required*) 
                    	begin 
          						let simulation_info = 
          			        {Mods.story_id=  0 ;
          			         Mods.story_time= compCounter.Mods.Counter.time ;
          			         Mods.story_event= compCounter.Mods.Counter.events ;
          			         Mods.profiling_info = ()}
          			      in 
          			      let story_profiling,event_list = 
          			      	List.fold_left 
          			       	(fun (story_profiling,event_list) (obs,phi) -> 
          			        	let lhs = State.kappa_of_id obs state in 
          			          Compression_main.D.S.PH.B.PB.CI.Po.K.store_obs story_profiling (obs,lhs,phi,simulation_info) event_list
          							)
          							(story_profiling,event_list) obs_from_rule_app
                			in 
            					(story_profiling,event_list)
          						end
          					else 
          						(story_profiling,event_list)
          				in
									causal <- (story_profiling,event_list) ;
          				
									(env,IntSet.union pert_ids pert_ids')
      					with Null_event i -> (Counter.stat_null i compCounter ; inc env dt true ; (env,IntSet.empty))
  							end
  					| _ -> failwith "Vol.react: Invalid event"
					end
				| Pert pert_ids -> (*Buffer contains time dependent perturbations*)
					match event with
					| Tau dt ->	((inc env dt true) ; (env,pert_ids))
					| _ -> failwith "Incompatible event"
					
		in
		
		(*checking if some perturbation(s) should be applied*)
		let state,env,obs_story,pert_events,_ = 
			External.try_perturbate [] compState pert_ids [] compCounter env 
		in
		compState <- state ;
		
		let story_profiling,event_list = causal in
		(*Adding perturbation event to story -if any*)
  	let story_profiling,event_list,cpt = 
  		if Environment.tracking_enabled env then (*if logging events is required*) 
    		begin
  			let story_profiling,event_list,cpt = 
  				List.fold_left 
  				(fun (story_prof,event_list,cpt) (r,phi,psi,side_effects) ->
  					let sp,el =
  						Compression_main.D.S.PH.B.PB.CI.Po.K.store_event story_prof
  						(Compression_main.D.S.PH.B.PB.CI.Po.K.import_event 
  						((r,phi,psi),(obs_story,r,cpt+1,side_effects))) event_list (*we are adding several events with the same id in the grid!*)
  						in
  						(sp,el,cpt+1)
  					) (story_profiling,event_list,Counter.event compCounter) pert_events
  			  in 
  		    (story_profiling,event_list,cpt) 
  		  end
  		else
  			(story_profiling,event_list,Counter.event compCounter)
  		in 
			causal <- (story_profiling,event_list) ;
  		compCounter.Counter.perturbation_events <- cpt ;
			env
end

module CompHeap = Heap.Make (struct type t = compartment let allocate = fun c id -> c#setId id let get_address = fun c -> c#getId end)
		