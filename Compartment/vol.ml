open Random_tree
open Mods

open ExceptionDefn
open Mods

(**Volume management*)
type modif = 
	| Rule of (Dynamics.rule * State.embedding_t) 
	| Outbound of (Dynamics.rule * State.embedding_t * int * bool) 
	| Pert of IntSet.t
	| Inbound of (Node.t list list * Dynamics.rule)  
	| Nothing 
type event = {dt:float ; kind : modif}

class compartment vol_num new_volume new_state new_counter new_causal new_plot =
	object (self)
	val mutable id:int = (-1)
	val mutable name:int = vol_num
	
	val volume:float = new_volume
	
	method getName = name
	method getHumanName env = Printf.sprintf "%s_%d" (Environment.volume_of_num self#getName env) self#getId
		 	
	val mutable causal:(Compression_main.D.S.PH.B.PB.CI.Po.K.P.log_info * Compression_main.D.S.PH.B.PB.CI.Po.K.step list) = new_causal
	val mutable plot:Plot.t = new_plot
	
	method private getCausal = causal
	method private setCausal c = causal <- c
	
	val mutable compState:State.implicit_state = new_state
	val mutable compCounter:Counter.t = new_counter
	
	method getState = compState
	method private setState s = compState <- s
	method getCounter = compCounter
	method private setCounter c = compCounter <- c
	
	val mutable event_buffer:event option =  None
	method getEvent = event_buffer
	
	method setBuffer event = event_buffer <- Some event
	method emptyBuffer () = event_buffer <- None 
			
	method getId = id
	method setId x = id <- x
		
	method private set_event_buffer env =  
		(*Computing next dt based on rules activity*)
		let rd = Random.float 1.0 
		and activity = (*Activity.total*) Random_tree.total compState.State.activity_tree 
		in
		let dt = -. (log rd /. activity) in
		if dt = infinity || activity = 0. then 
		begin
			if !Parameter.dumpIfDeadlocked then	
				let vol_nme = Environment.volume_of_num self#getName env in
				let desc = 
					if !Parameter.dotOutput then open_out ("deadlock_"^vol_nme^".dot") 
					else open_out ("deadlock_"^vol_nme^".ka") 
				in
				State.snapshot self#getState self#getCounter desc true env
			else () ;
		end ; 
		
		(*Checking if some perturbations should be applied before dt*)
		let env,pert_ids_time = State.update_dep self#getState (-1) Mods.TIME IntSet.empty self#getCounter env in
		let env,pert_ids,dt' = External.check_stopping_time dt pert_ids_time self#getState self#getCounter env in
		if not (IntSet.is_empty pert_ids) then 
			event_buffer <- Some {dt=dt' ; kind=Pert pert_ids}
		else  
		
  		(*Precomputing which rule should be applied if compartment is selected*)
  		let opt_instance,upd_state = 
  			try State.draw_rule self#getState self#getCounter env with Null_event i -> (Counter.stat_null i self#getCounter ; (None,self#getState))
  		in
  		self#setState upd_state ; (*State.draw_rule may update activity*)
  		match opt_instance with
  		| None -> event_buffer <- Some {dt = dt ; kind = Nothing}
  		| Some (r,embedding) -> 
  			begin
  				match r.Dynamics.diffuse with
  				| None -> event_buffer <- Some {dt = dt ; kind = Rule (r,embedding)}
  				| Some diff_rule -> 
  					let (_,added,_) = diff_rule.Diffusion.script in
						let create = IntSet.mem diff_rule.Diffusion.loc_out added 
						in 
  					let vol_num = try diff_rule.Diffusion.rhs.(diff_rule.Diffusion.loc_out) with _ -> failwith "next_event(): index out of bounds"
  					in
  					self#setBuffer {dt=dt; kind = Outbound (r,embedding,vol_num,create) }
  			end
	
	method getNextEvent env =
		match event_buffer with
		| Some e -> e
		| None -> (self#set_event_buffer env ; self#getNextEvent env)
	
	method private channel_out (cause:int) (node_ids:int list) (env:Environment.t) =
		let rec extract todo components pert_ids env =
			match todo with
			| [] -> (components,pert_ids,env)
			| n_id::tl -> 
				try 
					let state,nodes,pert_ids,env = State.deep_extraction self#getState cause n_id pert_ids self#getCounter env
					in 
					self#setState state ;
					extract tl (nodes::components) pert_ids env
				with 
					Not_found -> extract tl components pert_ids env (*node already removed*)
		in
		extract node_ids [] IntSet.empty env
					
	method react (random:int->compartment) env =

		(*May raise Null_event*)
		let apply_and_update state r embedding counter causal env =
    	(*Applying rule*)
    	let (env,state,side_effect,embedding_t,psi,pert_ids_rule) = 
    		State.apply state r embedding counter env
    	in
    	
    	(*Since event counter has increased, waking up potential event based perturbations*)
    	let env,pert_ids = 
    		State.update_dep state (-1) Mods.EVENT pert_ids_rule counter env 
    	in
    	
    	(*Local positive update: adding new partial injection*)
    	let env,state,pert_ids',new_injs,obs_from_rule_app = (*pert_ids': positive update based perturbations*)
    		State.positive_update state r (State.map_of embedding_t,psi) side_effect counter env
    	in
    	
    	(*Non local positive update: adding new possible intras*)
    	let state = 
    		if env.Environment.has_intra then 
    			NonLocal.positive_update r embedding_t new_injs state counter env 
    		else state 
    	in
    	  					
    	(****************recording rule based event for story production********************)
    	let phi = State.map_of embedding_t in
    	
    	let story_profiling,event_list = causal in
    	let story_profiling,event_list = 
    		if Environment.tracking_enabled env then (*if logging events is required*) 
    		  begin
          let story_profiling,event_list = 
    		  	Compression_main.D.S.PH.B.PB.CI.Po.K.store_event story_profiling (Compression_main.D.S.PH.B.PB.CI.Po.K.import_event ((r,phi,psi),(obs_from_rule_app,r,Counter.event self#getCounter,side_effect))) event_list 
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
      (state,(story_profiling,event_list),env,IntSet.union pert_ids pert_ids',psi)
		in  

		let inc env dt =
  		Plot.fill self#getState self#getCounter plot env dt ; (*updating plot*) 
  		Counter.inc_time self#getCounter dt ; (*incrementing time*)
  		Counter.inc_events self#getCounter ; (*incrementing event counter*) 
  		self#emptyBuffer ()  (*emptying buffer*)
		in
		
		(*no time perturbation should be applied since it would have been in the event horizon*)
		let env,pert_ids =
  		match event_buffer with
			  | None -> failwith "Event buffer is not initialized"	 
    		| Some event ->
					match event.kind with
					| Nothing -> (*Buffer contains a null event*)
						if !Parameter.debugModeOn then
							Debug.tag (Printf.sprintf "In Volume %s: Null event!" (self#getHumanName env)) ;
  					(inc env event.dt ; (env,IntSet.empty)) 
  					
    			| Outbound (r,embedding,vol_num,is_new) -> (*Buffer contains a diffusion event*)
    				begin
							if !Parameter.debugModeOn then
							Debug.tag (Printf.sprintf "In Volume %s: diffuse after applying %s" (self#getHumanName env) r.Dynamics.kappa) ;
							
							inc env event.dt ; 
    					try
								if is_new then failwith "Compartment creation is not yet implemented" 
								else
  							let c_out = random vol_num in
								let state,causal',env,pert_ids,creation_map = 
									apply_and_update self#getState r embedding self#getCounter causal env 
								in 
      					self#setCausal causal' ;
								self#setState state ;
								compCounter.Counter.cons_null_events <- 0 ;
								if (c_out#getName,c_out#getId) = (self#getName,self#getId) then (*diffusion is within the same compartment*) 
      						(env,pert_ids)
								else
									let node_ids = ref (IntMap.fold (fun _ id_graph cont -> id_graph::cont) creation_map []) 
									and cpt = ref 0 
									and n = (fun (_,preserved,_) -> preserved) r.Dynamics.balance
									and phi = State.map_of embedding 
									in 
									while !cpt < n do
										let n_id = try IntMap.find !cpt phi with Not_found -> failwith "Agent has no image by the embedding" in
										node_ids := n_id::!node_ids ;
										cpt := !cpt+1
									done;
									let components,pert_ids,env = self#channel_out r.Dynamics.r_id !node_ids env in
									c_out#setBuffer {dt=0. ; kind=Inbound (components,r)} ;
  								(env,pert_ids) 
  						with
  							| Not_found -> failwith "Vol.react: No compartment of the desired type is present"
								| Null_event i -> 
									(Counter.stat_null i self#getCounter ;
									Counter.inc_null_events self#getCounter ;
									Counter.inc_consecutive_null_events self#getCounter ;   
									(env,IntSet.empty))
    				end
     
    			| Rule (r,embedding) -> (*Buffer contains a internal rule*)
  					begin
							if !Parameter.debugModeOn then
							Debug.tag (Printf.sprintf "In Volume %s: Applying %s" (self#getHumanName env) r.Dynamics.kappa) ;
							inc env event.dt ; 
    					try
								let state,causal',env,pert_ids,_ = 
									apply_and_update self#getState r embedding self#getCounter causal env 
								in 
      					self#setCausal causal' ;
								self#setState state ;
								compCounter.Counter.cons_null_events <- 0 ;
      					(env,pert_ids)
      				with Null_event i -> 
								(Counter.stat_null i self#getCounter ;
								Counter.inc_null_events self#getCounter ;
								Counter.inc_consecutive_null_events self#getCounter ;   
								(env,IntSet.empty))
    				end
    				
  				| Pert pert_ids -> (*Buffer contains time dependent perturbations*)
  					((inc env event.dt) ;
							if !Parameter.debugModeOn then
								Debug.tag 
								(Printf.sprintf "In Volume %s: Applying perturbations %s" (self#getHumanName env)
								(Tools.string_of_set string_of_int IntSet.fold pert_ids)
								) ;
							Counter.stat_null 0 self#getCounter ;
							Counter.inc_null_events self#getCounter ;
							Counter.inc_consecutive_null_events self#getCounter ;
							(env,pert_ids))
							
					| Inbound (components,r) ->
						if !Parameter.debugModeOn then
							Debug.tag (Printf.sprintf "In Volume %s: Incoming packages!" (self#getHumanName env)) ;
						inc env event.dt ;
						let state = self#getState in 
						let state,new_nodes = (*adding new nodes to sg*)
							List.fold_left
							(fun (state,new_nodes) nodes ->
								List.fold_left 
								(fun (state,new_nodes) node ->
									let state = {state with State.graph = Graph.SiteGraph.add state.State.graph node} in
									let j =
										(try Graph.SiteGraph.( & ) node
    								with
    								| Not_found -> invalid_arg "State.apply: not allocated") 
    							in
									(state,IntMap.add j node new_nodes)
								) (state,new_nodes) nodes
							) (state,IntMap.empty) components
						in
						self#setState state ;
						(*Checking whether new injections can be applied to new nodes*)
						
						(*NOT EFFICIENT*)
						
						(*should filter here mixtures that may increased by adding nodes*)
						let map_mix f mixtures cont = Hashtbl.fold (fun _ mix cont -> f mix cont) mixtures cont in
						if !Parameter.debugModeOn then
							Debug.tag (Printf.sprintf "Searching for new embeddings!") ;
						let state = State.initialize_embeddings (IntMap.fold,new_nodes) self#getState (map_mix,state.State.kappa_variables) self#getCounter env 
						in
						Hashtbl.iter 
						(fun rule_id' _ -> State.update_activity state r.Dynamics.r_id rule_id' self#getCounter env
						) state.State.rules ;
						self#setState state ;
						let pert_ids = IntMap.fold (fun id _ set -> IntSet.add id set) state.State.perturbations IntSet.empty
						in
						(env,pert_ids)
		in
		
		(*checking if some perturbation(s) should be applied*)
		let state,env,obs_story,pert_events,_ = 
			External.try_perturbate [] self#getState pert_ids [] self#getCounter env 
		in
		self#setState state ;
		
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
  					) (story_profiling,event_list,Counter.event self#getCounter) pert_events
  			  in 
  		    (story_profiling,event_list,cpt) 
  		  end
  		else
  			(story_profiling,event_list,Counter.event self#getCounter)
  		in 
			causal <- (story_profiling,event_list) ;
  		compCounter.Counter.perturbation_events <- cpt ;
			env
end

module CompHeap = Heap.Make (struct type t = compartment let allocate = fun c id -> c#setId id let get_address = fun c -> c#getId end)
		