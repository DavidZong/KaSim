open Tools
open Mods
open State
open Random_tree

let version = "4.0-170613"

let usage_msg = "KaSim "^version^": \n"^"Usage is KaSim -i input_file [-e events | -t time] [-p points] [-o output_file]\n"
let version_msg = "Kappa Simulator: "^version^"\n"

let close_desc opt_env =
	List.iter (fun d -> close_out d) !Parameter.openOutDescriptors ;
	List.iter (fun d -> close_in d) !Parameter.openInDescriptors ;
	match opt_env with
		| None -> ()
		| Some env -> Environment.close_desc env

let main =
	let options = [
		("--version", Arg.Unit (fun () -> print_string (version_msg^"\n") ; flush stdout ; exit 0), "display KaSim version");
		("-i", Arg.String (fun fic -> Parameter.inputKappaFileNames:= fic:: (!Parameter.inputKappaFileNames)),
			"name of a kappa file to use as input (can be used multiple times for multiple input files)");
		("-e", Arg.Int (fun i -> if i < 0 then Parameter.maxEventValue := None else Parameter.maxTimeValue:= None ; Parameter.maxEventValue := Some i) ,
			"Number of total simulation events, including null events (negative value for unbounded simulation)");
		("-t", Arg.Float(fun t -> Parameter.maxTimeValue := Some t ; Parameter.maxEventValue := None), "Max time of simulation (arbitrary time unit)");
		("-p", Arg.Int (fun i -> Parameter.plotModeOn := true ; Parameter.pointNumberValue:= Some i), "Number of points in plot");
		("-o", Arg.String (fun s -> Parameter.outputDataName:=s ), "file name for data output") ;
		("-d", 
		Arg.String 
			(fun s -> 
				try 
					if Sys.is_directory s then Parameter.outputDirName := s 
					else (Printf.eprintf "'%s' is not a directory\n" s ; exit 1)  
				with Sys_error msg -> (*directory does not exists*) 
					(Printf.eprintf "%s\n" msg ; exit 1)
			), "Specifies directory name where output file(s) should be stored") ;
		("-load-sim", Arg.String (fun file -> Parameter.marshalizedInFile := file) , "load simulation package instead of kappa files") ; 
		("-make-sim", Arg.String (fun file -> Parameter.marshalizedOutFile := file) , "save kappa files as a simulation package") ; 
		("--implicit-signature", Arg.Unit (fun () -> Parameter.implicitSignature := true), "Program will guess agent signatures automatically") ;
		("-seed", Arg.Int (fun i -> Parameter.seedValue := Some i), "Seed for the random number generator") ;
		("--eclipse", Arg.Unit (fun () -> Parameter.eclipseMode:= true), "enable this flag for running KaSim behind eclipse plugin") ;
		("--emacs-mode", Arg.Unit (fun () -> Parameter.emacsMode:= true), "enable this flag for running KaSim using emacs-mode") ;
		("--compile", Arg.Unit (fun _ -> Parameter.compileModeOn := true), "Display rule compilation as action list") ;
		("--debug", Arg.Unit (fun () -> Parameter.debugModeOn:= true), "Enable debug mode") ;
		("--safe", Arg.Unit (fun () -> Parameter.safeModeOn:= true), "Enable safe mode") ;
		("--backtrace", Arg.Unit (fun () -> Parameter.backtrace:= true), "Backtracing exceptions") ;
		("--gluttony", Arg.Unit (fun () -> Gc.set { (Gc.get()) with Gc.space_overhead = 500 (*default 80*) } ;), "Lower gc activity for a faster but memory intensive simulation") ;
		("-rescale-to", Arg.Int (fun i -> Parameter.rescale:=Some i), "Rescale initial concentration to given number for quick testing purpose") ; 
  ]
	in
	try
		Arg.parse options (fun _ -> Arg.usage options usage_msg ; exit 1) usage_msg ;
		if not !Parameter.plotModeOn then ExceptionDefn.warning "No data points are required, use -p option for plotting data.";
		let abort =
			match !Parameter.inputKappaFileNames with
			| [] -> if !Parameter.marshalizedInFile = "" then true else false
			| _ -> false
		in
		if abort then (prerr_string usage_msg ; exit 1) ;
		let sigint_handle = fun _ ->
     raise (ExceptionDefn.UserInterrupted (fun t -> fun e -> Printf.sprintf "Abort signal received after %E t.u (%d events)" t e))
    in
    let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle sigint_handle) in
    
		Printexc.record_backtrace !Parameter.backtrace ; (*Possible backtrace*)
		
		(*let _ = Printexc.record_backtrace !Parameter.debugModeOn in*) 
		let result =
			Ast.init_compil() ;
			List.iter (fun fic -> let _ = KappaLexer.compile fic in ()) !Parameter.inputKappaFileNames ;
			!Ast.result
		in
		let (_: unit) =
			match !Parameter.seedValue with
			| Some seed -> Random.init seed
			| None ->
					begin
						Printf.printf "+ Self seeding...\n" ;
						Random.self_init() ;
						let i = Random.bits () in
						Random.init i ;
						Printf.printf "+ Initialized random number generator with seed %d\n" i
					end
		in
		
		let (env, state_map,vol_map) = 
			match !Parameter.marshalizedInFile with
				| "" -> Eval.initialize result (Counter.create 0.0 0 !Parameter.maxTimeValue !Parameter.maxEventValue)
				| marshalized_file ->
					try
						let d = open_in_bin marshalized_file in 
						begin
							if !Parameter.inputKappaFileNames <> [] then Printf.printf "+ Loading simulation package %s (kappa files are ignored)...\n" marshalized_file 
							else Printf.printf "+Loading simulation package %s...\n" marshalized_file ;
							let env,state_map,vol_map = 
								(Marshal.from_channel d : Environment.t * (State.implicit_state IntMap.t) * 
								(Dynamics.rule list *
          			(((int -> Mods.Num.t) -> (int -> Mods.Num.t) -> float -> int -> int -> float -> (int -> Mods.Num.t) -> Mods.Num.t) * bool * Mods.Num.t option * Mods.DepSet.t * string) list * 
								Mixture.t list *
          			(Dynamics.variable * Mods.DepSet.t * int) list *
          			Dynamics.perturbation list * Dynamics.rule list
								) Mods.IntMap.t) 
							in
							Pervasives.close_in d ;
							Printf.printf "Done\n" ;
							(env,state_map,vol_map) 
						end
					with
						| exn -> (Debug.tag "!Simulation package seems to have been created with a different version of KaSim, aborting..." ; exit 1) 
		in
		
		Parameter.setOutputName() ; (*changing output names if -d option was used*)
		Parameter.checkFileExists() ;
		
		let (_:unit) = 
			match !Parameter.marshalizedOutFile with
				| "" -> ()
				| file -> 
					let d = open_out_bin file in
					begin
						Marshal.to_channel d (env,state_map,vol_map) [Marshal.Closures] ;
						close_out d
					end
		in
		
		let compartment_map =
  		IntMap.fold 
  		(fun vol_id state comp_map ->
  			let vol_num = Environment.num_of_volume_id vol_id env in
  			let vol_label = Environment.volume_of_num vol_num env in
  			let volume = Environment.size_of_volume vol_num env in
  			let counter = Counter.create 0.0 0 !Parameter.maxTimeValue !Parameter.maxEventValue in
  	    let plot = Plot.create ((!Parameter.outputDataName)^vol_label^(string_of_int vol_id)) in
  	
  			let causal =
  				let profiling = Compression_main.D.S.PH.B.PB.CI.Po.K.P.init_log_info () in 
  				if Environment.tracking_enabled env then
    				let _ = 
    					if !Parameter.mazCompression || !Parameter.weakCompression then ()
    					else (ExceptionDefn.warning "Causal flow compution is required but no compression is specified, will output flows with no compresion"  ; 
    					Parameter.mazCompression := true)
    				in  
            let event_list = [] in 
            let profiling,event_list = 
            Compression_main.D.S.PH.B.PB.CI.Po.K.store_init profiling state event_list in 
            (profiling,event_list)
          else (profiling,[])
    		in 
  			let c = new Vol.compartment vol_num volume state counter causal plot
  	    in
  			let n = IntSet.cardinal (Environment.volume_id_of_num vol_num env) in
  			let comp_heap = try IntMap.find vol_num comp_map with Not_found -> Vol.CompHeap.create n in
  			let comp_heap = Vol.CompHeap.alloc c comp_heap in
  			IntMap.add vol_num comp_heap comp_map 
  		) state_map IntMap.empty
		in
		
		let top_state = IntMap.find 0 state_map in
		if !Parameter.influenceFileName <> ""  then 
			begin
				let desc = open_out !Parameter.influenceFileName in
				State.dot_of_influence_map desc top_state env ; (*the state of TOP is always defined and contains all that is needed for computing rules*)
				close_out desc 
			end ;  
		if !Parameter.compileModeOn then (Hashtbl.iter (fun i r -> Dynamics.dump r env) top_state.State.rules ; exit 0)
		else () ;
    ExceptionDefn.flush_warning () ; 
		Parameter.initSimTime () ; 
		
		
		let scd = ref (Scheduler.create compartment_map vol_map env) in
		
		try
  		begin
    		
			(try 
    		while(true) do
	    		scd := Scheduler.step !scd 
    		done 
  		with
  		Scheduler.End_of_sim i ->	Debug.tag ("Simulation terminated with code "^(string_of_int i))  
  		);
			
  		print_newline() ;
  		let scd = !scd in
			let counter = scd.Scheduler.clock in
  		Printf.printf "Simulation ended (eff.: %f, detail below)\n" 
  		((float_of_int (Counter.event counter)) /. (float_of_int (Counter.null_event counter + Counter.event counter))) ;
  		Array.iteri (fun i n ->
  			match i with
  				| 0 -> (Printf.printf "\tValid embedding but no longer unary when required: %f\n" ((float_of_int n) /. (float_of_int (Counter.null_event counter))))
  				| 1 -> (Printf.printf "\tValid embedding but not binary when required: %f\n" ((float_of_int n) /. (float_of_int (Counter.null_event counter))))
  				| 2 -> (Printf.printf "\tClashing instance: %f\n" ((float_of_int n) /. (float_of_int (Counter.null_event counter))))
  				| 3 -> (Printf.printf "\tLazy negative update: %f\n" ((float_of_int n) /. (float_of_int (Counter.null_event counter))))
  				| 4 -> (Printf.printf "\tLazy negative update of non local instances: %f\n" ((float_of_int n) /. (float_of_int (Counter.null_event counter))))
  				| 5 -> (Printf.printf "\tPerturbation interrupting time advance: %f\n" ((float_of_int n) /. (float_of_int (Counter.null_event counter))))
  				|	_ -> print_string "\tna\n"
  		) counter.Counter.stat_null ;
  		if !Parameter.fluxModeOn then 
  			begin
  				let d = open_out !Parameter.fluxFileName in
  				State.dot_of_flux d top_state env ;
  				close_out d
  			end 
  		else () ;
			end
		with
			| Invalid_argument msg -> 
				begin
					(*if !Parameter.debugModeOn then (Debug.tag "State dumped! (dump.ka)" ; let desc = open_out "dump.ka" in State.snapshot state counter desc env ; close_out desc) ; *)
				  let s = (* Printexc.get_backtrace() *) "" in Printf.eprintf "\n***Runtime error %s***\n%s\n" msg s ;
					exit 1
				end
			| ExceptionDefn.UserInterrupted f -> 
				begin
					let counter = !scd.Scheduler.clock in
					flush stdout ; 
					let msg = f (Counter.time counter) (Counter.event counter) in
					Printf.eprintf "\n***%s: would you like to record the current state? (y/N)***\n" msg ; flush stderr ;
					(match String.lowercase (Tools.read_input ()) with
						| "y" | "yes" ->
							begin 
								Parameter.dotOutput := false ;
								let desc = open_out !Parameter.dumpFileName in 
								State.snapshot top_state counter desc !Parameter.snapshotHighres env ;
								close_out desc ;
								Printf.eprintf "Final state dumped (%s)\n" !Parameter.dumpFileName
							end
						| _ -> ()
					) ;
					close_desc (Some env) (*closes all other opened descriptors*)
				end
			| ExceptionDefn.Deadlock ->
				let counter = !scd.Scheduler.clock in
				(Printf.printf "?\nA deadlock was reached after %d events and %Es (Activity = %.5f)\n"
				(Counter.event counter)
				(Counter.time counter) 
				(Random_tree.total top_state.activity_tree))
	with
	| ExceptionDefn.Semantics_Error (pos, msg) -> 
		(close_desc None ; Printf.eprintf "***Error (%s) line %d, char %d: %s***\n" (fn pos) (ln pos) (cn pos) msg)
	| Invalid_argument msg ->	(close_desc None; let s = (Printexc.get_backtrace()) in Printf.eprintf "\n***Runtime error %s***\n%s\n" msg s)
	| ExceptionDefn.UserInterrupted f -> 
		let msg = f 0. 0 in 
		(Printf.eprintf "\n***Interrupted by user: %s***\n" msg ; close_desc None)
	| ExceptionDefn.StopReached msg -> (Printf.eprintf "\n***%s***\n" msg ; close_desc None)
	| Sys_error msg -> (close_desc None; Printf.eprintf "%s\n" msg)