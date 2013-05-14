open Random_tree
open Mods

(**Volume management*)

type event = Out of (float * Node.t * int) list | Tau of float 

class virtual compartment id volume state counter causal plot =
	object (self)
	val mutable id:int = id
	val mutable volume:float = volume
	val mutable state:State.implicit_state = state
	val mutable counter:Counter.t = counter
	val mutable causal:(Compression_main.D.S.PH.B.PB.CI.Po.K.P.log_info * Compression_main.D.S.PH.B.PB.CI.Po.K.step list) = causal
	val mutable plot:Plot.t = plot
	
	method get_local_clock = counter.Counter.time 
	method set_local_clock = fun t -> (counter.Counter.time <- t)
		
	method virtual next_event : unit -> event  

end
		
class virtual passive id volume state counter causal plot =
	object (self) inherit compartment id volume state counter causal plot
end
	
class virtual active id volume state counter causal plot =
	object (self) inherit compartment id volume state counter causal plot
	
	val mutable job = false
	method enable () = job <- true
	method disable () = job <- false
	 
	method spawn t_max env = 
		if not job then ((*Thread.yield () ; Thread.delay 1.0 ;*) self#spawn t_max env) 
		else (self#disable () ; self#run t_max env) 
	
	method private run t_max env =
		let (story_profiling,event_list) = causal in
		counter <- {counter with Counter.max_time = Some t_max} ;
  	try
	 		Run.loop state story_profiling event_list counter plot env
  	with
  		| Invalid_argument msg -> 
  			begin
  				let s = (* Printexc.get_backtrace() *) "" in Printf.eprintf "\n***Runtime error %s occuring in Volume[%d]***\n%s\n" msg id s ;
  				exit 1
  			end
  		| ExceptionDefn.UserInterrupted f -> 
  			begin
  				flush stdout ; 
  				Printf.eprintf "\n***User interrupted simulation in Volume[%d]***\n" id ;
  				exit 0
  			end
  		| ExceptionDefn.Deadlock -> counter.Counter.time <- t_max
end
		

