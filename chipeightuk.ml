(* todo : fix keys *)

(** Types **)

type bytes = string

type stack = int Stack.t

type opcode = int * int * int * int

type state = { mem : bytes
	     ; v : bytes
	     ; pc_stack : stack
	     ; mutable delay_timer : int
	     ; mutable sound_timer : int
	     ; mutable pc : int
	     ; mutable i : int
	     ; mutable key_pressed : int option
	     ; mutable pressed_since : float
	     ; screen : bool array array
	     }

type config = { period : float
              ; ops_per_period : int
	      ; px_mod_bounds : bool
              ; px_size : int
              ; px_on : Graphics.color
              ; px_off : Graphics.color
	      ; sound_freq : int option
              ; chip8_keys : string
	      ; exec_keys : (char * (unit -> unit)) list
	      ; key_timeout : float
              ; sprites : string array
	      ; debug : bool
	      ; rom_file : string
              }

type precalc = { secs_per_op : float
               ; period_ms : int
	       ; x_coords : int array
	       ; y_coords : int array
	       ; real_px_size : int
	       ; real_width : int
	       ; real_height : int
               }

(** Exceptions **)

exception Escaped

exception End_of_rom

exception Not_implemented of opcode


(** Constants **)

let mem_size = 4096

let reg_size = 16

let mem_start = 0x200

let width = 64

let height = 32


(** Manipulating the [bytes] type **)

(* bytes_make : int -> int -> bytes
     [bytes_make n byte] creates a [n]-bytes long
     variable with all bytes initialized to [byte]. *)

let bytes_make n byte =
  String.make n (Char.chr byte)

(* bytes_get : bytes -> int -> int
     [bytes_get bytes i] returns the [i]-th byte of [bytes]. *)

let bytes_get bytes i =
  Char.code (String.get bytes i)

(* bytes_set : bytes -> int -> int -> unit
     [bytes_set bytes i byte] sets the [i]-th byte of [bytes]
     to [byte]. *)

let bytes_set bytes i byte =
  String.set bytes i
             (Char.chr (byte land 0xFF))

(* bytes_copy : bytes -> int -> string -> unit
     [bytes_copy bytes i str] copies every character in [str]
     to [bytes], starting at the [i]-th byte of [bytes]. *)

let bytes_copy bytes i str =
  String.blit str 0 bytes i
              (String.length str)

(* bytes_copy_from_file : bytes -> int -> string -> unit
     [bytes_copy bytes i file] copies every byte in [file]
     to [bytes], starting at the [i]-th byte of [bytes]. *)

let bytes_copy_from_file bytes i file =
  let ic = open_in_bin file in
  try really_input ic bytes i
                   (String.length bytes - i - 1)
  with End_of_file -> ();
  close_in ic


(** Screen related functions **)

(* open_screen : config -> unit
     [open_screen config] opens a window and sets its title. *)

let open_screen config =
  let geom = Printf.sprintf "%dx%d"
                            (config.px_size * width)
                            (config.px_size * height) in
  Graphics.open_graph (match Sys.os_type with
                       | "Unix" -> " " ^ geom
                       | _ -> geom);
  Graphics.set_window_title ("chipeightuk - "
                             ^ config.rom_file)

(* clear_screen : config -> state -> unit
     [clear_screen config state] turns off every pixel,
     updating [state]. *)

let clear_screen config precalc state =
  Graphics.set_color config.px_off;
  Graphics.fill_rect 0 0
                     precalc.real_width
                     precalc.real_height;
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      state.screen.(y).(x) <- false
    done
  done

(* flip_pixel : config -> precalc -> state -> int -> int -> bool
     [flip_pixel cfg precalc x y value] turns on the pixel at [(x,y)]
     if it was off, or turns it off if it was on, returning true if
     the pixel was initially on, false otherwise.
     [x] and [y] may be greather than screen boundaries, in which case
     the pixel will be either ignored or flipped modulo boundaries,
     depending on [cfg]. *)

let flip_pixel cfg precalc state x y =
  let x' = x mod width in
  let y' = y mod height in
  if cfg.px_mod_bounds || (x = x' && y = y') then
    begin
      let real_x = precalc.x_coords.(x') in
      let real_y = precalc.y_coords.(y') in
      let was_on = state.screen.(y').(x') in
      Graphics.set_color (if was_on then cfg.px_off else cfg.px_on);
      Graphics.fill_rect real_x real_y
                         precalc.real_px_size precalc.real_px_size;
      state.screen.(y').(x') <- not was_on;
      was_on
    end
  else
    false

(* draw_sprite : config -> precalc -> int -> int -> sprite -> unit
     [draw_sprite cfg x y spr] draws the 8-width sprite
     represented by [spr]'s eight least significant bits on
     the screen at position [(x,y)]. *)

let draw_sprite cfg precalc state x y spr =
  let any_was_on = ref false in
  for k = 0 to 7 do
    if spr land (1 lsl k) <> 0 then
      any_was_on := flip_pixel cfg precalc state (x + 7 - k) y
                   || !any_was_on
  done;
  !any_was_on


(** Events related functions **)

(* exec_key : config -> char -> unit
     [exec_key config key] calls the custom function bound
     to [key], if any. *)

let exec_key config key =
  try let f = List.assoc key config.exec_keys in
      f ()
  with Not_found -> ()

(* poll_key_press : config -> state -> int
     [poll_key_press config state] searches the events list for
     the latest key press, updating [state] and calling [exec_key]
     for each custom key press.
     [state] is also updated if the last key press has timed out. *)

let poll_key_press config state =
  let new_key = ref None in
  while Graphics.key_pressed () do
    let pressed_key = Graphics.read_key () in
    try new_key := Some (String.index
                           config.chip8_keys
                           pressed_key)
    with Not_found -> exec_key config pressed_key
  done;
  match !new_key with
  | Some code ->
      state.key_pressed <- Some code;
      state.pressed_since <- Unix.gettimeofday ()
  | None ->
      if state.key_pressed <> None then
        let pressed_for = (Unix.gettimeofday () -. state.pressed_since) in
        if pressed_for > config.key_timeout then
	  state.key_pressed <- None

(* wait_key : config -> state -> int
     [wait_key config state] empties the event list and waits until
     a game related key is pressed then updates [state] and returns
     the corresponding key code. *)

let rec wait_key config state =
  ignore (poll_key_press config state);
  let status = Graphics.wait_next_event [Graphics.Key_pressed] in
  try let code = String.index config.chip8_keys status.Graphics.key in
      state.key_pressed <- Some code;
      state.pressed_since <- Unix.gettimeofday ();
      code
  with Not_found -> exec_key config status.Graphics.key;
                    wait_key config state


(** Miscellaneous functions used by the main loop **)

(* sleep_for : float -> ()
     [sleep_for n] stops program execution for [n] seconds. *)

let sleep_for n =
  let until = Unix.gettimeofday() +. n in
  let rec delay t =
    try ignore (Unix.select [] [] [] t)
    with Unix.Unix_error (Unix.EINTR, _, _) ->
      let now = Unix.gettimeofday() in
      let remaining = until -. now in
      if remaining > 0. then
        delay remaining
  in delay n

(* split_byte : int -> int * int
     [split_byte 0xXY] is [(0xX, 0xY)] *)

let split_byte byte =
  (byte lsr 4, byte land 0xF)

(* combine_nibbles : int list -> int
     [combine_nibbles [X;Y;Z]] is [0xXYZ]. *)

let combine_nibbles l =
  List.fold_left (fun acc x -> (acc lsl 4) lor (x land 0xF)) 0 l

(* do_precalc : config -> precalc
     [precalc config] does some calculation once and for all *)
let do_precalc config =
  let os_dep = match Sys.os_type with
               | "Unix" -> (-1)
               | _ -> 0
  in
  { secs_per_op = config.period /. float_of_int config.ops_per_period
  ; period_ms = int_of_float (config.period *. 1000.)
  ; x_coords = Array.init width
                          (fun x -> x * config.px_size)
  ; y_coords = Array.init height
                          (fun y -> (height - y + 1) * config.px_size)
  ; real_px_size = config.px_size + os_dep
  ; real_width = width * config.px_size + os_dep
  ; real_height = height * config.px_size + os_dep
  }

(** Processor emulation functions **)

(* init_state : config -> state
     [init_state config] return the initial emulator state. *)

let init_state config =
  let init_mem = bytes_make mem_size 0 in
  bytes_copy init_mem 0 (Array.fold_left (^) "" config.sprites);
  { mem = init_mem
  ; v = bytes_make reg_size 0
  ; pc_stack = Stack.create ()
  ; delay_timer = 0
  ; sound_timer = 0
  ; pc = mem_start
  ; i = 0
  ; key_pressed = None
  ; pressed_since = 0.
  ; screen = Array.make_matrix height width false
  }

(* read_next_op : config -> state -> opcode
     [read_next_op state] reads the two next bytes from the rom,
     updating [state] and returning the corresponding opcode.
     [config] may be used in a later version. *)

let read_next_op config state =
  ignore (config);
  let (a, b) = split_byte (bytes_get state.mem state.pc) in
  let (c, d) = split_byte (bytes_get state.mem (state.pc + 1)) in
  state.pc <- state.pc + 2;
  (a, b, c, d)

(* exec_op : config -> precalc -> state -> opcode -> unit
     [exec_op config state op] executes operation [op],
     updating [state].
     [Not_implemented op] exception is raised if [op] is unknown,
     for more information please check the following link:
       http://en.wikipedia.org/wiki/CHIP-8#Virtual_machine_description *)

let exec_op config precalc state op =
  let get_v = bytes_get state.v in
  let set_v = bytes_set state.v in
  match op with
  | (0,0,0,0) -> raise End_of_rom
  | (0,0,0xE,0) -> clear_screen config precalc state
  | (0,0,0xE,0xE) -> state.pc <- Stack.pop state.pc_stack
  | (1,n2,n1,n0) -> state.pc <- combine_nibbles [n2;n1;n0]
  | (2,n2,n1,n0) -> Stack.push state.pc state.pc_stack;
                    state.pc <- combine_nibbles [n2;n1;n0]
  | (3,x,n1,n0) -> if get_v x = combine_nibbles [n1;n0] then
                     state.pc <- state.pc + 2
  | (4,x,n1,n0) -> if get_v x <> combine_nibbles [n1;n0] then
                     state.pc <- state.pc + 2
  | (5,x,y,0) -> if get_v x = get_v y then
                   state.pc <- state.pc + 2
  | (6,x,n1,n0) -> set_v x (combine_nibbles [n1;n0])
  | (7,x,n1,n0) -> set_v x (get_v x + combine_nibbles [n1;n0])
  | (8,x,y,0) -> set_v x (get_v y)
  | (8,x,y,1) -> set_v x (get_v x lor get_v y)
  | (8,x,y,2) -> set_v x (get_v x land get_v y)
  | (8,x,y,3) -> set_v x (get_v x lxor get_v y)
  | (8,x,y,4) -> let res = get_v x + get_v y in
	         set_v x res;
	         set_v 0xF (res lsr 8)
  | (8,x,y,5) -> let res = get_v x - get_v y in
		 if res >= 0 then
		   begin
		     set_v x res;
		     set_v 0xF 1
		   end
		 else
		   begin
		     set_v x (res + 256);
		     set_v 0xF 0
		   end
  | (8,x,y,6) -> let vx = get_v x in
		 set_v 0xF (vx land 1);
		 set_v x (vx lsr 1)
  | (8,x,y,7) -> let res = get_v y - get_v x in
		 if res >= 0 then
		   begin
		     set_v x res;
		     set_v 0xF 1
		   end
		 else
		   begin
		     set_v x (res + 256);
		     set_v 0xF 0
		   end
  | (8,x,y,0xE) -> let vx = get_v x in
		   set_v 0xF (vx lsr 7);
		   set_v x ((vx land 0x7F) lsl 1)
  | (9,x,y,0) -> if get_v x <> get_v y then
                   state.pc <- state.pc + 2
  | (0xA,n2,n1,n0) -> state.i <- combine_nibbles [n2;n1;n0]
  | (0xB,n2,n1,n0) -> state.pc <- get_v 0 + combine_nibbles [n2;n1;n0]
  | (0xC,x,n1,n0) -> set_v x (Random.int (1 + combine_nibbles [n1;n0]))
  | (0xD,x,y,n) -> let any_was_on = ref false in
		   let vx = get_v x in
		   let vy = get_v y in
		   for k = 0 to n - 1 do
                     let was_on = draw_sprite
                                    config
				    precalc
				    state
		                    vx (vy + k)
                                    (bytes_get state.mem (state.i + k))
                     in any_was_on := was_on || !any_was_on
                   done;
		   set_v 0xF (if !any_was_on then 1 else 0)
  | (0xE,x,0x9,0xE) -> if state.key_pressed = Some (get_v x) then
                         state.pc <- state.pc + 2
  | (0xE,x,0xA,1) -> if state.key_pressed <> Some (get_v x) then
                       state.pc <- state.pc + 2
  | (0xF,x,0,7) -> set_v x state.delay_timer
  | (0xF,x,0,0xA) -> (match state.key_pressed with
                      | Some k -> set_v x k
                      | None -> state.pc <- state.pc - 2)
  | (0xF,x,1,5) -> state.delay_timer <- get_v x
  | (0xF,x,1,8) -> state.sound_timer <- get_v x
  | (0xF,x,1,0xE) -> state.i <- state.i + get_v x
  | (0xF,x,2,9) -> state.i <- (get_v x) * 5
  | (0xF,x,3,3) -> let n2n1n0 = get_v x in
		   let (n2n1, n0) = (n2n1n0 / 10, n2n1n0 mod 10) in
		   let (n2, n1) = (n2n1 / 10, n2n1 mod 10) in
		   bytes_set state.mem state.i n2;
		   bytes_set state.mem (state.i + 1) n1;
		   bytes_set state.mem (state.i + 2) n0
  | (0xF,x,5,5) -> for k = 0 to x do
                     bytes_set state.mem (state.i + k) (get_v k)
                   done
  | (0xF,x,6,5) -> for k = 0 to x do
	             set_v k (bytes_get state.mem (state.i + k))
                   done
  | _ -> raise (Not_implemented op)

(* loop : config -> state -> float -> int -> unit
     [loop config state secs_per_op period_ms] executes
     one cycle of operations then updates delay and sound
     timers and performs a recursive call. *)

let rec loop config precalc state =
  poll_key_press config state;
  for i = 1 to config.ops_per_period do
    let init_time = Unix.gettimeofday () in
    let op = read_next_op config state in
    if config.debug then
    begin
      Printf.printf "I=%03X V=[" state.i;
      for k = 0 to 0xF do
        Printf.printf "%02X;" (bytes_get state.v k)
      done;
      Printf.printf "\b]\npc=%03X " state.pc;
      let (a,b,c,d) = op in
      Printf.printf "%X%X%X%X -> " a b c d;
      flush stdout;
      ignore (wait_key config state)
    end;
    exec_op config precalc state op;
    let elapsed = Unix.gettimeofday () -. init_time in
    let remaining = precalc.secs_per_op -. elapsed in
    if remaining > 0. then
      sleep_for remaining
    (*;if not config.debug then
      begin
        let (a,b,c,d) = op in
        Printf.printf "%X%X%X%X : %f us\n"
                      a b c d
                      (elapsed *. 10e6);
	flush stdout
      end*)
  done;
  if state.delay_timer > 0 then
    state.delay_timer <- state.delay_timer - 1;
  if state.sound_timer > 0 then
  begin
    (match config.sound_freq with
     | Some f -> Graphics.sound f (state.sound_timer * precalc.period_ms)
     | None -> ());
    state.sound_timer <- 0
  end;
  loop config precalc state

(* power_on : config -> unit
     [power_on config] starts the emulator. *)

let power_on config =
  Random.self_init ();
  open_screen config;
  let state = init_state config in
  bytes_copy_from_file state.mem 0x200 config.rom_file;
  let precalc = do_precalc config in
  clear_screen config precalc state;
  try loop config precalc state
  with End_of_rom | Escaped -> Graphics.close_graph ()


(** Command-line parsing **)

let colors_by_key = [ "black", Graphics.black
                    ; "white", Graphics.white
                    ; "red", Graphics.red
                    ; "green", Graphics.green
                    ; "blue", Graphics.blue
                    ; "yellow", Graphics.yellow
                    ; "cyan", Graphics.cyan
                    ; "magenta", Graphics.magenta
                    ]

let (ckeys, cvalues) = List.split colors_by_key

let colors_by_value = List.combine cvalues ckeys

let color_of_string str =
  List.assoc str colors_by_key

let string_of_color color =
  try List.assoc color colors_by_value
  with Not_found -> Printf.sprintf "0x%06X" color 

let base_cfg = { period = 1. /. 60.
               ; ops_per_period = 4
               ; debug = false
               ; px_mod_bounds = true
               ; px_size = 8
               ; px_on = Graphics.rgb 0xFF 0xcc 0x00
               ; px_off = Graphics.rgb 00 00 0x77
               ; sound_freq = Some 220
               ; chip8_keys = "0123456789/*-+\r." (* 0123456789ABCDEF *)
               ; exec_keys = [('\x1B', fun () -> raise Escaped)]
               ; key_timeout = 0.300
               ; sprites = [| "\xF0\x90\x90\x90\xF0" (* 0 *)
                            ; "\x20\x60\x20\x20\x70" (* 1 *)
                            ; "\xF0\x10\xF0\x80\xF0" (* 2 *)
                            ; "\xF0\x10\xF0\x10\xF0" (* 3 *)
                            ; "\x90\x90\xF0\x10\x10" (* 4 *)
                            ; "\xF0\x80\xF0\x10\xF0" (* 5 *)
                            ; "\xF0\x80\xF0\x90\xF0" (* 6 *)
                            ; "\xF0\x10\x20\x40\x40" (* 7 *)
                            ; "\xF0\x90\xF0\x90\xF0" (* 8 *)
                            ; "\xF0\x90\xF0\x10\xF0" (* 9 *)
                            ; "\xF0\x90\xF0\x90\x90" (* A *)
                            ; "\xE0\x90\xE0\x90\xE0" (* B *)
                            ; "\xF0\x80\x80\x80\xF0" (* C *)
                            ; "\xE0\x90\x90\x90\xE0" (* D *)
                            ; "\xF0\x80\xF0\x80\xF0" (* E *)
                            ; "\xF0\x80\xF0\x80\x80" (* F *)
                            |]
               ; rom_file = ""
               }

let _ =
  let usage = "\nUsage: chipeightuk <options> <file-list>\n\n"
            ^ "chipeightuk, a CHIP-8 emulator written in OCaml\n\n"
            ^ "Options are:" in
  let cfg = ref base_cfg in
  let rom_files = Queue.create () in
  let fun_anon_args x = Queue.push x rom_files in
  let specs =
    [  "-d"
     , Arg.Symbol
         ( ["on";"off"]
         , (fun str -> cfg := {!cfg with debug = (str = "on")})
         )
     , " Set debug mode "
       ^ (Printf.sprintf "(default %s)"
                         (if base_cfg.debug then "on" else "off"))
    ;
       "-pmb"
     , Arg.Symbol
         ( ["on";"off"]
         , (fun str -> cfg := {!cfg with px_mod_bounds = (str = "on")})
         )
     , " Set pixels modulo boundaries mode "
       ^ (Printf.sprintf "(default %s)"
                         (if base_cfg.px_mod_bounds then "on" else "off"))
    ;
       "-tf"
     , Arg.Float (fun f -> cfg := {!cfg with period = 1. /. f})
     , "<frequency>  Set timers frequency in Hertz "
       ^ (Printf.sprintf "(default %.2f)" (1. /. base_cfg.period))
    ;
       "-tp"
     , Arg.Float (fun p -> cfg := {!cfg with period = p /. 1000.})
     , "<period>  Set timers period in milliseconds "
       ^ (Printf.sprintf "(default %.2f)" (base_cfg.period *. 1000.))
    ;
       "-oc"
     , Arg.Int (fun oc -> cfg := {!cfg with ops_per_period = oc})
     , "<count>  Set number of opcodes executed during one period "
       ^ (Printf.sprintf "(default %d)" base_cfg.ops_per_period)
    ;
       "-km"
     , Arg.String (fun keys -> cfg := {!cfg with chip8_keys = keys})
     , "<keys>  Set whole 0123456789ABCDEF keymap "
       ^ (Printf.sprintf "(default %S)" base_cfg.chip8_keys)
    ;
       "-kt"
     , Arg.Float (fun t -> cfg := {!cfg with key_timeout = t /. 1000.})
     , "<time>  Set key-press timeout in milliseconds "
       ^ (Printf.sprintf " (default %.2f)" (base_cfg.key_timeout *. 1000.))
    ;
       "-sf"
     , Arg.Int (fun f -> cfg := {!cfg with sound_freq = Some f})
     , "<frequency>  Set sound output frequency in Hertz "
       ^ (Printf.sprintf "(default %d)"
                         (match base_cfg.sound_freq with
                          | Some f -> f
	                  | None -> (-1)))
    ;
       "-mute"
     , Arg.Unit (fun () -> cfg := {!cfg with sound_freq = None})
     , " Disable sound output "
       ^ (Printf.sprintf "(default %s)"
                         (match base_cfg.sound_freq with
                          | Some _ -> "enabled"
                          | None -> "disabled"))
    ;
       "-px"
     , Arg.Int (fun size -> cfg := {!cfg with px_size = size})
     , "<size>  Set on-screen size-per-pixel "
       ^ (Printf.sprintf "(default %d)" base_cfg.px_size)
    ;
       "-pxon"
     , Arg.Symbol
        ( ckeys
        , fun c -> cfg := {!cfg with px_on = color_of_string c}
        )
     , "  Set color for enabled pixels "
       ^ (Printf.sprintf "(default %s)" (string_of_color base_cfg.px_on))
    ;
       "-pxoff"
     , Arg.Symbol
        ( ckeys
        , fun c -> cfg := {!cfg with px_off = color_of_string c}
        )
     , "  Set color for disabled pixels "
       ^ (Printf.sprintf "(default %s)" (string_of_color base_cfg.px_off))
    ;
       "-"
     , Arg.String fun_anon_args
     , "<file>  Load a rom file with filename starting with a -"
    ] in
  Arg.parse specs fun_anon_args usage;
  try Queue.iter (fun file ->
                    power_on {!cfg with rom_file = file};
                    if (!cfg).debug then
                      print_newline ())
                 rom_files
  with Graphics.Graphic_failure _ -> if (!cfg).debug then
                                       print_newline ()
