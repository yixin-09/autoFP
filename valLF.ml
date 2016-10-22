open Cil_types

module Self = Plugin.Register(struct
	let name = "value range of variable according location"
	let shortname = "valLF"
	let help = "Please give the location for calculate"
end)

module Enable = Self.False(struct
	let option_name = "-valLF"
	let help ="when on (off by default), compute the val range of the given express"
end)

module GetLine = Self.Int(struct
	let option_name = "-vL"
	let default = 13
	let arg_name = "line-num"
	let help = "the line number"
end)

module Output = Self.String(struct
	let option_name = "-outf"
	let default = "output"
	let arg_name = "output-file"
	let help = "file where the val is output"
end)


let rec print_val e state fmt ki=
	(match e.enode with
	| BinOp (bp,e1,e2,ty) -> print_val e1 state fmt ki;print_val e2 state fmt ki;
	| Lval (lv,off) -> let get_vt = !Db.Value.access ki (lv,off) in
					Format.fprintf fmt "@[(%a)" !Ast_printer.d_lval (lv,off);
          				Db.Value.pretty fmt get_vt;
					Format.fprintf fmt "@]@.";			
	| UnOp (unop,exp,ty) -> print_val exp state fmt ki;
	| CastE(ty,e) -> print_val e state fmt ki;
	| _ ->();)

let print_Instr (fid:int) state  fmt ki i= 
  (match i with 
  | Set (dest, e, (la,lb)) ->  if( lb.Lexing.pos_lnum = fid ) then print_val e state fmt ki;
  | _ -> ();)

class vallfVisitor (fid:int) fmt= object
	inherit Visitor.frama_c_inplace
	method! vstmt_aux s =Cil.ChangeDoChildrenPost(s, fun s ->
			if Db.Value.is_computed () then
				begin
					(match s.skind with
					| Instr il -> 
						let val_state =
						    	try Db.Value.AfterTable.find s
						    	with Not_found -> Cvalue.Model.bottom
						  	in
						let ki = Kstmt s in
						print_Instr fid val_state fmt ki il;
					|_->(););s;
				end else s;)
end

let run () =
	(if Enable.get() then
		let fid = GetLine.get () in
		let filename = Output.get () in
		let chan = open_out filename in
		let fmt = Format.formatter_of_out_channel chan in
		let val_v = new vallfVisitor in
		Visitor .visitFramacFileSameGlobals (val_v fid fmt) (Ast.get ());
		close_out chan;)
		
		
let () = Db.Main.extend run






