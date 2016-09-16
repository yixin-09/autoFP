(* The goal of astop:*)
(* 1. Convert C program to ast.*)
(* 2. Operating the ast, save information*)
(* 3. According the information and the line number output the statement*)
(* find the arg that record the line number OF C PROGRAM*)

open Printf
open Cil

let debug_out = ref stdout 
let debug fmt = 
  let k result = begin
    output_string !debug_out result ; 
    output_string stdout result ; 
    flush stdout ; 
  end in
  Printf.kprintf k fmt 
(* construct costable *)
type costableh = (Cil.exp, Cil.varinfo) Hashtbl.t
(* whether the exp is a constant*) 
let costable = Hashtbl.create 4096

(* construct calltable *)
type calltableh = (Cil.lval,Cil.exp ) Hashtbl.t
(* whether the instr is a call*) 
let calltable : calltableh= Hashtbl.create 8192
type calltablehs = (Cil.lval,Cil.exp list) Hashtbl.t
let calltables : calltablehs = Hashtbl.create 8192

let rec is_const (e:Cil.exp) = 
	(match e with 
	| Const (const) -> (match const with 
                        | CInt64(_,_,_)-> true;
                        | CReal(_,_,_) -> true;
			| _->false)
	| BinOp (bp,e1,e2,ty) ->if (is_const e1) && (is_const e2) then begin true end else false;
	| UnOp (unop,exp,ty) -> if (is_const exp) then begin true end else false;
	| CastE(ty,exp)-> if (is_const exp) then begin true end else false;
	| Lval (vr) -> if Hashtbl.mem costable vr then begin true end else false;
	|_->false;)

(*convert constant label in Exp to constant *)	
let rec covertBpExp (e: exp) : exp =
	match e with
	| Lval (lb,lf) -> (match lb with 
	                         | Var (vr)-> (if Hashtbl.mem costable (lb,lf) then begin 
		                                  let csb = Hashtbl.find costable (lb,lf) in csb; 
						end else e;)
		                 | _ -> e;)
	| BinOp (bp, e1, e2,ty) -> (let e3 = covertBpExp e1 in let e4 = covertBpExp e2 in let e5 = BinOp(bp, e3, e4,ty) in e5;)
	| CastE(ty,exp)-> let exp1 = covertBpExp exp in exp1;
	| _ -> e;;

let save_to_costable (fid:int)(i: Cil.instr) = 
  ( match i with 
  | Set ((dest,offset), e, l) ->(if l.line < fid then begin (if (Hashtbl.mem costable (dest,offset)) && (not (is_const e)) then begin Hashtbl.remove costable (dest,offset);(); end else 
		(match e with 
		| BinOp (bp,e1,e2,ty) ->if (is_const e1) && (is_const e2) then begin Hashtbl.replace costable (dest,offset) (covertBpExp e); end else ();
		| UnOp (unop,exp,ty) -> if (is_const exp) then begin Hashtbl.replace costable (dest,offset) (covertBpExp e); end else ();
		| CastE(ty,exp)-> if (is_const exp) then begin Hashtbl.replace costable (dest,offset) (covertBpExp exp); end else ();
		| Lval (vr) -> if Hashtbl.mem costable vr then begin Hashtbl.replace costable (dest,offset) (covertBpExp e); end else ();
		| Const (const) -> Hashtbl.replace costable (dest,offset) (covertBpExp e);
		| _ -> ();)) end else ();)
  | (Asm (_, _, _, _, _, _)| Call (_, _, _, _))->();)

let move_out_costable_i (fid:int)(i: Cil.instr) = 
  (match i with 
  | Set ((dest,offset), e, l) ->(if l.line < fid then begin (if Hashtbl.mem costable (dest,offset) then begin Hashtbl.remove costable (dest,offset); end else 
		() ) end else ();)
  | Call (lv,exp,exps,l)->(if l.line < fid then begin 
				(match lv with 
				|Some x -> (if Hashtbl.mem costable x then begin Hashtbl.remove costable x; end else ())
				|None -> ();) end else ();)
  | Asm (_, _, _, _, _, _)->();)

let rec move_out_costable (fid:int)(s:Cil.stmt) =
	(match s.skind with 
		  | Switch(exp,b,stmts,l) -> (if l.line < fid then begin List.iter (move_out_costable fid) b.bstmts;List.iter (move_out_costable fid) stmts; end else ();)
		  | If (exp,b1,b2,l) -> (if l.line < fid then begin List.iter (move_out_costable fid) b1.bstmts;List.iter (move_out_costable fid) b2.bstmts;end else ();)
		  | Loop (b,l,s1,s2) -> (if l.line < fid then begin List.iter (move_out_costable fid) b.bstmts;
					 end else ();)
	          | Instr il-> List.iter (move_out_costable_i fid) il;
	          | _ -> ();
	)
	
(*go though the program, save the const value and its var into table*)
class cosVisitor (file : Cil.file)
		 (fid:int)
                  = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->		
	          match s.skind with 
		  | Switch(exp,b,stmts,l) -> List.iter (move_out_costable fid) b.bstmts;List.iter (move_out_costable fid) stmts;s;
		  | If (exp,b1,b2,l) -> List.iter (move_out_costable fid) b1.bstmts;List.iter (move_out_costable fid) b2.bstmts;s;
		  | Loop (b,l,s1,s2) -> (List.iter (move_out_costable fid) b.bstmts;
					s;)
	          | Instr il-> List.iter (save_to_costable fid) il;s;
	          | _ -> s;
    ) 
end 



let rec has_call (e:Cil.exp) = 
	(match e with 
	| BinOp (bp,e1,e2,ty) ->if ((has_call e1)||(has_call e2)) then begin true end else false;
	| UnOp (unop,exp,ty) -> if (has_call exp) then begin true end else false;
	| CastE(ty,exp)-> if (has_call exp) then begin true end else false;
	| Lval (vr) -> if Hashtbl.mem calltable vr then begin true end else false;
	|_->false;)

(*expand variable in Exp which belong to calltable *)	
let rec covertBpCall (e: exp) =
	(match e with
	| Lval (lb,lf) -> (match lb with 
	                         | Var (vr)-> (if Hashtbl.mem calltable (lb,lf) then begin 
		                                  let csb = Hashtbl.find calltable (lb,lf) in csb; 
						end else e;)
		                 | _ -> e;)
	| BinOp (bp, e1, e2,ty) -> (let e3 = covertBpCall e1 in let e4 = covertBpCall e2 in let e = BinOp(bp, e3, e4,ty) in e;)
	| _->e;)

let save_to_calltable (fid:int)(i:instr) =
	(match i with
	| Call (lv,exp,exps,l)->(if l.line < fid then begin (match lv with 
				|Some x -> (let ls = List.map covertBpExp exps in 
					    Hashtbl.add calltable x exp;
					    Hashtbl.add calltables x ls;)
				|None -> ();) end else ();)
	| Set ((dest,offset), e, l) -> (if l.line < fid then begin (if (Hashtbl.mem calltable (dest,offset)) && (not (has_call e)) then begin Hashtbl.remove calltable (dest,offset);(); end else 
		(match e with 
		| BinOp (bp,e1,e2,ty) ->if ((has_call e1)||(has_call e2)) then begin Hashtbl.replace calltable (dest,offset) e; end else ();
		| UnOp (unop,exp,ty) -> if (has_call exp) then begin Hashtbl.replace calltable (dest,offset) e; end else ();
		| CastE(ty,exp)-> if (has_call exp) then begin Hashtbl.replace calltable (dest,offset) exp; end else ();
		| Lval (vr) -> if Hashtbl.mem calltable vr then begin Hashtbl.replace calltable (dest,offset) e; end else ();
		| _ -> ();)) end else ();)
	|_->();)
let move_out_calltable_i (fid:int)(i: Cil.instr) = 
  (match i with 
  | Set ((dest,offset), e, l) ->(if l.line < fid then begin (if Hashtbl.mem calltable (dest,offset) then begin Hashtbl.remove calltable (dest,offset);Hashtbl.remove calltables (dest,offset); end 					else () ) end else ();)
  | Call (lv,exp,exps,l)->(if l.line < fid then begin 
				(match lv with 
				|Some x -> (if Hashtbl.mem calltable x then begin Hashtbl.remove calltable x;Hashtbl.remove calltables x; end else ())
				|None -> ();) end else ();)
  | Asm (_, _, _, _, _, _)->();)

let rec move_out_calltable (fid:int)(s:Cil.stmt) =
	(match s.skind with 
		  | Switch(exp,b,stmts,l) -> (if l.line < fid then begin List.iter (move_out_calltable fid) b.bstmts;List.iter (move_out_calltable fid) stmts; end else ();)
		  | If (exp,b1,b2,l) -> (if l.line < fid then begin List.iter (move_out_calltable fid) b1.bstmts;List.iter (move_out_calltable fid) b2.bstmts;end else ();)
		  | Loop (b,l,s1,s2) -> (if l.line < fid then begin List.iter (move_out_calltable fid) b.bstmts;
					 end else ();)
	          | Instr il-> List.iter (move_out_calltable_i fid) il;
	          | _ -> ();
	)
(*go though the program, save the call and its var into table*)
class callVisitor (file : Cil.file) 
                  (fid : int) 
                  = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->		
	          match s.skind with 
		  | Switch(exp,b,stmts,l) -> List.iter (move_out_calltable fid) b.bstmts;List.iter (move_out_calltable fid) stmts;s;
		  | If (exp,b1,b2,l) -> List.iter (move_out_calltable fid) b1.bstmts;List.iter (move_out_calltable fid) b2.bstmts;s;
		  | Loop (b,l,s1,s2) -> (List.iter (move_out_calltable fid) b.bstmts;s;)
	          | Instr il-> List.iter (save_to_calltable fid) il;s;
	          | _ -> s;
    ) 
end 
let print_exp  (fn:string) (exPrinter:Cil.defaultCilPrinterClass) (exp: Cil.exp)= 
	let fout = open_out_gen [Open_append;Open_text;Open_creat] 0b111100100 fn in 
    	Pretty.fprint fout 100 (Cil.printExp exPrinter () exp);
	Printf.fprintf fout "\t";
	close_out fout;;


let rec print_call (e:Cil.exp) = 
	let exPrinter = new Cil.defaultCilPrinterClass in 
	let filename = "exp"^".txt" in
	(match e with
	| BinOp (bp,e1,e2,ty) -> (if (has_call e1) then begin print_call e1; end else (););(if (has_call e2) then begin print_call e2; end else (););
	| Lval (vr) -> (if Hashtbl.mem calltable vr then begin 
				print_exp filename exPrinter e; 
				let exp = Hashtbl.find calltable vr in 
				(if (has_call exp) then begin print_exp filename exPrinter exp;print_call exp; end 
					else (let exps = Hashtbl.find calltables vr in List.iter (print_exp filename exPrinter) (List.append [exp] exps);))
				end else ();)
	| UnOp (unop,exp,ty) -> if (has_call exp) then begin print_call exp; end else ();
	| _ ->();)



let print_Instr (fid:int)(i: instr) = 
  (match i with 
  | Set (dest, e, l) -> (if l.line = fid then begin 
		(match e with 
		| BinOp (bp,e1,e2,ty) ->(let cvtExp = BinOp (bp, covertBpExp e1, covertBpExp e2,ty) in
		                      let exPrinter = new Cil.defaultCilPrinterClass in 
				      let fname = "a" ^ ".txt" in
				      let fout = open_out_gen [Open_append;Open_text;Open_creat] 0b111100100 fname in 
    				      Pretty.fprint fout 100 (Cil.printExp exPrinter () cvtExp);
    				      close_out fout ;);if(has_call e) then begin print_call e; end else ();
		| CastE(ty,e) ->(if l.line = fid then begin 
				(match e with 
				| BinOp (bp,e1,e2,ty) ->(let cvtExp = BinOp (bp, covertBpExp e1, covertBpExp e2,ty) in
		                      let exPrinter = new Cil.defaultCilPrinterClass in 
				      let fname = "a" ^ ".txt" in
				      let fout = open_out_gen [Open_append;Open_text;Open_creat] 0b111100100 fname in 
    				      Pretty.fprint fout 100 (Cil.printExp exPrinter () cvtExp);
    				      close_out fout ;);if(has_call e) then begin print_call e; end else ();
				| _ -> ();) end else ();)
		| _ -> ();) end else ();)
  | _ -> ();)



class fidVisitor (file : Cil.file) 
                  (fid : int) 
                  = object
  (* If the sid equal to the fid, we output the statement*) 
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s -> 	
	match s.skind with 
	| Instr il-> List.iter (print_Instr fid) il ;s;
	| _ -> s;
    ) 
end 

let f_exp = new fidVisitor
let s_cos = new cosVisitor
let c_call = new callVisitor
let main () = begin
  let usageMsg = "Prototype ast coverter\n" in 
  let do_cfg = ref false in 
  let do_empty = ref false in 
  let do_every = ref false in 
  let do_uniq = ref false in 
  let get_id = ref 0 in
  let filenames = ref [] in 

  let argDescr = [
    "--calls", Arg.Set do_cfg, " convert calls to end basic blocks";
    "--empty", Arg.Set do_empty, " allow changes to empty blocks";
    "--uniq", Arg.Set do_uniq, " print each visited stmt only once";
    "--every-instr", Arg.Set do_every, " allow changes between every statement";
    "--id", Arg.Set_int get_id, "set the id"
  ] in 
  let handleArg str = filenames := str :: !filenames in 
  Arg.parse (Arg.align argDescr) handleArg usageMsg ; 

  Cil.initCIL () ; 
  List.iter (fun arg -> 
    begin
      (* 1. Convert C program to ast.*)
      let file = Frontc.parse arg () in 
      let id = !get_id in
      Cfg.computeFileCFG file;
      (*cross the file, construct the table*)
      visitCilFileSameGlobals (s_cos file id) file;
      visitCilFileSameGlobals (c_call file id) file;  
      visitCilFileSameGlobals (f_exp file id) file;
      let clPrinter = new Cil.plainCilPrinterClass in 
      let fname = "b" ^ ".txt" in
      let fout = open_out fname in 
      Cil.dumpFile clPrinter fout "fileprinter" file  ;
    		close_out fout ;		
      let ast = arg ^ ".ast" in 
      let fout = open_out_bin ast in 
      Marshal.to_channel fout (file) [] ;
      close_out fout ; 
    end 
  ) !filenames ; 

end ;;

main () ;;
