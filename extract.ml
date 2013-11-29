open Core.Std

type read_status = Tag | TagEnd | Content | ContentEnd | Title | TitleEnd

exception IncorrectFileFormat
exception EmptyWordFileError

let total_tag = ref 0
let total_word = ref 0
let hMax = ref 0.0
let sumEj = ref 0.0

let prepare_dir dir_list =
	List.iter ~f:(fun name -> 
		if Sys.is_directory_exn name then ()
		else Unix.mkdir name 
	) dir_list

let get_occurrence full_path =
	In_channel.with_file full_path
	~f:(fun fd ->
		match In_channel.input_line fd with
		| None -> raise EmptyWordFileError
		| Some line -> let name = String.split ~on:' ' line in
						(List.nth_exn name 1, List.nth_exn name 2))

let write_string ~path ~s ~append =
	Out_channel.with_file path ~append
	~f:(fun fd -> Out_channel.output_string fd s)

let process_tag line _ =
	let tag_list2 = String.split ~on:' ' line in
		List.iter tag_list2 ~f:(fun tag ->
			if String.is_empty tag then ()
			else begin
				incr total_tag;
				let full_path = ("tags/" ^ tag ^ ".txt") in
				if Sys.is_file_exn full_path
				then begin
					let (occur,n) = get_occurrence full_path in
					let num = string_of_int ((int_of_string occur) + 1) in
						write_string ~path:full_path ~append:false ~s:(tag ^ " " ^ num ^ " " ^ n)
				end
				else begin
					write_string ~path:full_path ~append:true ~s:(tag ^ " 1 0")
				end
			end 
		);
		(List.filter ~f:(fun tag -> tag <> "") tag_list2)

let increase_col acc n tag full_path =
	write_string ~path:full_path ~append:false ~s:(tag ^ " " ^ acc ^ " " ^ (string_of_int ((int_of_string n)+1)))

let increase_row word add =
	let full_path = ("words/" ^ word ^ "/_count.txt") in
	let total =
	if Sys.is_file_exn full_path 
	then ((int_of_string (fst (get_occurrence full_path))) + add)
	else add
	in
	write_string ~path:full_path ~append:false ~s:(word ^ " " ^ (string_of_int total) ^ " 0")

let process_word line tag_list =
	let word_list = String.split ~on:' ' line in
		List.iter word_list ~f:(fun word ->
			if String.is_empty word then ()
			else begin
				prepare_dir [("words/" ^ word)];
				increase_row word (List.length tag_list);
				List.iter tag_list ~f:(fun tag ->
					let full_path = ("words/" ^ word ^ "/" ^ tag ^ ".txt") in
					let tag_path = ("tags/" ^ tag ^ ".txt") in
					let (acc,n) = get_occurrence tag_path in
					increase_col acc n tag tag_path; 
					incr total_word;
					if Sys.is_file_exn full_path
					then begin
						let (_,occur) = get_occurrence full_path in 
						let num = string_of_int ((int_of_string occur)+1) in
						write_string ~path:full_path ~append:false ~s:(word ^ " " ^ tag ^ " " ^ num)
					end
					else begin
						write_string ~path:full_path ~append:true ~s:(word ^ " " ^ tag ^ " 1")
					end 
				)
			end 
		);
		tag_list

let rec loop_file fd status tag_list =
	let process_line ~fd ~status ~f ~check ~tag_list =
	match In_channel.input_line fd with
	| None -> if check then raise IncorrectFileFormat else ()
	| Some line -> let tag_list2 = (f line tag_list) in (loop_file fd status tag_list2)
	in 
	match status with
	| Tag -> process_line ~fd ~status:TagEnd ~check:false ~tag_list 
			 ~f:(fun line tag_list-> process_tag line tag_list)
	| TagEnd -> process_line ~fd ~status:Content ~check:true ~tag_list 
			 ~f:(fun _ tag_list -> tag_list)
	| Content -> process_line ~fd ~status:ContentEnd ~check:true ~tag_list
			 ~f:(fun line tag_list -> process_word line tag_list)
	| ContentEnd -> process_line ~fd ~status:Title ~check:true ~tag_list
			 ~f:(fun _ tag_list -> tag_list)
	| Title -> process_line ~fd ~status:TitleEnd ~check:true ~tag_list
			 ~f:(fun line tag_list -> process_word line tag_list)
	| TitleEnd -> process_line ~fd ~status:Tag ~check:false ~tag_list
			 ~f:(fun _ tag_list -> tag_list)

let extract_file filename =
	In_channel.with_file filename 
		~f:(fun fd -> loop_file fd Tag []);
		printf "Extracted! %s word: %d tag: %d\n" filename !total_word !total_tag;
		Out_channel.flush stdout

(* Compute Activation *)
let count_base_level () =
	let tag_list = Sys.ls_dir "tags" in
		List.iter tag_list ~f:(fun tag -> 
			if String.is_suffix tag ~suffix:".txt" then begin
			let (num,_) = get_occurrence ("tags/" ^ tag) in
				let pro = (Float.of_string num) /. (Float.of_int !total_tag) in
					let base_level = log (pro /. (1.0 -. pro)) in
				write_string ~path:("tags/" ^ tag) ~append:true ~s:(" " ^ (Float.to_string base_level))
			end
			else ()
			)

let compute_strength_assoc () =
	let word_list = Sys.ls_dir "words" in
		List.iter word_list ~f:(fun word ->
			if Sys.is_directory_exn ("words/" ^ word) then begin
			let word_count_path = ("words/" ^ word ^ "/_count.txt") in
			let (nRow,_) = get_occurrence word_count_path in
			let tag_list = Sys.ls_dir ("words/" ^ word ) in
			let hJ = ref 0.0 in
			List.iter tag_list ~f:(fun tag ->
				if (String.is_suffix tag ~suffix:".txt") && (tag <> "_count.txt") then begin
					let tag_path = ("words/" ^ word ^ "/" ^ tag) in
					let (tag_name,nJI) = get_occurrence tag_path in
					let (_,nCol) = get_occurrence ("tags/" ^ tag_name ^ ".txt") in
					let sAssc = log (((Float.of_int !total_word) *. (Float.of_string nJI)) /. 
								((Float.of_string nRow) *. (Float.of_string nCol))) in
					let pIJ = ((Float.of_string nJI) /. (Float.of_string nRow)) in
					hJ := !hJ +. (pIJ *. ((log10 pIJ) /. (log10 2.0)));
					write_string ~path:tag_path ~append:true ~s:(" " ^ Float.to_string sAssc)
				end
				else ()
			);
			if !hMax < (-.(!hJ)) then hMax := (-.(!hJ));
			write_string ~path:word_count_path ~append:false ~s:(word ^ " " ^ nRow ^ " " ^ Float.to_string (-.(!hJ)))
			end
			else () 
		);
		printf "hMax: %f\n" !hMax

let compute_scaled_entropy () =
	let word_list = Sys.ls_dir "words" in
		List.iter word_list ~f:(fun word ->
			if Sys.is_directory_exn ("words/" ^ word) then begin
				let count_path = ("words/" ^ word ^ "/_count.txt") in
				let (_,hJ) = get_occurrence count_path in
				let eJ = 1.0 -. ((Float.of_string hJ) /. !hMax) in
				sumEj := !sumEj +. eJ;
				write_string ~path:count_path ~append:true ~s:(" " ^ Float.to_string eJ)
			end
			else ()
		);
		printf "sumEj: %f\n" !sumEj

let run_extract () =
	let file_list = Sys.ls_dir "crawl" in
		List.iter file_list ~f:(fun filename -> 
			if String.is_suffix filename ~suffix:".txt" then extract_file ("crawl/"^filename));
		count_base_level ();
		compute_strength_assoc ();
		compute_scaled_entropy ();
		write_string ~path:"stat.txt" ~append:false ~s:((Float.to_string !hMax) ^ " " ^ (Float.to_string !sumEj))

let () = 
	prepare_dir ["words";"tags"];
	Command.basic 
		~summary: "Training on the crawl dataset"
		Command.Spec.(
			empty
		)
		(fun () -> run_extract ())
	|> Command.run ~version:"0.1" ~build_info:"ENGG5013"
