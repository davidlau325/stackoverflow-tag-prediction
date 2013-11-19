open Core.Std

type read_status = Tag | TagEnd | Content | ContentEnd | Title | TitleEnd

exception IncorrectFileFormat
exception EmptyWordFileError

let total_tag = ref 0
let total_word = ref 0

let prepare_dir dir_list =
	List.iter ~f:(fun name -> 
		if Sys.is_directory_exn name then ()
		else Unix.mkdir name 
	) dir_list

let get_occurrence full_path is_tag=
	In_channel.with_file full_path
	~f:(fun fd ->
		match In_channel.input_line fd with
		| None -> raise EmptyWordFileError
		| Some line -> let name = String.split ~on:' ' line in
						if is_tag then List.nth_exn name 1
					 	else List.nth_exn name 2)

let process_tag line _ =
	let tag_list2 = String.split ~on:' ' line in
		List.iter tag_list2 ~f:(fun tag ->
			if String.is_empty tag then ()
			else begin
				incr total_tag;
				let full_path = ("tags/" ^ tag ^ ".txt") in
				if Sys.is_file_exn full_path
				then begin
					let num = string_of_int ((int_of_string (get_occurrence full_path true)) + 1) in
					Out_channel.with_file full_path ~append:false
					~f:(fun fd -> Out_channel.output_string fd (tag ^ " " ^ num))
				end
				else begin
					Out_channel.with_file full_path
					~f:(fun fd -> Out_channel.output_string fd (tag ^ " 1"))
				end
			end 
		);
		tag_list2

let process_word line tag_list =
	let word_list = String.split ~on:' ' line in
		List.iter word_list ~f:(fun word ->
			if String.is_empty word then ()
			else begin
				incr total_word;
				prepare_dir [("words/" ^ word)];
				List.iter tag_list ~f:(fun tag ->
					let full_path = ("words/" ^ word ^ "/" ^ tag ^ ".txt") in
					if Sys.is_file_exn full_path
					then begin
						let num = string_of_int ((int_of_string (get_occurrence full_path false))+1) in
						Out_channel.with_file full_path ~append:false
						~f:(fun fd -> Out_channel.output_string fd (word ^ " " ^ tag ^ " " ^ num))
					end
					else begin
						Out_channel.with_file full_path
						~f:(fun fd -> Out_channel.output_string fd (word ^ " " ^ tag ^ " 1"))
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

let count_base_level () =
	let tag_list = Sys.ls_dir "tags" in
		List.iter tag_list ~f:(fun tag -> 
			if String.is_suffix tag ~suffix:".txt" then begin
			let num = get_occurrence ("tags/" ^ tag) true in
				let pro = (Float.of_int (int_of_string num)) /. (Float.of_int !total_tag) in
					let base_level = log (pro /. (1.0 -. pro)) in
				Out_channel.with_file ("tags/" ^ tag) ~append:true
				~f:(fun fd -> Out_channel.output_string fd (" " ^ (Float.to_string_hum ~decimals:3 ~strip_zero:true base_level)))
			end
			else ()
			)

let extract_file filename =
	In_channel.with_file filename 
		~f:(fun fd -> loop_file fd Tag []);
		count_base_level ();
		printf "Done! %d\n" !total_word

let () = 
	prepare_dir ["words";"tags"];
	Command.basic 
		~summary: "Extract word co-occurances and tags information from file"
		Command.Spec.(
			empty
			+> anon ("[filename]" %: string)
		)
		(fun filename () -> extract_file filename)
	|> Command.run ~version:"0.1" ~build_info:"ENGG5013"
