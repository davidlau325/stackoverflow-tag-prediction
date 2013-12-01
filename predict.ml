open Core.Std

type read_status = Tag | TagEnd | Content | ContentEnd | Title | TitleEnd
type tag_activate = {tag_name:string;act:float}

exception IncorrectFileFormat
exception EmptyWordFileError

let sumEj = ref 0.0
let countTest = ref 0

let get_data tag_path first second =
	In_channel.with_file tag_path
	~f:(fun fd ->
		match In_channel.input_line fd with
		| None -> raise EmptyWordFileError
		| Some line -> let name = String.split ~on:' ' line in
						(List.nth_exn name first, List.nth_exn name second))

let get_original_tag line word_list =
	let tag_list = String.split ~on:' ' line in
		printf "%d - " !countTest;
		List.iter (List.filter ~f:(fun tag -> tag <> "") tag_list)
			~f:(fun tags -> printf "%s " tags);
			printf "\n";
			word_list

let get_word line word_list =
	let words = String.split ~on:' ' line in
		List.append word_list (List.filter ~f:(fun word -> word <> "") words)

let compute_tag tag word_list =
	let (tagName,baseLevel) = get_data ("tags/"^tag) 0 3 in
		let activate = (Float.of_string baseLevel) +.
			(List.fold word_list ~init:0.0 
			~f:(fun acc word ->
				let word_path = ("words/"^word^"/"^tagName^".txt") in
				if Sys.is_file_exn word_path then begin
				let (_,eJ) = get_data ("words/"^word^"/_count.txt") 0 3 in
				let (_,sAssoc) = get_data ("words/"^word^"/"^tagName^".txt") 0 3 in
				acc +. ((Float.of_string sAssoc) *. ((Float.of_string eJ)))
				end
				else acc 
			))
		in
		{tag_name=tagName;act=activate}	


let rec accumulate_tag tag_list word_list acc =
	match tag_list with
	| [] -> acc
	| x::xs -> if String.is_suffix x ~suffix:".txt" then begin
				let result = compute_tag x word_list in
				accumulate_tag xs word_list (result::acc)
			   end
			   else accumulate_tag xs word_list acc

let print_activate activate_list =
	Out_channel.with_file ("test-"^(string_of_int !countTest))
	~f:(fun fd ->
		List.iter activate_list
		~f:(fun record ->
			Out_channel.output_string fd (record.tag_name ^ " " ^ (Float.to_string record.act) ^ "\n")
		)
	);
	incr countTest

let compute_activation word_list =
	let tag_list = Sys.ls_dir "tags" in
	 	let activate_list = accumulate_tag tag_list word_list [] in
	 		let sorted_list = List.sort ~cmp:(fun r1 r2 -> Float.compare r2.act r1.act) activate_list in
			print_activate sorted_list;
			[]

let rec loop_file fd status word_list =
	let process_line ~fd ~status ~f ~check ~word_list =
	match In_channel.input_line fd with
	| None -> if check then raise IncorrectFileFormat else let _ =(f "dummy" word_list) in ()
	| Some line -> let words = (f line word_list) in loop_file fd status words
	in 
	match status with
	| Tag -> process_line ~fd ~status:TagEnd ~check:false ~word_list 
			 ~f:(fun line word_list -> get_original_tag line word_list)
	| TagEnd -> process_line ~fd ~status:Content ~check:true ~word_list 
			 ~f:(fun _ word_list -> word_list)
	| Content -> process_line ~fd ~status:ContentEnd ~check:true ~word_list
			 ~f:(fun line word_list -> get_word line word_list)
	| ContentEnd -> process_line ~fd ~status:Title ~check:true ~word_list
			 ~f:(fun _ word_list -> word_list)
	| Title -> process_line ~fd ~status:TitleEnd ~check:true ~word_list
			 ~f:(fun line word_list -> get_word line word_list)
	| TitleEnd -> process_line ~fd ~status:Tag ~check:false ~word_list
			 ~f:(fun _ word_list -> compute_activation word_list)

let test_file filename =
	In_channel.with_file filename
		~f:(fun fd -> (loop_file fd Tag []))

let run_test () =
	let (_,sum) = get_data "stat.txt" 0 1 in
	let file_list = Sys.ls_dir "test" in
	sumEj := (Float.of_string sum);
		List.iter file_list ~f:(fun filename ->
			if String.is_suffix filename ~suffix:".txt" then test_file ("test/"^filename)
		)


let () =
	Command.basic
		~summary: "Testing the test data set"
		Command.Spec.(
			empty
		)
		(fun () -> run_test ())
	|> Command.run ~version:"0.1" ~build_info:"ENGG5013"