open Core.Std
open Async.Std
open Re2

let query_uri query =
  let base_uri = Uri.of_string "http://stackoverflow.com/questions?sort=votes" in
  Uri.add_query_param base_uri ("page", [query])

let is_question line =
    let question = Regex.create_exn "(.*)class=\"question-hyperlink\">(.*)" in
    Regex.matches question line

let is_url line =
    let url = Regex.create_exn "href=\"(.*?)\"" in
      let sub = Regex.find_submatches_exn url line in
        match sub.(1) with
        | None -> "No found"
        | Some text -> text

let filter_question html =
    let html_list = String.split_lines html in
      let filtered_list = List.filter ~f: is_question html_list in
        let final_list = List.map ~f: is_url filtered_list in 
              String.concat ~sep:"\n" final_list
	
let get_html page =
  Cohttp_async.Client.get (query_uri page)
  >>= fun (_, body) ->
  Pipe.to_list body
  >>| fun strings -> String.concat strings

let print_result html =
	let fd = Out_channel.create "page1" in
    let filtered = filter_question html in
	protect ~f: (fun () -> 
  		Out_channel.output_string fd filtered;
  		printf "Done!\n"
	)
	~finally: (fun () -> Out_channel.close fd; Shutdown.shutdown 0)
  
let crawl_and_print page =
	get_html page 
	>>| fun results -> print_result results

let () = ignore(crawl_and_print "1"); 
          never_returns(Scheduler.go ())

 (* Command.async_basic
    ~summary:"Retrieve html content from the page"
    Command.Spec.(
      empty
    )
    (fun () -> crawl_and_print "1")
  |> Command.run *)


