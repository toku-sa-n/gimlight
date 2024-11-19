(*
Copyright (c) 2011, Jeremie Dimino <jeremie@dimino.org>
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Jeremie Dimino nor the names of his
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR AND CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
*)

open Lwt

let main () =
  let waiter, wakener = wait () in

  let game_model_repository = Repository.Game_model.make in
  let initialize_game_model_usecase =
    Usecase.Initialize_game_model.make game_model_repository
  in
  let press_first_button_usecase =
    Usecase.Press_first_button.make game_model_repository
  in

  let format_label_message n =
    Printf.sprintf "You pressed the Enter key %d times." n
  in

  let label = new LTerm_widget.label (format_label_message 0) in

  let () = initialize_game_model_usecase.execute () in

  let event_handler = function
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Enter; _ } ->
        let new_count = press_first_button_usecase.execute () in
        label#set_text (format_label_message (Z.to_int new_count));
        false
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Escape; _ } ->
        wakeup wakener ();
        true
    | _ -> false
  in

  let vbox = new LTerm_widget.vbox in
  vbox#add label;
  vbox#on_event event_handler;

  Lazy.force LTerm.stdout >>= fun term -> LTerm_widget.run term vbox waiter

let () = Lwt_main.run (main ())
