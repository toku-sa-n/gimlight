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

let loop map_repository select_button_handler =
  let waiter, weakner = wait () in

  let game_widget = new View.Terminal.game_widget in

  let event_handler = function
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Escape; _ } ->
        wakeup weakner ();
        true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Enter; _ } ->
        let output = select_button_handler map_repository in

        let model = { View.Model.is_wall = output } in

        game_widget#set_model model;

        true
    | _ -> false
  in

  game_widget#on_event event_handler;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm_widget.run term game_widget waiter

let run map_repository select_button_handler =
  Lwt_main.run (loop map_repository select_button_handler)
