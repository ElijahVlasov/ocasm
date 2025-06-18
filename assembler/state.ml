open Base
open Base.Fn
module Buffer = Stdlib.Buffer

type section_state = { mutable location_counter : int; contents : Buffer.t }
[@@deriving fields]

type t = {
  mutable curr_section : Section.t;
  mutable curr_section_state : section_state;
  section_states : (Section.t, section_state) Hashtbl.t;
}
[@@deriving fields]

let section_state section_states =
  Hashtbl.find_or_add
    ~default:(fun _ -> { location_counter = 0; contents = Buffer.create 1024 })
    section_states

let section_contents s = compose contents (section_state s.section_states)
let curr_section_contents = compose contents curr_section_state

exception DuplicateSections of Section.t list

let create_section_states secs =
  match
    Hashtbl.create_mapped
      (module Section)
      ~get_key:id
      ~get_data:(fun _ ->
        { location_counter = 0; contents = Buffer.create 1024 })
      secs
  with
  | `Ok tbl -> tbl
  | `Duplicate_keys secs -> raise @@ DuplicateSections secs

let create secs =
  let initial_sec = Section.Text in
  let section_states = create_section_states secs in
  {
    curr_section = initial_sec;
    curr_section_state = section_state section_states initial_sec;
    section_states;
  }

let finalize s = Hashtbl.map ~f:contents s.section_states

let on_contents s ~(f : Buffer.t -> 'a -> 'b) : 'a -> 'b =
  f (curr_section_contents s)

let write_byte = on_contents ~f:Buffer.add_char
let write_int32 = on_contents ~f:Buffer.add_int32_le
let write_int64 = on_contents ~f:Buffer.add_int64_le

let switch_section s sec =
  s.curr_section <- sec;
  s.curr_section_state <- section_state s.section_states sec;
  ()

let loc_counter s = s.curr_section_state.location_counter

let inc_loc_counter s off =
  s.curr_section_state.location_counter <-
    s.curr_section_state.location_counter + off
