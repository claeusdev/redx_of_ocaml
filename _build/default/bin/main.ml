type state = {
  count: int
}

type action = 
  | Increment
  | Decrement
;;

let reducer (current_state: state) (action: action): state =
  match action with
  | Increment -> { count = current_state.count + 1}
  | Decrement -> { count = current_state.count - 1 }
;;

module UI = struct
  let makeReducer reducer initial_value = 
    let state_ref = ref initial_value in
    let dispatch action = 
      let new_state = reducer !state_ref action 
    in 
    state_ref := new_state;
    ()
    in
    let useReducer () =
      (!state_ref, dispatch)
    in 
    useReducer
end
  ;;

let useCounterReducer = UI.makeReducer reducer { count = 0 };;

let () = 
  print_endline "---initial render---";

  let (current_state, dispatch) = useCounterReducer () in
  Printf.printf "Current state: %d\n" current_state.count;

    dispatch Increment;

    print_endline "---rerender--";

    let (current_state, dispatch) = useCounterReducer () in 
    Printf.printf "Current count is: %d\n" current_state.count;
    dispatch Decrement;
    dispatch Decrement;
;;





