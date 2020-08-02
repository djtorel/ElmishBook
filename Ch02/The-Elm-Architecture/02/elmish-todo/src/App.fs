module App

open Elmish
open Elmish.React
open Feliz


type DivProps =
    | DivClasses of string list
    | DivStyles of IStyleAttribute list

type Todo =
    { Id : int
      Description : string
      Completed : bool }

type TodoBeingEdited =
    { Id: int
      Description: string }

type State =
    { TodoList : Todo list
      TodoBeingEdited : TodoBeingEdited option
      NewTodo : string
      CurrentId : int }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of int
    | DeleteTodo of int
    | CancelEdit
    | ApplyEdit
    | StartEditingTodo of int
    | SetEditedDescription of string

let init() =
    { TodoList =
        [ { Id = 0
            Description = "Learn F#"
            Completed = false } ]
      TodoBeingEdited = None
      NewTodo  = ""
      CurrentId = 1 }

let update (msg: Msg) (state: State): State =
    match msg with
    | SetNewTodo todoText -> { state with NewTodo = todoText }
    | DeleteTodo todoId ->
        let nextTodoList =
            state.TodoList
            |> List.filter (fun todo -> todo.Id <> todoId)

        { state with TodoList = nextTodoList }
    | ToggleCompleted todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map (fun todo ->
                if todo.Id = todoId
                then { todo with Completed = not todo.Completed }
                else todo)

        { state with TodoList = nextTodoList }
    | AddNewTodo when state.NewTodo = "" -> state
    | AddNewTodo ->
        let nextTodo =
            { Id = state.CurrentId
              Description = state.NewTodo
              Completed = false }

        { state with
            NewTodo = ""
            CurrentId = state.CurrentId + 1
            TodoList = List.append state.TodoList [nextTodo] }
    | StartEditingTodo todoId ->
        let nextEditModel =
            state.TodoList
            |> List.tryFind (fun todo -> todo.Id = todoId)
            |> Option.map (fun todo -> { Id = todoId; Description = todo.Description })

        { state with TodoBeingEdited = nextEditModel }
    | CancelEdit ->
        { state with TodoBeingEdited = None }
    | ApplyEdit ->
        match state.TodoBeingEdited with
        | None -> state
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
        | Some todoBeingEdited ->
            let nextTodoList =
                state.TodoList
                |> List.map (fun todo ->
                    if todo.Id = todoBeingEdited.Id
                    then { todo with Description = todoBeingEdited.Description }
                    else todo)

            { state with TodoList = nextTodoList; TodoBeingEdited = None }
    | SetEditedDescription newText ->
        let nextEditModel =
            state.TodoBeingEdited
            |> Option.map (fun todoBeingEdited -> { todoBeingEdited with Description = newText })

        { state with TodoBeingEdited = nextEditModel }

let div (props: DivProps list) (children: Fable.React.ReactElement list) =
    Html.div [
        for divProp in props ->
            match divProp with
            | DivClasses classes -> prop.classes classes
            | DivStyles styles -> prop.style styles

        yield prop.children children
    ]

let appTitle =
    Html.p [
        prop.classes [ "title" ]
        prop.text "Elmish To-Do List"
    ]

let inputField (state: State) (dispatch: Msg -> unit) =
    div [ DivClasses [ "field"; "has-addons" ] ] [
        div [ DivClasses [ "control"; "is-expanded" ] ] [
            Html.input [
                prop.classes [ "input"; "is-medium" ]
                prop.valueOrDefault state.NewTodo
                prop.onChange (SetNewTodo >> dispatch)
            ]
        ]

        div [ DivClasses [ "control" ] ] [
            Html.button [
                prop.classes [ "button"; "is-primary"; "is-medium" ]
                prop.onClick (fun _ -> dispatch AddNewTodo)
                prop.children [
                    Html.i [prop.classes [ "fa"; "fa-plus" ] ]
                ]
            ]
        ]
    ]

let renderEditForm (todoBeingEdited: TodoBeingEdited) (dispatch: Msg -> unit) =
    div [ DivClasses [ "box" ] ] [
        div [ DivClasses [ "field"; "is-grouped" ] ] [
            div [ DivClasses ["control"; "is-expanded"] ] [
                Html.input [
                    prop.classes [ "input"; "is-medium" ]
                    prop.valueOrDefault todoBeingEdited.Description
                    prop.onTextChange (SetEditedDescription >> dispatch)
                ]
            ]

            div [ DivClasses [ "control"; "buttons" ] ] [
                Html.button [
                    prop.classes [ "button"; "is-primary" ]
                    prop.onClick (fun _ -> dispatch ApplyEdit)
                    prop.children [
                        Html.i [ prop.classes [ "fa"; "fa-save" ] ]
                    ]
                ]

                Html.button [
                    prop.classes [ "button"; "is-warning" ]
                    prop.onClick (fun _ -> dispatch CancelEdit)
                    prop.children [
                        Html.i [ prop.classes [ "fa"; "fa-arrow-right" ] ]
                    ]
                ]
            ]
        ]
    ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
    div [ DivClasses [ "box" ] ] [
        div [ DivClasses [ "columns"; "is-mobile"; "is-vcentered" ] ] [
            div [DivClasses [ "column" ] ] [
                Html.p [
                    prop.classes [ "subtitle" ]
                    prop.text todo.Description
                ]
            ]

            div [DivClasses [ "column"; "is-narrow" ] ] [
                div [DivClasses [ "buttons" ] ] [
                    Html.button  [
                        prop.classes [ "button"; if todo.Completed then "is-success" ]
                        prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
                        prop.children [
                            Html.i [ prop.classes [ "fa"; "fa-check" ] ]
                        ]
                    ]

                    Html.button [
                        prop.classes [ "button"; "is-primary" ]
                        prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
                        prop.children [
                            Html.i [ prop.classes [ "fa"; "fa-edit" ] ]
                        ]
                    ]

                    Html.button [
                        prop.classes [ "button"; "is-danger" ]
                        prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                        prop.children [
                            Html.i [ prop.classes [ "fa"; "fa-times" ] ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let todoList (state: State) (dispatch: Msg -> unit) =
    Html.ul [
        prop.children [
            for todo in state.TodoList ->
                match state.TodoBeingEdited with
                | Some todoBeingEdited when todoBeingEdited.Id = todo.Id ->
                    renderEditForm todoBeingEdited dispatch
                | _ ->
                    renderTodo todo dispatch
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
  div [DivStyles [ style.padding 20 ] ] [
      appTitle
      inputField state dispatch
      todoList state dispatch
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
