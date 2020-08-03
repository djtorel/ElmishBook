module App

open System
open Elmish
open Elmish.React
open Feliz
open Zanaptak.TypedCssClasses

type Bulma = CssClasses<"https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css", Naming.PascalCase>
type FA = CssClasses<"https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css", Naming.PascalCase>

type Todo =
    { Id : Guid
      Description : string
      Completed : bool }

type TodoBeingEdited =
    { Id: Guid
      Description: string }

type SelectedFilter =
    | ShowAll
    | ShowCompleted
    | ShowNotCompleted

type State =
    { TodoList : Todo list
      TodosBeingEdited : TodoBeingEdited list
      NewTodo : string
      SelectedFilter : SelectedFilter }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid
    | CancelEdit of Guid
    | ApplyEdit of Guid
    | StartEditingTodo of Guid
    | SetEditedDescription of Guid * string
    | SetSelectedFilter of SelectedFilter

let init() =
    { TodoList =
        [ { Id = Guid.NewGuid()
            Description = "Learn F#"
            Completed = false } ]
      TodosBeingEdited = [  ]
      NewTodo  = ""
      SelectedFilter = ShowAll }

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
            { Id = Guid.NewGuid()
              Description = state.NewTodo
              Completed = false }

        { state with
            NewTodo = ""
            TodoList = List.append state.TodoList [nextTodo] }

    | StartEditingTodo todoId ->
        let nextEditModel =
            state.TodoList
            |> List.tryFind (fun todo -> todo.Id = todoId)
            |> Option.map (fun todo -> { Id = todoId; Description = todo.Description })

        match nextEditModel with
        | Some todo -> { state with TodosBeingEdited = todo :: state.TodosBeingEdited }
        | _ -> state

    | CancelEdit todoId ->
        { state with TodosBeingEdited = state.TodosBeingEdited |> List.filter  (fun todo -> todo.Id <> todoId) }

    | ApplyEdit todoId ->
        state.TodosBeingEdited
        |> List.tryFind (fun todo -> todo.Id = todoId)
        |> function
            | None -> state
            | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
            | Some todoBeingEdited ->
                let nextTodoList =
                    state.TodoList
                    |> List.map (fun todo ->
                        if todo.Id = todoBeingEdited.Id
                        then { todo with Description = todoBeingEdited.Description }
                        else todo)

                { state with
                    TodoList = nextTodoList
                    TodosBeingEdited = state.TodosBeingEdited |> List.filter (fun todo -> todo.Id <> todoId) }

    | SetEditedDescription (todoId, newText) ->
        let todosBeingEdited =
            state.TodosBeingEdited
            |> List.map (fun todoBeingEdited ->
                if todoBeingEdited.Id = todoId
                then { todoBeingEdited with Description = newText }
                else todoBeingEdited)

        { state with TodosBeingEdited = todosBeingEdited }

    | SetSelectedFilter newFilter ->
        { state with SelectedFilter = newFilter; TodosBeingEdited = [  ] }

let newTag (tag: IReactProperty list -> Fable.React.ReactElement) (props: IReactProperty list) (children: Fable.React.ReactElement list) =
    tag (prop.children children :: props)

let div = newTag Html.div

let li = newTag Html.li

let button = newTag Html.button

let appTitle =
    Html.p [
        prop.classes [ Bulma.Title ]
        prop.text "Elmish To-Do List"
    ]

let inputField (state: State) (dispatch: Msg -> unit) =
    div [ prop.classes [ Bulma.Field; Bulma.HasAddons ] ] [
        div [ prop.classes [ Bulma.Control; Bulma.IsExpanded ] ] [
            Html.input [
                prop.classes [ Bulma.Input; Bulma.IsMedium ]
                prop.valueOrDefault state.NewTodo
                prop.onChange (SetNewTodo >> dispatch)
            ]
        ]

        div [ prop.classes [ Bulma.Control ] ] [
            button [ prop.classes [ Bulma.Button; Bulma.IsPrimary; Bulma.IsMedium  ]
                     prop.onClick (fun _ -> dispatch AddNewTodo) ] [
                Html.i [prop.classes [ FA.Fa; FA.FaPlus ] ]
            ]
        ]
    ]

let renderEditForm (todoBeingEdited: TodoBeingEdited) (todo: Todo) (dispatch: Msg -> unit) =
    li [ prop.classes [ Bulma.Box ] ] [
        div [ prop.classes [ Bulma.Field; Bulma.IsGrouped ] ] [
            div [ prop.classes [Bulma.Control; Bulma.IsExpanded] ] [
                Html.input [
                    prop.classes [ Bulma.Input; Bulma.IsMedium ]
                    prop.valueOrDefault todoBeingEdited.Description
                    prop.onTextChange (fun text -> dispatch (SetEditedDescription (todoBeingEdited.Id, text)))
                ]
            ]

            div [ prop.classes [ Bulma.Control; Bulma.Buttons ] ] [
                button [ prop.classes [ Bulma.Button
                                        if todoBeingEdited.Description <> todo.Description then Bulma.IsPrimary
                                        if todoBeingEdited.Description = todo.Description then Bulma.IsOutlined ]
                         prop.onClick (fun _ -> dispatch (ApplyEdit todoBeingEdited.Id))
                         prop.disabled (todoBeingEdited.Description = todo.Description) ] [
                    Html.i [ prop.classes [ FA.Fa; FA.FaSave ] ]
                ]

                button [ prop.classes [ Bulma.Button; Bulma.IsWarning ]
                         prop.onClick (fun _ -> dispatch (CancelEdit todoBeingEdited.Id)) ] [
                    Html.i [ prop.classes [ FA.Fa; FA.FaArrowRight ] ]
                ]
            ]
        ]
    ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
    li [ prop.classes [ Bulma.Box ] ] [
        div [ prop.classes [ Bulma.Columns; Bulma.IsMobile; Bulma.IsVcentered ] ] [
            div [prop.classes [ Bulma.Column ] ] [
                Html.p [
                    prop.classes [ Bulma.Subtitle ]
                    prop.text todo.Description
                ]
            ]

            div [prop.classes [ Bulma.Column; Bulma.IsNarrow ] ] [
                div [prop.classes [ Bulma.Buttons ] ] [
                    button [ prop.classes [ Bulma.Button; if todo.Completed then Bulma.IsSuccess ]
                             prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id)) ] [
                        Html.i [ prop.classes [ FA.Fa; FA.FaCheck ] ]
                    ]
                    button [ prop.classes [ Bulma.Button; Bulma.IsPrimary ]
                             prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id)) ] [
                        Html.i [ prop.classes [ FA.Fa; FA.FaEdit ] ]
                    ]
                    button [ prop.classes [ Bulma.Button; Bulma.IsDanger ]
                             prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id)) ] [
                        Html.i [ prop.classes [ FA.Fa; FA.FaTimes ] ]
                    ]
                ]
            ]
        ]
    ]

let todoList (state: State) (dispatch: Msg -> unit) =
    let filterSelection (todoList: Todo list) =
        todoList
        |> List.filter (fun todo ->
                match state.SelectedFilter with
                | ShowAll -> true
                | ShowCompleted -> todo.Completed
                | ShowNotCompleted -> not todo.Completed)

    Html.ul [
        prop.children [
            for todo in state.TodoList |> filterSelection  ->
                state.TodosBeingEdited
                |> List.tryFind (fun t -> t.Id = todo.Id)
                |> function
                    | Some todoBeingEdited when todoBeingEdited.Id = todo.Id ->
                        renderEditForm todoBeingEdited todo dispatch
                    | _ ->
                        renderTodo todo dispatch
        ]
    ]

let renderFilterTabs (state: State) (dispatch: Msg -> unit) =
    div [ prop.classes [ Bulma.Tabs; Bulma.IsToggle; Bulma.IsFullwidth ] ] [
        Html.ul [
            li [ prop.classes [ if state.SelectedFilter = ShowAll then Bulma.IsActive ] ] [
                Html.a [
                    prop.text "All"
                    prop.onClick (fun _ -> dispatch (SetSelectedFilter ShowAll))
                ]
            ]
            li [ prop.classes [ if state.SelectedFilter = ShowCompleted then Bulma.IsActive ] ] [
                Html.a [
                    prop.text "Completed"
                    prop.onClick (fun _ -> dispatch (SetSelectedFilter ShowCompleted))
                ]
            ]
            li [ prop.classes [ if state.SelectedFilter = ShowNotCompleted then Bulma.IsActive ] ] [
                Html.a [
                    prop.text "Not Completed"
                    prop.onClick (fun _ -> dispatch (SetSelectedFilter ShowNotCompleted))
                ]
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
  div [ prop.style [ style.padding 20 ] ] [
      appTitle
      inputField state dispatch
      renderFilterTabs state dispatch
      todoList state dispatch
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
