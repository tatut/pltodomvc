:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- dynamic todo/3.
:- prolog_listen(todo/3, updated(todo/3)).

max_todo_id(MaxId) :-
    findall(Id, todo(Id,_,_), Lst),
    max_list(Lst, MaxId).

add_todo(Name) :-
    transaction(( (max_todo_id(Id) -> MaxId=Id; MaxId=0),
                  NewId is MaxId + 1,
                  assertz(todo(NewId, Name, false)) )), !.

/* Called from JS to add a new todo */
add_todo :-
    Input := document.querySelector(".new-todo"),
    Todo := Input.value,
    Input.value := "",
    add_todo(Todo).

clear_todos() :-
    transaction(( retractall(todo(_,_,_)) )).

mark_complete(Id) :-
    transaction(( todo(Id, Name, _),
                  retractall(todo(Id,Name,_)),
                  assertz(todo(Id, Name, true)) )).

toggle(true, false).
toggle(false, true).

toggle(Id) :-
    transaction(( todo(Id, Name, Complete),
                  retractall(todo(Id,Name,Complete)),
                  toggle(Complete, NewComplete),
                  assertz(todo(Id, Name, NewComplete)) )).

destroy(Id) :- transaction(( retractall(todo(Id,_,_)) )).

todolist(Sorted) :-
    findall(todo(Id,Name,Complete),
            todo(Id,Name,Complete),
            Lst),
    sort(1, @<, Lst, Sorted).

some_todos() :-
    add_todo("Learn Prolog"),
    add_todo("Integrate with morphdom"),
    add_todo("Show it to the world").

html_to_string(Html, String) :-
    with_output_to(
        string(String),
        ( phrase(Html, TokenizedHtml, []),
          print_html(TokenizedHtml) )).

/* Call JS morphdom to match our UI */
morphdom(ElementId, Html) :-
    html_to_string(Html, HtmlString),
    Elt := document.getElementById(ElementId),
    _ := morphdom(ElementId, HtmlString).

onclick(Term, onclick=C) :-
    format(atom(C), 'P.call("~w.")', [Term]).

todo_class(true, class=completed).
todo_class(false, class='').
checked(true, checked).
checked(false, '').

todo_item(todo(Id, Name, Complete)) -->
    { onclick(toggle(Id), Toggle),
      onclick(destroy(Id), Destroy),
      todo_class(Complete, Cls),
      checked(Complete, Checked) },
    html([li([Cls, Toggle],
             [div(class='view',
                  [input([class=toggle, type='checkbox', Checked]),
                   label(Name),
                   button([class=destroy,Destroy],[])])])]).

todo_items -->
    { todolist(Items) },
    html(ul(class='todo-list', \sequence(todo_item, Items))).

render :-
    morphdom('app', html(section([id='app',class='main'], \todo_items))).

updated(Pred, Action, Context) :-
    writeln(updated(pred(Pred), action(Action), context(Context))),
    _ := render().
