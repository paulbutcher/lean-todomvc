/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

import HtmlSyntax
import HtmlRenderer
import Todo.Db
import Todo.Links

namespace Todo

open Html
open Routes

/-- Puts the anti-forgery token on `<body>` as an `hx-headers` attribute, which HTMX inherits
down the whole tree, so every mutating request below carries it. `none` when no `antiForgery`
middleware established a token, in which case there's nothing to send. -/
def csrfAttrs : Option String → Attrs
  | none => []
  | some token => reqAttr "hx-headers" ("{\"X-CSRF-Token\": \"" ++ token ++ "\"}")

def Filter.path : Filter → String
  | .all => links.index
  | .active => links.active
  | .completed => links.completed

def filterFromPath (path : String) : Filter :=
  if path.endsWith "/active" then .active
  else if path.endsWith "/completed" then .completed
  else .all

def itemView (item : Item) : Node .listItem :=
  let itemId := s!"todo-{item.id}"
  let id := item.id.toInt.toNat
  html! <li id={itemId} {optAttr "class" (if item.completed then some "completed" else none)}>
          <div class="view">
            <input type="checkbox" checked={item.completed} class="toggle"
              hx-post={links.toggle id} hx-target="#todo-list-section" hx-swap="outerHTML"/>
            <label hx-get={links.edit id} hx-trigger="dblclick" hx-target={s!"#{itemId}"}
              hx-swap="outerHTML">{item.title}</label>
            <button class="destroy" hx-delete={links.todo id} hx-target="#todo-list-section"
              hx-swap="outerHTML"></button>
          </div>
        </li>

def itemEditView (item : Item) : Node .listItem :=
  html! <li id={s!"todo-{item.id}"} class="editing">
          <input type="text" name="title" value={item.title} class="edit"
            hx-put={links.todo item.id.toInt.toNat} hx-trigger="blur, keyup[key=='Enter']"
            hx-target="#todo-list-section" hx-swap="outerHTML" autofocus/>
        </li>

def listSection (items : Array Item) : Node .flow :=
  let allCompleted := items.size > 0 && items.all (·.completed)
  html! <section id="todo-list-section" class="main">
          <input type="checkbox" checked={allCompleted} id="toggle-all" class="toggle-all"
            hx-post={links.toggleAll} hx-target="#todo-list-section" hx-swap="outerHTML"/>
          <label for="toggle-all"></label>
          <ul class="todo-list">{items.toList.map itemView}</ul>
        </section>

def filterLink (current target : Filter) (label : String) : Node .listItem :=
  html! <li>
          <a href={target.path}
            {optAttr "class" (if current == target then some "selected" else none)}>{label}</a>
        </li>

def countLabel(activeCount : Nat) : String :=
  if activeCount == 1 then "1 item left" else s!"{activeCount} items left"

def footerFragment (allItems : Array Item) (filter : Filter) : Node .flow :=
  let activeCount := (allItems.filter (!·.completed)).size
  let completedCount := allItems.size - activeCount
  let clearCompleted : Option (Node .flow) :=
    if completedCount > 0 then
      some (html! <button class="clear-completed" hx-delete={links.clearCompleted}
                    hx-target="#todo-list-section" hx-swap="outerHTML">Clear completed</button>)
    else none
  html! <footer id="todo-footer" class="footer" hx-swap-oob="true">
          <span class="todo-count">{countLabel activeCount}</span>
          <ul class="filters">
            {filterLink filter .all "All"}
            {filterLink filter .active "Active"}
            {filterLink filter .completed "Completed"}
          </ul>
          {clearCompleted}
        </footer>

def mutationFragment (items allItems : Array Item) (filter : Filter) : String :=
  Node.render (listSection items) ++ Node.render (footerFragment allItems filter)

def pageView (csrfToken : Option String) (items allItems : Array Item) (filter : Filter) : String :=
  document
    (html! <head>
             <meta charset="utf-8"/>
             <script src="https://cdn.jsdelivr.net/npm/htmx.org@2.0.10/dist/htmx.min.js"
               integrity="sha384-H5SrcfygHmAuTDZphMHqBJLc3FhssKjG7w/CeCpFReSfwBWDTKpkzPP8c+cLsK+V"
               crossorigin="anonymous"></script>
             <link rel="stylesheet" href="https://unpkg.com/todomvc-app-css@2.4.3/index.css"/>
             <link rel="icon" href="/favicon.svg"/>
             <title>todos</title>
           </head>)
    (html! <body {csrfAttrs csrfToken}>
             <section class="todoapp">
               <header class="header">
                 <h1>todos</h1>
                 <form hx-post={links.todos} hx-target="#todo-list-section" hx-swap="outerHTML"
                   hx-on::after-request="this.reset()">
                   <input name="title" placeholder="What needs to be done?" class="new-todo"
                     autofocus/>
                 </form>
               </header>
               {listSection items}
               {footerFragment allItems filter}
             </section>
             <footer class="info">
               <p>Double-click a todo to edit it.</p>
             </footer>
           </body>)
    (lang := "en")

end Todo
