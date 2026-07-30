import Std.Http.Server
import Postgres
import Todo

open Std Async
open Std Http Server

def main : IO Unit := Async.block do
  let db ← Postgres.open ""
  Todo.initSchema db
  let addr := .v4 ⟨.ofParts 127 0 0 1, 0⟩
  let server ← serve addr (Todo.app db)
  IO.println s!"Listening on http://{server.localAddr.get!}"
  server.waitShutdown
