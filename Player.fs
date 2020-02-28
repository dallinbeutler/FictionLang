namespace FS

open Godot

type Player() as this=
   inherit Node()

   override this._Ready() = 
       GD.Print("Hello from F#!")