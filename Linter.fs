namespace FS

open Godot




type Linter() as this=
   inherit Node()

   

   override this._Ready() = 
       GD.Print("Hello from F#!")

    