﻿namespace Model

open Aardvark.Base.Incremental
open Aardvark.UI.Primitives

[<DomainType>]
type Model = 
    {   
        cameraState : CameraControllerState
    }