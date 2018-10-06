﻿namespace Aardvark.UI.Animation

open Aardvark.Base                 
open Aardvark.Base.Incremental 

type Time = float
type RelativeTime = Time

type Animation<'m,'s,'a> = {
    state     : Option<'s>
    name      : string
    startTime : Option<Time>
    start     : 'm -> 's
    sample    : RelativeTime * Time -> 's -> Option<'s * 'a>
}

type Animate = On = 0 | Off = 1

type TaskId = string

[<DomainType>]
type TaskProgress = { percentage : float; [<NonIncremental>] startTime : System.DateTime }

[<DomainType>]
type AnimationModel = {
   cam        : CameraView
   animation  : Animate
   animations : plist<Animation<AnimationModel,CameraView,CameraView>>
}

type AnimationAction = 
 | Tick of Time
 | PushAnimation of Animation<AnimationModel,CameraView,CameraView>
 | RemoveAnimation of Index   

module CameraAnimations =
  open Aardvark.Base.Trafo

  let initial name = 
    {
      startTime = None
      state = None
      name = name
      start = fun m -> m.cam // initially, grab camera controller state
      sample = fun (_,_) (state : CameraView) -> None
    }

  let zoom (dir : float) (name : string) = 
    {
      (initial name) with 
        sample = fun (localTime, globalTime) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
          if localTime < 1.0 then
              let view = state.WithLocation(state.Location + dir * state.Forward * (localTime / 1.0))
              Some (state,view)
          else None
    }

  let interpolate (destination : CameraView) (duration : RelativeTime) (name : string) =
    {
      (initial name) with
        sample = fun (localTime, _) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
          if localTime < duration then
            let t = localTime / duration

            let vec      = destination.Location - state.Location
            let location' = state.Location + vec * t

            let a = Rot3d.FromFrame (state.Right, state.Up, state.Backward)
            let b = Rot3d.FromFrame (destination.Right, destination.Up, destination.Backward)
            let orientation = (a, b, t) |> Ipol.SlerpShortest |> M44d.Rotation

            let forward' = -orientation.C2.XYZ
            let right' = orientation.C0.XYZ
            let up' = orientation.C1.XYZ

            let view = CameraView (state.Sky, location', forward', up', right')

            Some (state,view)
          else None
    }

  let animateLocation (destination : V3d) (duration : RelativeTime) (name : string) = 
    {
      (initial name) with 
        sample = fun (localTime, globalTime) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
          if localTime < duration then
            let vec      = destination - state.Location
            let velocity = vec.Length / duration                  
            let dir      = vec.Normalized

            let location' = state.Location + dir * velocity * localTime
            let view = state.WithLocation(location')

            Some (state,view)
          else None
    }

  let animateLocationFixedLookAt (destination : V3d) (lookAt : V3d) (duration : RelativeTime) (name : string) = 
    {
      (initial name) with 
        sample = fun (localTime, globalTime) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
          if localTime < duration then
            let vec      = destination - state.Location
            let velocity = vec.Length / duration                  
            let dir      = vec.Normalized                  

            let location' = state.Location + dir * velocity * localTime
            let forward = (lookAt - location').Normalized

            let view = 
              state 
                |> CameraView.withLocation(location') 
                |> CameraView.withForward forward

            Some (state,view)
          else None
    }

  let animateLookAt (src : V3d)(dst : V3d) (duration : RelativeTime) (name : string) = 
    {
      (initial name) with 
        sample = fun (localTime, globalTime) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
          if localTime < duration then
            let vec      = dst - src
            let velocity = vec.Length / duration
            let dir      = vec.Normalized

            let lookAt' = src + dir * velocity * localTime
            let forward = (lookAt' - state.Location).Normalized

            let view = state |> CameraView.withForward forward                  
                            
            Some (state,view)
          else None
    }

  let animateFoward (dst : V3d) (duration : RelativeTime) (name : string) = 
    {
      (initial name) with 
        sample = fun (localTime, globalTime) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
          if localTime < duration then                  
            let rot = Rot3d(state.Forward, dst) * localTime / duration
            let forward' = rot.TransformDir(state.Forward)                  
            let view = state |> CameraView.withForward forward'

            Some (state,view)
          else None
    }

  let animateSky (dst : V3d) (duration : RelativeTime) (name : string) = 
    {
      (initial name) with 
        sample = fun (localTime, globalTime) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
          if localTime < duration then                  
            let rot = Rot3d(state.Up, dst) * localTime / duration
            let sky' = rot.TransformDir(state.Up)                  
            let view = state |> CameraView.withUp sky'

            Some (state,view)
          else None
    }