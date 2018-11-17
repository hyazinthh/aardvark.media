namespace Aardvark.UI

open Aardvark.UI

[<AutoOpen>]
module SvgExtensions =
    module Svg =

        let inline g x = elemNS "g" Svg.svgNS x

        let inline marker x = elemNS "marker" Svg.svgNS x

        let inline foreignObject x = elemNS "foreignObject" Svg.svgNS x

    module Incremental =

        module Svg =

            let inline g x = Incremental.elemNS "g" Svg.svgNS x

