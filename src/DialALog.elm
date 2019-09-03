module DialALog exposing (DMsg(..), Dialog(..), dcMap, dmMap, duMap, render)

import Element exposing (Element)


{-| dialogs are basically implementations of DialogFn.
-}
type alias DialogFn msg cmd =
    DMsg msg -> ( Dialog msg cmd, cmd )


{-| pass these messages to the ftn - correspongs to elm update or view.
-}
type DMsg msg
    = Msg msg
    | Render


{-| the return type for DialogFn - either a Dialog or a Rendering.
-}
type Dialog msg cmd
    = Dialog (DialogFn msg cmd)
    | Rendering (Element msg)


{-| call this on a DialogFn to maybe get it to render.
rename to view?
-}
render : Dialog msg cmd -> Maybe (Element msg)
render dlg =
    case dlg of
        Rendering elt ->
            Just elt

        Dialog dlg2 ->
            case dlg2 Render of
                ( Rendering elt, _ ) ->
                    Just elt

                _ ->
                    Nothing


{-| map ftn for DMsg.
-}
dmMap : DMsg a -> (a -> b) -> DMsg b
dmMap da fab =
    case da of
        Msg ma ->
            Msg (fab ma)

        Render ->
            Render


{-| map for the returned command.
-}
dcMap : Dialog am ac -> (ac -> bc) -> Dialog am bc
dcMap dlg resmap =
    case dlg of
        Dialog d ->
            Dialog <|
                \m ->
                    let
                        ( ndlg, cmd ) =
                            d m
                    in
                    ( dcMap ndlg resmap, resmap cmd )

        Rendering r ->
            Rendering r


{-| map to adapt a dialog to a new setting.
inmap : function from the DialogFn msg to the new message type.
outmap : maps from the new message type to the DialogFn msg.
resmap : maps the DialogFn cmd to the new cmd type.
-}
duMap : Dialog am ac -> (bm -> am) -> (am -> bm) -> (ac -> bc) -> Dialog bm bc
duMap dlg inmap outmap resmap =
    case dlg of
        Dialog dupA ->
            Dialog
                (\dmsgB ->
                    let
                        ( newDupA, rmsg ) =
                            dupA (dmMap dmsgB inmap)
                    in
                    ( duMap newDupA inmap outmap resmap, resmap rmsg )
                )

        Rendering elt ->
            Rendering (Element.map outmap elt)
