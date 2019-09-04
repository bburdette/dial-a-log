module DialALog exposing (DMsg(..), Dialog(..), DialogFn, dcMap, dmMap, dmsgMap, duMap, render)

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
dmsgMap : DMsg a -> (a -> b) -> DMsg b
dmsgMap da fab =
    case da of
        Msg ma ->
            Msg (fab ma)

        Render ->
            Render


{-| map for just the incoming message.
-}
dmMap : (bm -> am) -> (am -> bm) -> Dialog am ac -> Dialog bm ac
dmMap inmap outmap dlg =
    duMap inmap outmap identity dlg


{-| map for the returned command.
-}
dcMap : (ac -> bc) -> Dialog am ac -> Dialog am bc
dcMap resmap dlg =
    case dlg of
        Dialog d ->
            Dialog <|
                \m ->
                    let
                        ( ndlg, cmd ) =
                            d m
                    in
                    ( dcMap resmap ndlg, resmap cmd )

        Rendering r ->
            Rendering r


{-| map to adapt a dialog to a new setting.
inmap : function from the DialogFn msg to the new message type.
outmap : maps from the new message type to the DialogFn msg.
resmap : maps the DialogFn cmd to the new cmd type.
-}
duMap : (bm -> am) -> (am -> bm) -> (ac -> bc) -> Dialog am ac -> Dialog bm bc
duMap inmap outmap resmap dlg =
    case dlg of
        Dialog dupA ->
            Dialog
                (\dmsgB ->
                    let
                        ( newDupA, rmsg ) =
                            dupA (dmsgMap dmsgB inmap)
                    in
                    ( duMap inmap outmap resmap newDupA, resmap rmsg )
                )

        Rendering elt ->
            Rendering (Element.map outmap elt)
