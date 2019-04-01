{-# LANGUAGE OverloadedStrings #-}

module Machine.Semantics.Symbolic.TraceRender where

import qualified Text.Pretty.Simple as PPrint
import Data.Text.Lazy (unpack)
import Machine.Semantics.Symbolic.Types
import qualified Algebra.Graph as G
import qualified Algebra.Graph.Export.Dot as G
import Machine.Instruction.Decode

renderDagrejs :: GTrace -> (String, String)
renderDagrejs trace =
    let vs = G.vertexList trace
        es = G.edgeList trace
    in (concatMap renderVertex vs, concatMap renderEdge es)

renderVertex :: SymState -> String
renderVertex v =
    let label = mkVertexLabel v
        description = renderSymState v
    in show label <> ": { \n" <>
       "    description: `<pre>" <> description <> "</pre>`\n" <>
       "},"

renderEdge :: (SymState, SymState) -> String
renderEdge (s, t) = "g.setEdge(\"" <> mkVertexLabel s <> "\", \"" <>
                                      mkVertexLabel t <> "\" ,{ });\n"

mkVertexLabel :: SymState -> String
mkVertexLabel x =
    let ic = instructionCounter x
    in show (ic, decode . snd $ program x !! (fromIntegral ic))