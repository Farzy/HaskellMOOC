module Main where
-- Author: Farzad FARID
-- Date: 2016/10/13
-- Haskell MOOC week 4

import XMLParser ( parseXML )

rec1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<list><list-elt><record name=\"MkPersonRecord\"><elt key=\"name\">\"Wim Vanderbauwhede\"</elt>\n\
    \<elt key=\"address\"><record name=\"MkAddress\"><elt key=\"line1\">\"School of Computing Science\"</elt>\n\
    \<elt key=\"number\">17</elt>\n\
    \<elt key=\"street\">\"Lilybank Gdns\"</elt>\n\
    \<elt key=\"town\">\"Glasgow\"</elt>\n\
    \<elt key=\"postcode\">\"G12 8QQ\"</elt></record></elt>\n\
    \<elt key=\"id\">557188</elt>\n\
    \<elt key=\"labels\"><list><list-elt><adt>Green</adt></list-elt>\n\
    \<list-elt><adt>Red</adt></list-elt></list></elt></record></list-elt>\n\
    \<list-elt><record name=\"MkPersonRecord\"><elt key=\"name\">\"Jeremy Singer\"</elt>\n\
    \<elt key=\"address\"><record name=\"MkAddress\"><elt key=\"line1\">\"School of Computing Science\"</elt>\n\
    \<elt key=\"number\">17</elt>\n\
    \<elt key=\"street\">\"Lilybank Gdns\"</elt>\n\
    \<elt key=\"town\">\"Glasgow\"</elt>\n\
    \<elt key=\"postcode\">\"G12 8QQ\"</elt></record></elt>\n\
    \<elt key=\"id\">42</elt>\n\
    \<elt key=\"labels\"><list><list-elt><adt>Blue</adt></list-elt>\n\
    \<list-elt><adt>Yellow</adt></list-elt></list></elt></record></list-elt></list>"

rec2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<a><a-a attr1=\"foo\" attr2=\"bar\">\"toto\"</a-a><b id=\"baz\">\"zz\"</b>\n\
    \<c><d></d><e>\"Something\"</e></c></a>"

main :: IO ()
main = do
    putStrLn rec2
    putStrLn ""   
    putStrLn $ parseXML rec2

