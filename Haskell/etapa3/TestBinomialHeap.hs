module TestBinomialHeap where

import Data.Char (toUpper, chr, toLower)
import Data.List (sort)
import Data.Maybe (Maybe)

import BinomialHeap
import TestPP

{-
    Construiește un arbore binomial de rang dat, începând cu prioritatea și cu cheia
    date.
-}
dummyTreeOfRank :: (Num p, Num k) => Int -> p -> k -> BinomialTree p k
dummyTreeOfRank 0 p k = Node p k []
dummyTreeOfRank rank startPrio startKey = top{children = bot : children top}
  where
    top = dummyTreeOfRank (rank - 1) startPrio startKey
    bot = dummyTreeOfRank (rank - 1) (startPrio + 1) (startKey + 2 ^ (rank - 1))

{-
    Construiește un heap binomial pornind cu un rang, niște priorități și o cheie date.

    Prioritățile pot fi și negative, caz în care arborele de pe poziția
    respectivă va lipsi.

    Fiecare nod primește o cheie unică (numere consecutive începând cu `startKey`).
-}
dummyHeap :: (Num p, Ord p, Num k) => Int -> [p] -> k -> BinomialHeap p k
dummyHeap startRank priorities startKey = BinomialHeap size trees
  where
    size = sum (map getTreeSize trees)
    trees = zipWith buildTreeOrNot [startRank ..] priorities

    buildTreeOrNot rank prio
        | prio < 0 = EmptyTree
        | otherwise = dummyTreeOfRank rank prio nextUnusedKey
      where
        nextUnusedKey = startKey + sum usedSizes
        usedSizes = [2 ^ r | (r, p) <- zip [startRank .. rank - 1] priorities, p >= 0]

{-
    Verifică proprietatea de arbore binomial care respectă suplimentar
    proprietatea de heap.
-}
treeProperty :: (Ord p) => BinomialTree p k -> Bool
treeProperty EmptyTree = True
treeProperty tree =
    let
        noChildren = length $ children tree
        condNoChildren = f (children tree) (noChildren - 1)
          where
            f [] (-1) = True
            f (tree : trees) n = (length (children tree) == n) && f trees (n - 1)
            f _ _ = False
        condHeap = null (children tree) || prio tree <= minimum (map prio (children tree))
     in
        condHeap && foldl (&&) condNoChildren (map treeProperty (children tree))

{-
    Verifică proprietatea de heap binomial: arborii sunt testați cu funcția
    de mai sus, iar dimensiunea heap-ului trebuie să coincidă cu suma
    dimensiunilor arborilor.
-}
binomialHeapProperty :: (Ord p, Eq k) => BinomialHeap p k -> Bool
binomialHeapProperty (BinomialHeap reportedSize list) = sizeOk && treesOk
  where
    sizeOk = reportedSize == realSize && theoreticalSize == realSize
    realSize = sum $ map getTreeSize list
    theoreticalSize = sum $ map ((2 ^) . length . children) $ filter (/= EmptyTree) list

    treesOk = treesIndividiually && treesPositioned
    treesIndividiually = all treeProperty list
    treesPositioned =
        all (\(r, t) -> r == (length . children) t)
            . filter (\(_, t) -> t /= EmptyTree)
            . zip [0 ..]
            $ list

    rank = length list

{-
    Calculează dimensiunea unui arbore binomial, fără a presupune corectitudinea
    construcției acestuia.
-}
getTreeSize :: BinomialTree p k -> Int
getTreeSize EmptyTree = 0
getTreeSize (Node prio key children) = 1 + sum (map getTreeSize children)

{-
    Verifică dacă o listă de arbori binomiali respectă proprietățile de heap binomial.
-}
checkResult :: (Ord p, Eq k) => [BinomialTree p k] -> Bool
checkResult trees = binomialHeapProperty (BinomialHeap (sum (map getTreeSize trees)) trees)

{-
    Extrage perechile (prioritate, cheie) prezente într-un arbore.
-}
flattenTree :: BinomialTree p k -> [(p, k)]
flattenTree EmptyTree = []
flattenTree (Node p k c) = (p, k) : concatMap flattenTree c

{-
    Extrage perechile (prioritate, cheie) prezente într-un heap.
-}
flattenHeap :: BinomialHeap p k -> [(p, k)]
flattenHeap BinomialHeap{trees = t} = concatMap flattenTree t

{-
    Verifică dacă un arbore are conținutul dorit.
-}
treeHasContent :: (Ord p, Ord k) => BinomialTree p k -> [(p, k)] -> Bool
treeHasContent tree content = sort (flattenTree tree) == sort content

{-
    Verifică dacă o listă de arbori are conținutul dorit.
-}
treeListHasContent :: (Ord p, Ord k) => [BinomialTree p k] -> [(p, k)] -> Bool
treeListHasContent trees content = sort (concatMap flattenTree trees) == sort content

{-
    Verifică dacă un heap are conținutul dorit.
-}
heapHasContent :: (Ord p, Ord k) => BinomialHeap p k -> [(p, k)] -> Bool
heapHasContent heap content = sort (flattenHeap heap) == sort content

testIsolate :: TestData
testIsolate =
    tests 1 15
        [ testVal "isolate empty" [] $ isolate 0 []
        , testVal "isolate example 1" [(1, [0, 2, 3]), (2, [1, 0, 3]), (3, [1, 2])] $ isolate 0 [1, 2, 3]
        , testVal "isolate 1 element" [(False, [])] $ isolate True [False]
        , testVal "isolate 2 elements" [(10, [30, 20]), (20, [10])] $ isolate 30 [10, 20]
        , testVal "isolate string" [('d', "Xepres"), ('e', "dXpres"), ('p', "deXres"), ('r', "depXes"), ('e', "deprXs"), ('s', "depre")] $ isolate 'X' "depres"
        ]

testRemoveMin :: TestData
testRemoveMin =
    tests 2 30
        [ testCond "removeMin example 1 (structure)" $ binomialHeapProperty heap1
        , testCond "removeMin example 1 (contents)" $ heap1 `heapHasContent` tuples1
        , testCond "removeMin example 2 (structure)" $ binomialHeapProperty heap2
        , testCond "removeMin example 2 (contents)" $ heap2 `heapHasContent` tuples2
        , testCond "removeMin example 3 (structure)" $ binomialHeapProperty heap3
        , testCond "removeMin example 3 (contents)" $ heap3 `heapHasContent` tuples3
        , testCond "removeMin example 4 (structure)" $ binomialHeapProperty heap4
        , testCond "removeMin example 4 (contents)" $ heap4 `heapHasContent` tuples4
        , testCond "removeMin gap (structure)" $ binomialHeapProperty heap5
        , testCond "removeMin gap (contents)" $ heap5 `heapHasContent` tuples5
        , testCond "removeMin duplicate min (structure)" $ binomialHeapProperty heap6
        , testCond "removeMin duplicate min (contents)" $ heap6 `heapHasContent` tuples6
        , testCond "removeMin one tree (structure)" $ binomialHeapProperty heap7
        , testCond "removeMin one tree (contents)" $ heap7 `heapHasContent` tuples7
        , testCond "removeMin 101* careful addition (structure)" $ binomialHeapProperty heap8
        , testCond "removeMin 101* careful addition (contents)" $ heap8 `heapHasContent` tuples8
        , testCond "removeMin 011*1 (structure)" $ binomialHeapProperty heap9
        , testCond "removeMin 011*1 (contents)" $ heap9 `heapHasContent` tuples9
        , testCond "removeMin 1110*1 (structure)" $ binomialHeapProperty heap10
        , testCond "removeMin 1110*1 (contents)" $ heap10 `heapHasContent` tuples10
        ]
  where
    heap1 = removeMin BinomialHeap{size = 0, trees = []}
    tuples1 = [] :: [(Int, Int)]

    heap2 = removeMin BinomialHeap{size = 1, trees = [Node{prio = 1, key = 'a', children = []}]}
    tuples2 = []

    heap3 = removeMin BinomialHeap{size = 2, trees = [EmptyTree, Node{prio = 1, key = 'a', children = [Node{prio = 2, key = 'b', children = []}]}]}
    tuples3 = [(2, 'b')]

    heap4 = removeMin BinomialHeap{size = 3, trees = [Node{prio = 3, key = 'c', children = []}, Node{prio = 1, key = 'a', children = [Node{prio = 2, key = 'b', children = []}]}]}
    tuples4 = [(2, 'b'), (3, 'c')]

    -- TEAM: Keep `startRank` equal to 0, otherwise our implementation does not work
    -- (it needs the heap to start at rank 0 in order to do merging correctly).
    heap5 = removeMin $ dummyHeap 0 [2, -1, -1, 1] 0
    tuples5 = [(2, 0), (2, 2), (2, 3), (2, 5), (3, 4), (3, 6), (3, 7), (4, 8)]

    heap6 = removeMin $ dummyHeap 0 [30, 2, 5, 2] 0
    tuples6 = [(2, 7), (3, 2), (3, 8), (3, 9), (3, 11), (4, 10), (4, 12), (4, 13), (5, 3), (5, 14), (6, 4), (6, 5), (7, 6), (30, 0)]

    heap7 = removeMin $ dummyHeap 0 [-1, -1, 7] 0
    tuples7 = [(8, 1), (8, 2), (9, 3)]

    heap8 = removeMin $ dummyHeap 0 [2, -1, 1, 0] 0
    tuples8 = [(1, 1), (1, 6), (1, 7), (1, 9), (2, 0), (2, 2), (2, 3), (2, 8), (2, 10), (2, 11), (3, 4), (3, 12)]

    heap9 = removeMin $ dummyHeap 0 [-1, 3, 3, 1, 3] 0
    tuples9 = [(2, 7), (2, 8), (2, 10), (3, 0), (3, 2), (3, 9), (3, 11), (3, 12), (3, 14), (4, 1), (4, 3), (4, 4), (4, 13), (4, 15), (4, 16), (4, 18), (4, 22), (5, 5), (5, 17), (5, 19), (5, 20), (5, 23), (5, 24), (5, 26), (6, 21), (6, 25), (6, 27), (6, 28), (7, 29)]

    heap10 = removeMin $ dummyHeap 0 [3, 4, 5, -1, 1, 2] 0
    tuples10 = [(2, 8), (2, 9), (2, 11), (2, 15), (2, 23), (3, 0), (3, 10), (3, 12), (3, 13), (3, 16), (3, 17), (3, 19), (3, 24), (3, 25), (3, 27), (3, 31), (3, 39), (4, 1), (4, 14), (4, 18), (4, 20), (4, 21), (4, 26), (4, 28), (4, 29), (4, 32), (4, 33), (4, 35), (4, 40), (4, 41), (4, 43), (4, 47), (5, 2), (5, 3), (5, 22), (5, 30), (5, 34), (5, 36), (5, 37), (5, 42), (5, 44), (5, 45), (5, 48), (5, 49), (5, 51), (6, 4), (6, 5), (6, 38), (6, 46), (6, 50), (6, 52), (6, 53), (7, 6), (7, 54)]

testShowTree :: TestData
testShowTree =
    tests 3 20
        [ testVal "showTree example 1" str1 $ show heap1
        , testVal "showTree example 2" str2 $ show heap2
        , testVal "showTree example 3" str3 $ show heap3
        , testVal "showTree example 4" str4 $ show heap4
        , testVal "showTree large" str5 $ show heap5
        ]
  where
    heap1 = EmptyTree :: BinomialTree Int Int
    heap2 = Node 1 'a' []
    heap3 = Node 1 'a' [Node 2 'b' []]
    heap4 = Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    heap5 = dummyTreeOfRank 5 0 0

    str1 = "*"
    str2 = "1 ('a')"
    str3 = "1 ('a')\n  2 ('b')"
    str4 = "1 ('a')\n  3 ('c')\n    4 ('d')\n  2 ('b')"
    str5 = "0 (0)\n  1 (16)\n    2 (24)\n      3 (28)\n        4 (30)\n          5 (31)\n        4 (29)\n      3 (26)\n        4 (27)\n      3 (25)\n    2 (20)\n      3 (22)\n        4 (23)\n      3 (21)\n    2 (18)\n      3 (19)\n    2 (17)\n  1 (8)\n    2 (12)\n      3 (14)\n        4 (15)\n      3 (13)\n    2 (10)\n      3 (11)\n    2 (9)\n  1 (4)\n    2 (6)\n      3 (7)\n    2 (5)\n  1 (2)\n    2 (3)\n  1 (1)"

testShowHeap :: TestData
testShowHeap =
    tests 4 10
        [ testVal "showHeap empty" str1 $ show heap1
        , testVal "showHeap example 1" str2 $ show heap2
        , testVal "showHeap example 2" str3 $ show heap3
        , testVal "showHeap gaps" str4 $ show heap4
        , testVal "showHeap large" str5 $ show heap5
        ]
  where
    heap1 = BinomialHeap{size = 0, trees = []} :: BinomialHeap Int Int
    heap2 = BinomialHeap{size = 3, trees = [Node{prio = 3, key = 'c', children = []}, Node{prio = 1, key = 'a', children = [Node{prio = 2, key = 'b', children = []}]}]}
    heap3 = BinomialHeap{size = 5, trees = [Node{prio = 5, key = 'e', children = []}, EmptyTree, Node{prio = 1, key = 'a', children = [Node{prio = 3, key = 'c', children = [Node{prio = 4, key = 'd', children = []}]}, Node{prio = 2, key = 'b', children = []}]}]}
    heap4 = dummyHeap 0 [-1, 10, -1, -1, 3] 0
    heap5 = dummyHeap 0 [1, 9, 7, 7, -1, -1, 3] 0

    str1 = ""
    str2 = "3 ('c')\n1 ('a')\n  2 ('b')"
    str3 = "5 ('e')\n*\n1 ('a')\n  3 ('c')\n    4 ('d')\n  2 ('b')"
    str4 = "*\n10 (0)\n  11 (1)\n*\n*\n3 (2)\n  4 (10)\n    5 (14)\n      6 (16)\n        7 (17)\n      6 (15)\n    5 (12)\n      6 (13)\n    5 (11)\n  4 (6)\n    5 (8)\n      6 (9)\n    5 (7)\n  4 (4)\n    5 (5)\n  4 (3)"
    str5 = "1 (0)\n9 (1)\n  10 (2)\n7 (3)\n  8 (5)\n    9 (6)\n  8 (4)\n7 (7)\n  8 (11)\n    9 (13)\n      10 (14)\n    9 (12)\n  8 (9)\n    9 (10)\n  8 (8)\n*\n*\n3 (15)\n  4 (47)\n    5 (63)\n      6 (71)\n        7 (75)\n          8 (77)\n            9 (78)\n          8 (76)\n        7 (73)\n          8 (74)\n        7 (72)\n      6 (67)\n        7 (69)\n          8 (70)\n        7 (68)\n      6 (65)\n        7 (66)\n      6 (64)\n    5 (55)\n      6 (59)\n        7 (61)\n          8 (62)\n        7 (60)\n      6 (57)\n        7 (58)\n      6 (56)\n    5 (51)\n      6 (53)\n        7 (54)\n      6 (52)\n    5 (49)\n      6 (50)\n    5 (48)\n  4 (31)\n    5 (39)\n      6 (43)\n        7 (45)\n          8 (46)\n        7 (44)\n      6 (41)\n        7 (42)\n      6 (40)\n    5 (35)\n      6 (37)\n        7 (38)\n      6 (36)\n    5 (33)\n      6 (34)\n    5 (32)\n  4 (23)\n    5 (27)\n      6 (29)\n        7 (30)\n      6 (28)\n    5 (25)\n      6 (26)\n    5 (24)\n  4 (19)\n    5 (21)\n      6 (22)\n    5 (20)\n  4 (17)\n    5 (18)\n  4 (16)"

testFmapTree :: TestData
testFmapTree =
    tests 5 15
        [ testVal "fmapTree empty" (EmptyTree :: BinomialTree Int Int) $ fmap (+ 1) EmptyTree,
          testVal "fmapTree rank 0" (Node 1 'A' []) $ fmap toUpper (Node 1 'a' []),
          testVal "fmapTree rank 2" (Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]) $ fmap chr (Node 1 97 [Node 3 99 [Node 4 100 []], Node 2 98 []]),
          testVal "fmapTree rank 3" (dummyTreeOfRank 3 1 101) $ fmap (+ 1) (dummyTreeOfRank 3 1 100),
          testVal "fmapTree rank 7" (dummyTreeOfRank 7 10 1000) $ fmap (+ 1000) (dummyTreeOfRank 7 10 0)
        ]

testFmapHeap :: TestData
testFmapHeap =
    tests 6 10
        [ testVal "fmapHeap empty" heap1 $ fmap (+ 1) heap1
        , testVal "fmapHeap example 1" heap2' $ fmap toUpper heap2
        , testVal "fmapHeap example 2" heap3' $ fmap toUpper heap3
        , testVal "fmapHeap gaps" (mkHeap4 101) $ fmap (+ 1) (mkHeap4 100)
        , testVal "fmapHeap large" (mkHeap5 1000) $ fmap (+ 1000) (mkHeap5 0)
        ]
  where
    heap1 = BinomialHeap{size = 0, trees = []} :: BinomialHeap Int Int
    heap2 = BinomialHeap{size = 3, trees = [Node{prio = 3, key = 'c', children = []}, Node{prio = 1, key = 'a', children = [Node{prio = 2, key = 'b', children = []}]}]}
    heap3 = BinomialHeap{size = 5, trees = [Node{prio = 5, key = 'e', children = []}, EmptyTree, Node{prio = 1, key = 'a', children = [Node{prio = 3, key = 'c', children = [Node{prio = 4, key = 'd', children = []}]}, Node{prio = 2, key = 'b', children = []}]}]}
    mkHeap4 = dummyHeap 0 [-1, 10, -1, -1, 3]
    mkHeap5 = dummyHeap 0 [1, 9, 7, 7, -1, -1, 3]

    heap2' = BinomialHeap{size = 3, trees = [Node{prio = 3, key = 'C', children = []}, Node{prio = 1, key = 'A', children = [Node{prio = 2, key = 'B', children = []}]}]}
    heap3' = BinomialHeap{size = 5, trees = [Node{prio = 5, key = 'E', children = []}, EmptyTree, Node{prio = 1, key = 'A', children = [Node{prio = 3, key = 'C', children = [Node{prio = 4, key = 'D', children = []}]}, Node{prio = 2, key = 'B', children = []}]}]}

testFoldr :: TestData
testFoldr =
    tests 7 20
        [ testVal "foldrTree empty" 0 $ sum EmptyTree
        , testVal "foldrTree strings" "ABCD" $ foldr (<>) "" Node{prio = 1, key = "A", children = [Node{prio = 3, key = "B", children = [Node{prio = 4, key = "C", children = []}]}, Node{prio = 2, key = "D", children = []}]}
        , testVal "foldrTree sum" 312 $ sum (dummyTreeOfRank 4 0 12)
        , testVal "foldrTree digits" "04675231" $ concatDigits (dummyTreeOfRank 3 0 0)
        , testVal "foldrTree many digits" (concatDigitsNoFold tree5) (concatDigits tree5)
        ]
  where
    tree5 = dummyTreeOfRank 7 15 25
    concatDigits = foldr (\x acc -> show x <> acc) ""
    concatDigitsNoFold = concatMap (show . snd) . flattenTree

main :: IO ()
main =
    vmCheck
        [ testIsolate
        , testRemoveMin
        , testShowTree
        , testShowHeap
        , testFmapTree
        , testFmapHeap
        , testFoldr
        ]
