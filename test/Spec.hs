import World
import Data.Array
import Test.HUnit
import Control.Exception
import Control.Monad

-- test1 = TestCase (assertEqual "dummy test 1 == 1" 1 1)
-- test2 = TestCase (assertEqual "another dummy test 2 == 3" 2 2)

-- Assert that an exception is raised.
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
  handleJust isWanted (const $ return ()) $ do
    action
    assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)

chunk = chunkConst (3,5) 5 Wall
badchunk = array ((1,1), (3,2)) [((i,j), Wall) | i <- [1..3], j <- [1..2]]
chunkTests = test [

  "chunkSize value" ~: 5 ~=? chunkSize chunk,

  "chunkSize failure" ~: assertException (ErrorCall "Chunks must be square") $
    evaluate $ chunkSize $ badchunk,

  "chunkBounds value" ~: ChunkBounds (3,5) 5 ~=? chunkBounds chunk
  ]

main :: IO Counts
main = do
  putStrLn ""
  putStrLn "--- Chunk tests: \n"
  runTestTT chunkTests
