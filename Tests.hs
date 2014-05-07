{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Tests (Test(..), TesterState, runTests, run, runAllTests) where

import qualified Prelude as P

import Common
import System.IO hiding (putStr, putStrLn)
import System.Console.ANSI

data TestResult = TestSuccess [Name]
                | TestFailure [Name] Text Text Text
                | TestError [Name] Text Text
                | TestExpectedError [Name] Text Text

data TesterState = TesterState { indentLevel::Int
                               , spaceCount::Int
                               , groupNames::[Name]
                               , successes::[TestResult]
                               , failures::[TestResult]
                               , errors::[TestResult]
                               , expectedErrors::[TestResult]}

type Tester = StateT TesterState IO
data Test input result = Test Name input result
                       | SkipTest Name input result
                       | TestOnly Name input result
                       | TestGroup Name [Test input result]
                       | SkipTestGroup Name [Test input result]
                       | TestGroupOnly Name [Test input result]
                       | ShouldError Name input
                       | ShouldErrorOnly Name input
                       deriving (P.Show)

defaultState :: TesterState
defaultState = TesterState { indentLevel = 0
                           , spaceCount = 4
                           , successes = []
                           , errors = []
                           , failures = []
                           , groupNames = []
                           , expectedErrors = []}

runAll :: Monad m => [a -> m a] -> a -> m a
runAll [] result = return result
runAll (action:rest) res = action res >>= runAll rest


runAllTests :: [TesterState -> IO TesterState] -> IO TesterState
runAllTests = flip runAll defaultState

runTests :: (Eq result, Render input, Render result, Render error) =>
            (input -> Either error result) ->
            [Test input result] ->
            IO (TesterState)
runTests f ts = do
  fmap snd $ runStateT (runTests' f ts >> report) defaultState

run :: (Eq result, Render input, Render result, Render error) =>
       (input -> Either error result) ->
       [Test input result] ->
       TesterState ->
       IO (TesterState)
run f ts = fmap snd . runStateT (runTests' f ts >> report)

runTests' :: (Eq result, Render input, Render result, Render error) =>
            (input -> Either error result) ->
            [Test input result] ->
            Tester ()
runTests' function tests = do
  let run' = if any containsOnly tests then runTestOnly else runTest
  forM_ (zip [1..] tests) $ \(count, test) ->
    run' function count test

containsOnly :: Test input result -> Bool
containsOnly test = case test of
  TestOnly _ _ _ -> True
  TestGroup _ tests -> any containsOnly tests
  ShouldErrorOnly _ _ -> True
  TestGroupOnly _ _ -> True
  _ -> False

runTest :: (Eq result, Render input, Render result, Render error) =>
           (input -> Either error result) ->
           Int ->
           Test input result ->
           Tester ()
runTest function count test = case test of
  SkipTestGroup name _ -> do
    putStrI "| "
    addGroupName name
    groupName <- getGroupName
    withColor Blue $ withUL $ putStrLn' $ "(Skipped group " <> groupName <> ")"
    removeGroupName
    line
  TestGroup name tests -> do
    putStrI "| "
    addGroupName name
    withColor Cyan $ withUL $ putStrLn' $ "Test " <> name
    upIndent >> runTests' function tests >> downIndent
    removeGroupName
    line
  SkipTest name _ _ ->
    withColor Blue $ putStrLnI $ "(skipped test " <> show count <> " '" <> name <> "')"
  Test name input expect -> do
    -- print the header, e.g. "6. test sky is blue: "
    withColor Cyan $ putStrI $ show count <> ". "
    putStr' $ name <> ": "
    -- run the function on the input, one of three possibilities
    case function input of
      -- no errors, and result is what's expected
      Right result | result == expect -> do
        addSuccess name
        withColor Green $ putStrLn' "PASSED!"
      -- no errors, but unexpected result (failure)
      Right result -> do
        addFailure name input expect result
        withColor Magenta $ putStrLn' "FAILED!"
      -- errors occurred
      Left message -> do
        addError name input message
        withColor Red $ putStrLn' "ERROR!"
  ShouldError name input -> do
    withColor Cyan $ putStrI $ show count <> ". "
    putStr' $ name <> " (expect to error): "
    -- Run the function on the input; it should fail.
    case function input of
      -- no errors, which isn't what we want
      Right result -> do
        addExpectedError name input result
        withColor Magenta $ putStrLn' "DIDN'T ERROR!"
      -- errors occurred
      Left _ -> do
        addSuccess name
        withColor Green $ putStrLn' "ERROR AS EXPECTED!"

-- | Similar to @runTests@, but will only run a test if it's listed as "only".
runTestOnly :: (Eq result, Render input, Render result, Render error) =>
               (input -> Either error result) ->
               Int ->
               Test input result ->
               Tester ()
runTestOnly function count test = case test of
  SkipTestGroup _ _ -> return ()
  SkipTest _ _ _ -> return ()
  Test _ _ _ -> return ()
  ShouldError _ _ -> return ()
  TestGroup _ tests ->
    forM_ (zip [1..] tests) $ \(i, test') ->
      runTestOnly function i test'
  TestGroupOnly name tests -> do
    putStrI "| "
    addGroupName name
    withColor Cyan $ withUL $ putStrLn' $ "Test " <> name
    upIndent >> runTests' function tests >> downIndent
    removeGroupName
    line
  TestOnly name input expect -> do
    -- print the header, e.g. "6. test sky is blue: "
    withColor Cyan $ putStrI $ show count <> ". "
    putStr' $ name <> ": "
    -- run the function on the input, one of three possibilities
    case function input of
      -- no errors, and result is what's expected
      Right result | result == expect -> do
        addSuccess name
        withColor Green $ putStrLn' "PASSED!"
      -- no errors, but unexpected result (failure)
      Right result -> do
        addFailure name input expect result
        withColor Magenta $ putStrLn' "FAILED!"
      -- errors occurred
      Left message -> do
        addError name input message
        withColor Red $ putStrLn' "ERROR!"
  ShouldErrorOnly name input -> do
    withColor Cyan $ putStrI $ show count <> ". "
    putStr' $ name <> " (expect to error): "
    -- Run the function on the input; it should fail.
    case function input of
      -- no errors, which isn't what we want
      Right result -> do
        addExpectedError name input result
        withColor Magenta $ putStrLn' "DIDN'T ERROR!"
      -- errors occurred
      Left _ -> do
        addSuccess name
        withColor Green $ putStrLn' "ERROR AS EXPECTED!"

reportFailure :: TestResult -> StateT TesterState IO ()
reportFailure (TestFailure names input expect result) = do
  withColor Magenta $ withUL $ putStrLnI (intercalate ", " names)
  upIndent
  withColor Magenta $ putStrLnI "Input was:"
  withIndent $ withColor Yellow $ putStrLnI input
  withColor Magenta $ putStrLnI "Expected: "
  withIndent $ withColor Yellow $ putStrLnI expect
  withColor Magenta $ putStrLnI "Evaluated to: "
  withIndent $ withColor Yellow $ putStrLnI result
  downIndent

reportError :: TestResult -> StateT TesterState IO ()
reportError (TestError names input message) = do
  withColor Red $ withUL $ putStrLnI (intercalate ", " names)
  upIndent
  withColor Red $ putStrLnI "Input was:"
  withColor Yellow $ putStrLnI input
  withColor Red $ putStrLnI "Error message: "
  withColor Yellow $ putStrLnI (message <> "\n")
  downIndent

reportExpectedError :: TestResult -> StateT TesterState IO ()
reportExpectedError (TestExpectedError names input result) = do
  withColor Red $ withUL $ putStrLnI (intercalate ", " names)
  upIndent
  withColor Red $ putStrLnI "Input was:"
  withColor Yellow $ putStrLnI input
  withColor Red $ putStrLnI "Expected an error, but got: "
  withColor Yellow $ putStrLnI (result <> "\n")
  downIndent

report :: Tester ()
report = do
  s <- length <$> getSuccesses
  fails <- getFailures
  errs <- getErrors
  expErrs <- getExpectedErrors
  let (f, e, ee) = (length fails, length errs, length expErrs)
  let tot = s + f + e + ee
  let tot'= (fromIntegral tot)::Double
  line
  withColor Cyan $ putStrLn' $ replicate 10 "=--="
  withColor Cyan $ putStrLn' $ show tot <> " tests ran total"
  let rep n testType c = do
        let n' = (fromIntegral n)::Double
        let tests = if n == 1 then "1 test " else show n <> " tests "
        let percent = if tot' == 0 then "0" else show (round $ 100 * n' / tot' :: Int)
        let percentStr = " (" <> percent <> "%)"
        withColor c $ putStrLn' $ tests <> testType <> percentStr
  rep s "passed" Green
  rep f "failed" Magenta
  rep e "had errors" Red
  rep ee "didn't throw errors when expected" Cyan
  when (f > 0) $ line >> putStrLn' "Failures:" >> forM_ (reverse fails) reportFailure
  when (e > 0) $ line >> putStrLn' "Errors:" >> forM_ (reverse errs) reportError
  when (ee > 0) $ line >> putStrLn' "Expected Errors:" >> forM_ (reverse expErrs) reportExpectedError
  withColor Cyan $ putStrLn' $ replicate 10 "=--="

indent :: Text -> Tester Text
indent str = do lev <- getILevel
                sp <- getNSpaces
                str |> lines |> map (replicate (lev*sp) " " <>) |> intercalate "\n" |> return

withIndent :: Tester a -> Tester a
withIndent x = upIndent >> x >>= \result -> downIndent >> return result

line :: Tester ()
line = putStrLn' ""
putStrLn', putStr', putStrLnI, putStrI :: Text -> Tester ()
putStrLn' s = lift (putStrLn s) >> flush
putStr' s = lift (putStr s) >> flush
putStrLnI str = indent str >>= putStrLn'
putStrI str = indent str >>= putStr'
getSuccesses, getFailures, getErrors, getExpectedErrors :: Tester [TestResult]
getSuccesses = get <!> successes
getFailures = get <!> failures
getErrors = get <!> errors
getExpectedErrors = get <!> expectedErrors

getGroupNames :: Tester [Name]
getGroupNames = get <!> groupNames

getGroupName :: Tester Name
getGroupName = get <!> groupNames <!> reverse <!> intercalate ", "

addGroupName :: Name -> Tester ()
addGroupName name = modify (\s -> s { groupNames = name : groupNames s } )

removeGroupName :: Tester ()
removeGroupName = modify (\s -> s {groupNames = tail $ groupNames s})

getILevel, getNSpaces :: Tester Int
getILevel = indentLevel <$> get
getNSpaces = spaceCount <$> get

addSuccess :: Name -> Tester ()
addSuccess name = do
  modify $ \s -> s {successes = TestSuccess [name] : successes s}

addFailure :: (Render a, Render a1, Render a2) =>
              Name -> a -> a1 -> a2 -> StateT TesterState IO ()
addFailure name input expect result = do
  gNames <- getGroupNames
  let names = reverse $ name : gNames
  let f = TestFailure names (render input) (render expect) (render result)
  modify $ \s -> s { failures = f : failures s }

addExpectedError :: (Render a, Render a1) =>
                    Name -> a -> a1 -> StateT TesterState IO ()
addExpectedError name input result = do
  gNames <- getGroupNames
  let names = reverse $ name : gNames
  let f = TestExpectedError names (render input) (render result)
  modify $ \s -> s { expectedErrors = f : expectedErrors s }

addError :: (Render a, Render a1) =>
            Name -> a -> a1 -> StateT TesterState IO ()
addError name input message = do
  gNames <- getGroupNames
  let names = reverse $ name : gNames
  let e = TestError names (render input) (render message)
  modify $ \s -> s { errors = e : errors s }

upIndent :: Tester ()
upIndent = modify (\s -> s {indentLevel = indentLevel s + 1})

downIndent :: Tester ()
downIndent = modify (\s -> s {indentLevel = indentLevel s - 1})

flush :: Tester ()
flush = lift $ hFlush stdout

withColor :: Color -> Tester a -> Tester a
withColor color action = do
  lift $ setSGR [SetColor Foreground Vivid color]
  result <- action
  lift $ setSGR [Reset]
  return result

withUL :: Tester a -> Tester a
withUL action = do
  lift $ setSGR [SetUnderlining SingleUnderline]
  result <- action
  lift $ setSGR [SetUnderlining NoUnderline]
  return result

show :: P.Show a => a -> Text
show = P.show ~> pack

putStrLn, putStr :: Text -> IO ()
putStrLn = P.putStrLn . unpack
putStr = P.putStr . unpack
