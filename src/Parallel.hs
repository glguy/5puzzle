module Parallel where

import Control.Monad (foldM, when)
import Data.Foldable (traverse_)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

data TaskStatus a
  = Running ThreadId
  | Failed SomeException
  | Completed a
  deriving Show

-- | Return true if and only if a task is in running state
isRunning Running{} = True
isRunning _         = False

-- | Terminate a task if it is still running
cleanup :: TaskStatus a -> IO ()
cleanup (Running threadId) = killThread threadId
cleanup _                  = return ()

parallelSearch ::
  Int         {- ^ number of tasks to run in parallel -} ->
  (a -> Bool) {- ^ predicate for success -} ->
  [IO a]      {- ^ list of all tasks -} ->
  (IntMap (TaskStatus a) -> IO ()) ->
  IO ()
parallelSearch n isDone tasks onChange =
  do let (startup, delayed) = splitAt n (zip [0..] tasks)

     chan     <- newChan
     statuses <- foldM (launch chan) IntMap.empty startup
     loop chan delayed statuses
  where
    isDoneStatus (Completed x) = isDone x
    isDoneStatus _             = False

    -- handle all events until no tasks are running
    loop chan delayed statuses =
      do (i, event) <- readChan chan
         loopLaunch chan delayed =<< handleEvent i event statuses

    -- process a single task completion event, return updated statuses
    handleEvent i event statuses =
      case event of
        Left e -> return (IntMap.insert i (Failed e) statuses)
        Right x ->
          do let statuses' = IntMap.insert i (Completed x) statuses
                 (earlier,later) = IntMap.split i statuses'
             traverse_ cleanup (if isDone x then later else earlier)
             onChange statuses'
             return statuses'

    -- launch the next task if appropriate and return to event loop
    loopLaunch chan (task:tasks) statuses | not (any isDoneStatus statuses) =
      loop chan tasks =<< launch chan statuses task
    loopLaunch chan _ statuses =
      when (any isRunning statuses) (loop chan [] statuses)


    launch chan statuses (i, task) =
      do taskId <- forkFinally task (\res -> writeChan chan (i, res))
         let statuses' = IntMap.insert i (Running taskId) statuses
         onChange statuses'
         return statuses'
