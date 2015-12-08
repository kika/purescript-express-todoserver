module ToDoServer where

import Prelude hiding (apply)
import Data.Maybe
import Data.Either
import Data.Int
import Data.Array    as A
import Data.Foldable (foldl)
import Data.Foreign.EasyFFI
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler
import Node.HTTP (Server())

--- Model type definitions 
type Todo        = { desc :: String, isDone :: Boolean }
type IndexedTodo = { id :: Int, desc :: String, isDone :: Boolean }

-- Global state data
type AppStateData = Array Todo
type AppState     = Ref AppStateData
type AppError     = String

initState :: forall e. Eff (ref :: REF|e) AppState
initState = newRef ([] :: AppStateData)

{- 
  Model manipulation functions
  Each function receives current state data and returns a record with
  the updated state and Either error or value. If no value is assumed 
  then Unit is returned as a value. If Left error is returned then
  the state is returned unchanged.

  The returned record structure is compatible with modifyRef' function
  and thus error could be examined as a return value from modifyRef'
-}
type ChangeResult a = { state :: AppStateData, value :: Either AppError a }

addTodo :: Todo -> AppStateData -> ChangeResult Int
addTodo todo statedata = 
  { state: A.snoc statedata todo, value: Right (A.length statedata + 1) }
  
updateTodo :: Int -> String -> AppStateData -> ChangeResult Unit 
updateTodo id newDesc statedata =
  case A.modifyAt id (\el -> el { desc = newDesc }) statedata of
    Nothing  -> { state: statedata, value: Left "No such ID" }
    Just arr -> { state: arr,       value: Right unit }

deleteTodo :: Int -> AppStateData -> ChangeResult Unit
deleteTodo id statedata =
  case A.deleteAt id statedata of
    Nothing  -> { state: statedata, value: Left "No such ID" }
    Just arr -> { state: arr,       value: Right unit }

setDone :: Int -> AppStateData -> ChangeResult Unit
setDone id statedata =
  case A.modifyAt id (\el -> el { isDone = true }) statedata of
    Nothing  -> { state: statedata, value: Left "No such ID" }
    Just arr -> { state: arr,       value: Right unit }

getTodosWithIndexes :: Array Todo -> Array IndexedTodo
getTodosWithIndexes items =
  A.zipWith (\item idx -> {id: idx, desc: item.desc, isDone: item.isDone })
            items
            (A.range 0 $ A.length items)

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str


-- Monadic handlers
logger :: forall e. AppState -> Handler (console :: CONSOLE, ref :: REF | e)
logger state = do
  todos <- liftEff $ readRef state
  url   <- getOriginalUrl
  liftEff $ log (">>> " ++ url ++ " count =" ++ (show $ A.length todos))
  next

errorHandler :: forall e. AppState -> Error -> Handler e
errorHandler state err = do
  setStatus 400
  sendJson {error: message err}

help = { name: "Todo example"
     , purpose: "To present a subset of purescript-express package capabilities"
     , howToUse:
      { listTodos: "/list"
      , createTodo: "/create?desc=Do+something"
      , doTodo: "/done/:id"
      , updateTodo: "/update/:id?desc=Do+something+else"
      , deleteTodo: "/delete/:id"
      }
     , forkMe: "https://github.com/dancingrobot84/purescript-express"
     }

indexHandler :: forall e. AppState -> Handler e
indexHandler _ = do
  sendJson help

listTodosHandler :: forall e. AppState -> Handler (ref :: REF|e)
listTodosHandler state = do
  todos <- liftEff $ readRef state
  sendJson $ getTodosWithIndexes todos

createTodoHandler :: forall e. AppState -> Handler (ref :: REF | e)
createTodoHandler state = do
  descParam <- getQueryParam "desc"
  case descParam of
    Nothing -> nextThrow $ error "Description is required"
    Just desc -> do
      newId <- liftEff $ modifyRef' state $ addTodo { desc: desc, isDone: false }
      sendJson {status: "Created", id: newId}

updateTodoHandler :: forall e. AppState -> Handler (ref :: REF | e)
updateTodoHandler state = do
  idParam   <- getRouteParam "id"
  descParam <- getQueryParam "desc"
  case [idParam, descParam] of
    [Just id, Just desc] -> do
      res <- liftEff $ modifyRef' state $ updateTodo (parseInt id) desc
      case res of 
        Left msg -> nextThrow $ error msg
        _        -> sendJson {status: "Updated"}
    _ -> nextThrow $ error "Id and Description are required"

deleteTodoHandler :: forall e. AppState -> Handler (ref :: REF | e)
deleteTodoHandler state = do
  idParam <- getRouteParam "id"
  case idParam of
    Nothing -> nextThrow $ error "Id is required"
    Just id -> do
      res <- liftEff $ modifyRef' state $ deleteTodo (parseInt id)
      case res of 
        Left msg -> nextThrow $ error msg
        _        -> sendJson {status: "Deleted"}

doTodoHandler :: forall e. AppState -> Handler (ref :: REF | e)
doTodoHandler state = do
  idParam <- getRouteParam "id"
  case idParam of
    Nothing -> nextThrow $ error "Id is required"
    Just id -> do
      res <- liftEff $ modifyRef' state $ setDone (parseInt id)
      case res of 
        Left msg -> nextThrow $ error msg
        _        -> sendJson {status: "Done"}

appSetup :: forall e. AppState -> App (ref :: REF, console :: CONSOLE | e)
appSetup state = do
  liftEff $ log "Setting up"
  setProp "json spaces" 4.0
  use               (logger            state)
  get "/"           (indexHandler      state)
  get "/list"       (listTodosHandler  state)
  get "/create"     (createTodoHandler state)
  get "/update/:id" (updateTodoHandler state)
  get "/delete/:id" (deleteTodoHandler state)
  get "/done/:id"   (doTodoHandler     state)
  useOnError        (errorHandler      state)

main :: forall e. Eff (ref :: REF, express :: EXPRESS, console :: CONSOLE | e) Server
main = do
  state <- initState 
  port <- unsafeForeignFunction [""] "process.env.PORT || 8005"
  listenHttp (appSetup state) port \_ ->
    log $ "Listening on " ++ show port

