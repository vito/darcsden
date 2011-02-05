{-# LANGUAGE OverloadedStrings #-}
module DarcsDen.Handler.Repository where

import Control.Monad (when)
import Control.Monad.Trans
import Data.Char (toLower)
import Data.List (groupBy, inits, isPrefixOf, nub, partition, sortBy)
import Data.List.Split (wordsBy)
import Data.Map ((!))
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Ord (comparing)
import Data.Time (getCurrentTime)
import Database.CouchDB (doc)
import Snap.Types

import DarcsDen.Handler.Repository.Util
import DarcsDen.Handler.Repository.Browse
import DarcsDen.Handler.Repository.Changes
import DarcsDen.Handler.Repository.Forks
import DarcsDen.State.Comment
import DarcsDen.State.Issue
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.State.Util
import DarcsDen.Util
import DarcsDen.Validate
import DarcsDen.WebUtils
import qualified DarcsDen.Pages.Repository as Page


initialize :: Page
initialize s@(Session { sUser = Nothing }) = do
    warn "You must be logged in to create a repository." s
    redirectTo "/login"
initialize s = doPage (Page.init []) s

doInitialize :: Page
doInitialize s@(Session { sUser = Nothing }) = do
    warn "You must be logged in to create a repository." s
    redirectTo "/login"
doInitialize s@(Session { sUser = Just n }) = validate
    [ iff
        (And (nonEmpty "name") $
            predicate "name" isSane
                "contain only alphanumeric characters, -, or _")
        (\(OK i) ->
            io "destination repository already exists" $
                fmap isNothing (getRepository (n, i ! "name")))

    , Or (isEmpty "bootstrap")
        (Predicate "bootstrap"
                   ((`elem` ["http", "https"]) . takeWhile (/= ':'))
                   "start with http:// or https://")

    , io "user is not valid" (fmap (/= Nothing) (getUser n))
    ]
    (\(OK r) -> do
        now <- liftIO getCurrentTime
        desc <- input "description" ""
        site <- input "website" ""
        private <- getParam "private"

        let repo = Repository
                { rID = Nothing
                , rRev = Nothing
                , rName = r ! "name"
                , rOwner = n
                , rDescription = desc
                , rWebsite = site
                , rCreated = now
                , rForkOf = Nothing
                , rMembers = []
                , rIsPrivate = isJust private
                , rIssueCount = 0
                }

        url <- input "bootstrap" ""
        if length url > 0
            then bootstrapRepository repo url
            else newRepository repo

        success "Repository created." s
        redirectTo ("/" ++ n ++ "/" ++ (r ! "name")))
    (\(Invalid f) -> do
        is <- getInputs
        notify Warning s f
            >>= doPage (Page.init is))

explore :: Page
explore s = do
    page <- input "page" "1"

    let p = read page :: Int
        paginated rs =
            paginate 50 p $
                sortBy (comparing (map toLower . rName . fst)) rs

    rs <- fmap groupForks getRepositories

    let totalPages = ceiling ((fromIntegral (length rs) :: Double) / 50)

    doPage (Page.explore (paginated rs) p totalPages) s
  where
    groupForks rs = foldr addFork (map (flip (,) []) roots) forks
      where
        (forks, roots) = partition (isJust . rForkOf) rs

    addFork f [] = [(f, [])]
    addFork f ((r@(Repository { rID = y }), fs):rs)
        | rForkOf f == y = ((r, (f:fs)) : rs)
        | otherwise = (r, fs) : addFork f rs

browseRepo :: User -> Repository -> Page
browseRepo u r s = do
    Right dr <- liftIO $ getRepo (repoDir (rOwner r) (rName r))

    member <-
        case sUser s of
            Just un ->
                return (un `elem` rMembers r)
            Nothing ->
                return False

    f <- filePath
    fs <- liftIO $ getFiles dr f
    bl <- liftIO $ getBlob dr f

    case (fs, bl) of
        (Nothing, Nothing) -> notFound
        (Just fs', _) -> do
            readme <- liftIO $ getReadme dr f

            let files =
                    map (\i -> i
                        { iPath = pathToFile (f ++ [iName i])
                        }) fs'

            doPage (Page.repo u r files (crumb f) readme member) s
        (_, Just big) | isTooLarge big ->
            doPage (Page.blob u r (crumb f) Nothing) s
        (_, Just source) ->
            doPage
                (Page.blob u r
                    (crumb f)
                    (Just (highlightBlob (last f) (strictLBS source))))
                s
  where
    filePath = do
        rq <- getRequest
        return (wordsBy (== '/') (fromBS $ rqPathInfo rq))

    crumb = map pathToRepoItem . tail . inits

    pathToRepoItem p =
        RepoItem
            { iName = last p
            , iPath = pathToFile p
            , iIsDirectory = True
            }


repoChanges :: User -> Repository -> Page
repoChanges u r s = do
    p <- input "page" "1"
    let page = read p
    (patches, totalPages) <- liftIO $ getChanges (repoDir (rOwner r) (rName r)) page

    doPage (Page.changes u r patches page totalPages) s

repoChangesAtom :: User -> Repository -> Page
repoChangesAtom u r s = do
    (patches, _) <- liftIO $ getChanges (repoDir (rOwner r) (rName r)) 1
    doAtomPage (Page.changesAtom u r patches) s

repoPatch :: User -> Repository -> Page
repoPatch u r s = do
    p <- input "id" ""
    when (null p) (errorPage "No patch ID specified.")

    patch <- liftIO $ getPatch (repoDir (rOwner r) (rName r)) p

    doPage
        (Page.patch u r
            (pPatch patch)
            (summarize (pChanges patch))
            (filter isModification (pChanges patch)))
        s

editRepo :: User -> Repository -> Page
editRepo u r s = validate
    [ io "you do not own this repository" $
        return $ Just (rOwner r) == sUser s
    ]
    (\(OK _) -> do
        ms <- mapM getUser (rMembers r)

        let members = map fromJust . filter (/= Nothing) $ ms

        doPage (Page.edit u r members []) s)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")

doEditRepo :: User -> Repository -> Page
doEditRepo _ r s = validate
    [ io "you do not own this repository" $
        return $ Just (rOwner r) == sUser s

    , nonEmpty "name"

    , predicate "name" isSane
        "contain only alphanumeric characters, -, or _"
    ]
    (\(OK i) -> do
        removed <- removeMembers r (rMembers r)

        toAdd <- input "add-members" ""
        added <- addMembers removed (map strip . wordsBy (== ',') $ toAdd)
        new <- rename added (i ! "name")

        desc <- input "description" (rDescription r)
        site <- input "website" (rWebsite r)
        private <- getParam "private"
        updateRepository
            new { rDescription = desc
                , rWebsite = site
                , rIsPrivate = isJust private
                }

        success "Repository updated." s

        redirectTo ("/" ++ (rOwner r) ++ "/" ++ (i ! "name") ++ "/edit"))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ("/" ++ (rOwner r) ++ "/" ++ (rName r) ++ "/edit"))
  where
    rename r' n
        = if rName r' /= n
             then do
                 res <- renameRepository n r'
                 case res of
                     Nothing -> do
                         warn "There was an error renaming the repository." s
                         return r'
                     Just r'' -> return r''
             else return r'

    addMembers r' [] = return r'
    addMembers r' (m:ms) = do
        user <- getUser m
        case user of
             Just (User { uName = un }) -> do
                 done <- addMember r' un
                 case done of
                     Just r'' ->
                         addMembers r'' ms
                     Nothing -> do
                         warn ("There was an error adding member " ++ un ++ ".") s
                         return r'
             _ -> do
                 warn ("Could not add member " ++ m ++ "; user does not exist.") s
                 addMembers r' ms

    removeMembers r' [] = return r'
    removeMembers r' (m:ms) = do
        remove <- getParam (toBS $ "remove-" ++ show m)
        maybe (removeMembers r' ms) (\_ -> do
            removed <- removeMember r m
            flip maybe (flip removeMembers ms) (do
                user <- getUser m
                case user of
                     Just u' -> do
                         warn ("There was an error removing member " ++ uName u' ++ ".") s
                         removeMembers r' ms
                     Nothing -> do
                         warn failDunno s
                         removeMembers r' ms) removed) remove

    failDunno = unwords
        [ "There was an error removing a member that doesn't exist."
        , "So I can't tell you who it was. Way to go."
        ]


deleteRepo :: User -> Repository -> Page
deleteRepo u r s = validate
    [ io "you do not own this repository" $
        return $ Just (rOwner r) == sUser s
    ]
    (\(OK _) -> doPage (Page.delete u r) s)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")

doDeleteRepo :: User -> Repository -> Page
doDeleteRepo _ r s = validate
    [ io "you do not own this repository" $
        return $ Just (rOwner r) == sUser s
    ]
    (\(OK _) -> do
        destroyed <- destroyRepository r

        if destroyed
           then do
               success "Repository deleted." s
               redirectTo "/"
           else do
               warn "Repository deletion failed." s
               redirectTo ('/' : rOwner r ++ "/" ++ rName r))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ('/' : rOwner r ++ "/" ++ rName r))

forkRepo :: User -> Repository -> Page
forkRepo _ _ s@(Session { sUser = Nothing }) = do
    warn "You must be logged in to fork a repository." s
    redirectTo "/"
forkRepo u r s@(Session { sUser = Just n }) = validate
    [ io "destination repository already exists" $
        fmap isNothing (getRepository (n, rName r))
    ]
    (\(OK _) -> do
        forked <- forkRepository n (rName r) r
        success "Repository forked." s
        redirectTo ("/" ++ n ++ "/" ++ rName forked))
    (\(Invalid _) -> doPage (Page.fork u r (rName r)) s)

forkRepoAs :: User -> Repository -> Page
forkRepoAs _ _ s@(Session { sUser = Nothing }) = do
    warn "You must be logged in to fork a repository." s
    redirectTo "/"
forkRepoAs u r s@(Session { sUser = Just n }) = validate
    [ iff (nonEmpty "name") $ \(OK i) ->
        io "destination repository already exists" $
            fmap isNothing (getRepository (n, i ! "name"))
    ]
    (\(OK i) -> do
        forked <- forkRepository n (i ! "name") r
        success "Repository forked." s
        redirectTo ("/" ++ n ++ "/" ++ rName forked))
    (\(Invalid _) -> do
        name <- input "name" (rName r)
        doPage (Page.fork u r name) s)

repoPatches :: User -> Repository -> Page
repoPatches u r s = do
    fs <- getRepositoryForks (fromJust $ rID r)
    forks <- liftIO $ mapM getForkChanges fs

    ownPrivFs <- fmap (filter privateFork) (getOwnerRepositories (uName u))
    ownPrivForks <- liftIO $ mapM getForkChanges ownPrivFs

    doPage (Page.patches u r forks ownPrivForks) s
  where
    privateFork (Repository { rIsPrivate = True, rForkOf = f }) =
        f == rID r
    privateFork _ = False

repoMerge :: User -> Repository -> Page
repoMerge _ r s = validate
    [ io "you do not own this repository" $
        return $ Just (rOwner r) == sUser s
    ]
    (\(OK _) -> do
        is <- getInputs
        let ps = map (\(n, _) ->
                       let split = wordsBy (== ':') n
                       in (doc (split !! 1), split !! 2))
                     (filter (\(n, _) -> "merge:" `isPrefixOf` n) is)
            gps = groupBy (\a b -> fst a == fst b) ps
            groupedPatches = map (\r'@((k, _):_) -> (k, map snd r')) gps

        merge <- mapM (\(r', ps') -> do
                          Just f <- getRepositoryByID r'
                          liftIO $ mergePatches f ps' s) groupedPatches

        when (and merge) (success "Patches merged!" s >> return ())

        redirectTo ('/' : rOwner r ++ "/" ++ rName r ++ "/forks"))
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")

repoIssues :: User -> Repository -> Page
repoIssues u r s = do
    issues <- fmap (sortBy (flip $ comparing iUpdated)) $ getIssues r
    doPage (Page.issues u r issues) s

repoIssuesTag :: User -> Repository -> Page
repoIssuesTag u r s = validate
    [ nonEmpty "tag"
    ]
    (\(OK is) -> do
        let tags = map (wordsBy (== '^')) $ wordsBy (== '~') (is ! "tag")

        issues <- orFind tags

        doPage
            (Page.issuesByTags u r
                (sortBy (flip $ comparing iUpdated) issues)
                tags)
            s)
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo (repoURL r ++ "/issues"))
  where
    andFind :: [String] -> Snap [Issue]
    andFind (ft:ts) = do
        issues <- getIssuesByTag r ft
        return (foldr (\t i -> filter ((t `elem`) . iTags) i) issues ts)
    andFind _ = error "andFind: no tags"

    orFind :: [[String]] -> Snap [Issue]
    orFind as = do
        issues <- mapM andFind as
        return (nub (concat issues))

repoIssue :: User -> Repository -> Page
repoIssue u r s = validate
    [ numeric "number"
    ]
    (\(OK is) -> do
        mi <- getIssue (fromJust (rID r)) (read $ is ! "number")
        case mi of
            Just i -> do
                cs <- fmap (sortBy (comparing cUpdated)) $ getComments i
                doPage (Page.issue u r i cs) s
            Nothing -> notFound)
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo (repoURL r))

newIssue :: User -> Repository -> Page
newIssue _ _ s@(Session { sUser = Nothing }) = do
    warn "You must be logged in to create an issue." s
    redirectTo "/"
newIssue u r s =
    doPage (Page.newIssue u r) s

doNewIssue :: User -> Repository -> Page
doNewIssue _ _ s@(Session { sUser = Nothing }) = do
    warn "You must be logged in to create an issue." s
    redirectTo "/"
doNewIssue _ r s@(Session { sUser = Just un }) = validate
    [ nonEmpty "summary"
    , Predicate "description" (const True) "be provided"
    , Predicate "tags" (const True) "be provided"
    ]
    (\(OK is) -> do
        now <- liftIO getCurrentTime
        i <- addIssue Issue
            { iID = Nothing
            , iRev = Nothing
            , iNumber = rIssueCount r + 1
            , iSummary = is ! "summary"
            , iOwner = un
            , iDescription = is ! "description"
            , iTags = map strip $ wordsBy (== ',') (is ! "tags")
            , iCreated = now
            , iUpdated = now
            , iIsClosed = False
            , iRepository = fromJust (rID r)
            }

        updateRepository r
            { rIssueCount = rIssueCount r + 1
            }

        redirectTo (issueURL r i))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo (repoURL r ++ "/new-issue"))

repoComment :: User -> Repository -> Page
repoComment _ _ s@(Session { sUser = Nothing }) = do
    warn "You must be logged in to comment on an issue." s
    redirectTo "/"
repoComment _ r s@(Session { sUser = Just un }) = validate
    [ iff (numeric "number") $ \(OK is) -> io "issue does not exist" $
        fmap isJust (getIssue (fromJust (rID r)) (read $ is ! "number"))
    ]
    (\(OK is) -> do
        Just i <- getIssue (fromJust (rID r)) (read $ is ! "number")
        submit <- input "submit" ""
        summary <- input "summary" ""
        description <- input "description" ""
        c <- input "comment" ""
        ts <- fmap (map strip . wordsBy (== ',')) $ input "tags" ""

        let closed =
                case submit of
                    "and close" -> True
                    "and reopen" -> False
                    _ -> iIsClosed i

            issueChanged = or
                [ iSummary i /= summary
                , iDescription i /= description
                , not (null (diffTags (iTags i) ts))
                , iIsClosed i /= closed
                ]

            diffTags ots nts = concat
                [ map AddTag (filter (`notElem` ots) nts)
                , map RemoveTag (filter (`notElem` nts) ots)
                ]

            changes = concat
                [ if iSummary i /= summary
                    then [Summary (iSummary i) summary]
                    else []
                , if iDescription i /= description
                    then [Description (iDescription i) description]
                    else []
                , if iIsClosed i /= closed then [Closed closed] else []
                , if iTags i /= ts then diffTags (iTags i) ts else []
                ]

        if not issueChanged && null c
            then do
                warn "no changes and no comment; do something!" s
                redirectTo (issueURL r i)
            else do

        now <- liftIO getCurrentTime
        addComment Comment
            { cID = Nothing
            , cRev = Nothing
            , cBody = c
            , cChanges = changes
            , cAuthor = un
            , cIssue = fromJust (iID i)
            , cCreated = now
            , cUpdated = now
            }

        mni <-
            if issueChanged
                then updateIssue i
                    { iSummary = summary
                    , iDescription = description
                    , iTags = ts
                    , iIsClosed = closed
                    , iUpdated = now
                    }
                else return (Just i)

        case mni of
            Nothing -> do
                warn "issue could not be updated" s
                redirectTo (issueURL r i)
            Just ni -> do
                if not (null c)
                    then success "comment added" s
                    else success "issue updated" s

                redirectTo (issueURL r ni))
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
