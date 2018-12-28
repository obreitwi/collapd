{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Collapd.List (
  collapseHostEntries,
  getHostEntriesWith,
  getEntry,
  listAllHosts,
  listHostsWith,
  makeUnique
) where

import Prelude hiding (filter)

import           Control.Monad (join)
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Ldap.Client as Ldap

import qualified Debug.Trace as DT

import           Collapd.Conf as CC
import           Collapd.Utils


-- get all entries from 
getHostEntriesWith :: (BackendEntry -> Filter) -> [Attr] -> Mod Search -> BackendEntry
                   -> IO [SearchEntry]
getHostEntriesWith filter attrs mods entry = do
    results <- Ldap.with
      (Ldap.Plain (T.unpack . host $ entry)) (port entry) $ \l ->
        Ldap.search l
                    (base entry)
                    mods
                    (filter entry)
                    attrs
    case results of
      Left e -> return []
      Right r -> return r

collapseHostEntries :: Dn -> BackendEntry -> [SearchEntry] -> [CollapsedEntry]
collapseHostEntries collapsedBase entry =
  fmap (collapseEntry collapsedBase entry)

-- TODO make this more pretty!
listAllHosts :: Conf -> IO [CollapsedEntry]
listAllHosts = listHostsWith' CC.filter (scope WholeSubtree) attributesForward

listHostsWith :: Maybe Filter
              -> Maybe (Mod Search)
              -> [Attr]
              -> Conf
              -> IO [CollapsedEntry]
listHostsWith filter mods attrs' = listHostsWith'
    (case filter of
      Nothing -> CC.filter
      Just f  -> \h -> And . NE.fromList $ [f, (CC.filter h)]
    )
    (fromMaybe (scope WholeSubtree) mods)
    (makeGetAttrs attrs')

makeGetAttrs :: [Attr] -> Conf -> [Attr]
makeGetAttrs [] = attributesForward
makeGetAttrs [Attr "all"] = attributesForward
makeGetAttrs attrs = L.filter (`L.elem` attrs) . attributesForward

listHostsWith' :: (BackendEntry -> Filter)
               -> Mod Search
               -> (Conf -> [Attr])
               -> Conf
               -> IO [CollapsedEntry]
listHostsWith' filter mods getAttrs conf = do
    let cdn = collapsedBase conf
        attrs = getAttrs conf
    entries <- mapM (\h -> (collapseHostEntries cdn h . fmap appendAttrs)
                       <$> getHostEntriesWith filter attrs mods h)
            $ backends conf
    return . join $ entries
  where
    appendAttrs :: SearchEntry -> SearchEntry
    -- If there are no attributes, do not append because the user did not
    -- request any attributes.
    -- TODO: If the user does not request any attributes but still expects
    -- attributes to be added, this could lead to problems. In that case, we
    -- need some refactoring.
    appendAttrs entry@(SearchEntry _ []) = entry
    appendAttrs (SearchEntry dn attrs) = SearchEntry dn
                                         (attrs ++ attributesAppend conf)

-- Get entry where the Dn matches the collapsed Dn
getEntry :: Dn -> [CollapsedEntry] -> Maybe CollapsedEntry
getEntry dn entries | not (null matches) = Just $ head matches
                    | otherwise = Nothing
  where
    matches = L.filter (\e -> (getDn . collapsed $ e) == dn) entries

makeUnique :: [CollapsedEntry] -> [CollapsedEntry]
makeUnique = snd . L.foldl' go (HS.empty, [])
  where
    go :: (HS.HashSet T.Text, [CollapsedEntry])
       -> CollapsedEntry
       -> (HS.HashSet T.Text, [CollapsedEntry])
    go (set, entries) entry
        -- no duplicate entries
        | entryDn `HS.member` set = (set, entries)
        | otherwise = (entryDn `HS.insert` set, entry:entries)
      where
        entryDn :: T.Text
        entryDn = entryToDn entry

        entryToDn :: CollapsedEntry -> T.Text
        entryToDn = unDn . getDn . collapsed
