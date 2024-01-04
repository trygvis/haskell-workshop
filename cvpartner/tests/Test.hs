{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import CVPartner.Model
import CVPartner.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy Certification)
      propMimeEq MimeJSON (Proxy :: Proxy Country)
      propMimeEq MimeJSON (Proxy :: Proxy Course)
      propMimeEq MimeJSON (Proxy :: Proxy Cv)
      propMimeEq MimeJSON (Proxy :: Proxy CvList)
      propMimeEq MimeJSON (Proxy :: Proxy CvListEntry)
      propMimeEq MimeJSON (Proxy :: Proxy CvSection)
      propMimeEq MimeJSON (Proxy :: Proxy CvSectionProperties)
      propMimeEq MimeJSON (Proxy :: Proxy CvSummary)
      propMimeEq MimeJSON (Proxy :: Proxy Education)
      propMimeEq MimeJSON (Proxy :: Proxy ImageUrl)
      propMimeEq MimeJSON (Proxy :: Proxy KeyQualification)
      propMimeEq MimeJSON (Proxy :: Proxy Language)
      propMimeEq MimeJSON (Proxy :: Proxy Office)
      propMimeEq MimeJSON (Proxy :: Proxy Position)
      propMimeEq MimeJSON (Proxy :: Proxy Presentation)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectExperience)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectRole)
      propMimeEq MimeJSON (Proxy :: Proxy Recommendation)
      propMimeEq MimeJSON (Proxy :: Proxy SearchByNameReq)
      propMimeEq MimeJSON (Proxy :: Proxy Technology)
      propMimeEq MimeJSON (Proxy :: Proxy User)
      propMimeEq MimeJSON (Proxy :: Proxy UserImage)
      propMimeEq MimeJSON (Proxy :: Proxy WorkExperience)
      
