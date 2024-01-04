{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import CVPartner.Model
import CVPartner.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary Certification where
  arbitrary = sized genCertification

genCertification :: Int -> Gen Certification
genCertification n =
  Certification
    <$> arbitraryReducedMaybe n -- certificationOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- certificationStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- certificationDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- certificationVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- certificationExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- certificationOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- certificationCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- certificationUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- certificationName :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- certificationLongDescription :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- certificationOrganizer :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- certificationYear :: Maybe Text
    <*> arbitraryReducedMaybe n -- certificationMonth :: Maybe Text
    <*> arbitraryReducedMaybe n -- certificationYearExpire :: Maybe Text
    <*> arbitraryReducedMaybe n -- certificationMonthExpire :: Maybe Text
  
instance Arbitrary Country where
  arbitrary = sized genCountry

genCountry :: Int -> Gen Country
genCountry n =
  Country
    <$> arbitrary -- countryId :: Text
    <*> arbitrary -- countryCode :: Text
    <*> arbitraryReduced n -- countryOffices :: [Office]
  
instance Arbitrary Course where
  arbitrary = sized genCourse

genCourse :: Int -> Gen Course
genCourse n =
  Course
    <$> arbitraryReducedMaybe n -- courseOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- courseStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- courseDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- courseVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- courseExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- courseOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- courseCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- courseUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- courseName :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- courseLongDescription :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- courseProgram :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- courseYear :: Maybe Text
    <*> arbitraryReducedMaybe n -- courseMonth :: Maybe Text
  
instance Arbitrary Cv where
  arbitrary = sized genCv

genCv :: Int -> Gen Cv
genCv n =
  Cv
    <$> arbitraryReducedMaybe n -- cvId :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvCompanyId :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvName :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- cvTitle :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- cvEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvBornDay :: Maybe Int
    <*> arbitraryReducedMaybe n -- cvBornMonth :: Maybe Int
    <*> arbitraryReducedMaybe n -- cvBornYear :: Maybe Int
    <*> arbitraryReducedMaybeValue n -- cvNationality :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- cvPlaceOfResidence :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- cvEducations :: Maybe [Education]
    <*> arbitraryReducedMaybe n -- cvKeyQualifications :: Maybe [KeyQualification]
    <*> arbitraryReducedMaybe n -- cvProjectExperiences :: Maybe [ProjectExperience]
    <*> arbitraryReducedMaybe n -- cvWorkExperiences :: Maybe [WorkExperience]
    <*> arbitraryReducedMaybe n -- cvLanguages :: Maybe [Language]
    <*> arbitraryReducedMaybe n -- cvTechnologies :: Maybe [Technology]
    <*> arbitraryReducedMaybe n -- cvCertifications :: Maybe [Certification]
    <*> arbitraryReducedMaybe n -- cvCourses :: Maybe [Course]
    <*> arbitraryReducedMaybe n -- cvPresentations :: Maybe [Presentation]
    <*> arbitraryReducedMaybe n -- cvRecommendations :: Maybe [Recommendation]
    <*> arbitraryReducedMaybe n -- cvPositions :: Maybe [Position]
    <*> arbitraryReducedMaybe n -- cvImage :: Maybe UserImage
  
instance Arbitrary CvList where
  arbitrary = sized genCvList

genCvList :: Int -> Gen CvList
genCvList n =
  CvList
    <$> arbitrary -- cvListTotal :: Double
    <*> arbitraryReduced n -- cvListCvs :: [CvListEntry]
  
instance Arbitrary CvListEntry where
  arbitrary = sized genCvListEntry

genCvListEntry :: Int -> Gen CvListEntry
genCvListEntry n =
  CvListEntry
    <$> arbitrary -- cvListEntryPreviewUrl :: Text
    <*> arbitraryReduced n -- cvListEntryCv :: CvSummary
  
instance Arbitrary CvSection where
  arbitrary = sized genCvSection

genCvSection :: Int -> Gen CvSection
genCvSection n =
  CvSection
    <$> arbitraryReducedMaybe n -- cvSectionKeyQualification :: Maybe KeyQualification
    <*> arbitraryReducedMaybe n -- cvSectionEducation :: Maybe Education
    <*> arbitraryReducedMaybe n -- cvSectionWorkExperience :: Maybe WorkExperience
  
instance Arbitrary CvSectionProperties where
  arbitrary = sized genCvSectionProperties

genCvSectionProperties :: Int -> Gen CvSectionProperties
genCvSectionProperties n =
  CvSectionProperties
    <$> arbitraryReducedMaybe n -- cvSectionPropertiesOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- cvSectionPropertiesStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- cvSectionPropertiesDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- cvSectionPropertiesVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- cvSectionPropertiesExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSectionPropertiesOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSectionPropertiesCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSectionPropertiesUpdatedAt :: Maybe Text
  
instance Arbitrary CvSummary where
  arbitrary = sized genCvSummary

genCvSummary :: Int -> Gen CvSummary
genCvSummary n =
  CvSummary
    <$> arbitrary -- cvSummaryId :: Text
    <*> arbitraryReducedMaybe n -- cvSummaryUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSummaryName :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- cvSummaryImage :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- cvSummaryTitle :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- cvSummaryTitles :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- cvSummaryEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSummaryCompanyId :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSummaryUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSummaryOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSummaryCountryCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSummaryLanguageCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- cvSummaryLanguageCodes :: Maybe [Text]
  
instance Arbitrary Education where
  arbitrary = sized genEducation

genEducation :: Int -> Gen Education
genEducation n =
  Education
    <$> arbitraryReducedMaybe n -- educationOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- educationStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- educationDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- educationVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- educationExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- educationOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- educationCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- educationUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- educationDegree :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- educationDescription :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- educationYearFrom :: Maybe Text
    <*> arbitraryReducedMaybe n -- educationMonthFrom :: Maybe Text
    <*> arbitraryReducedMaybe n -- educationYearTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- educationMonthTo :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- educationSchool :: Maybe A.Value
  
instance Arbitrary ImageUrl where
  arbitrary = sized genImageUrl

genImageUrl :: Int -> Gen ImageUrl
genImageUrl n =
  ImageUrl
    <$> arbitraryReducedMaybe n -- imageUrlUrl :: Maybe Text
  
instance Arbitrary KeyQualification where
  arbitrary = sized genKeyQualification

genKeyQualification :: Int -> Gen KeyQualification
genKeyQualification n =
  KeyQualification
    <$> arbitraryReducedMaybe n -- keyQualificationOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- keyQualificationStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- keyQualificationDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- keyQualificationVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- keyQualificationExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- keyQualificationOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- keyQualificationCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- keyQualificationUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- keyQualificationRecentlyAdded :: Maybe Bool
    <*> arbitraryReducedMaybeValue n -- keyQualificationLabel :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- keyQualificationLongDescription :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- keyQualificationTagLine :: Maybe A.Value
  
instance Arbitrary Language where
  arbitrary = sized genLanguage

genLanguage :: Int -> Gen Language
genLanguage n =
  Language
    <$> arbitraryReducedMaybe n -- languageOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- languageStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- languageDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- languageVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- languageExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- languageOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- languageCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- languageUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- languageLevel :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- languageName :: Maybe A.Value
  
instance Arbitrary Office where
  arbitrary = sized genOffice

genOffice :: Int -> Gen Office
genOffice n =
  Office
    <$> arbitrary -- officeId :: Text
    <*> arbitrary -- officeName :: Text
  
instance Arbitrary Position where
  arbitrary = sized genPosition

genPosition :: Int -> Gen Position
genPosition n =
  Position
    <$> arbitraryReducedMaybe n -- positionOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- positionStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- positionDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- positionVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- positionExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- positionOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- positionCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- positionUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- positionName :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- positionDescription :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- positionYearFrom :: Maybe Text
    <*> arbitraryReducedMaybe n -- positionYearTo :: Maybe Text
  
instance Arbitrary Presentation where
  arbitrary = sized genPresentation

genPresentation :: Int -> Gen Presentation
genPresentation n =
  Presentation
    <$> arbitraryReducedMaybe n -- presentationOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- presentationStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- presentationDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- presentationVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- presentationExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- presentationOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- presentationCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- presentationUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- presentationDescription :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- presentationLongDescription :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- presentationYear :: Maybe Text
    <*> arbitraryReducedMaybe n -- presentationMonth :: Maybe Text
  
instance Arbitrary ProjectExperience where
  arbitrary = sized genProjectExperience

genProjectExperience :: Int -> Gen ProjectExperience
genProjectExperience n =
  ProjectExperience
    <$> arbitraryReducedMaybe n -- projectExperienceOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- projectExperienceStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectExperienceDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectExperienceVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- projectExperienceExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectExperienceOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectExperienceCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectExperienceUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectExperienceAreaAmt :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectExperienceAreaUnit :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- projectExperienceCustomer :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- projectExperienceCustomerAnonymized :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- projectExperienceCustomerDescription :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- projectExperienceDescription :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- projectExperienceLongDescription :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- projectExperienceIndustry :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- projectExperienceYearFrom :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectExperienceMonthFrom :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectExperienceYearTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectExperienceMonthTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectExperienceRoles :: Maybe [ProjectRole]
  
instance Arbitrary ProjectRole where
  arbitrary = sized genProjectRole

genProjectRole :: Int -> Gen ProjectRole
genProjectRole n =
  ProjectRole
    <$> arbitraryReducedMaybe n -- projectRoleOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- projectRoleStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectRoleDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- projectRoleVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- projectRoleExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRoleOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRoleCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRoleUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRoleCvRoleId :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectRoleDivergedFromMaster :: Maybe Bool
    <*> arbitraryReducedMaybeValue n -- projectRoleName :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- projectRoleSummary :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- projectRoleLongDescription :: Maybe A.Value
  
instance Arbitrary Recommendation where
  arbitrary = sized genRecommendation

genRecommendation :: Int -> Gen Recommendation
genRecommendation n =
  Recommendation
    <$> arbitraryReducedMaybe n -- recommendationOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- recommendationStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- recommendationDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- recommendationVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- recommendationExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- recommendationOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- recommendationCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- recommendationUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- recommendationDescription :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- recommendationRecommender :: Maybe A.Value
  
instance Arbitrary SearchByNameReq where
  arbitrary = sized genSearchByNameReq

genSearchByNameReq :: Int -> Gen SearchByNameReq
genSearchByNameReq n =
  SearchByNameReq
    <$> arbitraryReducedMaybe n -- searchByNameReqOfficeIds :: Maybe [Text]
    <*> arbitrary -- searchByNameReqOffset :: Double
    <*> arbitrary -- searchByNameReqSize :: Double
    <*> arbitraryReduced n -- searchByNameReqMust :: A.Value
  
instance Arbitrary Technology where
  arbitrary = sized genTechnology

genTechnology :: Int -> Gen Technology
genTechnology n =
  Technology
    <$> arbitraryReducedMaybe n -- technologyOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- technologyStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- technologyDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- technologyVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- technologyExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- technologyOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- technologyCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- technologyUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- technologyUncategorized :: Maybe Bool
    <*> arbitraryReducedMaybeValue n -- technologyCategory :: Maybe A.Value
  
instance Arbitrary User where
  arbitrary = sized genUser

genUser :: Int -> Gen User
genUser n =
  User
    <$> arbitrary -- userUserId :: Text
    <*> arbitrary -- userId :: Text
    <*> arbitraryReducedMaybe n -- userEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userUpn :: Maybe Text
    <*> arbitraryReducedMaybe n -- userName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTelephone :: Maybe Text
    <*> arbitraryReducedMaybe n -- userDefaultCvId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userDeactivated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userDeactivatedAt :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- userRole :: Maybe Text
    <*> arbitraryReducedMaybe n -- userExtraRoles :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userOfficeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userOfficeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCountryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCountryCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLanguageCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- userImage :: Maybe UserImage
  
instance Arbitrary UserImage where
  arbitrary = sized genUserImage

genUserImage :: Int -> Gen UserImage
genUserImage n =
  UserImage
    <$> arbitraryReducedMaybe n -- userImageUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- userImageThumb :: Maybe ImageUrl
    <*> arbitraryReducedMaybe n -- userImageFitThumb :: Maybe ImageUrl
    <*> arbitraryReducedMaybe n -- userImageLarge :: Maybe ImageUrl
    <*> arbitraryReducedMaybe n -- userImageSmallThumb :: Maybe ImageUrl
  
instance Arbitrary WorkExperience where
  arbitrary = sized genWorkExperience

genWorkExperience :: Int -> Gen WorkExperience
genWorkExperience n =
  WorkExperience
    <$> arbitraryReducedMaybe n -- workExperienceOrder :: Maybe Int
    <*> arbitraryReducedMaybe n -- workExperienceStarred :: Maybe Bool
    <*> arbitraryReducedMaybe n -- workExperienceDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- workExperienceVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- workExperienceExternalUniqueId :: Maybe Text
    <*> arbitraryReducedMaybe n -- workExperienceOwnerUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- workExperienceCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- workExperienceUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- workExperienceEmployer :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- workExperienceDescription :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- workExperienceLongDescription :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- workExperienceYearFrom :: Maybe Text
    <*> arbitraryReducedMaybe n -- workExperienceMonthFrom :: Maybe Text
    <*> arbitraryReducedMaybe n -- workExperienceYearTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- workExperienceMonthTo :: Maybe Text
  



