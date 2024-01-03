{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module CVPartner.Types (
  Certification (..),
  Country (..),
  Course (..),
  Cv (..),
  CvList (..),
  CvListEntry (..),
  CvSection (..),
  CvSectionProperties (..),
  CvSummary (..),
  Education (..),
  ImageUrl (..),
  KeyQualification (..),
  Language (..),
  Office (..),
  Position (..),
  Presentation (..),
  ProjectExperience (..),
  ProjectRole (..),
  Recommendation (..),
  SearchByNameReq (..),
  Technology (..),
  User (..),
  UserImage (..),
  WorkExperience (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)


-- | 
data Certification = Certification
  { certificationOrder :: Maybe Int -- ^ 
  , certificationStarred :: Maybe Bool -- ^ 
  , certificationDisabled :: Maybe Bool -- ^ 
  , certificationVersion :: Maybe Int -- ^ 
  , certificationExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , certificationOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , certificationCreatedUnderscoreat :: Maybe Text -- ^ 
  , certificationUpdatedUnderscoreat :: Maybe Text -- ^ 
  , certificationName :: Maybe Value -- ^ 
  , certificationLongUnderscoredescription :: Maybe Value -- ^ 
  , certificationOrganizer :: Maybe Value -- ^ 
  , certificationYear :: Maybe Text -- ^ 
  , certificationMonth :: Maybe Text -- ^ 
  , certificationYearUnderscoreexpire :: Maybe Text -- ^ 
  , certificationMonthUnderscoreexpire :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Certification where
  parseJSON = genericParseJSON optionsCertification
instance ToJSON Certification where
  toJSON = genericToJSON optionsCertification

optionsCertification :: Options
optionsCertification =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("certificationOrder", "order")
      , ("certificationStarred", "starred")
      , ("certificationDisabled", "disabled")
      , ("certificationVersion", "version")
      , ("certificationExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("certificationOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("certificationCreatedUnderscoreat", "created_at")
      , ("certificationUpdatedUnderscoreat", "updated_at")
      , ("certificationName", "name")
      , ("certificationLongUnderscoredescription", "long_description")
      , ("certificationOrganizer", "organizer")
      , ("certificationYear", "year")
      , ("certificationMonth", "month")
      , ("certificationYearUnderscoreexpire", "year_expire")
      , ("certificationMonthUnderscoreexpire", "month_expire")
      ]


-- | 
data Country = Country
  { countryUnderscoreid :: Text -- ^ 
  , countryCode :: Text -- ^ 
  , countryOffices :: [Office] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Country where
  parseJSON = genericParseJSON optionsCountry
instance ToJSON Country where
  toJSON = genericToJSON optionsCountry

optionsCountry :: Options
optionsCountry =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("countryUnderscoreid", "_id")
      , ("countryCode", "code")
      , ("countryOffices", "offices")
      ]


-- | 
data Course = Course
  { courseOrder :: Maybe Int -- ^ 
  , courseStarred :: Maybe Bool -- ^ 
  , courseDisabled :: Maybe Bool -- ^ 
  , courseVersion :: Maybe Int -- ^ 
  , courseExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , courseOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , courseCreatedUnderscoreat :: Maybe Text -- ^ 
  , courseUpdatedUnderscoreat :: Maybe Text -- ^ 
  , courseName :: Maybe Value -- ^ 
  , courseLongUnderscoredescription :: Maybe Value -- ^ 
  , courseProgram :: Maybe Value -- ^ 
  , courseYear :: Maybe Text -- ^ 
  , courseMonth :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Course where
  parseJSON = genericParseJSON optionsCourse
instance ToJSON Course where
  toJSON = genericToJSON optionsCourse

optionsCourse :: Options
optionsCourse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("courseOrder", "order")
      , ("courseStarred", "starred")
      , ("courseDisabled", "disabled")
      , ("courseVersion", "version")
      , ("courseExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("courseOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("courseCreatedUnderscoreat", "created_at")
      , ("courseUpdatedUnderscoreat", "updated_at")
      , ("courseName", "name")
      , ("courseLongUnderscoredescription", "long_description")
      , ("courseProgram", "program")
      , ("courseYear", "year")
      , ("courseMonth", "month")
      ]


-- | 
data Cv = Cv
  { cvId :: Maybe Text -- ^ 
  , cvUserUnderscoreid :: Maybe Text -- ^ 
  , cvCompanyUnderscoreid :: Maybe Text -- ^ 
  , cvName :: Maybe Text -- ^ 
  , cvTitle :: Maybe Value -- ^ 
  , cvEmail :: Maybe Text -- ^ 
  , cvBornUnderscoreday :: Maybe Int -- ^ 
  , cvBornUnderscoremonth :: Maybe Int -- ^ 
  , cvBornUnderscoreyear :: Maybe Int -- ^ 
  , cvNationality :: Maybe Value -- ^ 
  , cvPlaceUnderscoreofUnderscoreresidence :: Maybe Value -- ^ 
  , cvEducations :: Maybe [Education] -- ^ 
  , cvKeyUnderscorequalifications :: Maybe [KeyQualification] -- ^ 
  , cvProjectUnderscoreexperiences :: Maybe [ProjectExperience] -- ^ 
  , cvWorkUnderscoreexperiences :: Maybe [WorkExperience] -- ^ 
  , cvLanguages :: Maybe [Language] -- ^ 
  , cvTechnologies :: Maybe [Technology] -- ^ 
  , cvCertifications :: Maybe [Certification] -- ^ 
  , cvCourses :: Maybe [Course] -- ^ 
  , cvPresentations :: Maybe [Presentation] -- ^ 
  , cvRecommendations :: Maybe [Recommendation] -- ^ 
  , cvPositions :: Maybe [Position] -- ^ 
  , cvImage :: Maybe UserImage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Cv where
  parseJSON = genericParseJSON optionsCv
instance ToJSON Cv where
  toJSON = genericToJSON optionsCv

optionsCv :: Options
optionsCv =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cvId", "id")
      , ("cvUserUnderscoreid", "user_id")
      , ("cvCompanyUnderscoreid", "company_id")
      , ("cvName", "name")
      , ("cvTitle", "title")
      , ("cvEmail", "email")
      , ("cvBornUnderscoreday", "born_day")
      , ("cvBornUnderscoremonth", "born_month")
      , ("cvBornUnderscoreyear", "born_year")
      , ("cvNationality", "nationality")
      , ("cvPlaceUnderscoreofUnderscoreresidence", "place_of_residence")
      , ("cvEducations", "educations")
      , ("cvKeyUnderscorequalifications", "key_qualifications")
      , ("cvProjectUnderscoreexperiences", "project_experiences")
      , ("cvWorkUnderscoreexperiences", "work_experiences")
      , ("cvLanguages", "languages")
      , ("cvTechnologies", "technologies")
      , ("cvCertifications", "certifications")
      , ("cvCourses", "courses")
      , ("cvPresentations", "presentations")
      , ("cvRecommendations", "recommendations")
      , ("cvPositions", "positions")
      , ("cvImage", "image")
      ]


-- | 
data CvList = CvList
  { cvListTotal :: Double -- ^ 
  , cvListCvs :: [CvListEntry] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CvList where
  parseJSON = genericParseJSON optionsCvList
instance ToJSON CvList where
  toJSON = genericToJSON optionsCvList

optionsCvList :: Options
optionsCvList =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cvListTotal", "total")
      , ("cvListCvs", "cvs")
      ]


-- | 
data CvListEntry = CvListEntry
  { cvListEntryPreviewUnderscoreurl :: Text -- ^ 
  , cvListEntryCv :: CvSummary -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CvListEntry where
  parseJSON = genericParseJSON optionsCvListEntry
instance ToJSON CvListEntry where
  toJSON = genericToJSON optionsCvListEntry

optionsCvListEntry :: Options
optionsCvListEntry =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cvListEntryPreviewUnderscoreurl", "preview_url")
      , ("cvListEntryCv", "cv")
      ]


-- | 
data CvSection = CvSection
  { cvSectionKeyUnderscorequalification :: Maybe KeyQualification -- ^ 
  , cvSectionEducation :: Maybe Education -- ^ 
  , cvSectionWorkUnderscoreexperience :: Maybe WorkExperience -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CvSection where
  parseJSON = genericParseJSON optionsCvSection
instance ToJSON CvSection where
  toJSON = genericToJSON optionsCvSection

optionsCvSection :: Options
optionsCvSection =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cvSectionKeyUnderscorequalification", "key_qualification")
      , ("cvSectionEducation", "education")
      , ("cvSectionWorkUnderscoreexperience", "work_experience")
      ]


-- | 
data CvSectionProperties = CvSectionProperties
  { cvSectionPropertiesOrder :: Maybe Int -- ^ 
  , cvSectionPropertiesStarred :: Maybe Bool -- ^ 
  , cvSectionPropertiesDisabled :: Maybe Bool -- ^ 
  , cvSectionPropertiesVersion :: Maybe Int -- ^ 
  , cvSectionPropertiesExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , cvSectionPropertiesOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , cvSectionPropertiesCreatedUnderscoreat :: Maybe Text -- ^ 
  , cvSectionPropertiesUpdatedUnderscoreat :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CvSectionProperties where
  parseJSON = genericParseJSON optionsCvSectionProperties
instance ToJSON CvSectionProperties where
  toJSON = genericToJSON optionsCvSectionProperties

optionsCvSectionProperties :: Options
optionsCvSectionProperties =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cvSectionPropertiesOrder", "order")
      , ("cvSectionPropertiesStarred", "starred")
      , ("cvSectionPropertiesDisabled", "disabled")
      , ("cvSectionPropertiesVersion", "version")
      , ("cvSectionPropertiesExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("cvSectionPropertiesOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("cvSectionPropertiesCreatedUnderscoreat", "created_at")
      , ("cvSectionPropertiesUpdatedUnderscoreat", "updated_at")
      ]


-- | 
data CvSummary = CvSummary
  { cvSummaryId :: Text -- ^ 
  , cvSummaryUserUnderscoreid :: Maybe Text -- ^ 
  , cvSummaryName :: Maybe Text -- ^ 
  , cvSummaryImage :: Maybe Value -- ^ 
  , cvSummaryTitle :: Maybe Text -- ^ 
  , cvSummaryTitles :: Maybe Value -- ^ 
  , cvSummaryEmail :: Maybe Text -- ^ 
  , cvSummaryCompanyUnderscoreid :: Maybe Text -- ^ 
  , cvSummaryUpdatedUnderscoreat :: Maybe Text -- ^ 
  , cvSummaryOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , cvSummaryCountryUnderscorecode :: Maybe Text -- ^ 
  , cvSummaryLanguageUnderscorecode :: Maybe Text -- ^ 
  , cvSummaryLanguageUnderscorecodes :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CvSummary where
  parseJSON = genericParseJSON optionsCvSummary
instance ToJSON CvSummary where
  toJSON = genericToJSON optionsCvSummary

optionsCvSummary :: Options
optionsCvSummary =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cvSummaryId", "id")
      , ("cvSummaryUserUnderscoreid", "user_id")
      , ("cvSummaryName", "name")
      , ("cvSummaryImage", "image")
      , ("cvSummaryTitle", "title")
      , ("cvSummaryTitles", "titles")
      , ("cvSummaryEmail", "email")
      , ("cvSummaryCompanyUnderscoreid", "company_id")
      , ("cvSummaryUpdatedUnderscoreat", "updated_at")
      , ("cvSummaryOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("cvSummaryCountryUnderscorecode", "country_code")
      , ("cvSummaryLanguageUnderscorecode", "language_code")
      , ("cvSummaryLanguageUnderscorecodes", "language_codes")
      ]


-- | 
data Education = Education
  { educationOrder :: Maybe Int -- ^ 
  , educationStarred :: Maybe Bool -- ^ 
  , educationDisabled :: Maybe Bool -- ^ 
  , educationVersion :: Maybe Int -- ^ 
  , educationExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , educationOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , educationCreatedUnderscoreat :: Maybe Text -- ^ 
  , educationUpdatedUnderscoreat :: Maybe Text -- ^ 
  , educationDegree :: Maybe Value -- ^ 
  , educationDescription :: Maybe Value -- ^ 
  , educationYearUnderscorefrom :: Maybe Text -- ^ 
  , educationMonthUnderscorefrom :: Maybe Text -- ^ 
  , educationYearUnderscoreto :: Maybe Text -- ^ 
  , educationMonthUnderscoreto :: Maybe Text -- ^ 
  , educationSchool :: Maybe Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Education where
  parseJSON = genericParseJSON optionsEducation
instance ToJSON Education where
  toJSON = genericToJSON optionsEducation

optionsEducation :: Options
optionsEducation =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("educationOrder", "order")
      , ("educationStarred", "starred")
      , ("educationDisabled", "disabled")
      , ("educationVersion", "version")
      , ("educationExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("educationOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("educationCreatedUnderscoreat", "created_at")
      , ("educationUpdatedUnderscoreat", "updated_at")
      , ("educationDegree", "degree")
      , ("educationDescription", "description")
      , ("educationYearUnderscorefrom", "year_from")
      , ("educationMonthUnderscorefrom", "month_from")
      , ("educationYearUnderscoreto", "year_to")
      , ("educationMonthUnderscoreto", "month_to")
      , ("educationSchool", "school")
      ]


-- | 
data ImageUrl = ImageUrl
  { imageUrlUrl :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImageUrl where
  parseJSON = genericParseJSON optionsImageUrl
instance ToJSON ImageUrl where
  toJSON = genericToJSON optionsImageUrl

optionsImageUrl :: Options
optionsImageUrl =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("imageUrlUrl", "url")
      ]


-- | 
data KeyQualification = KeyQualification
  { keyQualificationOrder :: Maybe Int -- ^ 
  , keyQualificationStarred :: Maybe Bool -- ^ 
  , keyQualificationDisabled :: Maybe Bool -- ^ 
  , keyQualificationVersion :: Maybe Int -- ^ 
  , keyQualificationExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , keyQualificationOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , keyQualificationCreatedUnderscoreat :: Maybe Text -- ^ 
  , keyQualificationUpdatedUnderscoreat :: Maybe Text -- ^ 
  , keyQualificationRecentlyUnderscoreadded :: Maybe Bool -- ^ 
  , keyQualificationLabel :: Maybe Value -- ^ 
  , keyQualificationLongUnderscoredescription :: Maybe Value -- ^ 
  , keyQualificationTagUnderscoreline :: Maybe Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON KeyQualification where
  parseJSON = genericParseJSON optionsKeyQualification
instance ToJSON KeyQualification where
  toJSON = genericToJSON optionsKeyQualification

optionsKeyQualification :: Options
optionsKeyQualification =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("keyQualificationOrder", "order")
      , ("keyQualificationStarred", "starred")
      , ("keyQualificationDisabled", "disabled")
      , ("keyQualificationVersion", "version")
      , ("keyQualificationExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("keyQualificationOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("keyQualificationCreatedUnderscoreat", "created_at")
      , ("keyQualificationUpdatedUnderscoreat", "updated_at")
      , ("keyQualificationRecentlyUnderscoreadded", "recently_added")
      , ("keyQualificationLabel", "label")
      , ("keyQualificationLongUnderscoredescription", "long_description")
      , ("keyQualificationTagUnderscoreline", "tag_line")
      ]


-- | 
data Language = Language
  { languageOrder :: Maybe Int -- ^ 
  , languageStarred :: Maybe Bool -- ^ 
  , languageDisabled :: Maybe Bool -- ^ 
  , languageVersion :: Maybe Int -- ^ 
  , languageExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , languageOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , languageCreatedUnderscoreat :: Maybe Text -- ^ 
  , languageUpdatedUnderscoreat :: Maybe Text -- ^ 
  , languageLevel :: Maybe Value -- ^ 
  , languageName :: Maybe Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Language where
  parseJSON = genericParseJSON optionsLanguage
instance ToJSON Language where
  toJSON = genericToJSON optionsLanguage

optionsLanguage :: Options
optionsLanguage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("languageOrder", "order")
      , ("languageStarred", "starred")
      , ("languageDisabled", "disabled")
      , ("languageVersion", "version")
      , ("languageExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("languageOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("languageCreatedUnderscoreat", "created_at")
      , ("languageUpdatedUnderscoreat", "updated_at")
      , ("languageLevel", "level")
      , ("languageName", "name")
      ]


-- | 
data Office = Office
  { officeUnderscoreid :: Text -- ^ 
  , officeName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Office where
  parseJSON = genericParseJSON optionsOffice
instance ToJSON Office where
  toJSON = genericToJSON optionsOffice

optionsOffice :: Options
optionsOffice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("officeUnderscoreid", "_id")
      , ("officeName", "name")
      ]


-- | 
data Position = Position
  { positionOrder :: Maybe Int -- ^ 
  , positionStarred :: Maybe Bool -- ^ 
  , positionDisabled :: Maybe Bool -- ^ 
  , positionVersion :: Maybe Int -- ^ 
  , positionExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , positionOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , positionCreatedUnderscoreat :: Maybe Text -- ^ 
  , positionUpdatedUnderscoreat :: Maybe Text -- ^ 
  , positionName :: Maybe Value -- ^ 
  , positionDescription :: Maybe Value -- ^ 
  , positionYearUnderscorefrom :: Maybe Text -- ^ 
  , positionYearUnderscoreto :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Position where
  parseJSON = genericParseJSON optionsPosition
instance ToJSON Position where
  toJSON = genericToJSON optionsPosition

optionsPosition :: Options
optionsPosition =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("positionOrder", "order")
      , ("positionStarred", "starred")
      , ("positionDisabled", "disabled")
      , ("positionVersion", "version")
      , ("positionExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("positionOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("positionCreatedUnderscoreat", "created_at")
      , ("positionUpdatedUnderscoreat", "updated_at")
      , ("positionName", "name")
      , ("positionDescription", "description")
      , ("positionYearUnderscorefrom", "year_from")
      , ("positionYearUnderscoreto", "year_to")
      ]


-- | 
data Presentation = Presentation
  { presentationOrder :: Maybe Int -- ^ 
  , presentationStarred :: Maybe Bool -- ^ 
  , presentationDisabled :: Maybe Bool -- ^ 
  , presentationVersion :: Maybe Int -- ^ 
  , presentationExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , presentationOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , presentationCreatedUnderscoreat :: Maybe Text -- ^ 
  , presentationUpdatedUnderscoreat :: Maybe Text -- ^ 
  , presentationDescription :: Maybe Value -- ^ 
  , presentationLongUnderscoredescription :: Maybe Value -- ^ 
  , presentationYear :: Maybe Text -- ^ 
  , presentationMonth :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Presentation where
  parseJSON = genericParseJSON optionsPresentation
instance ToJSON Presentation where
  toJSON = genericToJSON optionsPresentation

optionsPresentation :: Options
optionsPresentation =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("presentationOrder", "order")
      , ("presentationStarred", "starred")
      , ("presentationDisabled", "disabled")
      , ("presentationVersion", "version")
      , ("presentationExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("presentationOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("presentationCreatedUnderscoreat", "created_at")
      , ("presentationUpdatedUnderscoreat", "updated_at")
      , ("presentationDescription", "description")
      , ("presentationLongUnderscoredescription", "long_description")
      , ("presentationYear", "year")
      , ("presentationMonth", "month")
      ]


-- | 
data ProjectExperience = ProjectExperience
  { projectExperienceOrder :: Maybe Int -- ^ 
  , projectExperienceStarred :: Maybe Bool -- ^ 
  , projectExperienceDisabled :: Maybe Bool -- ^ 
  , projectExperienceVersion :: Maybe Int -- ^ 
  , projectExperienceExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , projectExperienceOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , projectExperienceCreatedUnderscoreat :: Maybe Text -- ^ 
  , projectExperienceUpdatedUnderscoreat :: Maybe Text -- ^ 
  , projectExperienceAreaUnderscoreamt :: Maybe Text -- ^ 
  , projectExperienceAreaUnderscoreunit :: Maybe Text -- ^ 
  , projectExperienceCustomer :: Maybe Value -- ^ 
  , projectExperienceCustomerUnderscoreanonymized :: Maybe Value -- ^ 
  , projectExperienceCustomerUnderscoredescription :: Maybe Value -- ^ 
  , projectExperienceDescription :: Maybe Value -- ^ 
  , projectExperienceLongUnderscoredescription :: Maybe Value -- ^ 
  , projectExperienceIndustry :: Maybe Value -- ^ 
  , projectExperienceYearUnderscorefrom :: Maybe Text -- ^ 
  , projectExperienceMonthUnderscorefrom :: Maybe Text -- ^ 
  , projectExperienceYearUnderscoreto :: Maybe Text -- ^ 
  , projectExperienceMonthUnderscoreto :: Maybe Text -- ^ 
  , projectExperienceRoles :: Maybe [ProjectRole] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProjectExperience where
  parseJSON = genericParseJSON optionsProjectExperience
instance ToJSON ProjectExperience where
  toJSON = genericToJSON optionsProjectExperience

optionsProjectExperience :: Options
optionsProjectExperience =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("projectExperienceOrder", "order")
      , ("projectExperienceStarred", "starred")
      , ("projectExperienceDisabled", "disabled")
      , ("projectExperienceVersion", "version")
      , ("projectExperienceExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("projectExperienceOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("projectExperienceCreatedUnderscoreat", "created_at")
      , ("projectExperienceUpdatedUnderscoreat", "updated_at")
      , ("projectExperienceAreaUnderscoreamt", "area_amt")
      , ("projectExperienceAreaUnderscoreunit", "area_unit")
      , ("projectExperienceCustomer", "customer")
      , ("projectExperienceCustomerUnderscoreanonymized", "customer_anonymized")
      , ("projectExperienceCustomerUnderscoredescription", "customer_description")
      , ("projectExperienceDescription", "description")
      , ("projectExperienceLongUnderscoredescription", "long_description")
      , ("projectExperienceIndustry", "industry")
      , ("projectExperienceYearUnderscorefrom", "year_from")
      , ("projectExperienceMonthUnderscorefrom", "month_from")
      , ("projectExperienceYearUnderscoreto", "year_to")
      , ("projectExperienceMonthUnderscoreto", "month_to")
      , ("projectExperienceRoles", "roles")
      ]


-- | 
data ProjectRole = ProjectRole
  { projectRoleOrder :: Maybe Int -- ^ 
  , projectRoleStarred :: Maybe Bool -- ^ 
  , projectRoleDisabled :: Maybe Bool -- ^ 
  , projectRoleVersion :: Maybe Int -- ^ 
  , projectRoleExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , projectRoleOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , projectRoleCreatedUnderscoreat :: Maybe Text -- ^ 
  , projectRoleUpdatedUnderscoreat :: Maybe Text -- ^ 
  , projectRoleCvUnderscoreroleUnderscoreid :: Maybe Text -- ^ 
  , projectRoleDivergedUnderscorefromUnderscoremaster :: Maybe Bool -- ^ 
  , projectRoleName :: Maybe Value -- ^ 
  , projectRoleSummary :: Maybe Value -- ^ 
  , projectRoleLongUnderscoredescription :: Maybe Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProjectRole where
  parseJSON = genericParseJSON optionsProjectRole
instance ToJSON ProjectRole where
  toJSON = genericToJSON optionsProjectRole

optionsProjectRole :: Options
optionsProjectRole =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("projectRoleOrder", "order")
      , ("projectRoleStarred", "starred")
      , ("projectRoleDisabled", "disabled")
      , ("projectRoleVersion", "version")
      , ("projectRoleExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("projectRoleOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("projectRoleCreatedUnderscoreat", "created_at")
      , ("projectRoleUpdatedUnderscoreat", "updated_at")
      , ("projectRoleCvUnderscoreroleUnderscoreid", "cv_role_id")
      , ("projectRoleDivergedUnderscorefromUnderscoremaster", "diverged_from_master")
      , ("projectRoleName", "name")
      , ("projectRoleSummary", "summary")
      , ("projectRoleLongUnderscoredescription", "long_description")
      ]


-- | 
data Recommendation = Recommendation
  { recommendationOrder :: Maybe Int -- ^ 
  , recommendationStarred :: Maybe Bool -- ^ 
  , recommendationDisabled :: Maybe Bool -- ^ 
  , recommendationVersion :: Maybe Int -- ^ 
  , recommendationExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , recommendationOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , recommendationCreatedUnderscoreat :: Maybe Text -- ^ 
  , recommendationUpdatedUnderscoreat :: Maybe Text -- ^ 
  , recommendationDescription :: Maybe Value -- ^ 
  , recommendationRecommender :: Maybe Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Recommendation where
  parseJSON = genericParseJSON optionsRecommendation
instance ToJSON Recommendation where
  toJSON = genericToJSON optionsRecommendation

optionsRecommendation :: Options
optionsRecommendation =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("recommendationOrder", "order")
      , ("recommendationStarred", "starred")
      , ("recommendationDisabled", "disabled")
      , ("recommendationVersion", "version")
      , ("recommendationExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("recommendationOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("recommendationCreatedUnderscoreat", "created_at")
      , ("recommendationUpdatedUnderscoreat", "updated_at")
      , ("recommendationDescription", "description")
      , ("recommendationRecommender", "recommender")
      ]


-- | 
data SearchByNameReq = SearchByNameReq
  { searchByNameReqOfficeUnderscoreids :: Maybe [Text] -- ^ 
  , searchByNameReqOffset :: Double -- ^ 
  , searchByNameReqSize :: Double -- ^ 
  , searchByNameReqMust :: Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SearchByNameReq where
  parseJSON = genericParseJSON optionsSearchByNameReq
instance ToJSON SearchByNameReq where
  toJSON = genericToJSON optionsSearchByNameReq

optionsSearchByNameReq :: Options
optionsSearchByNameReq =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("searchByNameReqOfficeUnderscoreids", "office_ids")
      , ("searchByNameReqOffset", "offset")
      , ("searchByNameReqSize", "size")
      , ("searchByNameReqMust", "must")
      ]


-- | 
data Technology = Technology
  { technologyOrder :: Maybe Int -- ^ 
  , technologyStarred :: Maybe Bool -- ^ 
  , technologyDisabled :: Maybe Bool -- ^ 
  , technologyVersion :: Maybe Int -- ^ 
  , technologyExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , technologyOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , technologyCreatedUnderscoreat :: Maybe Text -- ^ 
  , technologyUpdatedUnderscoreat :: Maybe Text -- ^ 
  , technologyUncategorized :: Maybe Bool -- ^ 
  , technologyCategory :: Maybe Value -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Technology where
  parseJSON = genericParseJSON optionsTechnology
instance ToJSON Technology where
  toJSON = genericToJSON optionsTechnology

optionsTechnology :: Options
optionsTechnology =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("technologyOrder", "order")
      , ("technologyStarred", "starred")
      , ("technologyDisabled", "disabled")
      , ("technologyVersion", "version")
      , ("technologyExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("technologyOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("technologyCreatedUnderscoreat", "created_at")
      , ("technologyUpdatedUnderscoreat", "updated_at")
      , ("technologyUncategorized", "uncategorized")
      , ("technologyCategory", "category")
      ]


-- | 
data User = User
  { userUserUnderscoreid :: Text -- ^ 
  , userId :: Text -- ^ 
  , userEmail :: Maybe Text -- ^ 
  , userExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , userUpn :: Maybe Text -- ^ 
  , userName :: Maybe Text -- ^ 
  , userTelephone :: Maybe Text -- ^ 
  , userDefaultUnderscorecvUnderscoreid :: Maybe Text -- ^ 
  , userDeactivated :: Maybe Bool -- ^ 
  , userDeactivatedUnderscoreat :: Maybe Bool -- ^ 
  , userCreatedUnderscoreat :: Maybe Text -- ^ 
  , userUpdatedUnderscoreat :: Maybe Text -- ^ 
  , userRole :: Maybe Text -- ^ 
  , userExtraUnderscoreroles :: Maybe [Text] -- ^ 
  , userOfficeUnderscoreid :: Maybe Text -- ^ 
  , userOfficeUnderscorename :: Maybe Text -- ^ 
  , userCountryUnderscoreid :: Maybe Text -- ^ 
  , userCountryUnderscorecode :: Maybe Text -- ^ 
  , userLanguageUnderscorecode :: Maybe Text -- ^ 
  , userImage :: Maybe UserImage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON User where
  parseJSON = genericParseJSON optionsUser
instance ToJSON User where
  toJSON = genericToJSON optionsUser

optionsUser :: Options
optionsUser =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("userUserUnderscoreid", "user_id")
      , ("userId", "id")
      , ("userEmail", "email")
      , ("userExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("userUpn", "upn")
      , ("userName", "name")
      , ("userTelephone", "telephone")
      , ("userDefaultUnderscorecvUnderscoreid", "default_cv_id")
      , ("userDeactivated", "deactivated")
      , ("userDeactivatedUnderscoreat", "deactivated_at")
      , ("userCreatedUnderscoreat", "created_at")
      , ("userUpdatedUnderscoreat", "updated_at")
      , ("userRole", "role")
      , ("userExtraUnderscoreroles", "extra_roles")
      , ("userOfficeUnderscoreid", "office_id")
      , ("userOfficeUnderscorename", "office_name")
      , ("userCountryUnderscoreid", "country_id")
      , ("userCountryUnderscorecode", "country_code")
      , ("userLanguageUnderscorecode", "language_code")
      , ("userImage", "image")
      ]


-- | 
data UserImage = UserImage
  { userImageUrl :: Maybe Text -- ^ 
  , userImageThumb :: Maybe ImageUrl -- ^ 
  , userImageFitUnderscorethumb :: Maybe ImageUrl -- ^ 
  , userImageLarge :: Maybe ImageUrl -- ^ 
  , userImageSmallUnderscorethumb :: Maybe ImageUrl -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserImage where
  parseJSON = genericParseJSON optionsUserImage
instance ToJSON UserImage where
  toJSON = genericToJSON optionsUserImage

optionsUserImage :: Options
optionsUserImage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("userImageUrl", "url")
      , ("userImageThumb", "thumb")
      , ("userImageFitUnderscorethumb", "fit_thumb")
      , ("userImageLarge", "large")
      , ("userImageSmallUnderscorethumb", "small_thumb")
      ]


-- | 
data WorkExperience = WorkExperience
  { workExperienceOrder :: Maybe Int -- ^ 
  , workExperienceStarred :: Maybe Bool -- ^ 
  , workExperienceDisabled :: Maybe Bool -- ^ 
  , workExperienceVersion :: Maybe Int -- ^ 
  , workExperienceExternalUnderscoreuniqueUnderscoreid :: Maybe Text -- ^ 
  , workExperienceOwnerUnderscoreupdatedUnderscoreat :: Maybe Text -- ^ 
  , workExperienceCreatedUnderscoreat :: Maybe Text -- ^ 
  , workExperienceUpdatedUnderscoreat :: Maybe Text -- ^ 
  , workExperienceEmployer :: Maybe Value -- ^ 
  , workExperienceDescription :: Maybe Value -- ^ 
  , workExperienceLongUnderscoredescription :: Maybe Value -- ^ 
  , workExperienceYearUnderscorefrom :: Maybe Text -- ^ 
  , workExperienceMonthUnderscorefrom :: Maybe Text -- ^ 
  , workExperienceYearUnderscoreto :: Maybe Text -- ^ 
  , workExperienceMonthUnderscoreto :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WorkExperience where
  parseJSON = genericParseJSON optionsWorkExperience
instance ToJSON WorkExperience where
  toJSON = genericToJSON optionsWorkExperience

optionsWorkExperience :: Options
optionsWorkExperience =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("workExperienceOrder", "order")
      , ("workExperienceStarred", "starred")
      , ("workExperienceDisabled", "disabled")
      , ("workExperienceVersion", "version")
      , ("workExperienceExternalUnderscoreuniqueUnderscoreid", "external_unique_id")
      , ("workExperienceOwnerUnderscoreupdatedUnderscoreat", "owner_updated_at")
      , ("workExperienceCreatedUnderscoreat", "created_at")
      , ("workExperienceUpdatedUnderscoreat", "updated_at")
      , ("workExperienceEmployer", "employer")
      , ("workExperienceDescription", "description")
      , ("workExperienceLongUnderscoredescription", "long_description")
      , ("workExperienceYearUnderscorefrom", "year_from")
      , ("workExperienceMonthUnderscorefrom", "month_from")
      , ("workExperienceYearUnderscoreto", "year_to")
      , ("workExperienceMonthUnderscoreto", "month_to")
      ]

