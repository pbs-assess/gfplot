-- SQL code to get IPHC skate records (not used for IPHC index calculation) from GFBioSQL
-- Last modified: 2015-03-03

SET NOCOUNT ON -- prevents timeout errors

-- If any of this first call changes then update table in text (currently
--  (in Andy's yeye15reproduce repo, will get moved to gfsynopsis)
SELECT
  YEAR(T.TRIP_START_DATE) AS [year],
  FE.TRIP_ID AS tripID,
  FE.FE_PARENT_EVENT_ID AS skateID,
  FE.FE_MAJOR_LEVEL_ID AS "set",
  FE.FE_SUB_LEVEL_ID AS skate,
  --BAIT_LURE_CODE AS bait,
  COUNT(FE.FE_MINOR_LEVEL_ID) AS hookCount
FROM FISHING_EVENT FE
   INNER JOIN TRIP T ON FE.TRIP_ID = T.TRIP_ID 
    INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = T.TRIP_ID
	  INNER JOIN SURVEY S ON  TS.SURVEY_ID = S.SURVEY_ID
	--INNER JOIN [dbo].[LONGLINE_BAIT_LURE] LLBL ON FE.FISHING_EVENT_ID = LLBL.FISHING_EVENT_ID
WHERE
  FE.FE_MINOR_LEVEL_ID Is Not Null AND
  S.SURVEY_SERIES_ID IN (14)
GROUP BY YEAR(T.TRIP_START_DATE),
  FE.TRIP_ID,
  FE.FE_PARENT_EVENT_ID,
  FE.FE_MAJOR_LEVEL_ID,
  FE.FE_SUB_LEVEL_ID
  --BAIT_LURE_CODE
ORDER BY
  Year(T.TRIP_START_DATE),
  FE.TRIP_ID,
  FE.FE_MAJOR_LEVEL_ID,
  FE.FE_SUB_LEVEL_ID

--qu("gfb_iphc_skates.sql",dbName="GFBioSQL",strSpp="442")
--getData("gfb_iphc_skates.sql",dbName="GFBioSQL",strSpp="442",path="C:/Users/haighr/Files/Projects/R/Develop/PBStools/Authors/SQLcode/")

