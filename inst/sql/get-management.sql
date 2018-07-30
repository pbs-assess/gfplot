SELECT M.Mgmt_Id,
	MA.Area_Code,
	Area_Description,
	M.Fishery_Id,
	F.Fishery_Description,
	M.Species_Group_Code,
	SG.Species_Group_Description,
	Species_Code,
	SGS.Species_Common_Name,
	A.Action_Description,
	Action_Value,
	AU.Action_Unit_Description,
	YEAR(Action_Start_Date) AS Start_Year,
	Action_Start_Date,
	Action_End_Date,
	P.fldTitle AS Publication,
	P.fldType AS Publication_Type,
	Action_Comment
FROM GFManagement.dbo.Management M 
	LEFT OUTER JOIN GFManagement.dbo.Species_Group_Species SGS ON M.Species_Group_Code = SGS.Species_Group_Code 
	LEFT OUTER JOIN GFManagement.dbo.Area Area 
	INNER JOIN GFManagement.dbo.Management_Area MA ON Area.Area_Code = MA.Area_Code ON M.Mgmt_Id = MA.Mgmt_Id 
	LEFT OUTER JOIN GFManagement.dbo.Action A ON M.Action_Code = A.Action_Code 
	LEFT OUTER JOIN GFManagement.dbo.Species_Group SG ON M.Species_Group_Code = SG.Species_Group_Code 
	LEFT OUTER JOIN GFManagement.dbo.Action_Unit AU ON M.Action_Unit_Code = AU.Action_Unit_Code 
	LEFT OUTER JOIN GFManagement.dbo.tblPublications P ON M.fldPublicationID = P.fldPublicationID 
	LEFT OUTER JOIN GFManagement.dbo.Fishery F ON M.Fishery_Id = F.Fishery_Id
WHERE M.Mgmt_Id IS NOT NULL
-- insert species here
-- insert fishery here
-- insert species group here
-- insert area here
-- insert start year here
ORDER BY Action_Start_Date, Species_Group_Code, Species_Common_Name
