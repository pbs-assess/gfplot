SELECT M.Mgmt_Id,
	MA.Area_Code,
	Area_Description,
	M.Fishery_Id,
	F.Fishery_Description,
	M.Species_Group_Code,
	SG.Species_Group_Description,
	SGG.Species_Code,
	SGG.Species_Common_Name,
	A.Action_Description,
	Action_Value,
	AU.Action_Unit_Description,
	Action_Start_Date,
	Action_End_Date,
	Action_Comment
FROM PacManagement.dbo.Management M
	INNER JOIN PacManagement.dbo.Species_Group SG ON SG.Species_Group_Code = M.Species_Group_Code
	INNER JOIN PacManagement.dbo.Species_Group_Species SGG ON SGG.Species_Group_Code = SG.Species_Group_Code
	INNER JOIN PacManagement.dbo.Fishery F ON F.Fishery_Id = M.Fishery_Id
	INNER JOIN PacManagement.dbo.Action A ON A.Action_Code = M.Action_Code
	INNER JOIN PacManagement.dbo.Action_Unit AU ON AU.Action_Unit_Code = M.Action_Unit_Code
	INNER JOIN PacManagement.dbo.Management_Area MA ON MA.Mgmt_Id = M.Mgmt_Id
	INNER JOIN PacManagement.dbo.Area ON Area.Area_Code = MA.Area_Code