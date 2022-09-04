assert(not game:IsLoaded() and game.Loaded:Wait() or game)
local sethiddenprop = sethiddenprop or sethiddenproperty or function(o, i, v) pcall(function() o[i] = v end) end
local Lighting = game:GetService("Lighting")
local MaterialService = game:GetService("MaterialService")
local Network = settings():GetService("NetworkSettings")
local GameSettings = UserSettings():GetService("UserGameSettings")
local Rendering = settings():GetService("RenderSettings")
local Terrain = workspace.Terrain
sethiddenprop(Lighting, "GlobalShadows", false)
sethiddenprop(Lighting, "Technology", 2)
sethiddenprop(MaterialService, "Use2022Materials", false)
sethiddenprop(Network, "IncomingReplicationLag", 0)
sethiddenprop(GameSettings, "GraphicsQualityLevel", 1)
sethiddenprop(GameSettings, "VREnabled", false)
sethiddenprop(GameSettings, "HasEverUsedVR", false)
sethiddenprop(Rendering, "QualityLevel", 1)
sethiddenprop(Rendering, "MeshPartDetailLevel", 1)
sethiddenprop(Rendering, "EagerBulkExecution", false)
sethiddenprop(Terrain, "WaterWaveSpeed", 0)
sethiddenprop(Terrain, "WaterReflectance", 0)
sethiddenprop(Terrain, "WaterWaveSize", 0)
sethiddenprop(Terrain, "Decoration", false)
sethiddenprop(workspace, "LevelOfDetail", 2)
local Blacklisted = {
	[Enum.Material.Neon] = true,
	[Enum.Material.ForceField] = true
}
local function BoostFPS(v)
	if v:IsA("BasePart") then
		if not Blacklisted[v.Material] then
			v.Material = 272
		end
		v.Reflectance, v.CastShadow = 0, false
	end
	if v:IsA("PostEffect") then
		v.Enabled = false
	end
	if v:IsA("Model") then
		sethiddenprop(v, "LevelOfDetail", 2)
	end
end
for _, v in next, game:GetDescendants() do
	BoostFPS(v)
end
for _, v in next, {workspace, Lighting} do
	v.DescendantAdded:Connect(BoostFPS)
end