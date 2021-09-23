local Id = ({...})[1] or 142376088 -- id here

game:GetService("ContentProvider"):PreloadAsync({"rbxassetid://" .. Id}, function()
	local ME = game:GetService("Players").LocalPlayer
	local Sounds, Remotes = {}, {}
	local Hum = ME.Character:FindFirstChildWhichIsA("Humanoid", true)
	if Hum then
		task.wait(.1, Hum:UnequipTools())
	end
	for _, v in ipairs(ME.Backpack:GetChildren()) do
		if v:IsA("BackpackItem") and v.Name:lower():find("boomb", nil, false) then
			local Sound, Remote = v:FindFirstChildWhichIsA("Sound", true), v:FindFirstChildWhichIsA("RemoteEvent", true)
			if Sound and Remote then
				v.Parent = ME.Character
				Sounds[#Sounds + 1], Remotes[#Remotes + 1] = Sound, Remote
			end
		end
	end
	if #Sounds <= 0 then
		return game:GetService("StarterGui"):SetCore("SendNotification", {
			Title = "Hunter's Massplayer",
			Text = "You don't have any boomboxes!",
			Button1 = "Okay"
		})
	end
	for _, v in ipairs(Remotes) do
		task.spawn(v.FireServer, v, "PlaySong", Id)
	end
	for _, v in ipairs(Sounds) do
		while not v.IsLoaded do
			task.wait()
		end
		v.Playing, v.TimePosition = false, 0
	end
	for _, v in ipairs(Sounds) do
		task.spawn(function()
			v.Playing, v.TimePosition = true, 0
		end)
	end
	game:GetService("StarterGui"):SetCore("SendNotification", {
		Title = "Hunter's Massplayer",
		Text = "Boombox count: " .. #Sounds,
		Button1 = "Okay"
	})
end)
return "534144#1337 (820077059095003147)"
