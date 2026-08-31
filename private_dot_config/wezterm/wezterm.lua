-- Pull in the wezterm API
local wezterm = require 'wezterm'
local act = wezterm.action

-- This table will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- config.color_scheme = 'DoomOne'
-- config.color_scheme = 'Snazzy'
-- config.color_scheme = 'Catppuccin Mocha'
-- config.color_scheme = 'Ef-Autumn'
-- config.color_scheme = 'Material (base16)'
-- config.color_scheme = 'MaterialOcean'
-- config.color_scheme = 'OneDark (base16)'
-- config.color_scheme = 'Poimandres'
config.color_scheme = 'nord'

-- config.font = wezterm.font('Input Mono Compressed', { weight = 'Light', stretch = "ExtraCondensed" })
config.font = wezterm.font('Maple Mono SL NF')
config.font_size = 18.0

-- Tabs Style
config.hide_tab_bar_if_only_one_tab = true
config.window_frame = {
    font = wezterm.font { family = 'Maple Mono NF', weight = 'Bold' },
    font_size = 10.0,
}

config.use_dead_keys = false

-- config.window_background_opacity = 0.95
-- config.macos_window_background_blur = 10

config.window_decorations = "TITLE | RESIZE"

config.initial_cols = 150
config.initial_rows = 40

config.disable_default_key_bindings = true
config.keys = {
    {
        key = 'c',
        mods = 'SUPER',
        action = act.CopyTo 'ClipboardAndPrimarySelection',
    },
    {
        key = 'v',
        mods = 'SUPER',
        action = act.PasteFrom 'Clipboard',
    },
    {
        key = 'v',
        mods = 'SUPER',
        action = act.PasteFrom 'PrimarySelection',
    },
    -- Clears the scrollback and viewport leaving the prompt line the new first line.
    {
	key = 'l',
        mods = 'CTRL',
        action = act.ClearScrollback 'ScrollbackAndViewport',
    },
    { key = 'n', mods = 'SUPER', action = act.SpawnWindow },
    { key = 't', mods = 'SUPER', action = act.SpawnTab 'DefaultDomain' },
    {
        key = 'w',
        mods = 'CMD',
        action = wezterm.action.CloseCurrentPane { confirm = true },
    },
    { key = 'PageUp',   mods = 'SUPER', action = act.ScrollByPage(-1) },
    { key = 'PageDown', mods = 'SUPER', action = act.ScrollByPage(1) },
    {
        key = 'P',
        mods = 'CTRL|SHIFT',
        action = act.ActivateCommandPalette,
    },
    {
        key = '-',
        mods = 'CMD|ALT',
        action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
    },
    {
        key = '=',
        mods = 'CMD|ALT',
        action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
    { key = 'LeftArrow',  mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Left', },
    { key = 'RightArrow', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Right', },
    { key = 'UpArrow',    mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Up', },
    { key = 'DownArrow',  mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Down', },

}

-- CMD + number to activate that tab
for i = 1, 8 do
    table.insert(config.keys, {
        key = tostring(i),
        mods = 'SUPER',
        action = act.ActivateTab(i - 1),
    })
end

-- Open links with Ctrl-click
config.mouse_bindings = {
    {
        event = { Up = { streak = 1, button = 'Left' } },
        mods = 'CTRL',
        action = wezterm.action.OpenLinkAtMouseCursor,
    },
}

-- and finally, return the configuration to wezterm
return config
