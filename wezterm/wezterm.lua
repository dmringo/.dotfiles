local wezterm = require 'wezterm'
local act = wezterm.action

local config = {}
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- =============================================================================
-- Shell & Startup
-- =============================================================================
config.default_prog = { 'pwsh.exe', '-NoLogo' }
config.default_cwd = wezterm.home_dir

-- Launcher menu for quick switching between available shells
config.launch_menu = {
  {
    label = 'PowerShell 7',
    args = { 'pwsh.exe', '-NoLogo' },
  },
  {
    label = 'Arch Linux (WSL2)',
    args = { 'wsl.exe', '-d', 'archlinux' },
  },
  {
    label = 'Nushell',
    args = { 'nu.exe' },
  },
  {
    label = 'Command Prompt',
    args = { 'cmd.exe' },
  },
  {
    label = 'Windows PowerShell (5.1)',
    args = { 'powershell.exe', '-NoLogo' },
  },
  {
    label = 'MSYS2 Bash',
    args = { 'C:\\msys2\\usr\\bin\\bash.exe', '-l' },
  },
  {
    label = 'MSYS2 Zsh',
    args = { 'C:\\msys2\\usr\\bin\\zsh.exe', '-l' },
  },
}

-- =============================================================================
-- Fonts & Typography
-- =============================================================================
local DEFAULT_FONT = 'RecMonoCasual Nerd Font Mono'

config.font = wezterm.font_with_fallback({
  { family = DEFAULT_FONT },
  { family = 'Symbols Nerd Font Mono' },
  { family = 'JetBrains Mono' },
})
config.font_size = 12.0
config.line_height = 1.1

-- -----------------------------------------------------------------------------
-- Dynamic Monospace Font Switcher
-- -----------------------------------------------------------------------------
local cached_mono_fonts = nil

local function get_monospace_fonts(force_refresh)
  if cached_mono_fonts and not force_refresh then
    return cached_mono_fonts
  end

  local fonts_set = {}
  local fonts_list = {}

  -- 1. Native Win32 GDI enumeration via list_win_fonts.py (~85ms)
  local function find_helper(filename)
    local candidates = {
      wezterm.config_dir and (wezterm.config_dir .. '/' .. filename),
      wezterm.home_dir .. '/.config/wezterm/' .. filename,
      wezterm.home_dir .. '/repos/.dotfiles/wezterm/' .. filename,
    }
    for _, path in ipairs(candidates) do
      if path and wezterm.glob(path)[1] then
        return path
      end
    end
    return nil
  end

  local py_script = find_helper('list_win_fonts.py')
  local used_native = false

  if py_script then
    local ok, stdout = wezterm.run_child_process({ 'python3', py_script })
    if not ok or not stdout or #stdout == 0 then
      ok, stdout = wezterm.run_child_process({ 'python', py_script })
    end
    if ok and stdout and #stdout > 0 then
      used_native = true
      for line in stdout:gmatch('[^\r\n]+') do
        local name = line:gsub('^%s+', ''):gsub('%s+$', '')
        if #name > 0 and not fonts_set[name] then
          fonts_set[name] = true
          table.insert(fonts_list, name)
        end
      end
    end
  end

  -- 2. Fallback to wezterm ls-fonts if Python helper is unavailable
  if not used_native then
    local success, stdout, _ = wezterm.run_child_process({ 'wezterm', 'ls-fonts', '--list-system' })
    if success and stdout then
      local mono_patterns = {
        'mono', 'code', 'consolas', 'courier', 'lucida console',
        'hack', 'inconsolata', 'iosevka', 'terminus', 'fixedsys',
        'monaspace', 'typewriter', 'anonymous', 'fira', 'input'
      }
      for line in stdout:gmatch('[^\r\n]+') do
        local font_name = line:match('wezterm%.font%("([^"]+)"')
        if font_name and not fonts_set[font_name] then
          local lower = font_name:lower()
          local is_unicode_symbol = lower:find('unicode', 1, true) or lower:find('symbol', 1, true)
          if not is_unicode_symbol then
            for _, pat in ipairs(mono_patterns) do
              if lower:find(pat, 1, true) then
                fonts_set[font_name] = true
                table.insert(fonts_list, font_name)
                break
              end
            end
          end
        end
      end
    end
  end

  -- Ensure WezTerm's built-in JetBrains Mono is always included
  if not fonts_set['JetBrains Mono'] then
    table.insert(fonts_list, 'JetBrains Mono')
  end

  table.sort(fonts_list)
  cached_mono_fonts = fonts_list
  return cached_mono_fonts
end

local function create_font_selector_action()
  return wezterm.action_callback(function(window, pane)
    local fonts = get_monospace_fonts(false)
    local choices = {
      { id = '__RESET__', label = '↺  Reset to default font (' .. DEFAULT_FONT .. ')' },
      { id = '__REFRESH__', label = '↻  Refresh font cache from system' },
    }

    for _, font_name in ipairs(fonts) do
      table.insert(choices, {
        id = font_name,
        label = '🗛  ' .. font_name,
      })
    end

    window:perform_action(
      act.InputSelector {
        title = 'Select Monospace Font',
        choices = choices,
        fuzzy = true,
        action = wezterm.action_callback(function(win, p, id, _)
          if not id then
            return
          end

          if id == '__RESET__' then
            local overrides = win:get_config_overrides() or {}
            overrides.font = nil
            win:set_config_overrides(overrides)
            if win.toast_notification then
              win:toast_notification('WezTerm', 'Reset font to default: ' .. DEFAULT_FONT, nil, 2500)
            end
          elseif id == '__REFRESH__' then
            get_monospace_fonts(true)
            if win.toast_notification then
              win:toast_notification('WezTerm', 'Refreshed system font cache', nil, 2500)
            end
            -- Re-open selector with newly refreshed fonts
            win:perform_action(create_font_selector_action(), p)
          else
            local overrides = win:get_config_overrides() or {}
            overrides.font = wezterm.font_with_fallback({
              { family = id },
              { family = 'Symbols Nerd Font Mono' },
              { family = 'JetBrains Mono' },
            })
            win:set_config_overrides(overrides)
            if win.toast_notification then
              win:toast_notification('WezTerm', 'Font changed to: ' .. id, nil, 2500)
            end
          end
        end),
      },
      pane
    )
  end)
end

local font_switcher_action = create_font_selector_action()

-- =============================================================================
-- Colors & Theme
-- =============================================================================
config.color_scheme = 'Dark+'

-- =============================================================================
-- Window & UI
-- =============================================================================
-- Integrated buttons merge tabs and window controls into a single clean bar
config.window_decorations = 'INTEGRATED_BUTTONS|RESIZE'
config.use_fancy_tab_bar = true
config.hide_tab_bar_if_only_one_tab = false
config.tab_max_width = 32

config.initial_cols = 120
config.initial_rows = 40
config.window_padding = {
  left = 8,
  right = 8,
  top = 8,
  bottom = 8,
}
config.adjust_window_size_when_changing_font_size = false

-- Command Palette configuration
config.command_palette_rows = 16
config.ui_key_cap_rendering = 'WindowsLong'
-- =============================================================================
-- Cursor & Scrollback
-- =============================================================================
config.default_cursor_style = 'BlinkingBlock'
config.cursor_blink_rate = 500
config.scrollback_lines = 10000

-- =============================================================================
-- Keybindings
-- =============================================================================
config.keys = {
  -- Clipboard
  { key = 'c', mods = 'CTRL|SHIFT', action = act.CopyTo 'Clipboard' },
  { key = 'v', mods = 'CTRL|SHIFT', action = act.PasteFrom 'Clipboard' },

  -- Tabs
  { key = 't', mods = 'CTRL|SHIFT', action = act.SpawnTab 'CurrentPaneDomain' },
  { key = 'w', mods = 'CTRL|SHIFT', action = act.CloseCurrentTab { confirm = true } },
  { key = 'Tab', mods = 'CTRL', action = act.ActivateTabRelative(1) },
  { key = 'Tab', mods = 'CTRL|SHIFT', action = act.ActivateTabRelative(-1) },

  -- Tab navigation by number (Alt + 1..8, Alt + 9 for last tab)
  { key = '1', mods = 'ALT', action = act.ActivateTab(0) },
  { key = '2', mods = 'ALT', action = act.ActivateTab(1) },
  { key = '3', mods = 'ALT', action = act.ActivateTab(2) },
  { key = '4', mods = 'ALT', action = act.ActivateTab(3) },
  { key = '5', mods = 'ALT', action = act.ActivateTab(4) },
  { key = '6', mods = 'ALT', action = act.ActivateTab(5) },
  { key = '7', mods = 'ALT', action = act.ActivateTab(6) },
  { key = '8', mods = 'ALT', action = act.ActivateTab(7) },
  { key = '9', mods = 'ALT', action = act.ActivateTab(-1) },

  -- Panes / Splits (Alt+Shift+D matches Windows Terminal auto split)
  {
    key = 'd',
    mods = 'ALT|SHIFT',
    action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = '"',
    mods = 'ALT|SHIFT',
    action = act.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  {
    key = '%',
    mods = 'ALT|SHIFT',
    action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'x',
    mods = 'ALT|SHIFT',
    action = act.CloseCurrentPane { confirm = true },
  },
  {
    key = 'z',
    mods = 'ALT|SHIFT',
    action = act.TogglePaneZoomState,
  },

  -- Pane Navigation (Alt + Arrow keys)
  { key = 'LeftArrow', mods = 'ALT', action = act.ActivatePaneDirection 'Left' },
  { key = 'RightArrow', mods = 'ALT', action = act.ActivatePaneDirection 'Right' },
  { key = 'UpArrow', mods = 'ALT', action = act.ActivatePaneDirection 'Up' },
  { key = 'DownArrow', mods = 'ALT', action = act.ActivatePaneDirection 'Down' },

  -- Launcher & Command Palette (ActivateCommandPalette displays keybindings alongside commands)
  { key = 'l', mods = 'CTRL|SHIFT', action = act.ShowLauncher },
  { key = 'p', mods = 'CTRL|SHIFT', action = act.ActivateCommandPalette },
  -- Fullscreen & Key Passthrough
  { key = 'F11', action = act.ToggleFullScreen },
  { key = 'Enter', mods = 'ALT|SHIFT', action = act.ToggleFullScreen },
  { key = 'Enter', mods = 'ALT', action = act.DisableDefaultAssignment },

  -- Search
  { key = 'f', mods = 'CTRL|SHIFT', action = act.Search 'CurrentSelectionOrEmptyString' },

  -- Font zoom
  { key = '=', mods = 'CTRL', action = act.IncreaseFontSize },
  { key = '+', mods = 'CTRL', action = act.IncreaseFontSize },
  { key = '-', mods = 'CTRL', action = act.DecreaseFontSize },
  { key = '0', mods = 'CTRL', action = act.ResetFontSize },

  -- Font switcher (fuzzy-search monospace fonts)
  { key = 'f', mods = 'ALT|SHIFT', action = font_switcher_action },
}

-- =============================================================================
-- Command Palette Augmentation
-- =============================================================================
wezterm.on('augment-command-palette', function(window, pane)
  return {
    {
      brief = 'Font: Switch Font (System Monospace)',
      doc = 'Alt+Shift+F — Fuzzy-search and switch terminal font from detected system monospace fonts',
      icon = 'md_format_font',
      action = font_switcher_action,
    },
    {
      brief = 'Font: Reset to Default (' .. DEFAULT_FONT .. ')',
      doc = 'Reset active font override back to default configuration',
      icon = 'md_restore',
      action = wezterm.action_callback(function(win, _)
        local overrides = win:get_config_overrides() or {}
        overrides.font = nil
        win:set_config_overrides(overrides)
        if win.toast_notification then
          win:toast_notification('WezTerm', 'Reset font to default: ' .. DEFAULT_FONT, nil, 2500)
        end
      end),
    },
  }
end)

return config
