local vim = _G.vim
local confpath = vim.fn.stdpath("config")
-- force aniseed to compile while in nvim dir (in dev mode)
local isInConf = confpath == vim.fn.getcwd()
local log = function(output) print("[init.lua]: " .. output) end

local disabled_built_ins = {
  "2html_plugin",
  "getscript",
  "getscriptPlugin",
  "gzip",
  "logipat",
  "matchit",
  "netrw",
  "netrwFileHandlers",
  "netrwPlugin",
  "netrwSettings",
  "rrhelper",
  "spellfile_plugin",
  "tar",
  "tarPlugin",
  "vimball",
  "vimballPlugin",
  "zip",
  "zipPlugin",
}

for _, plugin in pairs(disabled_built_ins) do
    vim.g["loaded_" .. plugin] = 1
end


-- make sure aniseed path is available for macros lookup
-- installed with nix
pcall(vim.cmd, [[ packadd aniseed ]])
pcall(vim.cmd, [[ packadd parinfer-rust ]])
pcall(vim.cmd, [[ packadd lazy.nvim ]])

local ok, anenv = pcall(require, 'aniseed.env')
-- aniseed is available, compile and load
if ok then
    if isInConf then
        log("in conf path, force compiling")
    end
    anenv.init({force = isInConf})
else
    log('Aniseed not found. You need to run the ansible bootstrap role')
end
