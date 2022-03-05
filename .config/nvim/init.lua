-- check if aniseed is installed, if not, run make aniseed to install and
local vim = _G.vim
local confpath = vim.fn.stdpath("config")
local isInConf = confpath == vim.fn.getcwd()
local log = function(output) print("[init.lua]: " .. output) end

-- make sure aniseed path is available for macros lookup
pcall(vim.cmd, [[ packadd aniseed ]])

local ok, anenv = pcall(require, 'aniseed.env')

-- force aniseed to compile while in nvim dir (in dev mode)
-- aniseed is available, compile and load
if ok then
    if isInConf then
        -- ensure packer path
        log("in conf path, force compiling")
    end
    anenv.init({force = isInConf})
else
    log('Aniseed not found. Running bootstrap')
    print(vim.api.nvim_call_function('system', {'make bootstrap'}))
    ok, anenv = pcall(require, 'aniseed.env')

    if not ok then
        log('Could not load after bootstrap')
    else
        log("Bootstrap successful, compiling")
        anenv.init({force = isInConf})
    end

end
