-- check if aniseed is installed, if not, run make aniseed to install and
local vim = _G.vim
local confpath = vim.fn.stdpath("config")
local isInConf = confpath == vim.fn.getcwd()

if isInConf then
  -- ensure packer path
  os.execute('mkdir -p pack/packer')
  print("[init.vim]: in conf dir")
end

-- make sure aniseed path is available for macros lookup
vim.cmd [[ packadd aniseed ]]

local ok, anenv = pcall(require, 'aniseed.env')

-- force aniseed to compile while in nvim dir (in dev mode)
-- aniseed is available, compile and load
if ok then
  print("[init.vim]: force compile")
  anenv.init({ force = isInConf })
else
  print('Aniseed not found. Running aniseed install now')
  print(vim.api.nvim_call_function('system', {'make aniseed'}))
  local ok, anenv = pcall(require, 'aniseed.env')

  if not ok then
    print('Could not load after install')
  else
    print("[init.vim]: compile")
    anenv.init({ force = isInConf })
  end

end
