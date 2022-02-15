local vim = _G.vim
-- check if aniseed is installed, if not, run make aniseed to install and

-- ensure packer file
os.execute('mkdir -p pack/packer')
-- make sure aniseed path is available for macros lookup
vim.cmd [[ packadd aniseed ]]
local isInConf = vim.fn.stdpath("config") == vim.fn.getcwd()

-- force aniseed to compile while in nvim dir (in dev mode)
if isInConf then
  print("[init.vim]: in conf dir: force compiling")
end

local ok, res = pcall(require, 'aniseed.env')

-- aniseed is available, compile and load
if ok then
  res.init({ force = isInConf })
else
  print('Aniseed not found. Running aniseed install now')
  print(vim.api.nvim_call_function('system', {'make aniseed'}))
  local ok2, res2 = pcall(require, 'aniseed.env')

  if not ok2 then
    print('Could not load after install')
  else
    res2.init({ force = isInConf })
  end

end
