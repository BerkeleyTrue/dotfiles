final: prev:
{
  neovim = prev.neovim.overrideAttrs (oldAttrs:
    let
      nixpackdir = prev.pkgs.writeText "nixpackdir" ''
        set packpath^=${prev.vimUtils.packDir oldAttrs.passthrough.packpathDirs}
        set rtp^=${prev.vimUtils.packDir oldAttrs.passthrough.packpathDirs}
      '';
    in
    {
      postInstall = oldAttrs.postInstall + ''
        ln -s ${nixpackdir} $HOME/.config/nvim/nixpackdir.vim
      '';
    });
}
