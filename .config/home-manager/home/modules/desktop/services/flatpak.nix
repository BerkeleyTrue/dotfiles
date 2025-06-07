{...}: {
  services.flatpak = {
    enable = true;
    packages = [
      "app.ytmdesktop.ytmdesktop" # youtube music download
      "org.duckstation.DuckStation" # playstation emulator
      "org.bino3d.bino" # 3d video player

      # office tools
      "org.libreoffice.LibreOffice"
      "org.standardnotes.standardnotes" # synced notes

      # devtools
      "com.github.tchx84.Flatseal" # configure flatpaks
      "com.vscodium.codium" # open vscode
      "io.dbeaver.DBeaverCommunity" # sql explorer

      # messaging
      "com.discordapp.Discord"
      "org.signal.Signal"

      "com.librumreader.librum" # pdf/ebub reader
      "com.github.geigi.cozy" # listen to audiobooks

      # image manipulation tools
      "com.github.libresprite.LibreSprite" # pixel art tool
      # "org.kde.krita" # image tool saving crashes?
      "com.uploadedlobster.peek" # record desktop gifs

      # 3d modeling
      # "org.freecadweb.FreeCAD" # cad
      "com.ultimaker.cura" # 3d print slicer
      "io.github.f3d_app.f3d" # stl viewer

      # audio tools
      "org.pipewire.Helvum" # audio plumber
      "com.github.wwmm.pulseeffects" # advanced audio effects

      # browsers
      "io.github.zen_browser.zen" # firefox based
      "org.chromium.Chromium" # open chrome
    ];

    update = {
      # will run update weekly
      auto.enable = true;
    };
  };
}
