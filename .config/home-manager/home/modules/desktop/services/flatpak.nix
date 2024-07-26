{...}: {
  services.flatpak = {
    enable = true;
    packages = [
      "app.ytmdesktop.ytmdesktop"           
      "com.discordapp.Discord"              
      "com.github.geigi.cozy"               
      "com.github.libresprite.LibreSprite"  
      "com.github.tchx84.Flatseal"          
      "com.github.wwmm.pulseeffects"        
      "com.ultimaker.cura"                  
      "com.uploadedlobster.peek"            
      "com.vscodium.codium"                 
      "io.dbeaver.DBeaverCommunity"         
      "io.github.f3d_app.f3d"               
      "org.chromium.Chromium"               
      "org.duckstation.DuckStation"         
      "org.freecadweb.FreeCAD"              
      "org.gimp.GIMP"                       
      "org.inkscape.Inkscape"               
      "org.kde.krita"                       
      "org.libreoffice.LibreOffice"         
      "org.pipewire.Helvum"                 
      "org.standardnotes.standardnotes"
      "com.librumreader.librum" # pdf/ebub reader
      "org.signal.Signal" # messaging
    ];
  };
}
