akd_deset : dialog {
  label = "AKDProjections Settings";
  : boxed_column { label = "Heights (mm)";
    : edit_box { label = "Window height"; key = "win_h";    edit_width = 10; }
    : edit_box { label = "Window sill";   key = "win_sill"; edit_width = 10; }
    : edit_box { label = "Door height";   key = "door_h";   edit_width = 10; }
    : edit_box { label = "Wall height";   key = "wall_h";   edit_width = 10; }
    : edit_box { label = "Slab thickness";key = "slab_t";   edit_width = 10; }
    : edit_box { label = "Ground extension"; key = "gnd_ext"; edit_width = 10; }
  }
  : boxed_column { label = "Layers  (name / ACI color 1-255)";
    : row { : edit_box { label = "Cut hatch";        key = "n_hatch";  edit_width = 18; }
            : edit_box { label = "col"; key = "c_hatch"; edit_width = 4; } }
    : row { : edit_box { label = "Cut outline ELV1"; key = "n_elv1";   edit_width = 18; }
            : edit_box { label = "col"; key = "c_elv1"; edit_width = 4; } }
    : row { : edit_box { label = "BG/frames  ELV2";  key = "n_elv2";   edit_width = 18; }
            : edit_box { label = "col"; key = "c_elv2"; edit_width = 4; } }
    : row { : edit_box { label = "Mullions   ELV3";  key = "n_elv3";   edit_width = 18; }
            : edit_box { label = "col"; key = "c_elv3"; edit_width = 4; } }
    : row { : edit_box { label = "Slab/gnd   ELV5";  key = "n_elv5";   edit_width = 18; }
            : edit_box { label = "col"; key = "c_elv5"; edit_width = 4; } }
    : row { : edit_box { label = "Section line";     key = "n_sect";   edit_width = 18; }
            : edit_box { label = "col"; key = "c_sect"; edit_width = 4; } }
    : row { : edit_box { label = "Section frame";    key = "n_frame";  edit_width = 18; }
            : edit_box { label = "col"; key = "c_frame"; edit_width = 4; } }
    : row { : edit_box { label = "Annotation";       key = "n_anno";   edit_width = 18; }
            : edit_box { label = "col"; key = "c_anno"; edit_width = 4; } }
  }
  ok_cancel;
}
