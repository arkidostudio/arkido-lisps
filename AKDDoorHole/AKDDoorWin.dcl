akd_set : dialog {
  label = "AKDDoorWin Settings";
  spacer;
  : row {
    : boxed_column {
      label = "Category";
      : list_box { key = "cat"; width = 16; height = 14; }
    }
    : boxed_column {
      label = "Setting";
      : list_box { key = "var"; width = 18; height = 14; }
    }
    : boxed_column {
      label = "Value";
      : text { label = "Current:"; }
      : text { key = "cur"; width = 22; }
      spacer;
      : edit_box { label = "New value:"; key = "val"; width = 18; }
      spacer;
      : text { label = "For Layer settings:"; }
      : edit_box { label = "Layer name:"; key = "lyr"; width = 18; }
      : edit_box { label = "Color (1-256):"; key = "clr"; width = 18; }
    }
  }
  spacer;
  ok_cancel;
}
