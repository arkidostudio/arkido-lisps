akd_set : dialog {
  label = "Doors & Windows Settings - v2.3.4";
  : boxed_column {
    label = "Category";
    : popup_list { key = "cat"; width = 42; }
  }
  : boxed_column {
    label = "Settings";
    : list_box { key = "items"; width = 58; height = 14; }
  }
  : boxed_column {
    label = "Preview - 1000 opening / 150 wall";
    : image { key = "preview"; width = 58; height = 20; color = -15; }
  }
  : text { label = "Select a setting, then choose Edit."; }
  spacer;
  : row {
    : button { key = "edit"; label = "Edit Selected"; is_default = true; width = 18; }
    : button { key = "accept"; label = "Done"; width = 12; }
    : button { key = "cancel"; label = "Cancel"; is_cancel = true; width = 12; }
  }
}

akd_value : dialog {
  label = "Edit Value";
  : boxed_column {
    : text { key = "title"; width = 42; }
    : text { key = "current"; width = 42; }
    spacer;
    : edit_box { label = "New value:"; key = "value"; width = 24; }
  }
  spacer;
  ok_cancel;
}

akd_marker : dialog {
  label = "Choose Marker";
  : boxed_column {
    : text { key = "title"; width = 42; }
    spacer;
    : popup_list { label = "Marker:"; key = "marker"; width = 24; }
  }
  spacer;
  ok_cancel;
}

akd_layer : dialog {
  label = "Edit Layer Style";
  : boxed_column {
    : text { key = "title"; width = 42; }
    spacer;
    : edit_box { label = "Layer name:"; key = "layer"; width = 26; }
    : edit_box { label = "Colour index:"; key = "colour"; width = 12; }
    : text { label = "Use AutoCAD colour numbers 1-256."; }
  }
  spacer;
  ok_cancel;
}
