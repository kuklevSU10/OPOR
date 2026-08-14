// OPOR v3.6 DCL dialogs. CP1251! ѕравки Ч только с сохранением кодировки.

opor_mode : dialog {
  label = "OPOR";
  : image { key = "logo"; width = 30; height = 5; color = -15; alignment = centered; }
  spacer;
  : row {
    : button { key = "const";   label = "A";       width = 12; }
    : button { key = "var";     label = "B";       width = 12; }
    : button { key = "slope";   label = "Slope";   width = 12; }
  }
  : row {
    : button { key = "slopewr"; label = "%";       width = 12; }
    : button { key = "chkh";    label = "h";       width = 12; }
    : button { key = "ring";    label = "Ring";    width = 12; }
  }
  : row {
    : button { key = "wrlevl";  label = "+ 0.000"; width = 12; }
    : button { key = "clean";   label = "ќчистка"; width = 12; }
    : button { key = "check";   label = "?";       width = 12; }
  }
  : row {
    spacer;
    : button { key = "tin"; label = "TIN"; width = 12; }
    : button { key = "geo"; label = "√ео"; width = 12; }
    : button { key = "info"; label = "i"; width = 4; fixed_width = true; }
    spacer;
  }
  spacer;
  : button { key = "cancel"; label = "ќтмена"; is_cancel = true; fixed_width = true; alignment = centered; }
  spacer;
  : text { label = "project@sayangroup.ru      тел. +7 495 136 6050"; alignment = centered; }
}

opor_mode_help : dialog {
  label = "ѕодсказка OPOR";
  : boxed_column {
    label = "ќсновные расчЄты";
    : text { label = "A Ч посто€нна€ высота опор"; width = 62; }
    : text { label = "B Ч переменна€ высота по област€м и отметкам"; }
    : text { label = "Slope Ч раскрасить опоры и посчитать корректоры уклона"; }
    : text { label = "% Ч рассчитать и записать проценты в блоки уклона"; }
  }
  : boxed_column {
    label = "¬спомогательные команды";
    : text { label = "h Ч пересчитать выбранные отметки"; }
    : text { label = "Ring Ч ведомость уже расставленных опор"; }
    : text { label = "+0.000 Ч расставить пустые отметки"; }
    : text { label = "ќчистка Ч удалить созданные OPOR объекты"; }
    : text { label = "? Ч проверить высоту выбранной опоры"; }
    : text { label = "TIN Ч построить области высот по отметкам"; }
    : text { label = "√ео Ч отметки по геоподоснове со сло€ GEO_POINTS"; }
  }
  : text { key = "helpver"; label = ""; alignment = centered; }
  ok_only;
}

opor_params : dialog {
  label = "¬вод";
  : row {
    : boxed_column {
      label = "Ўаги опор";
      : edit_box { key = "stepx"; label = "вдоль вектора"; edit_width = 8; }
      : edit_box { key = "stepy"; label = "вдоль перпендикул€ра"; edit_width = 8; }
    }
    : boxed_column {
      label = "–азмер плитки";
      : edit_box { key = "tilex"; label = "вдоль вектора"; edit_width = 8; }
      : edit_box { key = "tiley"; label = "вдоль перпендикул€ра"; edit_width = 8; }
    }
    : boxed_radio_column {
      label = "ќбща€ длина";
      : radio_button { key = "ax_vect"; label = "вдоль вектора"; }
      : radio_button { key = "ax_perp"; label = "вдоль перпендикул€ра"; }
    }
  }
  : row {
    : edit_box { key = "floor";  label = "ќтметка чистого пола";    edit_width = 8; }
    : edit_box { key = "radius"; label = "–адиус обозн. опоры";     edit_width = 8; }
  }
  : row {
    : boxed_column {
      label = "ѕокрытие";
      : row {
        : radio_button { key = "cov_d"; label = "ƒоска"; }
        : radio_button { key = "cov_p"; label = "ѕлитка"; }
      }
      : edit_box { key = "boardwidth"; label = "ширина доски";  edit_width = 6; }
      : edit_box { key = "doska";      label = "толщина доски"; edit_width = 6; }
      : edit_box { key = "lagwidth";   label = "ширина лаги";   edit_width = 6; }
      : edit_box { key = "lag";        label = "высота лаги";   edit_width = 6; }
      : edit_box { key = "plitka"; label = "толщина плитки"; edit_width = 6; }
      : edit_box { key = "boardlen"; label = "длина доски"; edit_width = 7; }
      : popup_list { key = "boardlayout"; label = "раскладка"; edit_width = 12; }
    }
    : boxed_radio_column {
      label = "Ћинейка";
      : radio_button { key = "ln_3d";  label = "Level 3D"; }
      : radio_button { key = "ln_pro"; label = "Level PRO"; }
    }
  }
  : boxed_row {
    label = " репЄж";
    : popup_list { key = "flag";  label = "лаги";   edit_width = 14; }
    : popup_list { key = "ftile"; label = "плитки"; edit_width = 14; }
    : edit_box   { key = "fstep"; label = "шаг";    edit_width = 6; }
  }
  : row {
    : toggle { key = "tri";     label = "показать разбивку"; }
  }
  ok_cancel;
  errtile;
}

opor_support : dialog {
  label = "ќпора";
  : list_box { key = "list"; width = 46; height = 14; }
  ok_cancel;
}
