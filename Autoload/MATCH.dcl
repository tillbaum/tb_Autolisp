MATCH : dialog {
      label = "MatchLine by JefferyPSanders.com";
       : column {
         : column {
           : boxed_row {
             label = "Style";
             : row {
               : toggle { key = "style1"; label = ""; }
               : image {
                 key = "img1";
                 width = 15;
                 aspect_ratio = 0.75;
                 color = 0;
               }

               : toggle { key = "style2"; label = ""; }
               : image {
                 key = "img2";
                 width = 15;
                 aspect_ratio = 0.75;
                 color = 0;
               }
             }
           }
           
           : boxed_row {
             label = "Text";
             : edit_box {
               key = "matchmark";
               label = "Mark:";
               edit_width = 12;
               value = "";
             }
             : edit_box {
               key = "sheetnum";
               label = "Sht Num:";
               edit_width = 12;
               value = "";
             }
           }
           : boxed_row {
             label = "Geometry";
             : boxed_column {
               label = "Line";
               : edit_box {
                 key = "linethk";
                 label = "Thk.:";
                 edit_width = 12;
                 value = "";
               }
               : popup_list {
                 key = "linelay";
                 label = "Layer:";
                 multiple_select = "FALSE";
                 width = 12;
                 height = 2;
               }
             }
             : boxed_column {
               label = "Text";
               : edit_box {
                 key = "textsiz";
                 label = "Height:";
                 edit_width = 12;
                 value = "";
               }  
               : popup_list {
                 key = "textsty";
                 label = "Style:";
                 multiple_select = "FALSE";
                 width = 12;
                 height = 2;
               }  
               : popup_list {
                 key = "textlay";
                 label = "Layer:";
                 multiple_select = "FALSE";
                 width = 12;
                 height = 2;
               }  
             }
           }
             : boxed_column {
               : toggle { key = "scalef"; label = "Do not Multiply by scale factors"; }
               : toggle { key = "override"; label = "Override Auto-Circle Size"; }
               : edit_box {
                 key = "circsiz";
                 label = "Circle Size:";
                 edit_width = 12;
                 value = "";
               }               
             }
           
           : row {
             : button {
               key = "insert";
               label = "Insert";
               is_default = true;
             }
             : button {
               key = "cancel";
               label = "Cancel";
               is_default = false;
               is_cancel = true;
             }
           }
         }
       }    
}
