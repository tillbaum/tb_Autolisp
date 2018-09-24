CSVTABLE : dialog {
           label = "CSVTABLE - Create a table from a CSV file";
           : column {
             : boxed_column {
               : popup_list {
                 key = "linelay";
                 label = "Line Layer:";
                 width = 15;
                 value = "0";
               }         
               : popup_list {
                 key = "textlay";
                 label = "Text Layer:";
                 width = 15;
                 value = "0";
               }         
               : popup_list {
                 key = "textsty";
                 label = "Text Style:";
                 width = 15;
                 value = "0";
               }         
             }
             : boxed_row {
               : button {
                 key = "accept";
                 label = "  Okay  ";
                 is_default = true;
               }
               : button {
                 key = "cancel";
                 label = "  Cancel  ";
                 is_default = false;
                 is_cancel = true;
               }
             }
           }

}

