SELECTDIR : dialog {  
      label="Select Directory  -  Copyright 2004 by JefferyPSanders.com  All rights reserved.";
      : text {
        key = "currentdirectory";
        is_bold = true;
      } 
      : row { 
        :column {
          : popup_list {
            key = "drives";  
            label = "Select a Drive :";  
            width = 25;
            fixed_width_font = true;
          } 
          : list_box {
            key = "directories";  
            label = "Select a Directory :";  
            width = 25;
            fixed_width_font = true;
          } 
        }
      } 
      : row { 
        : text {
          key = "DIRCOUNT";
        } 
      } 
      : row {
        : button {
          key = "accept";
          label = "  Okay  ";
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
