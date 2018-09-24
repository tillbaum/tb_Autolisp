LOADLSP : dialog {
        label = "LOADLSP - Lisp Loader  v2.0   Copyright 2004 by JefferyPSanders.com  All rights reserved.";
        : column {
          : row {
            : boxed_column {
              : list_box {
                key = "shortlist";
                label = "Available AutoLisp Programs";
                multiple_select = "FALSE";
                width = 85;
                height = 30;
                fixed_width_font = true;
                value = "";
              }
            }   
          }
          : row {
            : boxed_row {
              : button {
                key = "revsrch";
                label = "Revise Search Paths";
                is_default = false;
              }
              : button {
                key = "revdes";
                label = "Revise Description";
                is_default = false;
              }
              : button {
                key = "accept";
                label = "  Load  ";
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
}

REVPATH : dialog {
        label = "Revise Search Paths";
        : column {
          : row {
            : boxed_column {
              : list_box {
                key = "pathlist";
                label = "Search Paths";
                multiple_select = "FALSE";
                width = 60;
              }
            }   
          }
          : row {
            : boxed_row {
              : button {
                key = "rempath";
                label = "Remove Path";
                is_default = false;
              }
              : button {
                key = "addpath";
                label = "Add Path";
                is_default = false;
              }
              : button {
                key = "saveclose";
                label = "Save and Close";
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
}

REVDESC : dialog {
        label = "Revise Program Description";
        initial_focus = "newdesc";
        : column {
          : column {
            : text {
               key = "olddesc";
               is_bold = true;
            } 
            : edit_box  {
              key = "newdesc";
              label = "New Description";
              edit_width = 60;
              value  = "";
            }
          }
          : row {
            : boxed_row {
              : button {
                key = "accept2";
                label = " Update ";
                is_default = true;
              }
              : button {
                key = "cancel2";
                label = "  Cancel  ";
                is_default = false;
                is_cancel = true;
              }
            }
          }
        }
}            