CAD2FILE : dialog {
     label = "Cad2File     JefferyPSanders.com     Ver 2.0";
     : column {
       : boxed_column {
         : popup_list { key = "entityType"; label = "Entity Type:"; width = 25; value = "0"; }
         : popup_list { key = "fileType";   label = "File Type:   ";   width = 25; value = "0"; }
       }
       : boxed_row {
         label = "Items to Send to File";
         : column {
           : toggle { key = "tog1";  label = "Name"; }
           : toggle { key = "tog2";  label = "Layer Name"; }
           : toggle { key = "tog3";  label = "Color"; }
           : toggle { key = "tog4";  label = "Insertion Point"; }
           : toggle { key = "tog5";  label = "Start/Center Point"; }
           : toggle { key = "tog6";  label = "End Point"; }
           : toggle { key = "tog10"; label = "Line Type"; }
           : toggle { key = "tog16"; label = "Rotation/Angle"; }
           : toggle { key = "tog17"; label = "Area"; }
           : toggle { key = "tog19"; label = "Handle"; }
         }
         : column {
           : toggle { key = "tog7";  label = "Tag"; }
           : toggle { key = "tog8";  label = "Text Value"; }
           : toggle { key = "tog9";  label = "Style"; }
           : toggle { key = "tog11"; label = "Radius/Height"; }
           : toggle { key = "tog12"; label = "Diameter/Width"; }
           : toggle { key = "tog13"; label = "Vertex/Control Points"; }
           : toggle { key = "tog14"; label = "Major Axis"; }
           : toggle { key = "tog15"; label = "Minor Axis"; }
           : toggle { key = "tog18"; label = "Length/Perimeter"; }
         }
       }
       : row {
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
}


