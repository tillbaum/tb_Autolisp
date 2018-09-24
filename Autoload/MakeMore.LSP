;;  MakeMore.LSP
;;  Concept inspired by CADALYST July 2008  www.cadalyst.com  Tip 2298:
;;    SetAs.lsp -- Make More of the Same (c) 2008 Mosad H. Elewa
;;  Thoroughly rewritten from scratch, corrected, expanded & improved by Kent Cooper
;;  Last revised March 2011
;
;;  To Make More of the same kind of entity as a selected object.
;;  Sets all appropriate properties (Layer, Color, Linetype, Linetype Scale, Lineweight, Thickness) to match
;;    selected object; leaves them that way, in case User wants to make more than one more matching object.
;;  Warns User if any properties other than Layer are different from default, suggesting resetting when done.
;;  Invokes the appropriate command to create the same kind of object.
;;  Offers choices where entity data alone cannot determine which command to use.
;;  Sets matching default values where possible, and/or offers other options, depending on object type [e.g.
;;    Offers default where entity data *suggests* a certain command for LWPolylines].
;;  Notifies User if selected object is not one that routine can replicate, but still sets its properties.
;;  [Note:  Using Enter to recall the last command, immediately after drawing something via MM, will
;;    recall MM itself; it will not recall the command that MM invoked.]
;
(defun C:MM
  (/ *error* cmde obj objdata objtyp mljust c1 c2 c3 plobj parpt equal42 pldata pltypdef plside pltyp dodia
  polytyp polymesh regtyp ins-name hXdata dohatch dtyp dbase drot ldrform tsty tht trot tins10 tins11 tjust72
  tjust73 tjust1 tjust2 tjust tstyht thtover twd tstylwd twdover lastent tcont tnext tlist tdata tjustno 3Dtyp htyp)
;
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); end if
    (setvar 'cmdecho cmde)
  ); end defun - *error*
;
  (vl-load-com)
  (setq cmde (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (while
    (not
      (setq obj (entsel "\nSelect object to Make More of the same: "))
    ); end not
    (prompt "\nNothing selected:")
  ); end while
  (setq
    objdata (entget (car obj))
    objtyp (cdr (assoc 0 objdata))
  ); end setq
  (setvar 'clayer (cdr (assoc 8 objdata)))
  (setvar 'cecolor (cond ((assoc 62 objdata) (itoa (cdr (assoc 62 objdata)))) (T "BYLAYER")))
  (setvar 'celtype (cond ((cdr (assoc 6 objdata))) (T "BYLAYER")))
  (setvar 'celtscale (cond ((cdr (assoc 48 objdata))) (T 1.0)))
  (setvar 'celweight (cond ((cdr (assoc 370 objdata))) (T -1)))
  (setvar 'thickness (cond ((cdr (assoc 39 objdata))) (T 0)))
  ; Warning & reset recommendation if certain things are not Bylayer or default:
  (if
    (or
      (/= (strcase (getvar 'cecolor)) "BYLAYER")
      (/= (strcase (getvar 'celtype)) "BYLAYER")
      (/= (getvar 'celtscale) 1.0)
      (/= (getvar 'celweight) -1)
      (/= (getvar 'thickness) 0)
    ); end or
    (alert
      (strcat
        "WARNING:  Recommend resetting\n"
        (if (/= (strcase (getvar 'cecolor)) "BYLAYER") "   Color to Bylayer\n" "")
        (if (/= (strcase (getvar 'celtype)) "BYLAYER") "   Linetype to Bylayer\n" "")
        (if (/= (getvar 'celtscale) 1.0) "   Linetype scale to 1\n" "")
        (if (/= (getvar 'celweight) -1) "   Lineweight to Bylayer\n" "")
        (if (/= (getvar 'thickness) 0) "   Thickness to 0\n" "")
        "when finished."
      ); end strcat
    ); end alert
  ); end if
;
  (cond; begin OVERARCHING TEST for each object type
;
    ((wcmatch objtyp
      "LINE,XLINE,RAY,SOLID,3DFACE,ARC,CIRCLE,ELLIPSE,SPLINE,IMAGE,WIPEOUT,TOLERANCE")
      (command (strcat "_." objtyp))
    ); end CATCH-ALL condition for object types whose command names are the same as their 'objtyp' values,
      ; and which don't need any other information extracted to set variables, nor have other complications
      ;;;;; pull IMAGE out and offer SUPERHATCH option, perhaps only if part of a Group?
      ;;;;; pull SOLID,3DFACE,WIPEOUT out and offer TEXTMASK option?
;
;;;;;  To offer SKETCH option for Line, if desired, delete "LINE," from catch-all above, delete these two lines,
;;;;;  and remove initial semicolons from 10 lines below
;    ((= objtyp "LINE")
;      (initget "Line Sketch")
;      (if (= (getkword "\nCommand option [Line/Sketch] <L>: ") "Sketch")
;        (progn ; then
;          (setvar 'skpoly 0)
;          (command "_.sketch")
;        ); end progn - Sketch option
;        (command "_.line"); else - ordinary-Line variety
;      ); end if - variety of Line
;    ); end cond - Line object type
;
    ((= objtyp "MLINE")
      (setq mljust (cdr (assoc 70 objdata)))
      (command "_.mline"
        "j"
          (cond
            ((= mljust 0) "T")
            ((= mljust 1) "Z")
            ((= mljust 2) "B")
          ); end cond - justification test
        "s" (cdr (assoc 40 objdata))
        "st" (cdr (assoc 2 objdata))
      ); end mline command
    ); end Mline object type
;
    ((= objtyp "TRACE")
      (setq c1 (cdr (assoc 10 objdata)) c2 (cdr (assoc 11 objdata)) c3 (cdr (assoc 12 objdata)))
      (setvar 'tracewid; default: perp. distance from 2nd corner to line between 1st and 3rd
        (distance; [if trace has been stretched and isn't constant width, this will be off]
          c2
          (inters c1 c3 c2 (polar c2 (+ (angle c1 c3) (/ pi 2)) 1) nil)
        ); end distance
      ); end setvar
      (command "_.trace")
    ); end Trace object type
;
    ((= objtyp "LWPOLYLINE")
      (if (= (getvar 'plinetype) 0) (setvar 'plinetype 2))
        ; in case set at 0 [old-style "heavy" 2D type]; value of 1 can remain [new ones still lightweight];
        ; assumes no desire to return it to 0 - add that resetting or option or recommendation, if desired
      (if (assoc 43 objdata); has global width
        (setvar 'plinewid (cdr (assoc 43 objdata))); then - match it
        (setvar 'plinewid 0); else - remove current width if non-zero
      ); end if
      (defun parpt (par); find Point at Parameter for Polyline type tests
        (vlax-curve-getPointAtParam plobj par)
      ); end defun
      (defun equal42 (val); find whether *all* bulge factors [(assoc 42) entries] have specified value
        (setq pldata objdata)
        (while (equal (cdr (assoc 42 pldata)) val 1e-6)
          (setq pldata (cdr (member (assoc 42 pldata) pldata))); remainder after this 42 entry
        ); end while
        (not (assoc 42 pldata)); returns T if they were all equal [none left]
      ); end defun
      (setq
        plobj (car obj)
        plverts (cdr (assoc 90 objdata)); number of vertices for type tests & to set 'polysides
        pltypdef ; PolyLine TYPe DEFault
          (cond
            ( (and
                (= plverts 4)
                (vlax-curve-isClosed plobj)
                (assoc 43 objdata); global width
                (equal42 0.0); all straight-line segments
                (equal (distance (parpt 0) (parpt 1)) (distance (parpt 2) (parpt 3)) 1e-8); opposite sides equal lengths
                (equal (distance (parpt 1) (parpt 2)) (distance (parpt 3) (parpt 0)) 1e-8)
                (equal (rem (abs (- (angle (parpt 0) (parpt 1)) (angle (parpt 1) (parpt 2)))) pi) (/ pi 2) 1e-8)
                  ; right angle first corner
              ); end and
              "Rectangle"
            ); end Rectangle condition
;;;;; Works only for four-sided square-cornered Rectangles; there are options, independent of similar options
;;;;; for general drawing: [Chamfer/Elevation/Fillet/Thickness/Width].
;;;;; Polyline with 8 vertices, 2nd & 6th segments same length, 4th & 8th segments same length, odd-numbered
;;;;; ones all same length, could be Rectangle with Chamfer or Fillet option.  If odd-numbered segments have
;;;;; (42 . 0.414214), Fillet [90-degree arc bulge factor].  If (equal42 0.0), Chamfer.
;;;;;  ***Don't know where those options are stored, or how to set them as defaults programmatically.***
            ( (and
                (> plverts 2)
                (vlax-curve-isClosed plobj)
                (member '(43 . 0.0) objdata); global width = 0
                (equal42 0.0); all straight-line segments
                (equal ; first two and last two segments, at least, all same length
                  (setq plside (distance (parpt 0) (parpt 1))); first segment length
                  (distance (parpt 1) (parpt 2)); second
                  1e-8
                ); end equal
                (equal (distance (parpt (- plverts 2)) (parpt (1- plverts))) plside 1e-8); next-to-last
                (equal (distance (parpt (1- plverts)) (parpt 0)) plside 1e-8); last
              ); end and
              "POlygon"
            ); end POlygon condition [does not check for equal angles]
            ( (and
                (= plverts 2)
                (vlax-curve-isClosed plobj)
                (assoc 43 objdata); global width, but only:
                (not (member '(43 . 0.0) objdata)); if non-zero
                (equal42 1.0); all full-semi-circle arc segments
              ); end and
              "Donut"
            ); end Donut condition
            ( (and
                (vlax-curve-isClosed plobj)
                (assoc 43 objdata)
                (equal42 0.520567); all Revcloud-type arc segments
              ); end and
              "Cloud"
            ); end Cloud condition
            (T "PLine"); none of the above [no default offering for Boundary or Sketch]
          ); end cond & pltypdef
      ); end setq
      (initget "PLine Rectangle POlygon Donut Cloud Boundary Sketch")
      (setq
        pltyp
          (getkword
            (strcat
              "\nPolyline Type [PLine/Rectangle/POlygon/Donut/Cloud/Boundary/Sketch] <"
              pltypdef
              ">: "
            ); end strcat
          ); end getkword
        pltyp (if pltyp pltyp pltypdef); use User entry, or default for Enter
      ); end setq
      (cond; variety of Polyline
        ((= pltyp "Rectangle")
          (command "_.rectangle" "w" (getvar 'plinewid))
        ); end Rectangle variety
        ((= pltyp "POlygon")
          (setvar 'polysides (if (> plverts 2) plverts 4))
          (setvar 'cmdecho 1)
          (command "_.polygon")
            (while (> (getvar 'cmdactive) 0) (command pause))
          (vlax-put (vlax-ename->vla-object (entlast)) 'ConstantWidth (getvar 'plinewid))
            ; Polygon doesn't honor width -- assign current [selected item's] width to new Polygon
        ); end POlygon variety
        ((= pltyp "Donut")
          (setq dodia; donut diameter at centerline
            (distance (vlax-curve-getStartPoint plobj) (vlax-curve-getPointAtParam plobj 1))
          ); end setq
          (setvar 'donutid (- dodia (getvar 'plinewid)))
          (setvar 'donutod (+ dodia (getvar 'plinewid)))
          (command "_.donut")
        ); end Donut variety
        ((= pltyp "Cloud") (command "_.revcloud"))
          ;;;;; Are arc min/max lengths determinable from objdata?
        ((= pltyp "Boundary")
          (setvar 'hpbound 1)
          (command "_.boundary")
        ); end Boundary variety
        ((= pltyp "Sketch")
          (setvar 'skpoly 1)
          (command "_.sketch")
        ); end Sketch variety
        (T (command "_.pline")); ordinary-PLine variety
      ); end cond - variety of LWPoly
    ); end cond - LWPoly object type
;
    ((= objtyp "POLYLINE")
      (setq polytyp (substr (cdr (assoc 100 (cdr (member (assoc 100 objdata) objdata)))) 5))
        ; *second* 100 value minus "AcDb" prefix
      (cond
        ((= polytyp "3dPolyline") (command "_.3dpoly"))
        ((= polytyp "2dPolyline")
          (initget "Heavy Lightweight")
          (if (= (getkword "\nMatch old Heavy 2D type, or use new Lightweight type? [H/L] <L>: ") "Heavy")
            (progn; then - old-style "heavy" 2D type
              (setvar 'plinetype 0)
              (alert "Recommend resetting the PLINETYPE\nSystem Variable to 1 or 2 when finished.")
                ; but doesn't save it and reset it, in case User needs to make more than one of them
              (command "_.pline")
            ); end progn
            (progn ; else - newer-style "lightweight" type
              (if (zerop (getvar 'plinetype)) (setvar 'plinetype 2))
                ; in case it was set at 0 [old-style "heavy" 2D type];
                ; value of 1 can remain [new ones still lightweight];
                ; assumes no desire to return it to 0 - add that resetting or option or recommendation, if desired
              (command "_.pline")
            ); end progn
          ); end if
        ); end second condition - 2dPolyline type
        ((= polytyp "PolygonMesh"); [couldn't find a way to differentiate types from entity data]
          (initget "3D 3DMesh Pface REvsurf RUlesurf Tabsurf")
          (setq polymesh (getkword
            "\nPolygon Mesh command [3D/3DMesh/Pface/REvsurf/RUlesurf/Tabsurf] <3D>: "))
          (if (or (not polymesh) (= polymesh "3D")); user hit Enter or typed 3D
            (3d x); then - use default [don't know why it works this way, and not in (command) function]
            (command (strcat "_." polymesh)); else - other entered option
          ); end if
        ); end third condition - mesh types
      ); end cond - variety of polyline
    ); end 3D/heavy 2D Polyline object type
;
    ((= objtyp "REGION")
      (initget "Objects Boundary Section")
      (setq regtyp (getkword "Region source [Objects/Boundary/Section] <O>: "))
      (cond
        ((= regtyp "Boundary")
          (setvar 'hpbound 0)
          (command "_.boundary")
        ); end Boundary-defined region source
        ((= regtyp "Section") (command "_.section"))
        (T (command "_.region"))
      ); end if - region source
    ); end Region object type
;
    ((= objtyp "INSERT"); overall Block/Minsert/Xref/Metafile/Light/old-style Hatch category
      ;;;;; offer SUPERHATCH option, perhaps only if part of a Group?
      (setq ins-name (cdr (assoc 2 objdata)))
      (cond
        ((= (substr ins-name 1 2) "*X"); identify old-style Hatch pattern
          (setq hXdata (cddadr (assoc -3 (entget (car obj) '("ACAD"))))); extended data
          (setvar 'hpname (cdr (assoc 1000 hXdata)))
          (setvar 'hpscale (cdr (assoc 1040 hXdata)))
          (setvar 'hpang (cdr (assoc 1040 (cddddr hXdata)))); second 1040 = rotation in radians
          (if (= (getvar 'hpname) "_U"); User-defined
            (progn
              (setvar 'hpspace (cdr (assoc 1040 hXdata)))
              (setvar 'hpdouble (cdr (assoc 1070 (cddddr hXdata)))); double-direction
            ); end progn
          ); end if
          (setq dohatch T); picked up by later (if) after overall object-type (cond) is done; also with Hatch object type
        ); end old-style Hatch variety
        ((= (logand 4 (cdr (assoc 70 (tblsearch "block" (cdr (assoc 2 objdata)))))) 4); identify Xref
          (initdia)
          (command "_.xref")
        ); end Xref variety
          ;;;;; XCLIP option?  identifiable from entity data?
        ((= (substr ins-name 1 3) "WMF"); identify Windows Metafile [if it hasn't been renamed]
          (command "_.wmfin")
        ); end Metafile variety
        ((wcmatch ins-name "direct,overhead,sh_spot"); identify Light [assuming names not used otherwise]
          (command "_.light")
        ); end Light variety
        (T; Minsert or ordinary Block [both need next line]
          (setvar 'insname ins-name)
          (if (= (cdr (assoc 100 (cdr (member (assoc 100 objdata) objdata)))) "AcDbMInsertBlock")
            ; identify Minsert by *second* 100 value
            (command "_.minsert"); then
            (progn (initdia) (command "_.insert")); else - ordinary Block
              ;;;;; offer Divide & Measure options?
          ); end if - Minsert or Block option
        ); end Minsert/Block variety
      ); end cond - variety of Insert
    ); end Insert object type
;
    ((= objtyp "SHAPE")
      (setvar 'shpname (cdr (assoc 2 objdata)))
      (command "_.shape")
    ); end Shape object type
;
    ((= objtyp "HATCH")
      (setvar 'hpname (cdr (assoc 2 objdata))); hatch pattern
      (if (wcmatch (getvar 'hpname) "U,_USER")
        (progn
          (setvar 'hpspace (cdr (assoc 41 objdata)))
          (setvar 'hpdouble (cdr (assoc 77 objdata)))
        ); end progn
      ); end if
      (if (/= (getvar 'hpname) "SOLID")
        (progn
          (setvar 'hpscale (cdr (assoc 41 objdata)))
          (setvar 'hpang (cdr (assoc 52 objdata)))
        ); end progn
      ); end if
      (setvar 'hpassoc (cdr (assoc 97 objdata)))
      (setq dohatch T); picked up by later (if) after overall object-type (cond) is done; also with Insert old-style Hatch object type
    ); end Hatch object type
;
    ((= objtyp "DIMENSION")
      (command "_.dimstyle" "r" (cdr (assoc 3 objdata)))
      (setvar 'cecolor "bylayer"); dimensions do not honor color overrides
      (setq dtyp (cdr (assoc 70 objdata))) 
      (setq dXdata (cadr (assoc -3 (entget (car obj) '("ACAD"))))); extended data
      (setvar 'dimse1 (if (member '(1070 . 75) dXdata) (cdadr (member '(1070 . 75) dXdata)) 0))
      (setvar 'dimse2 (if (member '(1070 . 76) dXdata) (cdadr (member '(1070 . 76) dXdata)) 0))
      (setvar 'dimsd1 (if (member '(1070 . 281) dXdata) (cdadr (member '(1070 . 281) dXdata)) 0))
      (setvar 'dimsd2 (if (member '(1070 . 282) dXdata) (cdadr (member '(1070 . 282) dXdata)) 0))
      ;;;;; matches suppression [if any] of Dimension & Extension lines only; could add more override matches
      ;;;;; match Obliquing?
      ;;;;; QDIM option?
      (cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; doesn't account for relocated text [adds 128 bit to (assoc 70) value]
        ((= dtyp 163) (command "_.dimdiameter"))
        ((= dtyp 164) (command "_.dimradius"))
        (T ; continue and baseline options available for all but diameter and radius
          (initget "New Continue Baseline")
          (setq dbase (getkword "Dimension basis [New/Continue/Baseline] <N>: "))
          (cond
            ((= dbase "Continue") (command "_.dimcontinue" "s" obj))
            ((= dbase "Baseline") (command "_.dimbaseline" "s" obj))
            ((= dtyp 33) (command "_.dimaligned"))
            ((or (= dtyp 34) (= dtyp 37)) (command "_.dimangular"))
            ((or (= dtyp 38) (= dtyp 102)) (command "_.dimordinate")); 38 Ydatum, 102 Xdatum
            ((= dtyp 32)
              (setq drot (cdr (assoc 50 objdata))); dimension line rotation
              (if (not (or (= drot 0) (= drot (/ pi 2)))); other than 0 or 90 degrees
                (command "_.dimrotated" (* (/ drot pi) 180))
                (command "_.dimlinear")
              ); end if
            ); end Rotated varieties
          ); end cond - dimension basis option
        ); end continuable types
      ); end cond - variety of dimension
    ); end Dimension object type
;
    ((= objtyp "LEADER")
      (command "_.dimstyle" "r" (cdr (assoc 3 objdata)))
      (setvar 'cecolor "BYLAYER"); leaders don't honor color overrides
      (setvar 'celtscale 1.0); leaders don't honor linetype scale [even though they do honor linetypes]
      (setq ldrform (if (= (cdr (assoc 72 objdata)) 0) "ST" "S")); STraight vs. Spline format
      (setvar 'cmdecho 1)
      (command "_.leader" pause pause "f" ldrform)
    ); end Leader object type
    ;;;;; QLEADER option?
;
    ((= objtyp "TEXT")
      (setvar 'textstyle (setq tsty (cdr (assoc 7 objdata))))
      (setvar 'textsize (setq tht (cdr (assoc 40 objdata))))
      (setq
        trot (angtos (cdr (assoc 50 objdata))); rotation - current angular units
        tins10 (cdr (assoc 10 objdata)); left end of baseline [insertion point if Left justified]
        tins11 (cdr (assoc 11 objdata)); insertion point [other than Left justified]
        tjust72 (cdr (assoc 72 objdata))
        tjust73 (cdr (assoc 73 objdata))
        tjust1 (nth tjust73 '(0 "B" "M" "T"))
        tjust2 (nth tjust72 '("L" "C" "R" "A" "M" "F"))
        tjust (if (= tjust73 0) tjust2 (strcat tjust1 tjust2)); if single-letter justification, 72 only; otherwise, combination of 73+72
        tstyht (cdr (assoc 40 (tblsearch "style" tsty))); Style's height, to check for override to fixed-height Style
        thtover (and (/= tstyht 0) (/= tstyht tht) (/= tjust "A"))
          ; T if Style is fixed-height, but selected text has different height override, not from Aligned justification
        twd (cdr (assoc 41 objdata)); width factor
        tstywd (cdr (assoc 41 (tblsearch "style" tsty))); Style's width factor, to check for override
        twdover (and (/= tstywd twd) (/= tjust "F"))
          ; T if selected text has width override different from Style's width factor, not from Fit justification
      ); end setq
        ; [NOTE: height and/or width overrides must be imposed *after* end of command; new Text entities
        ; will appear at standard height/width during command, and will have overrides imposed afterwards;
        ; if MM is followed by Text or Dtext for more of the same, such overrides will not be imposed -- must
        ; use Match Properties, or use MM again instead of Text/Dtext command.]
      (if (or thtover twdover); if selected has either non-Aligned-based height override or non-Fit-based width override
        (setq lastent (entlast)); put marker at last entity, to gather all subsequent entities later
      ); end if
      (initget "New Continuation")
      (setq tcont (getkword "\nNew insertion point or Continuation of selected text [N/C] <N>: "))
      (setvar 'cmdecho 1)
      (cond; - Text insertion-point New-vs.-Continuation choice
        ((or (= tcont "New") (not tcont)); when with New user-supplied insertion point [explicit or default]
          (cond; non-fixed- vs. fixed-height Style
            ((= (cdr (assoc 40 (tblsearch "style" tsty))) 0); NON-fixed-height Style
              (cond; justification
                ((= tjust "A") (command "_.dtext" "j" "a" pause pause)); Aligned [no height or rotation]
                ((= tjust "F") (command "_.dtext" "j" "f" pause pause "")); Fit [no rotation]
                ((and (= tjust72 0) (= tjust73 0)) (command "_.dtext" pause "" trot)); Plain-left
                (T (command "_.dtext" "s" tsty "j" tjust pause "" trot)); other justifications
              ); end cond - justification sub-category
            );end NON-fixed-height Style Text category
            (T; FIXED-height Style
              (cond; justification
                ((or (= tjust "A") (= tjust "F")) (command "_.dtext" "j" "a" pause pause)); Aligned or Fit [no rotation]
                ((and (= tjust72 0) (= tjust73 0)) (command "_.dtext" pause trot)); Plain-left
                (T (command "_.dtext" "j" tjust pause trot)); other justifications
              ); end cond - justification sub-category
            );end FIXED-height Style Text category
          ); end cond - non-fixed vs. fixed determination
        ); end cond - New user-supplied insertion point option
        (T; when Continuation of selected text
          (cond; non-fixed- vs. fixed-height Style
            ((= (cdr (assoc 40 (tblsearch "style" tsty))) 0); NON-fixed-height Style
              (cond; justification
                ((= tjust "A") (command "_.text" "j" "a" tins10 tins11 "" "_.dtext" "")); Aligned [no height or rotation]
                ((= tjust "F") (command "_.text" "j" "f" tins10 tins11 "" "" "_.dtext" "")); Fit [no rotation]
                ((and (= tjust72 0) (= tjust73 0)) (command "_.text" tins10 "" trot "" "_.dtext" "")); Plain-left
                (T (command "_.text" "j" tjust tins11 "" trot "" "_.dtext" "")); other justifications
              ); end cond - justification sub-category
            );end NON-fixed-height Style Text category
            (T; FIXED-height Style
              (cond; justification
                ((or (= tjust "A") (= tjust "F")) (command "_.text" "j" "a" tins10 tins11 "" "_.dtext" "")); Aligned or Fit [no rotation]
                ((and (= tjust72 0) (= tjust73 0)) (command "_.text" tins10 trot "" "_.dtext" "")); Plain-left
                (T (command "_.text" "j" tjust tins11 trot "" "_.dtext" "")); other justifications
              ); end cond - justification sub-category
            );end FIXED-height Style Text category
          ); end cond - non-fixed vs. fixed determination
        ); end cond - Continuation of selected Text option
      ); end cond - Text insertion-point New-vs.-Continuation choice
      (if (or thtover twdover); if either or both kind(s) of override
        (progn ; make list of new Dtext command's new entities [can be more than one]
          (while (> (getvar 'cmdactive) 0)
            (command pause)
          ); end while - wait for completion of dtext command
          (setq tnext (entnext lastent) tlist (list tnext)); start list with first of entities
          (while (entnext tnext) (setq tnext (entnext tnext) tlist (cons tnext tlist))); put remainder into list
        ); end progn
      ); end if
      (if thtover
        (foreach x tlist
            (setq
              tdata (entget x)
              tdata (subst (cons 40 tht) (assoc 40 tdata) tdata); impose height override
            ); end setq
            (entmod tdata)
            (entupd x)
        ); end foreach
      ); end if
        ; [NOTE: if Text of fixed-height Style has height override, must be imposed *after* command;
        ; if doing sequential lines using Enter between, individual Text entity heights will have height
        ; override imposed to match selected Text, but line *spacing* will be as for height in Style
        ; definition, *not* adjusted for imposed override height.]
      (if twdover
        (foreach x tlist
            (setq
              tdata (entget x)
              tdata (subst (cons 41 twd) (assoc 41 tdata) tdata); impose width override
            ); end setq
            (entmod tdata)
            (entupd x)
        ); end foreach
      ); end if
    ); end Text object type
;
    ((= objtyp "MTEXT")
      (setq
        tjustno (cdr (assoc 71 objdata)); justification number
        tjust; justification text
          (nth tjustno '(0 "TL" "TC" "TR" "ML" "MC" "MR" "BL" "BC" "BR"))
        trot (angtos (cdr (assoc 50 objdata))); rotation - current angular units
        tstyht (cdr (assoc 40 (tblsearch "style" (cdr (assoc 7 objdata))))); Style's height
      ); end setq
      (setvar 'textstyle (cdr (assoc 7 objdata))); must do outside Mtext command, or line spacings revert to defaults
      (setvar 'textsize (cdr (assoc 40 objdata)))
      (setvar 'tspacetype (cdr (assoc 73 objdata)))
      (setvar 'tspacefac (cdr (assoc 44 objdata)))
      (setvar 'cmdecho 1)
      (initdia)
      (if (/= tstyht (cdr (assoc 40 objdata))); Mtext of fixed-height style has different height override
        (command "_.mtext" pause "h" (cdr (assoc 40 objdata)) "j" tjust "r" trot); ask for height
        (command "_.mtext" pause "j" tjust "r" trot); don't
      ); end if
    ); end Mtext object type
;
    ((= objtyp "RTEXT")
      (command
        "_.text" "_style" "standard" (getvar 'viewctr) 1 (angtos (cdr (assoc 50 objdata))) "delete"
          ; Assumes Standard text style has 0 height; rotation is in current angular units.
          ; Draws temporary piece of text to make selected object's angle current -- apparently
          ; no System Variable to set, and seems to need to be set prior to Rtext command.
        "_.erase" "_last" ""
      ); end command
      (setvar 'textstyle (setq tsty (cdr (assoc 7 objdata))))
      (setvar 'textsize (setq tht (cdr (assoc 40 objdata))))
      (C:rtext)
    ); end Rtext object type
;
    ((= objtyp "ARCALIGNEDTEXT")
      (setvar 'textstyle (setq tsty (cdr (assoc 7 objdata))))
      (command "_.arctext")
    ); end Arctext object type
;
    ((= objtyp "POINT")
      (initget 1 "Divide Measure")
      (setq pdm (getpoint "\nSpecify a point or [Divide/Measure]: "))
      (cond
        ((= pdm "Divide") (command "_.divide"))
        ((= pdm "Measure") (command "_.measure"))
        (T
          (command "_.point" pdm)
          (setvar 'cmdecho 1)
          (while T (command "_.point" pause))
        ); end specified-Point condition
      ); end cond
    ); end Point object type
;
    ((= objtyp "3DSOLID"); [couldn't figure a way to distinguish types from entity data]
      (initget "Box Wedge CYlinder COne Sphere Torus Extrude Revolve Union Intersect")
      (setq 3Dtyp (getkword
        "\n3D Solid command [Box/Wedge/CYlinder/COne/Sphere/Torus/Extrude/Revolve/Union/Intersect] <B>: "))
      (if (not 3Dtyp); user hit Enter
        (command "_.box"); then - use default
        (command (strcat "_." 3Dtyp)); else - entered option
      ); end if
    ); end 3DSolid object type
;
    ((= objtyp "ATTDEF") (initdia) (command "_.ATTDEF"))
;
    ((= objtyp "VIEWPORT") (command "_.vports"))
;
    ((= objtyp "BODY")
      (initget "Acisin Explode")
      (command
        (strcat
          "_."
          (getkword "\nAcisIn/Explode 3D solid [A/E]? ")
        ); end strcat
      ); end command
    ); end Body object type
;
    ((= objtyp "PLANT") (C:lsnew))
;
    ; other possible up-to-2004 entity types/commands: 3DSIN?
    ; other newer-than-2004 entity types/commands: 3DDWF? DGNATTACH/DGNIMPORT? DIMJOGGED/DIMJOGLINE?
    ; DISTANTLIGHT? DWFATTACH? FIELD? HELIX? IMPRESSION? IMPRINT? JOGSECTION? LOFT? MARKUP?
    ; MESH? MLEADER? PDFATTACH? PLANESURF? POINTLIGHT? POLYSOLID? QVDRAWING? SECTIONPLANE?
    ; SPOTLIGHT? TABLE? TINSERT? DYNAMIC BLOCK?
;
    (T ; none of the above object types
      (alert
        (strcat
          "Routine is not yet set up to Make More of the "
          objtyp
          " object type,\nbut has set current Properties to match it."
        ); end strcat
      ): end alert
    ); end none-of-the-above condition
;
  ); end OVERARCHING TEST for each object type
;
  (if dohatch; (command) part if referred to from Hatch or Insert old-style Hatch object types above
    (progn
      (initget "Bhatch Hatch")
      (setq htyp (getkword "\nBhatch or Hatch [B/H] <B>: "))
      (if (= htyp "Hatch")
        (command "_.hatch")
        (progn (initdia) (command "_.bhatch"))
      ); end if - type of hatch command
    ); end progn
  ); end if
;
  (setvar 'cmdecho cmde)
  (princ)
); end defun
(prompt "\nType MM to Make More the same as an existing object.")
; end MakeMore.lsp
