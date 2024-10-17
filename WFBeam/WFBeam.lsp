
;****************************************************************************************************************
  ; Creates a wide flange beam for structural detailing 
  ; Credit to Lee Mac for the csv parser that saved me what feels like a billion years - https://www.lee-mac.com/
;****************************************************************************************************************
 
(defun C:WFBeam ( / oldsnap oldexp oldcel oldcelt FoundHidden
                    item data dlist maxs 
                    count chrct numb WFBeamSize searchLength file fp DesignationImperial
                    tmp:DepthInt tmp:WeightInt tmp:BeamLength disp:BeamLength
                    Depth Width WebThickness FlangeThickness FlangeToWeb FlangeToFlange WebToFlange
                    ip wf_fv_p1 wf_fv_p2 wf_fv_p3 wf_fv_p4 wf_fv_p5 wf_fv_p6 wf_fv_p7 wf_fv_p8 wf_fv_p9 wf_fv_p10 wf_fv_p11 wf_fv_p12 wf_fv_p13 wf_fv_p14 wf_fv_p15 wf_fv_p16 wf_fv_p17 wf_fv_p18 wf_fv_p19
                    wf_sv_ip wf_sv_p1 wf_sv_p2 wf_sv_p3 wf_sv_p4 wf_sv_p5 wf_sv_p6 wf_sv_p7
                    wf_tv_ip wf_tv_p1 wf_tv_p2 wf_tv_p3 wf_tv_p4 wf_tv_p5 wf_tv_p6 wf_tv_p7
                    wf_fv_topmidp wf_fv_cp
                    *error*
                 ) 

;****************************************************************************************************************
  ; Define Error
;****************************************************************************************************************
  
  (defun *error* ( msg )
    
    (if oldsnap (setvar "osmode" oldsnap))
    (if oldexp (setvar "expert" oldexp))
    (if oldcel (setvar "celtype" oldcel))
    (if oldcelt (setvar "celtscale" oldcelt))
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))      
    );end if
    (princ)
  );end defun

;****************************************************************************************************************
  ; Save Sysvars
;****************************************************************************************************************
 
  ;Save Snap Settings
  (setq oldsnap (getvar 'osmode))
  ;Turn off snaps
  (setvar "osmode" 0)
  ;Save Expert setting
  (setq oldexp (getvar "expert"))
  ;Turn Expert up to 3
  (setvar "expert" 3)

  
;****************************************************************************************************************
  ; Get Hidden Line Type
;****************************************************************************************************************
  
  ;Check if desired linetype is loaded
  (setq FoundHidden (tblsearch "LTYPE" "HIDDENX2"))
  ;If Hidden is not found load Hidden
  (if (= FoundHidden nil)
    (command "-linetype" "load" "HIDDENX2" "acad.lin" "")
  );end if
  
  ;Save Celtype
  (setq oldcel (getvar "celtype"))
  ;Save Celtscale
  (setq oldcelt (getvar "celtscale"))
  
;****************************************************************************************************************
  ; Set Defaults
;****************************************************************************************************************
  
  (if (or (null global:DepthInt) (= global:DepthInt ""))
    (setq global:DepthInt 12)
  ); end if
  
  (if (or (null global:WeightInt) (= global:WeightInt ""))
    (setq global:WeightInt 35)
  ); end if
  
  (if (or (null global:BeamLength) (= global:BeamLength ""))
    (setq global:BeamLength 72.0)
  ); end if

  (setq disp:BeamLength (rtos global:BeamLength 4 4))
  
;****************************************************************************************************************
  ; Assign Variables
;****************************************************************************************************************

  ;Get Depth as an integer to convert to string later
  (initget 6)
  (if (setq tmp:DepthInt (getint (strcat "\nEnter WF Beam Depth <" (itoa global:DepthInt) "> : ")))	;enter WFBeamSize req'd
    (setq global:DepthInt tmp:DepthInt)
  ); end if
  
  ;Get Weight as an integer to convert to string later
  (initget 6)
  (if (setq tmp:WeightInt (getint (strcat "\nEnter WF Beam Weight <" (itoa global:WeightInt) "> : ")))	;enter WFBeamSize req'd
    (setq global:WeightInt tmp:WeightInt)
  ); end if
  
  ;Get Length of Beam
  (setq tmp:BeamLength (getstring T (strcat "\nLength of WF Beam <" disp:BeamLength "> : ")))
  (cond
    
    ((and (= (ascii tmp:BeamLength) 32) (/= (ascii (substr tmp:BeamLength 2)) 32))
     (setq tmp:BeamLength (substr tmp:BeamLength 2))
     (setq global:BeamLength (distof tmp:BeamLength 4))
     ); cond 1
    
    ((and (= (type tmp:BeamLength) 'STR) (/= tmp:BeamLength ""))
     (setq global:BeamLength (distof tmp:BeamLength 4))
     ); cond 2
    
    (T nil); final cond
    
  ); end cond
  
;****************************************************************************************************************
  ; Get Insertion Point
;****************************************************************************************************************
  
  (initget 1)
  (setq ip (getpoint "\nInsertion Point : "))

;****************************************************************************************************************
  ; Parse CSV and Assign Variables 
  ; Lee Mac Made this go to his site if you need that sweet nectar we call knowledge - https://www.lee-mac.com/
;****************************************************************************************************************

  (setq	dlist nil
	WFBeamSize  (strcat "W " (itoa global:DepthInt) " x " (itoa global:WeightInt))
  searchLength (strlen WFBeamSize)
	file  (findfile "C:/YourFilePathHere/WFBeam/WFBeamData.dat")			;find data file
	fp    (open file "r")				;open file to read
	item  (read-line fp)				;first line is label for file
  );setq
  (while item						;process each line of file
    (setq searchItem (substr item 1 searchLength))
    (if	(= searchItem  WFBeamSize)					;compare values
      (setq data item		;read a line
	          item nil					;stop searching for item
      );setq
      (setq item (read-line fp))			;keep searching for item
    );if
  );while
  (if data						;if the  WFBeamSize has been found
    (progn
      (setq maxs  (strlen data)				;establish length of input
	          count 1					;initiliaze count
	          chrct 1					;initiliaze char position
      );setq
      (while (< count maxs)				;process string one chr at a time
	(if (/= "," (substr data count 1))		;look for the commas
	  (setq chrct (1+ chrct))			;increment to next position
	  (setq	numb  (atof 
               (substr data 
		  (1+ (- count chrct)) chrct))  	;convert to real
		dlist (append dlist (list numb))	;add it to the list
		chrct 1					;resets field ct
	  );setq
	);if
	(setq count (1+ count))				;increment the counter
      );while
      (setq numb  (atof 
           (substr data 
                  (1+ (- count chrct))))        	;convert to real
	    dlist (append dlist (list numb))		;add it to the list
      );setq
    );progn
  );if data
(mapcar 'set '(DesignationImperial Depth Width WebThickness FlangeThickness) dlist)				;allocate to variables
  (close fp)						;close data file
 
;****************************************************************************************************************
  ; Arithmatic into Variables
;****************************************************************************************************************

  (setq FlangeToWeb (- (/ Width 2) (/ WebThickness 2)))
  (setq WebToFlange FlangeToWeb)
  (setq FlangeToFlange (- Depth (* FlangeThickness 2)))
  
;****************************************************************************************************************
  ; Polar math front view
;****************************************************************************************************************

  (setq wf_fv_p1 (polar ip (dtr 90.0) FlangeThickness))
  (setq wf_fv_p2 (polar wf_fv_p1 (dtr 0.0) Width))
  (setq wf_fv_p3 (polar wf_fv_p2 (dtr 270.0) FlangeThickness))
  (setq wf_fv_p4 (polar wf_fv_p3 (dtr 180.0) (- FlangeToWeb FlangeThickness)))
  
  ;Center point 1
  (setq wf_fv_p5 (polar wf_fv_p4 (dtr 270.0) FlangeThickness))
  (setq wf_fv_p6 (polar wf_fv_p5 (dtr 180.0) FlangeThickness))
  (setq wf_fv_p7 (polar wf_fv_p6 (dtr 270.0) (- FlangeToFlange (* FlangeThickness 2))))
  
  ;Center point 2
  (setq wf_fv_p8 (polar wf_fv_p7 (dtr 0.0) FlangeThickness))
  (setq wf_fv_p9 (polar wf_fv_p8 (dtr 270.0) FlangeThickness))
  (setq wf_fv_p10 (polar wf_fv_p9 (dtr 0.0) (- WebToFlange FlangeThickness)))
  (setq wf_fv_p11 (polar wf_fv_p10 (dtr 270.0) FlangeThickness))
  (setq wf_fv_p12 (polar wf_fv_p11 (dtr 180.0) Width))
  (setq wf_fv_p13 (polar wf_fv_p12 (dtr 90.0) FlangeThickness))
  (setq wf_fv_p14 (polar wf_fv_p13 (dtr 0.0) (- FlangeToWeb FlangeThickness)))
  
  ;Center point 3
  (setq wf_fv_p15 (polar wf_fv_p14 (dtr 90.0) FlangeThickness))
  (setq wf_fv_p16 (polar wf_fv_p15 (dtr 0.0) FlangeThickness))
  (setq wf_fv_p17 (polar wf_fv_p16 (dtr 90.0) (- FlangeToFlange (* FlangeThickness 2))))
  
  ;Center point 4
  (setq wf_fv_p18 (polar wf_fv_p17 (dtr 180.0) FlangeThickness))
  (setq wf_fv_p19 (polar wf_fv_p18 (dtr 90.0) FlangeThickness))
  
  ;Wide Flange Center Point
  (setq wf_fv_topmidp (polar ip (dtr 0.0) (/ Width 2)))
  (setq wf_fv_cp (polar wf_fv_topmidp (dtr 270.0) (/ Depth 2)))
  
;****************************************************************************************************************
  ; Polar math side view
;****************************************************************************************************************
  
  (setq wf_sv_ip (polar wf_fv_p2 (dtr 0.0) (* Width 0.3)))
  (setq wf_sv_p1 (polar wf_sv_ip (dtr 0.0) global:BeamLength))
  (setq wf_sv_p2 (polar wf_sv_p1 (dtr 270.0) Depth))
  (setq wf_sv_p3 (polar wf_sv_p2 (dtr 180.0) global:BeamLength))
  (setq wf_sv_p4 (polar wf_sv_p3 (dtr 90.0) FlangeThickness))
  (setq wf_sv_p5 (polar wf_sv_p4 (dtr 0.0) global:BeamLength))
  (setq wf_sv_p6 (polar wf_sv_p4 (dtr 90.0) FlangeToFlange))
  (setq wf_sv_p7 (polar wf_sv_p6 (dtr 0.0) global:BeamLength))
  
;****************************************************************************************************************
  ; Polar math top view
;****************************************************************************************************************
  
  (setq wf_tv_ip (polar wf_fv_p1 (dtr 90.0) (* Depth 0.1)))
  (setq wf_tv_p1 (polar wf_tv_ip (dtr 90.0) global:BeamLength))
  (setq wf_tv_p2 (polar wf_tv_p1 (dtr 0.0) Width))
  (setq wf_tv_p3 (polar wf_tv_p2 (dtr 270.0) global:BeamLength))
  (setq wf_tv_p4 (polar wf_tv_ip (dtr 0.0) FlangeToWeb))
  (setq wf_tv_p5 (polar wf_tv_p4 (dtr 90.0) global:BeamLength))
  (setq wf_tv_p6 (polar wf_tv_p4 (dtr 0.0) WebThickness))
  (setq wf_tv_p7 (polar wf_tv_p6 (dtr 90.0) global:BeamLength))

;****************************************************************************************************************
  ; Draw Front View
;****************************************************************************************************************

  (command  "PLine" ip wf_fv_p1 wf_fv_p2 wf_fv_p3 wf_fv_p4 "a" "ce" wf_fv_p5 wf_fv_p6 "l" wf_fv_p7 "a" "ce" wf_fv_p8 wf_fv_p9 "l" wf_fv_p10 wf_fv_p11 wf_fv_p12 wf_fv_p13 wf_fv_p14 "a" "ce" wf_fv_p15 wf_fv_p16 "l" wf_fv_p17 "a" "ce" wf_fv_p18 wf_fv_p19 "l" "cl")
  (command "-hatch" "p" "ANSI32" "2.0" "0.0" wf_fv_cp "")
  
;****************************************************************************************************************
  ; Draw Side View
;****************************************************************************************************************

  (command "PLine" wf_sv_ip wf_sv_p1 wf_sv_p2 wf_sv_p3 "c")
  (command "PLine" wf_sv_p4 wf_sv_p5 "")
  (command "PLine" wf_sv_p6 wf_sv_p7 "")
  
;****************************************************************************************************************
  ; Draw Top View
;****************************************************************************************************************

  (command "PLine" wf_tv_ip wf_tv_p1 wf_tv_p2 wf_tv_p3 "c")
  
  ;Change Celtype to hidden
  (setvar "celtype" "HIDDENX2")
  (setvar "celtscale" 0.05)
  
  ;Draw Hidden web lines
  (command "PLine" wf_tv_p4 wf_tv_p5 "")
  (command "PLine" wf_tv_p6 wf_tv_p7 "")
  
;****************************************************************************************************************
  ; Reset Sysvars
;****************************************************************************************************************

  (setvar "osmode" oldsnap)
  (setvar "expert" oldexp)
  (setvar "celtype" oldcel)
  (setvar "celtscale" oldcelt)
  
;****************************************************************************************************************
  ; Finish Cleanly
;****************************************************************************************************************

  (princ)
);defun

;****************************************************************************************************************
  ; Define Degrees to Radians
;****************************************************************************************************************

(defun dtr (x)
	;Define degrees to radians function
 
	(* pi (/ x 180))
	;Divide the angle by 180 then
	;Multiply by the constant PI

) 	;End of function

;****************************************************************************************************************
