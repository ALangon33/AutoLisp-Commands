(defun c:Angle (/ oldsnap 
                tmp:Height tmp:Width tmp:Thickness tmp:AngleLength
                disp:Height disp:Width disp:Thickness disp:AngleLength
                LegToCorner CornerToLeg 
                ang_ip ang_p1 ang_p2 ang_p3 ang_p4 ang_p5 ang_p6 ang_p7 ang_p8 ang_p9 ang_hatchp
                ang_sv_ip ang_sv_p1 ang_sv_p2 ang_sv_p3 ang_sv_p4 ang_sv_p5
                ang_tv_ip ang_tv_p1 ang_tv_p2 ang_tv_p3 ang_tv_p4 ang_tv_p5
                *error*)
  
;****************************************************************************************************************
  ;Define Error
;****************************************************************************************************************
 
  (defun *error* ( msg )
  
    (if oldsnap (setvar "osmode" oldsnap))
    (setq global:Height nil
          global:Width nil
          global:Thickness nil
          global:AngleLength nil
    ); end setq
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))      
    );end if
    (princ)
  );end defun

;****************************************************************************************************************
  ;Save Sysvars
;****************************************************************************************************************
  
  ;Save Snap Settings
  (setq oldsnap (getvar 'osmode))
  ;Turn off snaps
  (setvar "osmode" 0)
  
;**************************************************************************************************************** 
  ;Set Defaults
;**************************************************************************************************************** 
 
  (if (or (null global:Height) (= global:Height ""))
  (setq global:Height 3.0)
  ); end if
  
  (if (or (null global:Width) (= global:Width ""))
  (setq global:Width 3.0)
  ); end if
  
  (if (or (null global:Thickness) (= global:Thickness ""))
  (setq global:Thickness 0.25)
  ); end if
  
  (if (or (null global:AngleLength) (= global:AngleLength))
  (setq global:AngleLength 72.0)
  ); end if
  
  (setq disp:Height (rtos global:Height 4 4))
  (setq disp:Width (rtos global:Width 4 4))
  (setq disp:Thickness (rtos global:Thickness 4 4))
  (setq disp:AngleLength (rtos global:AngleLength 4 4))
  
;**************************************************************************************************************** 
  ;Assign Variables
;****************************************************************************************************************

    (setq tmp:Height (getstring T (strcat "\nHeight <" disp:Height "> : ")))
    (cond 

      ((and (= (ascii tmp:Height) 32) (/= (ascii (substr tmp:Height 2)) 32))
      (setq tmp:Height (substr tmp:Height 2))
        (setq global:Height (distof tmp:Height 4))
      ); cond 1
      
      ((and (= (type tmp:Height) 'STR) (/= tmp:Height ""))
        (setq global:Height (distof tmp:Height 4))
      ); cond 2

      (T nil); final cond

    ); end cond

    (setq tmp:Width (getstring T (strcat "\nWidth <" disp:Width "> : ")))
    (cond 

      ((and (= (ascii tmp:Width) 32) (/= (ascii (substr tmp:Width 2)) 32))
      (setq tmp:Width (substr tmp:Width 2))
      (setq global:Width (distof tmp:Width 4))
      ); cond 1

      ((and (= (type tmp:Width) 'STR) (/= tmp:Width ""))
        (setq global:Width (distof tmp:Width 4))
      ); cond 2        

      (T nil); final cond

    ); end cond

    (setq tmp:Thickness (getstring T (strcat "\nThickness <" disp:Thickness "> : ")))
    (cond

      ((and (= (ascii tmp:Thickness) 32) (/= (ascii (substr tmp:Thickness 2)) 32))
      (setq tmp:Thickness (substr tmp:Thickness 2))
      (setq global:Thickness (distof tmp:Thickness 4))
      ); cond 1

      ((and (= (type tmp:Thickness) 'STR) (/= tmp:Thickness ""))
      (setq global:Thickness (distof tmp:Thickness 4))
      ); cond 2

      (T nil); final cond

    ); end cond
  
    (setq tmp:AngleLength (getstring T (strcat "\nLength of Angle <" disp:AngleLength "> : ")))
    (cond 
      
      ((and (= (ascii tmp:AngleLength) 32) (/= (ascii (substr tmp:AngleLength 2)) 32))
      (setq tmp:AngleLength (substr tmp:AngleLength 2))
      (setq global:AngleLength (distof tmp:AngleLength 4))
      ); cond 1

      ((and (= (type tmp:AngleLength) 'STR) (/= tmp:AngleLength ""))
      (setq global:AngleLength (distof tmp:AngleLength 4))
      ); cond 2

      (T nil); final cond

    ); end cond
  
;**************************************************************************************************************** 
  ;Arithmatic into Variables
;****************************************************************************************************************

  (setq LegToCorner (- global:Width global:Thickness))
  (setq CornerToLeg (- global:Height global:Thickness))

;**************************************************************************************************************** 
  ;Get Insertion Point
;****************************************************************************************************************
  
  (initget 1)
  (setq ang_ip (getpoint "\nInsertion Point : "))
  
;**************************************************************************************************************** 
  ;Polar math front view
;****************************************************************************************************************

  (setq ang_p1 (polar ang_ip (dtr 270.0) global:Height))
  (setq ang_p2 (polar ang_p1 (dtr 0.0) global:Width))
  
  ;Center Point 1
  (setq ang_p3 (polar ang_p2 (dtr 180.0) global:Thickness))
  (setq ang_p4 (polar ang_p3 (dtr 90.0) global:Thickness))
  (setq ang_p5 (polar ang_p4 (dtr 180.0) (- LegToCorner (* global:Thickness 2))))
  
  ;Center Point 2
  (setq ang_p6 (polar ang_p5 (dtr 90.0) global:Thickness))
  (setq ang_p7 (polar ang_p6 (dtr 180.0) global:Thickness))
  (setq ang_p8 (polar ang_p7 (dtr 90.0) (- CornerToLeg (* global:Thickness 2))))
  
  ;Center Point 3
  (setq ang_p9 (polar ang_p8 (dtr 180.0) global:Thickness))
  
  ;Polar Hatch point
  (setq ang_hatchp (polar ang_p1 (dtr 45.0) (/ global:Thickness 2)))
  
;**************************************************************************************************************** 
  ;Polar math side view
;****************************************************************************************************************

  (setq ang_sv_ip (polar ang_p2 (dtr 0.0) global:Width))
  (setq ang_sv_p1 (polar ang_sv_ip (dtr 90.0) global:Height))
  (setq ang_sv_p2 (polar ang_sv_p1 (dtr 0.0) global:AngleLength))
  (setq ang_sv_p3 (polar ang_sv_p2 (dtr 270.0) global:Height))
  (setq ang_sv_p4 (polar ang_sv_ip (dtr 90.0) global:Thickness))
  (setq ang_sv_p5 (polar ang_sv_p4 (dtr 0.0) global:AngleLength))
  
;**************************************************************************************************************** 
  ;Polar math top view
;****************************************************************************************************************

  (setq ang_tv_ip (polar ang_ip (dtr 90.0) global:Height))
  (setq ang_tv_p1 (polar ang_tv_ip (dtr 90.0) global:AngleLength))
  (setq ang_tv_p2 (polar ang_tv_p1 (dtr 0.0) global:Width))
  (setq ang_tv_p3 (polar ang_tv_p2 (dtr 270.0) global:AngleLength))
  (setq ang_tv_p4 (polar ang_tv_ip (dtr 0.0) global:Thickness))
  (setq ang_tv_p5 (polar ang_tv_p4 (dtr 90.0) global:AngleLength))
  
;**************************************************************************************************************** 
  ;Draw Front View
;****************************************************************************************************************

  (command "PLine" ang_ip ang_p1 ang_p2 "a" "ce" ang_p3 ang_p4 "l" ang_p5 "a" ang_p7 "l" ang_p8 "a" "cl")
  (command "-hatch" "p" "ANSI32" "2.0" "0.0" ang_hatchp "")
  
;**************************************************************************************************************** 
  ;Draw Side View
;****************************************************************************************************************

  (command "PLine" ang_sv_ip ang_sv_p1 ang_sv_p2 ang_sv_p3 "c")
  (command "PLine" ang_sv_p4 ang_sv_p5 "")
  
;**************************************************************************************************************** 
  ;Draw Top View
;****************************************************************************************************************

  (command "PLine" ang_tv_ip ang_tv_p1 ang_tv_p2 ang_tv_p3"c")
  (command "PLine" ang_tv_p4 ang_tv_p5 "")
  
;**************************************************************************************************************** 
  ;Reset Sysvars
;****************************************************************************************************************

  (setvar "osmode" oldsnap)
  
;****************************************************************************************************************
  ;Finish Cleanly
;****************************************************************************************************************

  (princ)
); end defun

;****************************************************************************************************************
  ;Define Degrees to Radians
;****************************************************************************************************************

(defun dtr (x)
	;Define degrees to radians function
 
	(* pi (/ x 180))
	;Divide the angle by 180 then
	;Multiply by the constant PI

) 	;End of function

;****************************************************************************************************************
