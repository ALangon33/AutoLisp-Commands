; Define Equipment Count Function
(defun c:ECOUNT ( / oldsnap prefix TextHeight n txt p)
  
;****************************************************************************************************************

  (defun *error* ( msg )
  
    (if oldsnap (setvar "osmode" oldsnap))
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))      
    );end if
    (princ)
  );end defun

;****************************************************************************************************************

  ; Save & Alter Sys Vars
  (setq oldsnap (getvar 'osmode))
  (setvar "osmode" 0)
  
;****************************************************************************************************************
  ;Get Defaults
;****************************************************************************************************************

  ;Set Defaults
  (if (or (null global:TextHeight) (= global:TextHeight ""))
  (setq global:TextHeight 1.5)
  ); end if
  
  (if (or (null global:n) (= global:n ""))
    (setq global:n 1)  
    ); end if
  
  (setq disp:TextHeight (rtos global:TextHeight 4 4))

;****************************************************************************************************************

  ; Aquire string that will become prefix
  (setq prefix (getstring "\nPrefix : "))
  ; Get Text Height
  (setq tmp:TextHeight (getstring T (strcat "\nText Height <" disp:TextHeight "> : ")))
  (cond

    ((and (= (ascii tmp:TextHeight) 32) (/= (ascii (substr tmp:TextHeight 2)) 32))
     (setq tmp:TextHeight (substr tmp:TextHeight 2))
     (setq global:TextHeight (distof tmp:TextHeight 4))
     ); cond 1
    
    ((and (= (type tmp:TextHeight) 'STR) (/= tmp:TextHeight ""))
     (setq global:TextHeight (distof tmp:TextHeight 4))
     ); cond 2
    
    (T nil); final cond
    
  ); end cond
  
  ; Make a counter for the while loop to concatenate into the output
  (initget 4)
  (if (setq tmp:n (getint (strcat "\nStarting Number <" (itoa global:n) "> : ")))
    (setq global:n tmp:n)
  ); end if
  
;****************************************************************************************************************

  ;| Make a while loop that until "Enter" is pressed generates the concatenation of prefix and n
    for as many points as are aquired |;
  (while
    (setq p (getpoint "\nSpecify Text Insertion Point : "))
    (setq txt (strcat prefix (itoa global:n)))
    (setq global:n (+ global:n 1))
    (command "text" "j" "mc" p global:TextHeight 0 txt "")
  ); End While
  
;****************************************************************************************************************

  ; Reset Sys Vars
  (setvar "osmode" oldsnap)
  
;****************************************************************************************************************

  (princ); Finish Cleanly  
); End Defun