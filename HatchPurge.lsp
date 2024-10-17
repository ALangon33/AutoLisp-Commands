;; Selects & Deletes All Hatches Matching the Selected Patterns
(defun c:HatchPurge ( / oldsnap oldpick ssha eha enthatch hatchpatnum hatchpatname ct n hass)
  
  (defun *error* ( msg )

    (if oldsnap (setvar "osmode" oldsnap))
    (if oldpick (setvar "pickstyle" oldpick))
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))      
    );end if
    (princ)
  );end defun
  
  (setq oldsnap (getvar 'osmode))
  (setvar "osmode" 0)
  (setq oldpick (getvar "pickstyle"))
  (setvar "pickstyle" 0)
  
  ; Force user onto graphics screen [ might be redundant for everyone but me ;( ]
  (graphscr)
  
  ;|  *****NOTE : ADD ERROR TRAPPER / INITGET EQUIVALENT FOR ENTITIES THAT AREN'T HATCHES*****   |;
  
  ; Have user select hatch w/ the pattern of their choice
  (prompt "\nSelect Hatch Patterns You'd Like to Purge")
  (setq ssha (ssget))
  ; Set a counter and number
  (setq ct 0)
  (setq n (sslength ssha))
  
  ; Set the DXF # for patterns in hatches
  (setq hatchpatnum 2)
  
  ; While sslen > 1
  (while (< ct n) 
  
    (setq eha (ssname ssha ct))
    
    ; Pull the entity info
    (setq enthatch (entget eha))
    ; If not Hatch do nothing, else erase
    (cond ((= (cdr (cadr enthatch)) "HATCH")
           
    ; Get pattern name
    (setq hatchpatname (vl-remove-if-not '(lambda ( x ) (= hatchpatnum (car x))) enthatch))
    
    (setq hass (ssget "_X" hatchpatname))

    (command "Erase" hass "")
    ); cond 1
      
      (T nil); final cond
      
    ); end cond
      
    (setq ct (+ ct 1))
    
  ); end while
 
  (setvar "osmode" oldsnap)
  (setvar "pickstyle" oldpick)

  (princ)
  
); end defun