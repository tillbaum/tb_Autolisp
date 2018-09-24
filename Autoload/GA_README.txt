GA ist ähnlich wie PickAREA
(Flächenbestimmung mit temporärer Erstellung von Hatches)
 nur mit textausgabe!!



(defun C:flaechetxt()(/ flae umf)

	(setq pt(getpoint)
	(command "_pickarea" pt)
	
	(setq flae(getvar "_area")
	(setq umf(getvar "_perimeter")
	