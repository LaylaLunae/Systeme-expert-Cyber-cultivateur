;; Code source TP3 Cyber Cultivateur

;; Base de règles
(setq baseDeRegles '((planter-ble (precond (ble 1)) (effets (ble 2)))
                     (planter-mais (precond (mais 1)) (effets (mais 2)))
                     (planter-carotte (precond (carotte 1)) (effets (carotte 2)))
                     (planter-canne-sucre (precond (canne-sucre 1)) (effets (canne-sucre 2)))
                     
                     (acheter-ble (precond (argent 5)) (effets (ble 1)))
                     (acheter-mais (precond (argent 5)) (effets (mais 1)))
                     (acheter-carotte (precond (argent 10)) (effets (carotte 1)))
                     (acheter-canne-sucre (precond (argent 10)) (effets (canne-sucre 1)))
                     
                     (recolter-lait (precond (vache 1) (nourriture-vache 1)) (effets (lait 1)))
                     (recolter-oeuf (precond (poule 1) (nourriture-poule 1)) (effets (oeuf 1)))
                     (acheter-vache (precond (argent 100)) (effets (vache 1)))
                     (acheter-poule (precond (argent 60)) (effets (poule 1)))
                     (faire-nourriture-vache (precond (mais 2) (carotte 1)) (effets (nourriture-vache 1)))
                     (faire-nourriture-poule (precond (mais 1) (ble 2)) (effets (nourriture-poule 1)))
                     
                     (faire-pain (precond (ble 3)) (effets (pain 1)))
                     (vendre-pain (precond (pain 1)) (effets (argent 100)))
                     (faire-popcorn (precond (mais 2)) (effets (popcorn 1)))
                     (vendre-popcorn (precond (popcorn 1)) (effets (argent 100)))
                     (faire-beurre (precond (lait 2)) (effets (beurre 1)))
                     (vendre-beurre (precond (beurre 1)) (effets (argent 100)))
                     (faire-popcorn-beurre (precond (mais 2) (beurre 1)) (effets (popcorn-beurre 1)))
                     (vendre-popcorn-beurre (precond (popcorn-beurre 1)) (effets (argent 200)))
                     
                     (faire-sucre (precond (canne-sucre 2)) (effets (sucre 1)))
                     (vendre-sucre (precond (sucre 1)) (effets (argent 100)))
                     (faire-cookie (precond (sucre 1) (ble 2) (oeuf 2)) (effets (cookie 1)))
                     (vendre-cookie (precond (cookie 1)) (effets (argent 350)))
                     (faire-gateau-carotte (precond (sucre 1) (beurre 1) (carotte 2)) (effets (gateau-carotte 1)))
                     (vendre-gateau-carotte (precond (gateau-carotte 1)) (effets (argent 500)))
                     (faire-tarte-carotte (precond (carotte 3) (ble 2) (oeuf 1)) (effets (tarte-carotte 1)))
                     (vendre-tarte-carotte (precond (tarte-carotte 1)) (effets (argent 450)))
                     (faire-jus-carotte (precond (carotte 3)) (effets (jus-carotte 1)))
                     (vendre-jus-carotte (precond (jus-carotte 1)) (effets (argent 100)))
                     
                     (acheter-maison (precond (argent 1000)) (effets (maison 1)))
                     ))


;; Fonction pour obtenir le nom du joueur
(defun obtenir-nom-joueur (liste-joueur)
  (cadr (cadr (assoc 'identificateur liste-joueur))))

;; Fonction pour obtenir les ressources
(defun obtenir-ressources (liste-joueur)
  (cdr (assoc 'ressources liste-joueur)))

;; Fonction pour accéder aux préconditions d'une règle
(defun obtenir-preconditions (regle)
  (cdr (assoc 'precond regle)))

;; Fonction pour accéder aux effets d'une règle
(defun obtenir-effets (regle)
  (cdr (assoc 'effets regle)))

;; Fonction pour accéder à l'ensemble d'une règle
(defun obtenir-regle (nom-regle base-de-regles)
  (cdr (assoc nom-regle base-de-regles)))

;; Fonction pour accéder à la quantité d'une ressource dans les préconditions
(defun obtenir-quantite-precondition (ressource preconditions)
  (cadr (assoc ressource preconditions)))

;; Fonction pour accéder à la quantité d'une ressource dans les effets
(defun obtenir-quantite-effet (ressource effets)
  (cadr (assoc ressource effets)))

;; Fonction pour obtenir une production
(defun obtenir-produit (nom liste)
  (assoc nom liste))



;; Règles valides
;; Algorithme
; reglesValides (ressources bdr)
; regles-valides = liste vide
; pour chaque règle dans la base de règles
;   - sol = true et precond = préconditions de la règle
;   - tant qu'on a des préconditions et que la solution est à true
;     - on prend la production de la précondition
;     - on cherche la production dans les ressources du joueur
;     - s'il n'y a pas de production ou si la quantité de la production est trop faible
;       sol = nil
;   - si la solution est toujours à true, alors on ajoute la règle aux règles valides

(defun reglesValides (ressources bdr)
  (let ((regles-valides '()))
    (dolist (x bdr regles-valides)
       (let ((precond (obtenir-preconditions (cdr x)))
             (sol t))
         (while (and sol precond)
           (setq prod (pop precond))
           (setq prod-joueur (assoc (car prod) ressources))
           (cond
            ((equal prod-joueur nil) (setq sol nil))
            ((not (>= (cadr prod-joueur) (cadr prod))) (setq sol nil))
            )
           )
         (if (equal sol t) (push (car x) regles-valides))
         )
      )))


;; Modification d'une ressource individuelle (soustraction)
(defun modifRessIndivMoins (liste quantite)
  (progn
    (setq new-quant (- (cadr liste) quantite))
    (setq new-liste (list (car liste) new-quant))
    ))

;; Modification d'une ressource individuelle (addition)
(defun modifRessIndivPlus (liste quantite)
  (progn
    (setq new-quant (+ (cadr liste) quantite))
    (setq new-liste (list (car liste) new-quant))
    ))


;; Modification des ressources en fonction de la règle
;; Algorithme
; modifRessources (ressources regle bdr)
; precond = préconditions de la règle, effet = effet de la règle
; new-ressources = liste vide
; pour chaque ressources du joueur
;   - prod-precond = précondition de la règle si la ressource est égale à la ressource de la règle
;   - prod-effet = effet de la règle si la ressource est égale à la ressource de la règle
;   - si prod-precond et prod-effet existent
;     - on actualise la ressource en retirant et ajoutant les quantités de la précondition et de l'effet
;   - si prod-precond existe
;     - on actualise la ressource en retirant les quantités de la précondition
;   - si prod-effet existe
;     - on actualise la ressource en ajoutant les quantités de l'effet
;   - sinon
;     - on ne touche pas à la ressource
; si l'effet n'existait pas dans les ressources, on l'ajoute aux ressources

(defun modifRessources (ressources regle)
  (let ((precond (obtenir-preconditions regle))
        (effet (obtenir-effets regle))
        (new-ressources '()))
    (dolist (x ressources)
      (progn
        (setq prod-precond (assoc (car x) precond))
        (setq prod-effet (assoc (car x) effet))
        (cond
         ((and prod-precond prod-effet)
          (setq x (modifRessIndivMoins x (cadr prod-precond)))
          (setq x (modifRessIndivPlus x (cadr prod-effet))) (push x new-ressources))
         (prod-precond 
                       (setq x (modifRessIndivMoins x (cadr prod-precond)))
                       (push x new-ressources))
         (prod-effet (setq x (modifRessIndivPlus x (cadr prod-effet)))
                     (push x new-ressources))
         (t (push x new-ressources))
         )))
    (if (not (assoc (car (car effet)) new-ressources)) (push (car effet) new-ressources))
    (if t new-ressources)
    ))


;; Afficher les règles valides
(defun afficher (reglesValides bdr)
  (progn
    (format t "~%Voici les actions possibles :")
    (dolist (x reglesValides)
      (progn
        (setq regle (obtenir-regle x bdr))
        (format t "~%~s, demande : ~s, effet : ~s" x (cdr (car regle)) (cadr (cadr regle)))
        )
      )
    ))


;; Choix du nom du joueur
(defun choix_nom_joueur ()
  (progn
    (format t "~%Veuillez entrer votre nom : ")
    (setq nom (read))
    ))

;;
(defun choix_depart ()
  (progn
    (format t "~%Veuillez entrer votre choix de depart (ble ou argent) : ")
    (setq depart (read))
    ))


;; Départ
;; Algorithme
; depart ()
; initialisation du nom du joueur et du départ
; tant que le profil du joueur est vide
;   - si le départ est égal à blé
;     - on met à jour le profil avec ce départ
;   - si le départ est égal à argent
;   - sinon
;     - le départ est invalide, on le redemande à l'utilisateur

(defun depart ()
  (let ((nom-joueur (choix_nom_joueur))
        (depart (choix_depart))
        (liste-joueur '()))
    (while (equal liste-joueur nil)
      (cond
       ((equal depart 'ble) (setq liste-joueur (list (list 'identificateur (list 'joueur nom))
                                                     (list 'ressources (list 'ble 2)
                                                           (list 'argent 0)))))
       ((equal depart 'argent) (setq liste-joueur (list (list 'identificateur (list 'joueur nom))
                                     (list 'ressources (list 'argent 10)))))
       )
      (if (equal liste-joueur nil)
          (progn
            (format t "~%Votre choix de depart est invalide.")
            (setq depart (choix_depart))
            )
        ))
    (if t liste-joueur)))
     

;; suppression des ressources vides
(defun supRessource (ressources)
  (dolist (x ressources ressources)
    (if (and (equal (cadr x) 0) (not (equal (car x) 'argent)))
        (delete x ressources))
    ))



;; Système expert
;; Algorithme
; cyberCultivateur (bdr sol)
; initialisation du profil du joueur
; initialisation des ressources du joueur et de la variable problème
; tant que la solution n'est pas dans les ressources et qu'il n'y a pas de problème
;   - initialisations des règles et de la vérification à 0
;   - si les règles sont vides alors le joueur a perdu, le problème est mis à 0
;   - sinon
;     - tant que la vérification est à 0, c'est-à-dire que le joueur n'a pas choisi une règle possible
;       - on affiche les règles possibles
;       - on lit le choix du joueur
;       - on récupère la règle choisie
;       - si la règle est bien récupérée, la vérification passe à 1
;     - on met à jour les ressources en fonction de la règle
;     - on supprime les ressources qui n'existent plus
; s'il y a eu un problème, le joueur a perdu, sinon le joueur a gagné

(defun cyberCultivateur (bdr sol)
  (progn
    (setq liste-joueur (depart))
    (let ((ressources (obtenir-ressources liste-joueur))
          (pb 1))
      (while (and (equal pb 1) (not (equal sol (car (obtenir-produit sol ressources)))))
        (format t "~%Vos ressources : ~s" ressources)
          (let ((regles (reglesValides ressources bdr))
                (verif 0))
            (if (equal regles nil) (setq pb 0)
              (progn
               (while (equal verif 0)
                (afficher regles bdr)
                (format t "~%Votre choix (recopier le nom de la règle) : ")
                (setq choix (read))
                (setq regleApplique (obtenir-regle choix bdr))
                (if regleApplique (setq verif 1))
                )
                (setq ressources (modifRessources ressources regleApplique))
                (supRessource ressources))
              )
            )
        )
      (if (equal pb 0)
          (format t "~%Désolé ~s, tu n'as pas su gérer une fermer.~%" (obtenir-nom-joueur liste-joueur))
        (format t "~%Bravo ~s ! Tu as réussi à acheter ta maison.~%" (obtenir-nom-joueur liste-joueur))
        )
      )))


        


;; Exécution du système expert
(cyberCultivateur baseDeRegles 'maison)







