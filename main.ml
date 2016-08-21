(*************************************************
==================================================
        OCaml 4d-maze renderer
        2012 (some things fixed in 2016)
        Etienne JACOB
==================================================
**************************************************)
#load "graphics.cma";;
#load "Bigarray.cma";;
open Graphics;;
open Bigarray;;
open Genarray;;

let proj x y z u v w p t =
	let par = (u*.u +. v*.v +. w*.w)/.(u*.x +. v*.y +. w*.z) in
        let i = x*.par -. u
        and j = y*.par -. v
        and k = z*.par -. w
        and d = ref 0.
        and e = ref 0. in
            if (par>0.) then begin
                        d := i*.cos(p)*.cos(t) +. j*.sin(p)*.cos(t) -. k*.sin(t);
                        e := j*.cos(p) -. i*.sin(p);
            end;
            ( !d, !e);;

(*un point de coordonnées (x,y,z) doit être affiché, la caméra a pour coordonnées (0,0,0).
p et t désignet les angles phi et theta de la camera,
	(u,v,w) est un point calculé en fonction de phi, theta, et du zoom g. 
Il définit un plan perpendiculaire à "la droite dirigée par (u,v,w) et passant par O"  et passant par (u,v,w).
- proj renvoie les coordonnées (2 valeurs car la 3e est nulle) du point d'intersection entre la "droite dirigée par (x,y,z) passant par O" et le plan de la camera dans un nouveau repère adapté (de centre de coordonnées (u,v,w) dans le repère de centre O).
*)

let affproj2 x y z p t g =
	let u = g*.sin(t)*.cos(p)
	and v = g*.sin(t)*.sin(p)
	and w = g*.cos(t) in
	let d = proj x y z u v w p t in
        let a = fst d
        and b = snd d in
            ((((size_x ())/2) - int_of_float(5.*.b)),(((size_y ())/2) - int_of_float(5.*.a)))
;;

(* x,y,p,t désignent toujours la même chose. g désigne le zoom plus G est élevé, plus le plan de la caméra s'éloigne.
- affproj2 renvoie des valeurs entieres prêtes à être affichées
*)

let affproj x y z p t g =
    Graphics.plot (fst(affproj2 x y z p t g)) (snd(affproj2 x y z p t g))
;;

(*
- affproj affiche simplement les valeurs de affproj2
*)

let arrayx = [|0.;0.;0.;0.;1.;1.;1.;1.|];;
let arrayy = [|0.;0.;1.;1.;0.;0.;1.;1.|];;
let arrayz = [|0.;1.;0.;1.;0.;1.;0.;1.|];;

(* ces arrays vont être utilisés dans l'affichage de cubes
ils représentent les coordonnées des points d'un cube
*)

let distance x y z=
sqrt((x*.x)+.(y*.y)+.(z*.z));; 

(*fonction pour calculer la distance de (x,y,z) par rappor à la camera

*)

let vrai bool= match bool with
|true -> 1.
|false -> 0.;;
(*fonction servant à eviter l'emploi de "if"s
*)

let distanceface x y z e o m =
    let a = (x+. (vrai (o=1))*.(float_of_int(m)-.0.5)*.e)
    and b = (y+. (vrai (o=2))*.(float_of_int(m)-.0.5)*.e)
    and c = (z+. (vrai (o=3))*.(float_of_int(m)-.0.5)*.e) in
        distance a b c
;;
(* (x,y,z)=coordonnées du cube à afficher
e=taille du cube
o=orientation de la face entre 1 et 3
m=position de la face parmi celle d'orientation "o". 0 ou 1.
*)

let distances x y z e=
let dist = [|0.;0.;0.;0.;0.;0.|]
    and a = ref 0.
    and b = ref 0.
    and c = ref 0. in
        for m=0 to 1 do
            for o=1 to 3 do
            a := (x+. (vrai (o=1))*.(float_of_int(m)-.0.5)*.e);
            b := (y+. (vrai (o=2))*.(float_of_int(m)-.0.5)*.e);
            c := (z+. (vrai (o=3))*.(float_of_int(m)-.0.5)*.e);
            dist.(o+(3*m)- 1) <- distance ( !a) ( !b) ( !c);
            done
        done;
        dist
;;
(*(x,y,z)=coordonnées du cube à afficher
e=taille du cube
- distances renvoie la liste de la distance des centre des faces par rappor à la camera
*)

let placeface dist x y z e o m=
let a = ref 0
and d = distanceface x y z e o m in
    for i=0 to 5 do
        if dist.(i)<d then a := !a+1;
    done;
    !a
;;
(*
- placeface renvoie la place (en distance) de la face (o,m) du cube (x,y,z,e)
Plus (o,m) est proche de la camera, moins la place est élevée (minimum=0)
*)

let arr d= match d with
|1 -> arrayx
|2 -> arrayy
|3 -> arrayz
|_ -> arrayx
;;

let face d b =
    let list=[|0;0;0;0|]
    and c = ref 0 in
        for i=0 to 7 do
            if (arr d).(i)=float_of_int(b) then
                (list.( !c) <- i;
                c := !c + 1)
        done;
        list
;;
(*
- face renvoie l'indice de des points de la face (d,b)(=(o,m) précédement) concernée
*)

let difference i j=
    (abs(int_of_float(arrayx.(j) -. arrayx.(i))) + abs(int_of_float(arrayy.(j) -. arrayy.(i))) + abs(int_of_float(arrayz.(j) -. arrayz.(i))))
;;
(* difference calcule le nombre de différences de caractéristiques entre 2 points d'un cube
*)

let bonordre list=
    let a = ref 0 in
        for i=0 to 1 do
            if (difference (list.(i)) (list.(i+1))) !=1 then begin
                a := list.(i+1);
                list.(i+1) <- list.(i+2);
                list.(i+2) <- !a; end
        done;
        list
;;
(* bon ordre remet une liste obtenue avec "face" dans le bonne ordre pour l'affichage du polygone correspondant
*)

let afficherface listx listy o m=
    let list = ref [|0;0;0;0|]
    and arra = [|(0,0);(0,0);(0,0);(0,0)|] in
        Graphics.set_color (rgb (8*(o+3*m)+170) (10*(o+3*m)+170) (12*(o+3*m))+170);
        list := face o m;
        list := bonordre (!list);
        for i=0 to 3 do
            arra.(i) <- (listx.(( !list).(i)),listy.(( !list).(i)));
        done;
        Graphics.fill_poly arra;
;;
(* affiche une face (o,m) grise.
les coordonnées des points des faces sont dans listx et listy
*)

let afficherfacejaune listx listy o m=
    let list = ref [|0;0;0;0|]
    and arra = [|(0,0);(0,0);(0,0);(0,0)|] in
        Graphics.set_color (rgb (10*(o+3*m)+150) (10*(o+3*m)+150) (5*(o+3*m))+50);
        list := face o m;
        list := bonordre ( !list);
        for i=0 to 3 do
            arra.(i) <- (listx.(( !list).(i)),listy.(( !list).(i)));
        done;
        Graphics.fill_poly arra;
;;


let afficherfacerouge listx listy o m=
    let list = ref [|0;0;0;0|]
    and arra = [|(0,0);(0,0);(0,0);(0,0)|] in
        Graphics.set_color (rgb (10*(o+3*m)+150) (5*(o+3*m)+100) (5*(o+3*m))+100);
        list := face o m;
        list := bonordre ( !list);
        for i=0 to 3 do
            arra.(i) <- (listx.(( !list).(i)),listy.(( !list).(i)));
        done;
        Graphics.fill_poly arra;
;;

let afficherfacebleu listx listy o m=
    let list = ref [|0;0;0;0|]
    and arra = [|(0,0);(0,0);(0,0);(0,0)|] in
        Graphics.set_color (rgb (5*(o+3*m)+100) (5*(o+3*m)+100) (10*(o+3*m))+150);
        list := face o m;
        list := bonordre ( !list);
        for i=0 to 3 do
            arra.(i) <- (listx.(( !list).(i)),listy.(( !list).(i)));
        done;
        Graphics.fill_poly arra;
;;

let affichercube5 x y z p t g e =
    let listx = (Array.make 8 0)
    and listy = (Array.make 8 0)
    and dist = (distances x y z e) in
        for i=0 to 7 do
            let a = arrayx.(i) in
            let b = arrayy.(i) in
            let c = arrayz.(i) in
            let d = affproj2 (x+.a*.e -.0.5*.e) (y+.b*.e -.0.5*.e) (z+.c*.e-.0.5*.e) p t g in
            listx.(i) <- fst d;
            listy.(i) <- snd d;
        done;
        for k=0 to 2 do
            for o=1 to 3 do
                for m=0 to 1 do
                    if (placeface dist x y z e o m)=2-k then begin
                        afficherface listx listy o m end
                done 
            done
        done;;
(* affiche le cube (x,y,z,e)*)

let affichercubejaune x y z p t g e =
    let listx = (Array.make 8 0)
    and listy = (Array.make 8 0)
    and dist = (distances x y z e) in
        for i=0 to 7 do
            let a = arrayx.(i) in
            let b = arrayy.(i) in
            let c = arrayz.(i) in
            let d = affproj2 (x+.a*.e -.0.5*.e) (y+.b*.e -.0.5*.e) (z+.c*.e-.0.5*.e) p t g in
            listx.(i) <- fst d;
            listy.(i) <- snd d;
        done;
        for k=0 to 2 do
            for o=1 to 3 do
                for m=0 to 1 do
                    if (placeface dist x y z e o m)=2-k then begin
                        afficherfacejaune listx listy o m end
                done 
            done
        done;;

let affichercuberouge x y z p t g e =
    let listx = (Array.make 8 0)
    and listy = (Array.make 8 0)
    and dist = (distances x y z e) in
    for i=0 to 7 do
        let a = arrayx.(i) in
        let b = arrayy.(i) in
        let c = arrayz.(i) in
        let d = affproj2 (x+.a*.e -.0.5*.e) (y+.b*.e -.0.5*.e) (z+.c*.e-.0.5*.e) p t g in
        listx.(i) <- fst d;
        listy.(i) <- snd d;
    done;
    for k=0 to 2 do
        for o=1 to 3 do
            for m=0 to 1 do
                if (placeface dist x y z e o m)=2-k then begin
                    afficherfacerouge listx listy o m end
            done 
        done
    done;;

let affichercubebleu x y z p t g e =
    let listx = (Array.make 8 0)
    and listy = (Array.make 8 0)
    and dist = (distances x y z e) in
        for i=0 to 7 do
            let a = arrayx.(i) in
            let b = arrayy.(i) in
            let c = arrayz.(i) in
            let d = affproj2 (x+.a*.e -.0.5*.e) (y+.b*.e -.0.5*.e) (z+.c*.e-.0.5*.e) p t g in
            listx.(i) <- fst d;
            listy.(i) <- snd d;
        done;
        for k=0 to 2 do
            for o=1 to 3 do
                for m=0 to 1 do
                    if (placeface dist x y z e o m)=2-k then begin
                        afficherfacebleu listx listy o m end
                done 
            done
        done;;

let tfillpar dimx dimy dimz dimt murs x y z p t g=
    Graphics.open_graph "My window";
    Graphics.resize_window 1200 900;
    let a=ref 0
    and b=ref 0
    and e=ref 0
    and f=ref 0
    and d = ref 0.
    and cursz = ref 0
    and sz = dimt*(2*dimz-1)*(2*dimx-1)*(2*dimy-1) in
        let to_sort = Array.make sz (10000000.0,0)
        and data = Array.make sz (0,0,0,0) in
        for l=1 to dimt do
        for k=0 to 2*dimz do
        for i=0 to 2*dimx do
        for j=0 to 2*dimy do
            if (murs.{(i),(j),(k),l} >= 1) && (murs.{(i),(j),(k),l} <= 4) then begin
                a := 2*dimx - i;
                b := 2*dimy - j;
                e := k;
                f := l-1;
                d := distance (float_of_int( !a) +. x +. 4.*.(float_of_int( !f)*.float_of_int(dimx)+.2.)) (float_of_int( !b)+. y) (float_of_int( !e)+. z);
                to_sort.(!cursz) <- (-.1.0*.(!d),!cursz);
                data.(!cursz) <- (i,j,k,l); 
                cursz := !cursz + 1;
            end
        done
        done
        done
        done;
        Array.sort compare to_sort;
        for ind=0 to !cursz-1 do
            let ind2 = snd to_sort.(ind) in let (i,j,k,l) = data.(ind2) in
                a := 2*dimx - i;
                b := 2*dimy - j;
                e := k;
                f := l-1;
                d := distance (float_of_int( !a) +. x +. 4.*.(float_of_int( !f)*.float_of_int(dimx)+.2.)) (float_of_int( !b)+. y) (float_of_int( !e)+. z);
                if murs.{(i),(j),(k),l} =1 then affichercube5 (float_of_int( !a) +. x +. 4.*.(float_of_int( !f)*.float_of_int(dimx)+.2.)) (float_of_int( !b)+. y) (float_of_int( !e)+. z) p t g 1.;
                if murs.{(i),(j),(k),l} =2 then affichercubejaune (float_of_int( !a) +. x +. 4.*.(float_of_int( !f)*.float_of_int(dimx)+.2.)) (float_of_int( !b)+. y) (float_of_int( !e)+. z) p t g 1.;
                if murs.{(i),(j),(k),l} =3 then affichercuberouge (float_of_int( !a) +. x +. 4.*.(float_of_int( !f)*.float_of_int(dimx)+.2.)) (float_of_int( !b)+. y) (float_of_int( !e)+. z) p t g 1.;
                if murs.{(i),(j),(k),l} =4 then affichercubebleu (float_of_int( !a) +. x +. 4.*.(float_of_int( !f)*.float_of_int(dimx)+.2.)) (float_of_int( !b)+. y) (float_of_int( !e)+. z) p t g 1.;
        done;
        print_int (!cursz)
        ;;
(*fonction de tracage de labyrinthe 4d, portée de l'affichage : 110*)

(*--------------------------------*)
let correct x dimx=
0<x&&x<2*dimx+1 
;;
let correctt t dimt=
0<t&&t<dimt+1
;;
(*fonctions pour savoir si une case est hors limite *)

let tchoix x y z t zmurs dimx dimy dimz dimt= 
let a = ref 0
and b= [|0;0;0;0;0;0;0;0|] in
    (if (correct (x+2) dimx)&& zmurs.{(x+2),(y),(z),t}=0 then (a := !a + 1; b.(0)<- 1);
    if (correct (x-2) dimx)&& zmurs.{(x-2),(y),(z),t}=0 then (a := !a + 1; b.(1)<- 1);
    if (correct (y+2) dimy)&& zmurs.{(x),(y+2),(z),t}=0 then (a := !a + 1; b.(2)<- 1);
    if (correct (y-2) dimy)&& zmurs.{(x),(y-2),(z),t}=0 then (a := !a + 1; b.(3)<- 1);
    if (correct (z+2) dimz)&& zmurs.{(x),(y),(z+2),t}=0 then (a := !a + 1; b.(4)<- 1);
    if (correct (z-2) dimz)&& zmurs.{(x),(y),(z-2),t}=0 then (a := !a + 1; b.(5)<- 1);
    if (correctt (t+1) dimt)&& zmurs.{(x),(y),(z),t+1}=0 then (a := !a + 1; b.(6)<- 1);
    if (correctt (t-1) dimt)&& zmurs.{(x),(y),(z),t-1}=0 then (a := !a + 1; b.(7)<- 1);
    ( !a, b));;



let tsedeplacer1 a b=
let c = ref 0
and i = ref 0
and j = ref 0 in
    c := 1 + Random.int (a);
    while !i != !c do
        if b.( !j) !=0 then i := !i+1;
        j := !j+1
    done;
    !j;;
(*on choisit un indice de direction parmi celle proposée par b*)

let tsedeplacer2 x y z t j murs=
	if !j=1 then begin
		murs.{(x+1),(y),(z),t}<- 1;
		murs.{(x+2),(y),(z),t}<- 1;
		end;
	if !j=2 then begin
		murs.{(x-1),(y),(z),t}<- 1;
		murs.{(x-2),(y),(z),t}<- 1;
		end;
	if !j=3 then begin
		murs.{(x),(y+1),(z),t}<- 1;
		murs.{(x),(y+2),(z),t}<- 1;
		end;
	if !j=4 then begin
		murs.{(x),(y-1),(z),t}<- 1;
		murs.{(x),(y-2),(z),t}<- 1;
		end;
	if !j=5 then begin
		murs.{(x),(y),(z+1),t}<- 1;
		murs.{(x),(y),(z+2),t}<- 1;
		end;
	if !j=6 then begin
		murs.{(x),(y),(z-1),t}<- 1;
		murs.{(x),(y),(z-2),t}<- 1;
		end;
	if !j=7 then begin
		if murs.{(x),(y),(z),t}<2 then	murs.{(x),(y),(z),t}<- 3 else murs.{(x),(y),(z),t}<- 4;
		murs.{(x),(y),(z),t+1}<- 2;
		end;
	if !j=8 then begin
		if murs.{(x),(y),(z),t}<2 then	murs.{(x),(y),(z),t}<- 2 else murs.{(x),(y),(z),t}<- 4;
		murs.{(x),(y),(z),t-1}<- 3;
		end;
;;
(*on remplit l'array en fonction de la direction
*)

let tpromenade x y z t murs dimx dimy dimz dimt=
    let i = ref x
    and j = ref y
    and k = ref z
    and l = ref t
    and a = ref 1
    and c = ref 0
    and d = ref (0,[|0;0;0;0;0;0;0;0|])
    and b = ref [|0;0;0;0;0;0;0;0|] in
        if murs.{(x),(y),(z),t} = 0 then
            murs.{(x),(y),(z),t} <- 1;
            while ( !a!=0) do
            d:= tchoix ( !i) ( !j) (!k) ( !l) murs dimx dimy dimz dimt;
            a := fst !d;
            b := snd !d;
            if !a!=0 then begin 
                c := tsedeplacer1 ( !a) ( !b);
                tsedeplacer2 ( !i) (!j) (!k) ( !l) (c) murs;
                if !c = 1 then i:= !i+2;
                if !c = 2 then i:= !i-2;
                if !c = 3 then j:= !j+2;
                if !c = 4 then j:= !j-2;
                if !c = 5 then k:= !k+2;
                if !c = 6 then k:= !k-2;
                if !c = 7 then l:= !l+1;
                if !c = 8 then l:= !l-1;
            end
        done;;
(*on se déplace jusqu'à être bloqué*)

let tmaze dimx dimy dimz dimt x y z p t g=
let murs = create int c_layout [|(2*dimx+1);(2*dimy+1);(2*dimz+1);(dimt+2)|] in
    fill murs 0;
    for u=0 to 2 do
        for l=1 to dimt do
            for k=0 to dimz-1 do
                for i=0 to dimx-1 do
                    for j=0 to dimy-1 do
                        if murs.{(2*i+1),(2*j+1),(2*k+1),(l)}>0||(i=0&&j=0&&k=0&&l=1) then
                        tpromenade (2*(i)+1) (2*(j)+1) (2*(k)+1) (l) murs dimx dimy dimz dimt;
                    done
                done
            done
        done
    done;
    tfillpar dimx dimy dimz dimt murs x y z p t g;
;;
(*génère un labyrinthe 4d de taille dimx*dimy*dimz*dimt à l'emplacement (x,y,z) (minimum en x y et z du labyrinthe)
p, t et g permettent de régler respectivement l'angle phi (droite, gauche), l'angle theta (haut bas) et le zoom  de la camera*)


tmaze 25 25 25 5 (0.) (35.) (-.35.) (0.5) (1.7) 130.;;

let last = Scanf.scanf "%d" (fun x->x);;
