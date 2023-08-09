#lang racket
;; Universidad Nacional de Itapua
;; Proyecto de Generador de Oraciones Aleatorias
;; 
;; Materia: Paradigmas de la Programacion
;;
;; Alumno: Annia Micaela Benitez Hofbauer
;;
;; Basado en: http://www-cs-faculty.stanford.edu/~zelenski/rsg/handouts/107_hw1c.pdf

"Elija que tipo de oracion quiere"
" - Gramatica-para-poemas"
" - Gramatica-No-termine-mi-proyecto-porque"

;; La funcion GOA es el punto de entrada para ejecutar el programa.
;; 
;; Ejemplo de uso:
;;        (goa 'Gramatica-No-termine-mi-proyecto-porque)
;;
;; El parametro corresponde a un nombre de una gramatica guardada en la 
;; variable global *gramaticas*
;;

(define (goa nombre-de-gramatica)
  (ensamblar (flatten (buscar-listado '<inicio> (buscar-listado nombre-de-gramatica *gramaticas*)))
             (remove (list '<inicio> (flatten (buscar-listado '<inicio> (buscar-listado nombre-de-gramatica *gramaticas*)))) (buscar-listado nombre-de-gramatica *gramaticas*))))

;; Ensambla la lista sustituyendo los expandibles por posibles
;; gramaticas, en caso de que hayan expandibles 
;;
;; Primer parametro es la frase de la gramatica, segundo parametro los
;; posibles expandibles
;;

(define (ensamblar frase lista-datos)  
  (cond [(ormap (lambda (x) (es-expandible x)) frase) (ensamblar (flatten (map (lambda (x) (if (es-expandible x)
                                                                                               (elemento-random (buscar-listado x lista-datos))
                                                                                               x)) frase)) lista-datos)]
        [else frase]))

;; Busca la primera coincidencia de un elemento dado en una
;; lista que se pasa como parametro, devolviendo la cadena perteneciente al elemento
;;
;; Ej. (buscar-listado '<inicio> listaPrincipal)
;; >  (Las <objeto> <verbo> esta noche#\.)
;;

(define (buscar-listado titulo lista)  
  (cond [(empty? lista) '(No se encuentran elementos)]
        [(list? (caar lista)) (buscar-listado titulo (car lista))]
        [(equal? (caar lista) titulo) (cdar lista)]
        [else (buscar-listado titulo (cdr lista))]))

;; elemento-random lista
;;
;; Esta funcion devuelve un elemento cualquiera de una lista
;; que se le pase como parametro
;;

(define (elemento-random lista)
  (list-ref lista (random (length lista))))

;; Esta funcion tal vez te sea util
;;
;; Ejemplo de uso:
;;
;;  (es-expandible '<inicio>)   ==> T
;;
;;  (es-expandible 'palabra)    ==> nil
;;
;; Esta funcion nos sirve para distinguir entre simbolos
;; que son terminales o no. 
;;

(define (es-expandible simbolo)
  (if (symbol? simbolo) 
      [let* ((palabra (~a simbolo))
             (longitud (string-length palabra))
             (inicio (- longitud 1)))
        [cond ((not (equal? "<" (substring palabra 0 1))) #f)
              ((not (equal? ">" (substring palabra inicio longitud))) #f)
              (else #t)]]
      #f))


;; Definicion de gramaticas
;; 
;; Es importante que tus nuevas gramaticas sigan el mismo formato que las
;; incluidas aqui
;;
(define *gramaticas* '(

                       ;; Gramatica para generar poemas
                       ;;
                       (Gramatica-para-poemas
                        (<inicio>
                         (Las <objeto> <verbo> esta noche#\.))

                        (<objeto>
                         (olas)
                         (flores amarillas grandes)
                         (sanguijuelas))

                        (<verbo>
                         (suspiran <adverbio>)
                         (presagian como <objeto>)
                         (mueren <adverbio>))

                        (<adverbio>
                         (cautelosamente)
                         (cascarrabiosamente)))



                       ;; Gramatica para generar excusas para pedir prorrogas para tu proyecto
                       ;;
                       (Gramatica-No-termine-mi-proyecto-porque

                        (<inicio>
                         (Necesito una prórroga porque <suplica> #\.))

                        (<suplica>
                         (<excusa-dudosa>)
                         (<excusa-dudosa>)
                         (<excusa-dudosa>)
                         (<excusa-dudosa>)
                         (<excusa-dudosa>)
                         (<excusa-dudosa> #\, y luego <suplica> )
                         (<excusa-dudosa> #\, y encima de eso <suplica> )
                         (<excusa-dudosa> #\, y como si eso no fuera suficiente <suplica> )
                         (<excusa-dudosa> #\, y escucha esto #\, <suplica> )
                         (<excusa-dudosa> #\, y justo entonces <suplica> )
                         (<excusa-dudosa> #\, y #\, bueno estoy un poco avergonzado por esto #\, pero <suplica> )
                         (<excusa-dudosa> #\, y estoy seguro que has escuchado esto antes #\, pero <suplica> )
                         (<excusa-dudosa> #\, o #\, y luego <suplica> )
                         (<excusa-dudosa> #\, y y si recuerdo correctamente <suplica> ))


                        (<excusa-dudosa>
                         (mi disco duro se borro)
                         (el perro comió mi <algo>)
                         (la persona con la que vivo comió mi <algo>)
                         (no sabía que yo estaba en esta materia)
                         (pensé que ya me había graduado)
                         (mi casa se quemó)
                         (pasé todo el fin de semana con una resaca) 
                         (tuve <mucho-trabajo>)
                         (tuve <mucho-trabajo>)
                         (bueno #\, ya no recuerdo mucho de lo que pasó)
                         (tuve que irme a <evento-atletico>)
                         (tuve que practicar para <evento-atletico>)
                         (tuve que preocuparme de <evento-atletico>)
                         (perdí plata apostando en <evento-atletico>)
                         (me olvidé de cómo escribir)
                         (todos mis lápices se rompieron)
                         (la librería ya no tenía borradores)
                         (se me terminé todo mi papel)
                         (tuve que irme a un evento muy importante)
                         (me quedé atrapado en una tormenta)
                         (mi karma no estaba bueno)
                         (no tenía ganas de trabajar)
                         (estaba demasiado lindo afuera)
                         (el lenguaje de programación no era suficientemente abstracto)
                         (tuve que lavar mi ropa)
                         (perdí mi <algo>)
                         (mi <algo> tuve un problema privado)
                         (mi <algo> fue confiscado por aduana)
                         (mi <algo> fue envuelto en una bruma misteriosa por tres dias y luego desapareció)
                         (tuve sueños recurrentes sobre <algo>))

                        (<mucho-trabajo>
                         (<numero-impresionante> parciales)
                         (<numero-impresionante> parciales y <numero-impresionante> tareas)
                         (que terminar mi propuesta de tesis)
                         (<numero-impresionante> programas en <numero-impresionante> lenguajes distintos))

                        (<evento-atletico>
                         (las unimpiadas)
                         (una lucha libre de yacares)
                         (un partido de futbol)
                         (las semi-finales de danza paraguaya))

                        (<numero-impresionante>
                         (4)
                         (7)
                         (como #\, un millón de)
                         (toneladas de)
                         (mega)
                         (como #\, muchisimos #\,))

                        (<algo>
                         (disco duro)
                         (cd)
                         (mochila)
                         (mente)
                         (sentido de propósito)
                         (libro)
                         (anotaciones)
                         (mi compu)
                         (mi notebook)
                         (especificacion del módulo)
                         (código fuente)
                         (sueños)
                         (motivación)))
                       ))