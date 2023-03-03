
(*
Modelar a los animales, de ellos se sabe:
- su coeficiente intelectual (un número),
- su especie (que puede ser Elefante, Ratón, o un Perro, del cual conocemos su raza) 
- sus capacidades (strings).

Definir algunos ejemplos de animales.
*)

type especie =
    | Raton
    | Elefante
    | Perro of raza: string // La variante Perro tiene un campo raza de tipo string

// nota: los records no tienen constructores como en Haskell.
type animal =
    { ci: int
      especie: especie
      capacidades: string list }

let cerebro: animal =
    { ci = 100
      especie = Raton
      capacidades = [] }

let dumbo: animal =
    { ci = 30
      especie = Elefante
      capacidades = [] }

let beethoven: animal =
    { ci = 40
      especie = Perro "San Bernardo"
      capacidades = [] }

(* inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual. *)
let inteligenciaSuperiorConLambda: int -> animal -> animal = 
    fun n a -> { a with ci = a.ci + n }
let inteligenciaSuperior (n: int) (animal: animal) : animal = {animal with ci = animal.ci + n}
// usando pattern matching:
    // match animal with     // nota: la keyword match sirve para hacer pattern matching
    // | { ci = coef
    //     especie = unaEspecie
    //     capacidades = unasCapacidades } ->
    //     { ci = coef + n
    //       especie = unaEspecie
    //       capacidades = unasCapacidades }

(* pinkificar: Quita todas las habilidades que tiene el animal. *)

let pinkificar (a: animal) : animal = { a with capacidades = [] }
// usando pattern matching: 
    // match a with
    // | { ci = coef
    //     especie = unaEspecie
    //     capacidades = unasCapacidades } ->
    //     { ci = coef
    //       especie = unaEspecie
    //       capacidades = [] }


(*
nuevoComienzo: Pinkifica al animal 
y luego le incrementa el coeficiente intelectual en 20 
y le agrega el superpoder “aprendizaje rapido”.
Nota: Usar composición de funciones. 
*)
                 // input        // input      // output                      // cabeza :: cola
let agregarPoder (poder: string) (a: animal) : animal = {a with capacidades = poder :: a.capacidades}

// nota: >> es composicion de funciones. 
// En Haskell seria: (agregarPoder "aprendizaje rapido") . (inteligenciaSuperior 20) . pinkificar
// Al igual que Haskell, se pueden definir funciones point-free/tacitas.
let nuevoComienzo: animal -> animal =
    (agregarPoder "aprendizaje rapido") >> (inteligenciaSuperior 20) >> pinkificar

(*
superpoderes: Le da habilidades nuevas	
En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la habilidad de “hablar”. 
En caso de ser un perro, si la cantidad de letras de su raza es par, se le agrega la habilidad “cazador de ratones”, 
en caso contrario lo deja como está, sin agregarle ninguna habilidad.
*)

// hay inferencia de tipos, el tipo inferido es: 
// esPar: string -> bool
let esPar palabra = String.length palabra % 2 = 0

let superpoderes (unAnimal: animal) =
    // el match mezcla guardas con pattern matching. 
    // La condicion de la guarda va despues del when. El patron antes
    match unAnimal.especie with 
    | Elefante -> agregarPoder "no tenerle miedo a los ratones" unAnimal
    | Raton when unAnimal.ci > 100 -> agregarPoder "hablar" unAnimal
    | Perro raza when esPar raza -> agregarPoder "cazador de ratones" unAnimal
    | _ -> unAnimal // otherwise / cualquier otro caso

(*
cambiarEspecie: Cambia la especie del animal teniendo en cuenta su especie actual:
Si es un elefante pasa a ser un ratón.
Si es un ratón pasa a ser un Perro chihuahua.
Si es un perro de raza dogo pasa a ser un elefante, sino pasa a ser un ratón.
*)

let cambiarEspecie (unAnimal: animal) : animal = 
    match unAnimal.especie with
    | Elefante -> { unAnimal with especie = Raton }
    | Raton -> { unAnimal with especie = Perro "chihuahua" }
    | Perro "dogo" -> { unAnimal with especie = Elefante }
    | _ -> { unAnimal with especie = Raton }

(*
3. Los científicos muchas veces desean saber si un animal cumple ciertas propiedades, 
porque luego las usan como criterio de éxito de una transformación. 
Desarrollar los siguientes criterios:
*)

(*
    antropomórfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60.
*)

let antropomorfico unAnimal = List.contains "hablar" unAnimal.capacidades && unAnimal.ci > 60

(*
noTanCuerdo: si tiene más de dos habilidades de hacer sonidos pinkiescos. 
Hacer una función pinkiesco, que significa que la habilidad empieza con “hacer ”, 
y luego va seguido de una palabra "pinkiesca", es decir, con 4 letras o menos y al menos una vocal. 

Ejemplo:
> pinkiesco “hacer narf”
True

> pinkiesco “hacer asd”
True
*)
let esVocal (letra : char) : bool = List.contains letra ['a'; 'e'; 'i'; 'o'; 'u'] 

// let x = expr in expr2 funciona igual que en haskell
let esPinkiesca (palabra : string) : bool = 
    let listaDeCaracteres: char list = List.ofSeq palabra // se debe convertir a lista de caracteres explicitamente.
    let tieneMenosDe4Letras = String.length palabra <= 4
    let tieneAlMenosUnaVocal = List.exists esVocal listaDeCaracteres
    in tieneMenosDe4Letras && tieneAlMenosUnaVocal

let empiezaConHacer (palabra: string) : bool = palabra[..5] = "hacer "

let terminaConPalabraPinkiesca (palabra : string) : bool = esPinkiesca palabra[6..]

let pinkiesco (sonido: string) : bool = empiezaConHacer sonido && terminaConPalabraPinkiesca sonido

(*
4. Los científicos construyen experimentos: un experimento se compone de un conjunto de transformaciones 
sobre un animal y un criterio de éxito. 
Se pide:
Desarrollar experimentoExitoso: Dado un experimento y un animal, indica si al aplicar sucesivamente todas las transformaciones 
se cumple el criterio de éxito. 


Dar un ejemplo de consulta para representar la siguiente situación:


"En un ratón de coeficiente intelectual 17, con habilidades de destruenglonir el mundo y hacer planes desalmados, 
hacer un experimento que consista en pinkificarlo, luego darle inteligencia superior de 10 y por último darle superpoderes. 
Como criterio de éxito, ver si quedó antropomórfico" 
*)