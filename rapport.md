# Rapport du projet NanoGo

## Typing

En plus de faire le typing comme demandé dans le sujet, je profite de cette passe pour assigner la positions des arguments et des champs des fonctions et des structures.

## Production de code

Tous les programmes ont des fonctions et variables qui permettent de gérer la fonction `fmt.Print`, la comparaison de chaînes de caractères et l'allocation de mémoire.

- Lors de l'appel de `fmt.Print`, on évalue les arguments dans le même ordre qu'une fonction en NanoGo (voir la partie *Différence avec le sujet*).
L'affichage est le même que la fonction `fmt.Print` en go :
    - Il y a un espace entre deux arguments si aucun des deux arguments n'est de type `string`.
    - Les pointeurs de structures affichent une `&` suivie d'un print de la structure.
    - Les autres pointeurs sont affichés sous forme hexadécimal.
    - Les champs de structures sont affichés entre accolade et séparés par des espaces (exemple : `{5 10 15}`).
    - Les champs de structure qui sont de types pointeurs affichent la valeurs du pointeurs sous forme hexadécimal.
    - Les champs de structures qui sont de types structures affichent la structures.

Il a été choisie que toutes les fonctions renvoient leurs paramètres dans la pile. Toutefois si une fonction ne renvoie qu'une seule valeurs de retour et que cette dernière a un type de taille 8 alors la valeur de retour est également mise dans `rdi`.

Les fonctions utilises toujours le label `E_function` pour sortir.

Pour l'assignation des variables ou pour récupérer les addresses des variables une fonction récursive a été crée.


### Les structures

Les structures sont implémentées. Si une structure est défini comme variable locale alors elle est stockée dans la pile.
Le premier champs d'une structure se trouve à l'addresse la plus petite de la plage d'addresse alloué pour la structure. Et inversement le dernier champs d'une structure se trouve à l'addresse la plus grande de la plage d'addresse alloué pour la structure. 
Cela permet d'avoir le même code d'accès à un champs d'une structure quand la structure est stockée dans la pile ou dans le tas.
Exemple :
La structure S défini comme suit :
```go
type S struct{
    a int,
    b string,
    c bool
}
```
Est représenté comme suit :
|:-:|
| c | 
| b |
| a | <- pointeur vers la structure

- Le test d'égalité sur les structure est un test de valeur et non de positions.

# Différence avec le sujet

- L'ordre d'évaluation des expressions dans un return n'était pas spécifié dans la sémantique d'éxécution. Le choix a donc été fait d'évaluer les expressions de gauches à droite, comme en Go. Ainsi `return e1,e2,e3` évalue d'abord `e1` puis `e2` et enfin `e3`.

- Contrairement à ce qu'il est marqué dans le sujet, les fonctions peuvent avoir un effet de bord.
```go
func f(ptr *int) int{
    *ptr++
    return *ptr
}

func main(){
    var a = 0;
    fmt.Print(f(&a),f(&a),f(&a))
}
```
En fonction de l'ordre d'évaluation des arguments des fonctions le résultat peut changer, on peut avoir `1 2 3` (comme en Go) ou `3 2 1` (comme en NanoGo)
Ainsi  l'ordre d'évaluation des arguments d'une fonctions a été implémenté tel que décrite dans le sujet (cet à dire de droite à gauche)

## Cas donnant un résultat indéterminé.
- Actuallement les variables locales (y compris les structures) sont stockées sur la pile, donc si un pointeur d'une variable locale est retourné cela produit un effet non prévu.
Ainsi les codes suivants ne sont pas garanti de donner un résultat correcte dans le compilateur actuelle de NanoGo.

```go
func f() *int{
    var a = 0;
    return &a
}
```
```go
func main(){
    var ptr *int;
    {
        var a = 0;
        ptr = &a
    }
    fmt.Print(*ptr)
}
```
Pour régler ce problème une passe suplémentaire pourrait être envisagée afin de savoir si une référence de la variable peut être retourné. Si une référence de la variable peut être retourné alors elle doit être stockée dans le tas.


