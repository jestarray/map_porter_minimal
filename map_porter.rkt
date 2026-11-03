#lang racket
(require json)
(require racket/port)
(require racket/struct)
(require racket/string)
(require racket/runtime-path)

(define-runtime-path curr-dir ".")

(define hori #x80000000)
(define vert #x40000000)
(define diag #x20000000)
(define (vector-index-of v p?)
  (for/first ([x (in-vector v)]
              [i (in-naturals)]
              #:when (p? x)) i))

(define dmm-default-vars
  (make-hash
   (call-with-input-file
       (build-path curr-dir "ztlcpaths_default_vars.clj")
     read)))

; lookup-default: (String Symbol) -> Any | #f
(define (lookup-tlc-default dmmpath varname)
  (define find-path (hash-ref dmm-default-vars dmmpath #f))
  (cond
    [find-path
     (define find-var (hash-ref find-path varname #f))
     find-var]
    [else
     #f]))

; String -> (ListOf Pairs)
; GIVEN: (extract-instance-vars "/turf/enviroment/deco/junk_3{tag = \"icon-junk_3\"; icon_state = \"junk_3\"; dir = 4}")
; Produce: (list ("tag" "icon-junk_3") ("icon_state" "junk_3") ("dir" "4"))
; the assoc list of variables, and parsed numbers, otherwise empty
(define (extract-instance-vars str)
  ; match on first closing brace
  (define match-braces (regexp-match #px"\\{(.*?)\\}" str))
  (cond
    [(false? match-braces) empty]
    [else
     (define de-variables (second match-braces))
     (define assignments (string-split de-variables "; "))
     (for/list ([assign-str (in-list assignments)])
       (define first-equal-index (string-find assign-str " = "))
       (define splits
         (list (substring assign-str 0 first-equal-index)
               (substring assign-str (+ 3 first-equal-index))))
       (define res
         (match
             splits
           [`(,k ,v)
            (list k (string-replace v "\"" ""))] ; remove nested quotes
           [_
            (println (string-append "!!WARNING!!: COULD NOT PARSE VALUE OUT ON: " str))
            splits]))
       res)]))

; List<(String %X)> String %Y -> %X | %Y
; produces the value in the kv pair or
; the default parameter if key not found
(define (get-instance-val pairs-lst key-name default)
  (define find (assoc key-name pairs-lst))
  (if (false? find) default (second find)))

; (ListOf Any) -> (ListOf Pairs)
; example: (list 1 2 3 4) -> '((1 2) (3 4))
(define (group-into-pairs ls)
  (cond
    [(empty? ls)
     empty]
    [(odd? (length ls))
     (raise-user-error "list must be even to create matching pairs")]
    [else
     (cons (take ls 2) (group-into-pairs (drop ls 2)))]))

;todo, chose some big random 32bit number to represent insided and outside markers for now
; no need to parse out /obj/destructable/b_cades/cades/cades_1{pull_disable = 1} the brackets, we can just do (string-contains /obj/destructable/b_cades/cades/cades_1)
; parse out de_set_map_author = "bob"
; make sure this works with bigger objects like street poles n such

; pt stands for ported tile
;  string (string int)
; dmmpath (tilesetname tile_rel_id)
; /turf/environment/floors/floor_24 (outside_nature_floors 50); represents a mapping to the grass tile
; /turf/enviroment/floors/floor_23 (outdoor_city_floors 12) ;outdoor cenement block
; dmm path can be multiple chained together by a comma since we could have scenarios where 1 tile maps to multiple dmm paths
; need to add first-gid to id to resolve the final thing
(struct pt (dmpath rt) #:transparent)

(struct rt (tsetname id class) #:transparent)

; List<Hash> Symbol -> AnyOrFalse
; ListOf<TiledProperty> -> Value
; https://doc.mapeditor.org/en/stable/reference/json-map-format/#property
(define (tiled-find-prop-val lst key)
  (define find-prop
    (findf (lambda (hsh)
             (define pname (hash-ref hsh 'name))
             (eq? (string->symbol pname) key)) lst))
  (cond [(false? find-prop) #f]
        [else (hash-ref find-prop 'value)]))

; Hash-> Any | False
; TiledTile -> Any | False
; https://doc.mapeditor.org/en/stable/reference/json-map-format/#json-tile
; in the .tsj json file's .tiles array items
(define (tiled-get-prop-val hsh prop-name)
  (define props (hash-ref hsh 'properties #f))
  (define members (hash-ref hsh 'members #f)) ; for objecttypes, props are in members array
  (cond [props
         (tiled-find-prop-val props prop-name)]
        [members
         (tiled-find-prop-val members prop-name)]
        [else #f]))

(define tset-dir (build-path curr-dir "tsd"))
; List<List<Symbol, List<Tiles>>>
(define tilesets
  (filter-map
   (lambda (path)
     (define hsh (call-with-input-file (build-path tset-dir path) read-json))
     (define tsetname (string->symbol (path->string path)))
     (define tiles (hash-ref hsh 'tiles empty))
     (if (not (empty? tiles))
         (list tsetname tiles)
         #f))
   (directory-list tset-dir)))

; Hash<String, rt>
; example: "/area/inside" -> (rt 'waypoints_text.tsj 2)
; maps dmm path rt struct
(define dmm-path-table
  (make-immutable-hash
   (foldr
    (lambda (fname&tiles-info acc)
      (define tsetname (first fname&tiles-info))
      (define tiles (second fname&tiles-info))

      (define mapped-bypaths
        (foldr
         (lambda (thsh acc)
           (define get-bypath (tiled-get-prop-val thsh 'bypath))
           (define classname (hash-ref thsh 'type #f))
           (cond
             [get-bypath
              (define res
                (map (lambda (p)
                       (define tile-rel-id (hash-ref thsh 'id))
                       (cons p (rt tsetname tile-rel-id classname))) (string-split get-bypath ",")))
              #;
              (when (empty? res)
                (println (list "!!WARNING!! empty tileset.tsj tiles?" tsetname thsh)))

              (if (empty? res) acc (append res acc))]
             [else
              acc])) empty tiles))
      (append mapped-bypaths acc))
    empty
    tilesets)))


(define stan (call-with-input-file (build-path curr-dir "STANDARD" "stan_new.json") read-json))
(define objecttypes (call-with-input-file (build-path curr-dir "objecttypes.json") read-json))

; String -> Hash | False
(define (find-objecttype typename)
  (findf (lambda (hsh)
            (equal? (hash-ref hsh 'name) typename))
          objecttypes))

; max layers of type: "tile-layers", not including obj, etc
; 1 marker layer
(define top-layers 3)
(define bot-layers 4)
(define max-floor-layers 8)
; (println stan)

(define first-gids (map (lambda (hsh)
                          (define gid (hash-ref hsh 'firstgid))
                          (define src (string->symbol (string-trim (hash-ref hsh 'source) "../tsd/")))
                          (cons src gid)) (hash-ref stan 'tilesets)))

(define (get-first-gid tsetname)
  (define search (findf (lambda (p)
                          (eq? (car p) tsetname)) first-gids))
  (when
      (equal? search #false)
    (println (list "missing tilset " tsetname)))
  (cdr search))

; rt -> int
(define (rt->gid in)
  (+ (get-first-gid (rt-tsetname in)) (rt-id in)))


(define (1d->2d i width)
  (cons (modulo i width) (/ i width)))

(define (2d->1d x y w)
  (+ x (* w y)))

(define object-template
  '#hasheq((gid . 0)
           (height . 32)
           (id . 1)
           (name . "")
           (properties . ())
           (rotation . 0)
           (type . "")
           (visible . #t)
           (width . 32)
           (x . 0)
           (y . 32)))

; String -> List
; GIVEN: (split-path-string "/turf/enviroment/walls/wall_18/fill,/turf/enviroment/deco/arrows/arrow_down2,/area/inside")
; PRODUCE: (list "/turf/enviroment/walls/wall_18/fill" "/turf/enviroment/deco/arrows/arrow_down2" "/area/inside")
(define (split-path-string str)
  (define remove-parens (string-trim str #rx"[()]"))
  (define split-comma/ (string-split remove-parens ",/"))
  ; need to add the slash back
  (cons (first split-comma/)
        (map (lambda (i) (string-append "/" i))
             (rest split-comma/))))


; "\"aa\" = (/turf/enviroment/floors/voids/void,/area/inside)" -> List<String> | String | Empty
; ("aa" '())
; returns the string back for strings that are not like the format above, empty if invalid

; String -> (ListOf String)
; GIVEN: (slice-kv-string "\"aB\" = (/obj/destructable/b_cades/cades/cades_1{pull_disable = 1},/turf/enviroment/floors/floor_2,/area/inside)")
; PRODUCES: '("aB" "/obj/destructable/b_cades/cades/cades_1{pull_disable = 1}" "/turf/enviroment/floors/floor_2" "/area/inside")
(define (kv-string->list str)
  (define first-equal-index (string-find str " = "))
  (define key (substring str 1 (sub1 first-equal-index))) ; remove quotes
  (define paths-part (substring str (+ first-equal-index (string-length " = "))))
  (define val (split-path-string paths-part))
  (cond
    [(false? first-equal-index)
     (println (string-append "!!WARNING!! BAD KV STRING: " str))
     empty]
    [else
     (define (remove-trailing-commas s)
       (define lst (string->list s))
       (cond [(and (cons? lst) (char=? (last lst) #\,))
              (substring s 0 (sub1 (string-length s)))]
             [else s]))
     (define v2 (map remove-trailing-commas val))
     (cons key v2)]))

(define dbg-custom-data empty)
(define all-map-stas.csv (open-output-string))
(define all-map-stats.csv-wrote-headers-flag #false) ; write headers only once

;read from file
(define missing-textures-from-file (call-with-input-file (build-path curr-dir "MISSING_TEXTURES.sx")
                                     (lambda (i)
                                       (define (remove-quotes str) (substring str 1 (sub1 (string-length str))))
                                       (list->vector (map remove-quotes (port->lines i))))))
(define dbg-missing-textures empty)

(define nonblock-walltops
  (list
   "/turf/enviroment/walls/top_2/wall_top_11"
   "/turf/enviroment/walls/top_2/wall_top_12"
   "/turf/enviroment/walls/top/wall_top_12"
   "/turf/enviroment/walls/top/wall_top_13"
   "/turf/enviroment/walls/top/wall_top_14"
   "/turf/enviroment/walls/top/wall_top_15"
   "/turf/enviroment/walls/top/wall_top_16"
   "/turf/enviroment/walls/top_3/wall_top_3"
   "/turf/enviroment/walls/top_3/wall_top_4"))

(define overhead-tiles
  (append
   nonblock-walltops
   (list "/turf/enviroment/deco/house/counters/counter_2/counter_7"
         "/turf/enviroment/deco/house/counters/counter_3/counter_7"))) ;top of the counters

; (ListOfString -> (cons HashMap<String, String> Number))
; the number represents the string-length of the key
; e.g "az" = (/area ...) , (string-length "az")
; also fixes up duplicates,bad tile placement, and custom layer positions /etc
(define (create-dm-hash kv-strings-lst)
  (define pairs
    (foldl
     (lambda (curr acc)
       ; for a single line: '("eH" "/turf/enviroment/floors/floor_22" "/area")
       (define key-&-listofpaths (kv-string->list curr))

       ; List -> List
       (define (remove-useless-areas ls)
         (filter-not
          (lambda (str)
            (member str
                    '("/area/outside/outside" ; WARNING: DO NOT MOVE OUTSIDE PLAYER BLOCKS
                      "/area/outside/outside_in"
                      "/area/outside/outside/outside_underlay"
                      "/area") string=?)) ls))

       ; replaces common combination of cement and roadlines with a single merged tile
       (define (replace-road-lines ls)
         (define cement-list (list "/turf/enviroment/floors/floor_7" "/turf/enviroment/floors/floor_23" "/turf/enviroment/floors/floor_27"))
         (define find-cement (findf (lambda (v) (member v cement-list string=?)) ls))
         (define road-lines (filter (lambda (v) (string-contains? v "road_line")) ls))
         ; do a filter map
         (define postfix
           (match road-lines
             ['() ""]
             ['("/turf/enviroment/floors/road_lines/road_line_1") "1"]
             ['("/turf/enviroment/floors/road_lines/road_line_2") "2"]
             ['("/turf/enviroment/floors/road_lines/road_line_3") "3"]
             ['("/turf/enviroment/floors/road_lines/road_line_4") "4"]

             ; + plus looking road lines
             [(list "/turf/enviroment/floors/road_lines/road_line_1" "/turf/enviroment/floors/road_lines/road_line_2") "12"]
             [(list "/turf/enviroment/floors/road_lines/road_line_2" "/turf/enviroment/floors/road_lines/road_line_1") "12"]

             [(list "/turf/enviroment/floors/road_lines/road_line_3" "/turf/enviroment/floors/road_lines/road_line_4") "34"]
             [(list "/turf/enviroment/floors/road_lines/road_line_4" "/turf/enviroment/floors/road_lines/road_line_3") "34"]

             ; other weird coombinations that are "bug" with the map, especially devils peak does this mix
             [(list "/turf/enviroment/floors/road_lines/road_line_1" "/turf/enviroment/floors/road_lines/road_line_4") "4"]
             [(list "/turf/enviroment/floors/road_lines/road_line_1" "/turf/enviroment/floors/road_lines/road_line_3") "3"]
             [(list "/turf/enviroment/floors/road_lines/road_line_2" "/turf/enviroment/floors/road_lines/road_line_3") "3"]
             [(list "/turf/enviroment/floors/road_lines/road_line_3" "/turf/enviroment/floors/road_lines/road_line_2") "3"]
             [(list "/turf/enviroment/floors/road_lines/road_line_2" "/turf/enviroment/floors/road_lines/road_line_4") "4"]
             [_ "34"]))
         (define res 
           (filter-map 
            (lambda (v)
              (cond 
                [(and (not (string=? postfix "")) (member v cement-list string=?))
                 (string-append v "_" postfix)]
                [(and find-cement (string-contains? v "road_line")) ; remove the roadline
                 #f]
                [else v])) ls))
         res)

       ; todo: strip floors from any walls that cover the entire tile in general?
       ; do not strip floors from NON-BLOCK/TRANS WALLTOPS since they need a floor underneath em
       ; List -> List
       (define (remove-floors-under-hardwall-tops ls)
         (define has-hard-walltop?
           (not
            (empty? (filter (lambda (i)
                              (and (string-contains? i "/wall_top_")
                                   (not (member i nonblock-walltops)))) ls))))
         (define (is-floor-under-hardwalltop? str)
           (and has-hard-walltop?
                (or (string-contains? str "/floors/")
                    (and (member str nonblock-walltops)
                         ; some use this nonblock ontop of a hard walltop for valid deco, see 'the park'
                         (not (string=? str "/turf/enviroment/walls/top_3/wall_top_4")))
                    (string-contains? str "/obj/destructable/b_cades"))))
         (filter-not is-floor-under-hardwalltop? ls))

       ; replaces all ammo with items_random! Because we dynamically determine the items based on other factors(like playercount, etc)
       ; NOTE: do not replace health stuff and melee weps(shang tree maps will fail)
       (define (replace-ammo ls)
         (remove-duplicates
          (map
           (lambda (str)
             (if (string-contains? str "/obj/pickup/ammo")
                 "/obj/other_cool/ran_stuff/items_ammo"
                 str)) ls)))


       ; List -> List
       (define (remove-shadows ls)
         (filter-not (lambda (str)
                       (or (string-contains? str "shadow")
                           (string-contains? str "/turf/enviroment/walls/lines"))) ls))

       ; the little tip that goes on the top of walls to give em extra height
       ; remove all but the last one, ONLY if there are multiple
       ; List -> List
       (define (remove-dup-wallframes ls)
         (define wall-frame-indexes
           (indexes-where
            ls
            (lambda (str)
              (and (not (string-contains? str "wall_top"))
                   (string-contains? str "/top")))))
         (cond
           [(and (>= (length wall-frame-indexes) 2))
            (define last-index (last wall-frame-indexes))
            (define-values (before-kept-wallframe-ls keep-wallframe-ls) (split-at ls last-index))

            (define rm-rest
              (filter (lambda (str)
                        (or (string-contains? str "wall_top")
                            (and
                             (not (string-contains? str "wall_top"))
                             (not (string-contains? str "/top"))))) before-kept-wallframe-ls))
            (define res (append rm-rest keep-wallframe-ls))
            res]
           [else
            ls]))

       ; List -> List
       ; tries to keep 1 blood unless overflowing way too much
       (define (remove-too-much-blood ls)
         (define has-area-inside? (findf (lambda (str) (string-contains? str "/area")) (cdr ls)))
         (define area-offset (if has-area-inside? 1 0))
         (define valid-items-in-layer (- (length (cdr ls)) area-offset))
         (cond [(> valid-items-in-layer max-floor-layers)
                (define (is-blood? str) (string-contains? str "blood"))
                (define last-blood (findf is-blood? (reverse ls)))
                (define all-blood-removed (filter-not is-blood? ls))
                (define leave-1-blood (append all-blood-removed (if last-blood (list last-blood) empty)))
                ;removing all blood!
                (define res
                  (if (> (- (length (cdr leave-1-blood)) area-offset) max-floor-layers)
                      all-blood-removed
                      leave-1-blood))
                (println
                 (list "WARNING: not enough layers, removing blood " valid-items-in-layer "->" (- (length (cdr res)) area-offset) ls res))
                res]
               [else ls]))

       (define (add-red-carpet ls)
         (define has-floor31 (findf (lambda (v) (string-contains? v "/turf/enviroment/floors/floor_31")) ls))
         (cond
           [has-floor31
            ; slice it into the 2nd element
            (define key (first ls))
            (cons key (cons "/turf/enviroment/floors/floor_22" (rest ls)))]
           [else
            ls]))

       #| (when (not (findf (lambda (str)
                             (or (string-contains? str "/area/inside")
                                 (string-contains? str "/area/outside"))) key-&-listofpaths))
           (println (list "DEBUG: NOT labeled inside or outside!" key-&-listofpaths))) |#

       ;"bB" = (/turf/enviroment/floors/floor_23,/turf/enviroment/floors/floor_8/grass_egdes/e4,/turf/enviroment/floors/road_lines/road_line_3,/turf/enviroment/outside/objects/wires/wire_1/w1,/turf/enviroment/floors/floor_8/grass_egdes/e4,/area/outside/outside)
       ;grass_edge/e4 is duplicated!! BUT WE WANT TO KEEP THE TOPMOST(last one) SO WE REVERSE TWICE. See fortress x: 29, y: 61
       (define deduped-keep-last-dup (reverse (remove-duplicates (reverse key-&-listofpaths))))
       (define cleaned
         (apply (compose 
                 add-red-carpet
                 replace-road-lines
                 remove-too-much-blood
                 remove-useless-areas
                 remove-floors-under-hardwall-tops
                 replace-ammo
                 remove-shadows
                 remove-dup-wallframes)
                (list deduped-keep-last-dup)))
       ; (define debug (if (string=? "bHZ" key) (println cleaned) 0))
       ; insert a tile so you can easily find out where it is with search
       ; removing dupliate stacked fences!
       ; if there is a walltop(that is not transparent walkthrough), then there shant not be any floors underneath it!!!
       ; todo: get rid of stacked fences
       ; sort by layer value
       (define sorted-paths
         (sort
          (rest cleaned) ; exclude first cause it contains the key
          (lambda (i1 i2)
            ; String -> Number | #f
            (define (custom-layer-prop path)
              (cond
                [(string-contains? path "layer = ")
                 (define instance-vars (extract-instance-vars path))
                 (define res (get-instance-val instance-vars "layer" #f))
                 (if (string? res) (string->number res) res)]
                [else #f]))

            ; String -> Number
            ; if it's not in ztlcpaths_layerid or if it didn't have a custom prop
            ; var/const
            ; OVERHEAD_LAYER = 9
            ; UNDERHEAD_LAYER = 7
            ; N_MOB_LAYER = 6
            ; N_OBJ_LAYER = 5
            ; N_TURF_LAYER = 4
            ; SHDAOWOVER_LAYER = 3
            ; FLOOR_LAYER = 2
            (define (ptable-layers path)
              (cond [(string-contains? path "edge") 4]
                    [(string-contains? path "egdes") 4] ; grass
                    ; need floors on the bottom!
                    [(string-contains? path "floor") 1]
                    [(string-contains? path "turf") 4]
                    [(string-contains? path "obj") 5]
                    [(string-contains? path "mob") 6]
                    [(string-contains? path "area") 100]
                    [else 0]))
            (define (determine-layer path)
              (define layer-prop (custom-layer-prop path))
              (cond
                [(number? layer-prop) layer-prop]
                ;the transparent holes tilie should be above any turfs ?
                ; careful! buggy check, floor 25 is ontop of everything e.g in rock the boat 1st floor
                ; floor_25 is not on the ztlcpaths
                [(string-contains? path "/turf/enviroment/floors/floor_25") 2]
                [else
                 (define default (lookup-tlc-default path 'layer))
                 (if default default (ptable-layers path))]
                ))
            (define v1 (determine-layer i1))
            (define v2 (determine-layer i2))
            ; < for stable sort <= for unstable sort, leave it for now
            (< v1 v2))))
       (cons (cons (first key-&-listofpaths) sorted-paths) acc))
     empty
     kv-strings-lst))
  (define TILE-KEY-LEN (string-length (caar pairs)))
  (cons (make-immutable-hash pairs) TILE-KEY-LEN))

; (ListOf String) -> (ListOf (ListOf String))
(define (extract-blocks coords&floor)
  (define res empty)
  (define group empty)
  (for ([l coords&floor])
    (cond
      [(string-contains? l "{")
       ; start a new group
       (set! group empty)]
      [(string-contains? l "}")
       ; put it onto the result
       (set! res (cons (reverse group) res))]
      [(not (string=? l ""))
       (set! group (cons l group))]))
  (reverse res))

; for all of the floors! 3D list
; List<List<List<String>>>
; Floor<Row<Tile<Paths>>>
; e.g (((/area, /somethingfloor) (/somethingfloor)))
(define (convert-floor-to-3dpath pr coord&floor)
  (define dm-dict-hash (car pr))
  (define TILE-KEY-LEN (cdr pr))

  (define floor-chunk (extract-blocks coord&floor))
  (define floor-data (first floor-chunk))

  (define width-in-tiles (/ (string-length (first floor-data)) TILE-KEY-LEN))
  (define height-in-tiles (length floor-data))
  (for/list ([floor (in-list floor-chunk)])
    (for/list ([line floor]
               [_dbg-y-index (in-range 0 height-in-tiles)])
      (define row
        (for/list ([i (in-range 0 width-in-tiles)])
          (define k (substring line (* i TILE-KEY-LEN) (+ TILE-KEY-LEN (* i TILE-KEY-LEN))))
          (define v (hash-ref dm-dict-hash k))
          v))
      row)))

(define (wrap dmm-filepath)
  ; id starts from 1
  ; this is also written to "nextobjectid" as tiled continues generating ids from here
  ; todo: also assign a name
  (define ID-COUNTER! 1)

  (define dmm-file-lines (call-with-input-file dmm-filepath port->lines))
  ; kv-strings is (list "\"aa\" = (/turf/enviroment/floors/voids/void,/area/inside_player_block)" ...)
  (define-values (kv-strings coord&floor)
    (split-at dmm-file-lines (index-where dmm-file-lines (compose zero? string-length))))
  (define hash&hashlen (create-dm-hash kv-strings))

  ; List<List<List<String>>>
  ; where string is the dmpath
  (define 3D-paths (convert-floor-to-3dpath hash&hashlen coord&floor))

  (define (get-tile x y z)
    (list-ref (list-ref (list-ref 3D-paths z) y) x))

  (define map-width (length (list-ref (list-ref 3D-paths 0) 0)))
  (define map-height (length (list-ref 3D-paths 0)))
  (define floor-count (length 3D-paths))

  ; should be 6, 3 for top 3 for bottom
  ; 8 for the markings of in and out

  ; layer of tiles
  ; Vector<Vector<Vector<Int>>>
  ; Vector<Floor<Layer<Int>>
  ; DO NOT NEST MAKE-VECTOR! IT CAN CREATE MUTABLE ALIASES TO THE SAME THING
  (define arrays-of-tiles
    (for/vector #:length floor-count
      ([_ (in-range floor-count)])
      (for/vector #:length max-floor-layers
        ([_ (in-range max-floor-layers)])
        (for/vector #:length (* map-width map-height)
          ([_ (in-range (* map-width map-height))])
          0))))

  ; List<List<Objects>>
  ; List<Floor<Objects>>
  ; object layer tiles
  (define all-custom-tiles! empty)

  ; unique objects, e.g objects that have unique tile properties
  ; Hash<TiledHashProps, TiledObj>
  ; the key is a hash of props because thats what we want to detect and avoid duplicates of.
  ; the value is the first tiled object encountered with a unique set of props
  (define all-unique-obj-tiles (make-hash))

  ; references to the switches in all-unique-obj-tiles
  (define switch-refs empty)
  ; Hash<Number, List<NumberOrListNumber>>, the link id and the tiled obj id of the objects that share the same link id
  ; switches are wrapped in a list to differentiate them
  (define link-id-objs (make-hash))

  ; (define missing-textures! empty)
  (define (convert! ifloors floor-index map-width map-height)
    (define custom-tiles-on-floor! empty)
    (define vary-counter 0)

    (for ([y-index (in-range 0 map-height)]
          [row ifloors])
      (for ([x-index (in-range 0 map-width)]
            [list-of-paths row])
        (for ([dmpath list-of-paths]
              [layer-index (in-range 0 (length list-of-paths))])
          ; NOTE: indexes in byond map files are bottom left relative while tlcR is top left
          ; NOTE: BYOND DMM ALSO STARTS COUNTING FROM 1
          ; need to get index of the {} instanced tiles because custom objects should have a layer property as to figure out how they stack draw
          (define has-opening-brace? (string-contains? dmpath "{"))
          (define stripped-path (if has-opening-brace? (car (string-split dmpath "{")) dmpath))

          (define special
            (list "/turf/enviroment/deco/house/poster/poster_rand" "/obj/bestiary/enemys" "/turf/other/scenario_settings" "/obj/havens{" "mob" "/obj/other_cool/barricades/boarding" "/obj/other_cool/ran_stuff/herbs" "/obj/other_cool/ran_stuff/melee" "/obj/other_cool/ran_stuff/items_ammo" "/area/inside_player_block" "/area/inside_cade_block" "/area/outside/outside_player_block" "/area/outside/outside_cade_block" "/turf/locations/begin" "/obj/effects/spawn_zone" "/obj/scenario/evac_zone" "/obj/scenario/chopter/spawn_points" "/obj/scenario/chopter/way_points" "/obj/triggys/spawn_locators" "/obj/triggys/traps/bad/spawn_trigger" "/obj/triggys/traps/good/gamemode_trigger/escape_win" "/obj/triggys/traps/bad/death_hole" "/mob/player/NPC/Mob/Soldier" "/mob/player/NPC/Mob/Survivor" "/obj/pickup/items/other/Wood" "/obj/destructable/b_cades/" "/turf/enviroment/outside/objects/poles/street_poles/street_pole_1/p_2/pf" "/turf/enviroment/outside/objects/poles/street_poles/street_pole_1/p_1/pf" "/obj/destructable/vehicle/tl" "/obj/power_sources/lights/normal_lights/light_2" "/obj/power_sources/lights/normal_lights/light_1" "/obj/other_sources/candles/candle_1"))

          (define is-special? (member dmpath special string-contains?))

          (define find-full-path (hash-ref dmm-path-table dmpath #f))
          (define find-strip-path (hash-ref dmm-path-table stripped-path #f))
          (define rel-tile (if find-full-path find-full-path find-strip-path))


          (define gid (if (rt? rel-tile) (rt->gid rel-tile) 0))
          (define from-tileset (if (rt? rel-tile) (rt-tsetname rel-tile) 0))

          (define ignore-missing-textures (list "/turf/other/scenario_settings" "food" "triggys" "scenario" "begin" "/turf/other/de_map_settings" "/turf/enviroment/floors/voids/void_2" "/turf/enviroment/floors/voids/void" "/turf/enviroment/outside/objects/poles/street_poles/street_pole_1/p_1/p_2" "/obj/effects/spawn_zone" "area" "shadow" "mob" "floor_39" "couch_5" "street_poles" "fridge" "void" "stairs" "wall_color" "neon_text"))

          ; DEBUG: missing textures that still need to be ported
          (when (and (zero? gid)
                     (not (member dmpath ignore-missing-textures string-contains?))
                     (not is-special?)
                     (not (vector-member dmpath missing-textures-from-file)))
            (set! dbg-missing-textures (cons dmpath dbg-missing-textures)))

          ; todo: what if there are two custom instanced objects on the same tile?!
          ; if theres just an icon_state variable, we should just map it to a tile, e.g paper
          ; or spawnzone and any other special tiles
          (define floor-array (vector-ref arrays-of-tiles floor-index))

          ; + Marking a tile "outside" applies weather effects and an ambient light level of the outside world
          ; + Marking a tile "inside" applies sound distrotion and a different ambient light level than that of outside
          ; + NOTE: need to find a way to mark rooms for item finding RE style
          ; + By default in TLC, if neither outside or inside is marked, behavior is that of "inside", e.g no weather effects, no lighting
          (when (string-contains? dmpath "/area/inside")
            ; MARK INSIDES, because we process /area/inside/player_block below and it wont hit the other branch that does this
            ; write to last marker layer
            (define gid (rt->gid (hash-ref dmm-path-table "/area/inside")))
            (define last-layer (vector-ref floor-array (sub1 max-floor-layers)))
            (vector-set! last-layer (2d->1d x-index y-index map-width) gid))

          (cond
            [(or has-opening-brace? is-special?)
             ; CREATE OBJECTS OF THOSE THAT ARE SPECIAL OR HAVE OPENING BRACE
             (define obj-temp (hash-copy object-template))
             ; we dont really care what the byond instance objects names because they dont serve much of a purpose
             (define ignore-vars (list "name" "de_on" "de_lme" "alpha" "is_camzone"))
             (define all-instance-vars (extract-instance-vars dmpath))
             (define important-instance-vars (filter-not (lambda (i) (member (car i) ignore-vars string-contains?)) all-instance-vars))

             (define icon-state-vars (assoc "icon_state" important-instance-vars))
             (define find-dir (assoc "dir" important-instance-vars string=?))
             ;(define tag (assoc "tag" important-instance-vars string=?))
             (define has-important-data?
               (findf
                (lambda (i)
                  (or (string=? (car i) "l_id_num")
                      (string=? (car i) "E_s_id_num")
                      (string=? (car i) "pull_disable")
                      (string=? (car i) "j_disable")
                      (string=? (car i) "de_ki_key_id")
                      (string=? (car i) "de_ki_card_key_name")
                      (string=? (car i) "de_ki_description")
                      (string=? (car i) "de_send_extraction_chopter")
                      (string=? (car i) "de_send_message")
                      (string=? (car i) "de_max_floors") ;elevator floors
                      (string=? (car i) "de_ntext")
                      (string=? (car i) "l_start_on")
                      (string=? (car i) "health")
                      (string=? (car i) "gas")
                      (string=? (car i) "x_loc")
                      (string=? (car i) "y_loc")
                      (string=? (car i) "z_loc")
                      (string=? (car i) "density")
                      (string=? (car i) "opacity")
                      (string-contains? (car i) "description")
                      )) important-instance-vars))

             (define is-shaded (assoc "de_is_shaded" important-instance-vars string=?))
             ; treat shaded walls as non-objects
             ; (define process-wall-shaded (not (empty? is-shaded)))
             ; exclude the door but create object for shutters and those with important ids
             ; use dir to determine flips in horizontal and such
             ; TODO: does not account for roatated doors with a key_id
             ; useless instance vars should just be converted to a regular tile and not an object
             (cond
               [(and (not is-special?)
                     (not has-important-data?)
                     (or is-shaded find-full-path find-strip-path))

                ; PROCESS AS REGULAR TILE WITH FLIPS
                (define l (vector-ref floor-array layer-index))
                ; (println (list dmpath rel-tile find-full-path direction))
                ; these directions might be for doors only
                (define north "1") ;flip horizontally
                (define east "4") ;rotate right
                (define west "8") ;rotate left
                (define determine-gid
                  (cond
                    [find-dir
                     ; (println (list dmpath rel-tile find-full-path direction))
                     ; for tiles not even in the map maker and only accessible via iconstate vars!
                     ; e.g directions n such that require tile flipping and rotation in the custom props
                     ; diag vert = rotate-left by 90 degrees
                     ; diag hori = rotate-right by 90 degrees

                     ; hori vert - rotate double left
                     ; vert - rotate right flip hori
                     (cond [(string=? (cadr find-dir) north) (bitwise-ior gid hori)]
                           [(string=? (cadr find-dir) west) (bitwise-ior gid diag)]
                           [(string=? (cadr find-dir) east) (bitwise-ior gid diag hori)]
                           [else gid])
                     ]
                    [(false? icon-state-vars) gid]
                    [find-full-path gid]
                    [else
                     gid
                     ]))
                (set! dbg-custom-data (cons dmpath dbg-custom-data))
                (vector-set! l (2d->1d x-index y-index map-width) determine-gid)
                ]
               [else
                ; PROCESS AS OBJECT LAYER
                (define properties-template '#hasheq((name . "") (type . "") (value . "")))
                (define _to-hash
                  (filter-map
                   (lambda (kv)
                     ;todo : try parse number, also try to make de_object ones reference a tiled object to render arrows
                     (define cp-props (hash-copy properties-template))
                     (define key (car kv))
                     (define val (cadr kv))
                     ; (println (car kv))
                     ; set the object name to the number of the ids!
                     #;
                     (cond [(or (string=? key "l_id_num") (string=? key "E_s_id_num"))
                            (hash-set! obj-temp 'name val)])

                     ; RENAME PROPS
                     ; disabling pull is important for maps like rock the boat
                     ; NOTE: RENAMED "l_id_num" to "link_id"
                     (cond
                       [(string=? key "health")
                        (hash-set! cp-props 'name "hp")]
                       [(string=? key "max_health")
                        (hash-set! cp-props 'name "max_hp")]
                       [(string=? key "pull_disable")
                        (hash-set! cp-props 'name "pull")]
                       [(string=? key "j_disable")
                        (hash-set! cp-props 'name "jumpable")]
                       [(string=? key "description")
                        (hash-set! cp-props 'name "desc")]
                       [(string=? key "x_loc")
                        (hash-set! cp-props 'name "tp_x")]
                       [(string=? key "y_loc")
                        (hash-set! cp-props 'name "tp_y")]
                       [(string=? key "z_loc")
                        (hash-set! cp-props 'name "tp_z")]
                       [(string=? key "l_start_on") ;powered on
                        (hash-set! cp-props 'name "on")]
                       [(string=? key "l_id_num")
                        (hash-set! cp-props 'name "link_id")]
                       [(string=? key "E_s_id_num")
                        (hash-set! cp-props 'name "trap_id")]
                       [else
                        (hash-set! cp-props 'name key)])
                     ; NOTE: opacity, j_disable, pull_disable, density, can all be bools
                     ; density and opacity can be part of Collision bitflags? ATM the bitflags are always changing so we will keep it bool for now until it stabalizes
                     ; keep "l_start_on" an int? because there are 3 states, on, off and nonpowered
                     (define try-number (string->number val))
                     (cond
                       [(and try-number (member key '("j_disable" "pull_disable") string=?))
                        ; must be flipped compared to the rest,
                        ; e.g disable_pull = 1 means its on(no pulling)
                        ; but tlcr defaults to "pull" to 1 to mean allow pulling
                        ; so we must flip it
                        (hash-set! cp-props 'type "int")
                        (hash-set! cp-props 'value (if (zero? try-number) 1 0))]
                       [(and try-number (string-contains? key "loc"))
                        (hash-set! cp-props 'type "float")
                        ; offset cords to top left origin
                        (define modified
                          (cond [(string=? key "y_loc") (- map-height try-number)]
                                [(>= (sub1 try-number) 0) (sub1 try-number)]
                                [else try-number]
                                ))
                        ; NOTE: GRID COORDS ARE CONVERTED TO PIXELS HERE:
                        (hash-set! cp-props 'value (* modified 32))
                        ]
                       [try-number
                        ; note that some of these can be ints, e.g tp_x, tp_y
                        (hash-set! cp-props 'type "float")
                        (hash-set! cp-props 'value try-number)
                        ]
                       [else
                        (hash-set! cp-props 'type "string")
                        (hash-set! cp-props 'value val)
                        ])

                     (if (or (string=? key "density") (string=? key "opacity"))
                         #f ; DONT DO ANYTHING, WE NEED TO PROCESS THESE BOTH AT THE SAME TIME
                         cp-props)) important-instance-vars))
                ; https://discord.com/channels/524610627545595904/524610627545595906/1507078728797388901 (yes, propertytype is all lowercase and not camelcase for now)
                (define collide_etypes_prop (make-hash '((name . "collide_etypes") (propertytype . "collide_etypes") (type . "string") (value . ""))))
                (define collide_motions_prop (make-hash '((name . "collide_motions") (propertytype . "collide_motions") (type . "string") (value . ""))))

                (define custom-density (findf (lambda (kv) (string=? (car kv) "density")) important-instance-vars))
                (define custom-opacity (findf (lambda (kv) (string=? (car kv) "opacity")) important-instance-vars))

                ; fasle or strings
                (define mut-etypes  #f)
                (define mut-motions #f)

                ; KAISER KEEP SEE THROUGH WALLS SHOULD HAVE ALL FLAGS BUT OPAQUE, so we refer to the objecttypes to get the defaults
                ; kaiser keep has walk on walls for the watch towers and ladders also(so does TrenchWarfare)
                (when (and custom-density rel-tile) ; rel-tiles can be #f in rare cases
                  (define default-dense (lookup-tlc-default stripped-path 'density))
                  (define custom-val (string->number (cadr custom-density)))
                  (define same-as-default (equal? default-dense custom-val))
                  (define type-name (rt-class rel-tile))
                  (define objtype (find-objecttype type-name))
                  (when (and default-dense (not same-as-default) objtype)
                    (define defs-from-objtypes (tiled-get-prop-val objtype 'collide_motions))
                    (define val (if (and (zero? custom-val) defs-from-objtypes)
                                    (string-replace defs-from-objtypes "BUMP," "") 
                                    defs-from-objtypes))
                    (set! mut-motions val)))

                (when (and custom-opacity rel-tile)
                  (define default-opacity (lookup-tlc-default stripped-path 'opacity))
                  (define custom-val (string->number (cadr custom-opacity)))
                  (define same-as-default (equal? default-opacity custom-val))
                  (define type-name (rt-class rel-tile))
                  (define objtype (find-objecttype type-name))
                  (when (and default-opacity (not same-as-default) objtype)
                    (define defs-from-objtypes (tiled-get-prop-val objtype 'collide_etypes))
                    ; tlc byond doesn't have concept of projectiles, its just shoot whatever you can see
                    (define val (if (and (zero? custom-val) defs-from-objtypes)
                                    (string-replace (string-replace defs-from-objtypes "OPAQUE," "") "PROJECTILE" "")
                                    defs-from-objtypes))
                    (set! mut-etypes val)))

                (define to-hash
                  (cond
                    [(and mut-etypes mut-motions) ; custom density and opacity
                     (hash-set! collide_etypes_prop 'value mut-etypes)
                     (hash-set! collide_motions_prop 'value mut-motions)
                     (append (list collide_etypes_prop collide_motions_prop) _to-hash)]
                    [mut-etypes ; custom opacity
                     (hash-set! collide_etypes_prop 'value mut-etypes)
                     (cons collide_etypes_prop _to-hash)]
                    [mut-motions ; custom density
                     (hash-set! collide_motions_prop 'value mut-motions)
                     (cons collide_motions_prop _to-hash)]
                    [else
                     _to-hash]))

                (cond [(> gid 0)
                       (cond [(symbol=? from-tileset 'objects_32x64.tsj)
                              (hash-set! obj-temp 'width 32) (hash-set! obj-temp 'height 64)]
                             [(symbol=? from-tileset 'objects_vehicles.tsj)
                              ; most if not all cars are roated 90 degrees?? but it seems to displace it also..
                              (hash-set! obj-temp 'width 64) (hash-set! obj-temp 'height 64)]
                             [(string=? stripped-path "/obj/bestiary/lab/test_tube_under")
                              (hash-set! obj-temp 'width 38) (hash-set! obj-temp 'height 64)]
                             [(symbol=? from-tileset 'object_outdoors_64x96.tsj)
                              (hash-set! obj-temp 'width 64) (hash-set! obj-temp 'height 96)]
                             [(or (string=? stripped-path "/turf/enviroment/outside/objects/poles/street_poles/street_pole_1/p_2/pf")
                                  (string=? stripped-path "/turf/enviroment/outside/objects/poles/street_poles/street_pole_1/p_1/pf"))
                              (hash-set! obj-temp 'width 64) (hash-set! obj-temp 'height 128)])
                       (define flip-flags
                         (if (string=? stripped-path "/turf/enviroment/outside/objects/poles/street_poles/street_pole_1/p_2/pf")
                             hori
                             0))
                       (hash-set! obj-temp 'gid (bitwise-ior gid flip-flags))
                       ])
                ; write type!
                (define-values (set-type? add-zlayer-info?)
                  (cond
                    [(string=? stripped-path "/obj/triggys/traps/bad/spawn_trigger")  (values "SpawnTrigger" #f)]
                    [(string=? stripped-path "/obj/triggys/spawn_locators")  (values "SpawnLocator" #f)]
                    [(string=? stripped-path "/obj/effects/spawn_zone")  (values "MobSpawn" #f)]
                    [(string=? stripped-path "/turf/locations/begin")  (values "PlayerSpawn" #f)]
                    [(string=? stripped-path "/obj/scenario/chopter/spawn_points")  (values "HeliSpawn" #f)]
                    [(string=? stripped-path "/obj/scenario/chopter/way_points")  (values "HeliWayPoint" #f)]
                    [(string=? stripped-path "/obj/scenario/evac_zone")  (values "EvacZone" #f)]
                    [(string=? stripped-path "/area/inside_player_block")  (values "BlockP" #f)]
                    [(string=? stripped-path "/area/outside/outside_player_block")  (values "BlockP" #f)]
                    [(string=? stripped-path "/area/inside_cade_block")  (values "BlockC" #f)]
                    [(string=? stripped-path "/area/inside_flash_club_cade_block")  (values "BlockC" #f)]
                    [(string=? stripped-path "/area/outside/outside_cade_block")  (values "BlockC" #f)]
                    [(string=? stripped-path "/obj/other_cool/ran_stuff/items_ammo") (values "RanAmmo" #f)]
                    [(string=? stripped-path "/obj/other_cool/ran_stuff/herbs") (values "RanHerb" #f)]
                    [(string=? stripped-path "/obj/other_cool/ran_stuff/melee") (values "RanMelee" #f)]
                    [(string-contains? dmpath "/turf/enviroment/deco/house/poster/poster_rand") (values "RanPoster" #t)]
                    [(string=? stripped-path "/obj/other_cool/barricades/boarding") (values "Boarded" #f)]
                    [(string=? stripped-path "/mob/player/NPC/Mob/Survivor") (values "Survivor" #f)]
                    [(string=? stripped-path "/mob/player/NPC/Mob/Soldier") (values "Soilder" #f)]
                    [(string-contains? dmpath "/obj/havens{de_on = 1") (values "PlayerSpawn" #f)]
                    [(string-contains? dmpath "/turf/locations/begin/blue") (values "SpawnBlue" #f)]
                    [(string-contains? dmpath "/turf/locations/begin/pink") (values "SpawnPink" #f)]
                    [(string-contains? dmpath "/turf/locations/begin/green") (values "SpawnGreen" #f)]
                    [(string-contains? dmpath "/turf/locations/begin/red") (values "SpawnRed" #f)]
                    [(string-contains? dmpath "/obj/destructable/windows/window") (values "Window" #t)]
                    [(string-contains? dmpath "/obj/pickup/items/other/Wood") (values "Boards" #f)]
                    [(string-contains? dmpath "/obj/other_sources/candles/candle_1") (values "Candle" #f)]
                    [(or (string-contains? stripped-path "/obj/power_sources/lights/normal_lights/light_1")
                         (string-contains? stripped-path "/obj/power_sources/lights/normal_lights/light_2"))
                     (values "LightBulb" #f)]
                    [(or (string-contains? dmpath "/obj/destructable/vehicle/tl")
                         (string-contains? dmpath "/obj/destructable/vehicle")) (values "Car" #f)]
                    [(or (string-contains? dmpath "/turf/enviroment/outside/objects/poles/street_poles/street_pole_1/p_2/pf")
                         (string-contains? dmpath "/turf/enviroment/outside/objects/poles/street_poles/street_pole_1/p_1/pf"))
                     (values "StreetLight" #f)]
                    ; [(string-contains? dmpath "b_cades") (values "Cade" #t)]
                    ;[(string-contains? dmpath "/obj/power_sources/o_switch") "Switch"]
                    ;[(string-contains? dmpath "/obj/power_sources/elevators/elevator_switch") "Switch"]
                    ;[(string-contains? dmpath "/obj/power_sources/l_switchs/normal_l_switchs/n_light_switch_1") "Switch"]
                    ;[(string-contains? dmpath "/obj/scenario/mode_objects/raido") "Switch"]
                    [(string-contains? dmpath "/obj/triggys/traps/good/gamemode_trigger/escape_win") (values "EscapeWin" #f)]
                    [(string-contains? dmpath "/turf/other/scenario_settings") (values "ScenarioSettings" #f)]
                    [(string-contains? stripped-path "/obj/destructable/b_cades/") (values #f #f)]
                    [(string-contains? stripped-path "switch") (values #f #f)]
                    [(string-contains? stripped-path "shutter") (values #f #f)]
                    [(string-contains? stripped-path "Monster") (values #f #f)]
                    [(string-contains? stripped-path "bestiary") (values #f #f)]
                    [(string-contains? stripped-path "fence") (values #f #f)]
                    [else 
                     ;(println dmpath)
                     (values #f #t)] ; add zinfo layer to the rest
                    ))
                (cond [(string? set-type?)
                       (hash-set! obj-temp 'type set-type?)])
                ; write mob name:
                (cond [(string-contains? stripped-path "/mob")
                       (hash-set! obj-temp 'name (last (string-split stripped-path "/")))]
                      [(string-contains? stripped-path "bestiary")
                       (hash-set! obj-temp 'name (string-append "tube_" (last (string-split stripped-path "/"))))])

                (cond
                  [add-zlayer-info?
                   (define z-prop (hash-copy properties-template))
                   ; need to add the zlayer on some of the tiles(namely windows, ranposter, and boarded) because we don't know which layer they would be on. E.G Joint City Done has windows above the player rather than where the player is?! Or for a RanWallposter, we don't know which the layer the wall is at
                   (hash-set! z-prop 'name "zlayerslice")
                   (hash-set! z-prop 'value layer-index)
                   (hash-set! z-prop 'type "int")
                   (hash-set! obj-temp 'properties (cons z-prop to-hash))]
                  [else
                   ; write we must write out props!
                   (hash-set! obj-temp 'properties to-hash)])

                (hash-set! obj-temp 'x (* x-index 32))
                (hash-set! obj-temp 'y (* y-index 32))
                (hash-set! obj-temp 'id ID-COUNTER!)

                (define is-switch? (string-contains? stripped-path "switch"))
                (when is-switch?
                  (define links-prop (hash-copy properties-template))
                  (hash-set! links-prop 'name "links")
                  (hash-set! links-prop 'type "list")
                  ; need to add a unique val to make them unique so they can be inserted into the global unique obj tiles
                  (hash-set! links-prop 'value (list (make-hash (list (cons 'type "object") (cons 'value (add1 ID-COUNTER!))))))
                  (define added-prop (cons links-prop (hash-ref obj-temp 'properties)))
                  (hash-set! obj-temp 'properties added-prop))

                (define mod-props (hash-ref obj-temp 'properties))
                (define link-id (tiled-find-prop-val mod-props 'link_id))
                ; note that we wrap ids that are swtiches in a list to differentiate them
                (when link-id
                  (define existing (hash-ref link-id-objs link-id #f))
                  (cond
                    [(and is-switch? existing)
                     (hash-set! link-id-objs link-id (cons (list ID-COUNTER!) existing))]
                    [existing
                     (hash-set! link-id-objs link-id (cons ID-COUNTER! existing))]
                    [else
                     (hash-set! link-id-objs link-id (list (if is-switch? (list ID-COUNTER!) ID-COUNTER!)))]))

                ;(define props-without-link-id (remove "link_id" mod-props (lambda (l-id hsh) (string=? (hash-ref hsh 'name "") l-id))))
                ;(hash-set! obj-temp 'properties props-without-link-id)
                (define props (hash-ref obj-temp 'properties))
                ; comment this cond block out if you want properties on all objects again
                (cond
                  [(and (not (empty? props)) (not (hash-has-key? all-unique-obj-tiles props)))
                   (define new-unique (hash-copy obj-temp))
                   (set! ID-COUNTER! (add1 ID-COUNTER!))
                   (hash-set! new-unique 'id ID-COUNTER!)
                   (define name-from-instance-vars (findf (lambda (pr) (equal? (first pr) "name")) all-instance-vars))
                   (define new-name
                     (if name-from-instance-vars
                         (second name-from-instance-vars)
                         (number->string ID-COUNTER!)))
                   (hash-set! obj-temp 'name new-name) ; set the new name on the current obj also
                   (hash-set! obj-temp 'properties empty) ; but erase the props cause they have been moved to the unique layer
                   (hash-set! new-unique 'name new-name)
                   (hash-set! all-unique-obj-tiles props new-unique)
                   (when is-switch?
                     (set! switch-refs (cons (list link-id new-unique) switch-refs)))
                   ]
                  [(and (not (empty? props)) (hash-has-key? all-unique-obj-tiles props))
                   ; for those without any custom props, e.g blockp and marker tiles, we don't consider them unique
                   ; we don't write out the props!
                   (define obj-stamp (hash-ref all-unique-obj-tiles props))
                   (define new-name (hash-ref obj-stamp 'name))
                   (hash-set! obj-temp 'name new-name) ; set the name like above
                   (hash-set! obj-temp 'properties empty) ; delete the props
                   ])

                (set! ID-COUNTER! (add1 ID-COUNTER!))

                (set! custom-tiles-on-floor! (cons obj-temp custom-tiles-on-floor!))
                ])
             ]
            ; MARK INSIDES
            [(string-contains? dmpath "/area/inside")
             ; write to last marker layer
             (define last-layer (vector-ref floor-array (sub1 max-floor-layers)))
             (vector-set! last-layer (2d->1d x-index y-index map-width) gid)
             ]
            [(or (and (string-contains? dmpath "/turf/enviroment/walls/wall_")
                      (string-contains? dmpath "top"))
                 (member dmpath overhead-tiles)
                 (string-contains? dmpath "/turf/enviroment/outside/objects/wires/wire_1")
                 (string-contains? dmpath "/turf/enviroment/deco/webs"))
             ; check if top layers (indexes 4, 5, 6) already have something on them? if not keep bumping up and try to occupy it
             (define try-place-top-layers-top
               (for/or ([layer-index (in-range bot-layers (+ bot-layers top-layers))])
                 (define layer (vector-ref floor-array layer-index))
                 (define tile (vector-ref layer (2d->1d x-index y-index map-width)))
                 (cond
                   [(= tile 0)
                    ; if there's nothing on the layer, place gid there and break
                    (vector-set! layer (2d->1d x-index y-index map-width) gid)
                    #t]
                   [else
                    ; otherwise continue checking other layers for an empty slot
                    #f])))
             (if try-place-top-layers-top
                 #t
                 (println "!!WARNING!!, NOT ENOUGH ROOM ON TOP LAYER" ))]
            [else
             ; regular tiles with non-instance vars
             ; put blocking wall-tops on the lowest floor layer level because there shouldn't be a floor below it
             (define adj-layer-index (cond [(and (string-contains? dmpath "/wall_top_")
                                                 (not (member dmpath nonblock-walltops))) 0]
                                           [else layer-index]))
             (define adj-gid
               (cond
                 [(string=? dmpath "/turf/enviroment/deco/park/rocks/rocks/rocks_1")
                  (define vary-rock (+ gid (modulo vary-counter 4)))
                  (set! vary-counter (add1 vary-counter))
                  vary-rock]
                 [(string-contains? dmpath "grave_1")
                  (define vary-grave (+ gid (modulo vary-counter 8)))
                  (set! vary-counter (add1 vary-counter))
                  vary-grave]
                 [(string-contains? dmpath "grave_2")
                  (define vary-grave (+ gid (modulo vary-counter 4)))
                  (set! vary-counter (add1 vary-counter))
                  vary-grave]
                 [(string=? dmpath "/turf/enviroment/deco/industrial/barrels/barrels_1")
                  (define vary-barrel (+ gid (modulo vary-counter 7))) ; exclude single
                  (set! vary-counter (add1 vary-counter))
                  vary-barrel]
                 [(string=? dmpath "/turf/enviroment/deco/gamr_turfs/pack_1/barrels_1d")
                  ; flipp horzi
                  (bitwise-ior gid hori)]
                 [(string=? dmpath "/turf/enviroment/deco/markets/shop_select")
                  (define vary (+ gid (modulo vary-counter 8)))
                  (set! vary-counter (add1 vary-counter))
                  vary]
                 [(string=? dmpath "/turf/enviroment/floors/floor_14")
                  (define vary (+ gid (modulo vary-counter 8)))
                  (set! vary-counter (if (= vary 0) 1 0))
                  vary]
                 [else
                  gid]))
             (define tile (vector-ref floor-array adj-layer-index))
             ; (println (list floor-index layer-index))
             ; since we made this a 32x32 sprite from 32x64, we have to shift it up by one
             (define adj-y
               (cond [(string-contains? dmpath "/turf/enviroment/deco/house/kitchen/cupboards/cupboard_1") (sub1 y-index)]
                     [else y-index]))

             ; no need to add voids on the 0th floor? because by default there's nothing under for it to bleed through
             #;
             (when
                 (not (and (string=? dmpath "/turf/enviroment/floors/voids/void") (not (zero? floor-index))))
               (vector-set! tile (2d->1d x-index adj-y map-width) adj-gid))

             (vector-set! tile (2d->1d x-index adj-y map-width) adj-gid)

             ])
          )))

    ; combines individual objects into a biger rectangle objects
    (define (merge-adjacent-customtiles ctiles)
      (define ignore (list "Cade" "Soldier" "Window" "Boarded" "PlayerSpawn" "Boards" "LightBulb" "Candle")) ; todo, haven and begin spawns will merge, we should make them distinct? e.g make havens a special playerspawn?
      (define (merged-adj-x ctiles)
        (define (aux curr-ctiles acc)
          (cond
            [(empty? curr-ctiles) acc]
            [else
             (define hsh (car curr-ctiles))
             (define x (hash-ref hsh 'x))
             (define y (hash-ref hsh 'y))
             (define type (hash-ref hsh 'type))
             ; (println (list type (member type ignore)))
             (cond
               ; ignore empty strings because cades and shutters, etc, dont have set types! they are implicitly defined by the tileset!
               [(or (string=? type "") (member type ignore))
                (aux (rest curr-ctiles) (cons hsh acc))]
               [else
                (define (adj-xs)
                  (define *dist-x* 32)
                  (filter
                   (lambda (hsh2)
                     (cond
                       [(and
                         (equal? type (hash-ref hsh2 'type))
                         (= y (hash-ref hsh2 'y))
                         (= (hash-ref hsh2 'x) (+ x *dist-x*)))
                        (set! *dist-x* (+ *dist-x* 32))
                        #t]
                       [else
                        #f])) curr-ctiles))
                (define adj-xes (adj-xs))
                ; (println adj-xes)
                (define most-adj-x (if (empty? adj-xes) hsh (last adj-xes)))
                (cond
                  [(equal? hsh most-adj-x)
                   ; (println (list "SAME " hsh most-adj-x))
                   (aux (rest curr-ctiles) (cons hsh acc))]
                  [else
                   (define filtered
                     (filter-not (lambda (h)
                                   (member h adj-xes)) (rest curr-ctiles)))
                   (define combined (hash-copy hsh))
                   #;
                   (println
                    (list "DIFF "
                          (hash-ref hsh 'id) (/ x 32)
                          (hash-ref most-adj-x 'id) (/ (hash-ref most-adj-x 'x) 32) combined))
                   (hash-set! combined 'width
                              (- (+ 32 (hash-ref most-adj-x 'x))
                                 x))
                   (aux filtered (cons combined acc))])])]))
        (aux ctiles empty))

      ; merge on same dimmension
      (define (merge-adj-y ctiles)
        (define (get-adj-ys hsh ls)
          (define type (hash-ref hsh 'type))
          (define x (hash-ref hsh 'x))
          (define y (hash-ref hsh 'y))
          (define w (hash-ref hsh 'width))
          (define h (hash-ref hsh 'height))
          ; (println (list type x y w h (hash-ref hsh 'id)))
          (define *dist-y* 32)
          (filter
           (lambda (fh)
             (cond
               [(and
                 ; (not (equal? (hash-ref hsh 'id) (hash-ref fh 'id)))
                 (equal? type (hash-ref fh 'type))
                 (= w (hash-ref fh 'width))
                 (= h (hash-ref fh 'height))
                 (= x (hash-ref fh 'x))
                 (= (hash-ref fh 'y) (+ y *dist-y*)))
                (set! *dist-y* (+ 32 *dist-y*))
                #t]
               [else #f])) ls))

        (define (aux curr-ctiles acc)
          (cond
            [(empty? curr-ctiles) acc]
            [else
             (define hsh (car curr-ctiles))
             (define type (hash-ref hsh 'type))
             (define most-adj-ys (get-adj-ys hsh curr-ctiles))
             (cond
               [(empty? most-adj-ys)
                (aux (rest curr-ctiles) (cons hsh acc))]
               [(or (string=? type "") (member type ignore))
                ; we gotta (cons hsh acc), keep the x'es here!
                (aux (rest curr-ctiles) (cons hsh acc))]
               [else
                (define final-hsh-y (hash-copy (last most-adj-ys)))
                (hash-set! final-hsh-y 'height (- (+ (hash-ref final-hsh-y 'y) 32) (hash-ref hsh 'y)))
                ; NOTE: use the id of the last one but set its start to the first one
                (hash-set! final-hsh-y 'y (hash-ref hsh 'y))
                (define adj (filter-not
                             (lambda (i)
                               ; remove self as well cause it will be merged in with something else
                               (member i (cons hsh most-adj-ys))) curr-ctiles))
                (aux adj (cons final-hsh-y acc))])
             ]))
        (aux ctiles empty))
      ; merge on x then use that to merge on y
      (define merged-y (merge-adj-y (reverse (merged-adj-x ctiles))))
      (define sorted (sort merged-y (lambda (a b)
                                      (>
                                       (hash-ref a 'id)
                                       (hash-ref b 'id)))))

      ;(define dedup (map (lambda (hsh) (hash-ref hsh 'id)) sorted))
      ;(when (not (equal? (length dedup) (length sorted)))
      ;  (println "MIGHT HAVE DELETED SOMETHING? OR USED SAME IDS WITH SOMETHING ELSE?"))

      sorted)
    (define merged (merge-adjacent-customtiles (reverse custom-tiles-on-floor!)))
    ;(println (list merged-x (length merged-x)))
    (set! all-custom-tiles! (cons (reverse merged) all-custom-tiles!))
    ;(set! all-custom-tiles! (cons (reverse custom-tiles!) all-custom-tiles!))
    )

  (for ([f 3D-paths]
        [i (range floor-count)])
    (convert! f i map-width map-height))
  ;(println (write-json custom-tiles!))

  (for ([pair switch-refs])
    (define link-id (first pair))
    (define hsh (second pair))
    (define obj-id (hash-ref hsh 'id #f))
    (define props (hash-ref hsh 'properties))
    (define links-prop (findf (lambda (phsh) (equal? (hash-ref phsh 'name #f) "links")) props))
    (define share-link-ids (hash-ref link-id-objs link-id))
    (define relevant-link-id-objs
      (filter-map
       (lambda (maybe-lst)
         (cond
           [(list? maybe-lst)
            (define val (first maybe-lst))
            (if (= (add1 val) obj-id)
                #f
                (make-hash `((type . "object") (value ,@val))))]
           [else
            (make-hash `((type . "object") (value ,@maybe-lst)))])) share-link-ids))
    (hash-set! links-prop 'value relevant-link-id-objs))
  #;
  (define tile-layer-utilization
    (for/list
        ([layer (vector-ref arrays-of-tiles 0)])
      (exact->inexact (* 100 (/ (vector-count (compose not zero?) layer) (* map-width map-height))))))

  (define zeros
    (for/sum
        ([floor arrays-of-tiles])
      (for/sum
          ([layer floor])
        (vector-count (compose zero?) layer))))

  (define non-zeros
    (for/sum
        ([floor arrays-of-tiles])
      (for/sum
          ([layer floor])
        (vector-count (compose not zero?) layer))))

  ; Calculate statistics on whether we should store all the tiles in the game as a HashMap<((x,y,z,layer_index), u32)> or flat array
  ; if most tile layers are just empty zeros, and the cost to store the hashkey(x, y, z, layer_index) for all other tiles is less than storing every 0 in the flat array approach
  ; https://gemini.google.com/app/9476ee027878ff3e
  ; nonzeros  * (4 + 1) u32s for the coord(x,y,z,layer_index) where z is floor index and layer is what zorder + the gid itself
  (define hm-size (* non-zeros (* 4 4))) ; 5 u32s * 4 bytes each
  ; (define hm-size (* non-zeros (* 4 3))) ; 5 u32s * 4 bytes each
  (define flat-array-size (* map-width map-height max-floor-layers 4)) ; 4 bytes per tile
  ; capture only the last 2 backslashes going backwards
  (define simplified-path (second (regexp-match #rx"(/[^/]+/[^/]+)$" (path->string dmm-filepath))))
  (define print-status
    (list
     (list "name" simplified-path)
     (list "floors" floor-count)
     (list "width" map-width)
     (list "height" map-height)
     (list "total-tiles" (* map-width map-height))
     (list "custom-objs" (length (car all-custom-tiles!)))
     (list "unique-objs"(length (hash->list all-unique-obj-tiles)))
     ;(list "nonzeros" non-zeros)
     ;(list "zeros" zeros)
     ; (list "total" (+ non-zeros zeros))
     (list "hmap_kilobytes" (exact->inexact (/ hm-size 1000)))
     (list "flatarr_kilobytes" (exact->inexact (/ flat-array-size 1000)))
     (list "savings%" (round (exact->inexact (* 100 (- 1 (/ hm-size flat-array-size))))))
     ))

  ; LOG TO TABLE FILE
  (unless all-map-stats.csv-wrote-headers-flag
    (set! all-map-stats.csv-wrote-headers-flag #true)
    (define headers (map first print-status))
    (for ([v headers])
      (display (string-append v "|") all-map-stas.csv))
    (newline all-map-stas.csv)
    )

  (define vals (map second print-status))
  (for ([v vals])
    (define to-string (if (string? v) v (number->string v)))
    (display (string-append to-string "|") all-map-stas.csv))
  (newline all-map-stas.csv)

  ; REGULAR PRINT LOGGING:
  (for ([v vals])
    (define to-string (if (string? v) v (number->string v)))
    (display (string-append to-string "|")))
  (newline)

  ; (print-row (vector-ref (vector-ref lot 0) 0) map-width map-height)

  ; WARNING! CLEARS THE ENTIRE LAYER DATA ARRAY TO 0
  ; List<List<Int>>
  ; Floor<Layer<Tiles>>
  (define (create-map json width height data custom-tiles unique-objs)
    (define res (hash-copy json))
    (hash-set! res 'width width)
    (hash-set! res 'height height)
    (define root-layers (hash-ref res 'layers))
    (define floor-0-group (findf (lambda (hsh) (string=? (hash-ref hsh 'name) "floor_0")) root-layers))
    ; create floors
    (define group-layer-floors
      (for/list ([_ (in-range 0 (vector-length data))])
        (hash-copy floor-0-group)))
    (define group-layers
      (for/list ([z (in-range (vector-length data))]
                 [g-layers group-layer-floors])
        (define floor-arr (vector-ref data z))
        (define new-floor-name (string-append "floor_" (number->string z)))
        (hash-set! g-layers 'name new-floor-name)
        (define floor-layers (hash-ref g-layers 'layers))
        (define updated-floor-layers
          (for/list ([y (in-range (length floor-layers))]
                     [layers (map hash-copy floor-layers)])
            (cond [(string=? (hash-ref layers 'type) "objectgroup")
                   ; (println layers)
                   (hash-set! layers 'objects (list-ref custom-tiles z))
                   ; (println custom-tiles)
                   ]
                  [else
                   ; regular tile layer
                   (define row (vector-ref floor-arr y))
                   (hash-set! layers 'width width)
                   (hash-set! layers 'height height)
                   (hash-set! layers 'data (vector->list row))
                   (hash-set! g-layers 'layers layers)
                   ])
            layers))
        (hash-set! g-layers 'layers updated-floor-layers)
        g-layers))

    (for ([_ (range 0 (length group-layers))])
      (hash-set! res 'layers group-layers))

    (define global-obj-layer (findf (lambda (hsh) (string=? (hash-ref hsh 'name) "STAMPS")) root-layers))
    (when global-obj-layer
      (define mut-global-obj (hash-copy global-obj-layer))
      (hash-set! mut-global-obj 'objects unique-objs)
      (hash-update! res 'layers (lambda (lay) (cons mut-global-obj lay))))

    ; Assign map-wide layer IDs after all floors and STAMPS are assembled.
    (define next-layer-id 1)
    (define (assign-layer-id layer)
      (define updated (hash-copy layer))
      (hash-set! updated 'id next-layer-id)
      (set! next-layer-id (add1 next-layer-id))
      (when (hash-has-key? updated 'layers)
        (hash-set! updated 'layers
                   (map assign-layer-id (hash-ref updated 'layers))))
      updated)
    (hash-set! res 'layers (map assign-layer-id (hash-ref res 'layers)))
    (hash-set! res 'nextlayerid next-layer-id)

    ;todo: parse weather settings, daytime settings ,etc
    (hash-set! res 'nextobjectid ID-COUNTER!)
    (define dhash (flatten (hash-values (car hash&hashlen))))
    (define find-settings (findf (lambda (str) (string-contains? str "de_map_settings")) dhash))
    (define de_settings (if (false? find-settings) empty (extract-instance-vars find-settings)))
    (define author (get-instance-val de_settings "de_set_map_author" "UNCLAIMED"))
    ; summer winter ; todo add fall and spring
    (define season (get-instance-val de_settings "de_set_season" ""))
    ; dawn dusk day night
    (define time-of-day (get-instance-val de_settings "de_set_time_of_day_start" ""))
    ; "Clear","Cloudy","Windy","Rain","Storm"
    (define weather (get-instance-val de_settings "de_set_weather_start" ""))

    (hash-set!
     res 'properties
     (for/list ([prop (in-list (hash-ref res 'properties))])
       (define prop-name (hash-ref prop 'name))
       (cond
         [(string=? prop-name "desc")
          (hash-update prop 'value (lambda (v) (string-append "Created by: " author)))]
         [(string=? prop-name "season") (hash-update prop 'value (lambda (v) season))]
         [(string=? prop-name "time_of_day")
          (hash-update prop 'value (lambda (v) time-of-day))]
         [(string=? prop-name "weather") (hash-update prop 'value (lambda (v) weather))]
         [(string=? prop-name "author") (hash-update prop 'value (lambda (v) author))]
         [else prop])))
    res)

  (create-map
   stan
   map-width
   map-height
   arrays-of-tiles
   (reverse all-custom-tiles!)
   (sort
    (map cdr (hash->list all-unique-obj-tiles))
    <
    #:key (lambda (hsh) (hash-ref hsh 'id)))))

(define (convert-folder folder)
  (for ([dmm-fname (directory-list folder)]
        #:when (and (file-exists? (build-path folder dmm-fname))
                    (path-has-extension? dmm-fname #".dmm")))
    (define tjsonhsh (wrap (build-path folder dmm-fname)))
    (define filename (path-replace-extension dmm-fname ".json"))
    (define out-dir (path-add-extension folder "_REMAKE"))
    ; (println tjsonhsh)
    (make-directory* out-dir)
    (call-with-output-file
        (build-path out-dir filename)
      (lambda (out) (write-json tjsonhsh out)) #:exists 'truncate/replace)))

(define custom-maps-path (build-path curr-dir "custom_maps"))

; main start function
(convert-folder custom-maps-path)

(call-with-output-file (build-path curr-dir "all_map_stats.csv")
  (lambda (out)
    (display (get-output-string all-map-stas.csv) out)) #:exists 'replace)

; Record missing textures encountered during conversion.
(call-with-output-file (build-path curr-dir "MISSING_TEXTURES.sx")
  (lambda (out)
    (for ([i (remove-duplicates dbg-missing-textures)])
      (println i out))) #:exists 'append)

(call-with-output-file (build-path curr-dir "CUSTOM_TILE_DATA.sx")
  (lambda (out)
    (define p1 (filter-not empty? (remove-duplicates dbg-custom-data)))
    (define p2 (map (lambda (i)
                      (define vars (extract-instance-vars i))
                      (define res (car vars))
                      res) p1))
    (define remove-same-var-names (remove-duplicates p2 (lambda (a b) (string=? (car a) (car b)))))
    (for ([i remove-same-var-names])
      (println i out))) #:exists 'append)

;pretty prints the grid in ascii
(define (print-row vec width height)
  (define 2d (for/list ([y (range 0 height)])
               (for/list ([x (range 0 width)])
                 (define offset (2d->1d x y width))
                 (vector-ref vec offset))))
  (map (lambda (row)
         (println row)) 2d)
  empty)

