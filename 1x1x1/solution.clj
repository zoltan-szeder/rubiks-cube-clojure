(ns Solution
  (:gen-class))

(defn output [msg] (println msg) (flush))
(defn debug [msg] (binding [*out* *err*] (println msg) (flush)))

(defprotocol Cube
    (rotate [this direction])
    (face [this face]))

(defrecord ColoredCube [F B U D L R]
  Cube
    (rotate [this rotation]
      (case rotation
        "x"  (ColoredCube. D U F B L R)
        "x'" (ColoredCube. U D B F L R)
        "y"  (ColoredCube. R L U D F B)
        "y'" (ColoredCube. L R U D B F)
        "z"  (ColoredCube. F B L R D U)
        "z'" (ColoredCube. F B R L U D)))

    (face [this face]
      (cond
        (= face F) "F"
        (= face B) "B"
        (= face U) "U"
        (= face D) "D"
        (= face L) "L"
        (= face R) "R")))

(defn do-rotations [rotations cube]
  (debug rotations)
  (debug cube)
  (if (empty? rotations)
    cube
    (recur (rest rotations) (.rotate cube (first rotations)))))

(defn -main [& args]
  (let [rotations (read-line)
        face1 (read-line)
        face2 (read-line)
        cube (ColoredCube. "F" "B" "U" "D" "L" "R")]
    
    (def rotated-cube (do-rotations (clojure.string/split rotations #" ") cube))
    ; (debug "Debug messages...")
    
    ; Write answer to stdout
    (output (.face rotated-cube face1))
    (output (.face rotated-cube face2))))
