extensions [ nw rnd]

turtles-own [
  age
  prob-a
  prob-b
  prob-c
  lingual
  Sa
  Sb
  Sc
  Na
  Nb
  Nc
  la
  lb
  lc
  penalty-a
  penalty-b
  penalty-c
  select-state
  choose-state
  spoken-state
  contact?
]
;;modification
links-own
[
  rewired?
]

globals [
  number-rewired
  clustering-coefficient
]

;;;
;;; SETUP PROCEDURE
;;;

to setup
  clear-all
  set-default-shape turtles "circle"
  ask patches [set pcolor gray]
  if network-type = "Preferential Attachment"
  [ repeat N [ make-node ]
    create-network
    repeat 1000 [layout]
    distribute1]

  if network-type = "Small world"
  [ repeat N [make-node]
    layout-circle (sort turtles) max-pxcor - 1
    wire-them
    distribute2]
  assign-color
  ask turtles [
  lingual-state ]
  plot-degree
  reset-ticks
end

to make-node
  create-turtles 1 [
    rt random-float 360
    fd max-pxcor
    set size 2
    set prob-a 0
    set prob-b 0
    set prob-c 0
    set age 1
    set contact? false
  ]
end

to distribute1
  ask n-of ( 0.12 * N ) turtles
    [ set age 2 ]
  ask n-of ( 0.2 * N ) turtles
    [ set age 3 ]
  ask n-of ( 0.4 * N ) turtles
    [ set age 4 ]
  ask n-of ( 0.16 * N ) turtles
    [ set age 5 ]
  let adults turtles with [age != 1]
  let N-adults count adults
  let effect adults with [ count link-neighbors > 12 ]
  ask one-of effect
    [ set prob-a 1.0
      set contact? true]
  ask one-of effect with [not contact?]
    [ set prob-b 1.0
      set contact? true]
  ask one-of effect with [not contact?]
    [ set prob-c 1.0
      set contact? true]
  ask turtles[
    set spoken-state ""]

end

to distribute2
  ask n-of ( 0.12 * N ) turtles
    [ set age 2 ]
  ask n-of ( 0.2 * N ) turtles
    [ set age 3 ]
  ask n-of ( 0.4 * N ) turtles
    [ set age 4 ]
  ask n-of ( 0.16 * N ) turtles
    [ set age 5 ]
  let adults turtles with [age != 1]
  let N-adults count adults
  ask one-of adults
    [ set prob-a 1.0
      set contact? true]
  ask one-of adults with [not contact?]
    [ set prob-b 1.0
      set contact? true]
  ask one-of adults with [not contact?]
    [ set prob-c 1.0
      set contact? true]
  ask turtles[
    set spoken-state ""]

end

to cluster
  let n0 count turtles with [not contact?]
  if n0 = 0 [stop]
  spread
  if ticks > 0 and ticks mod 30 = 0 [
  fullfill ]
  if ticks > 0 and ticks mod 50 = 0 [
  fullfill ]
  ask turtles [lingual-state]
  tick
end

to spread
  let target turtles with [contact?]
  let N1 count turtles with [prob-a = 1]
  let N2 count turtles with [prob-b = 1]
  let N3 count turtles with [prob-c = 1]
  let target1 nobody
  let target2 nobody
  let target3 nobody

  ifelse N * (percent-grammar-1 / 100) - N1 < 0 [set target1 target with [prob-a = 0]] [set target1 target]
  ifelse N * (percent-grammar-2 / 100) - N2 < 0 [set target2 target1 with [prob-b = 0]] [set target2 target1]
  ifelse N * ((100 - percent-grammar-1 - percent-grammar-2) / 100) - N3 < 0 [set target3 target2 with [prob-c = 0]] [set target3 target2]

  ask target3 [
    let nearby one-of link-neighbors with [ not contact? ]
    if nearby != nobody
    [ask nearby[
      set prob-a [ prob-a ] of myself
      set prob-b [ prob-b ] of myself
      set prob-c [ prob-c ] of myself
      set contact? true ]]]

  assign-color

end


to fullfill
  let N0 0
  let N1 count turtles with [prob-a = 1]
  let N2 count turtles with [prob-b = 1]
  let N3 count turtles with [prob-c = 1]
  let zero nobody
  if N * (percent-grammar-1 / 100) - N1 > 0
  [ set N0 count turtles with [not contact?]
    let max-degree max [count link-neighbors] of turtles with [not contact?]
    ifelse max-degree < 3
    [set zero n-of (min list (N * (percent-grammar-1 / 100) - N1) N0) turtles with [not contact?]]
    [set zero one-of turtles with [not contact? and count link-neighbors > (max-degree - 2)] ]
    if zero != nobody[
      ask zero [
        set prob-a 1.0
        set contact? true ]]]

  if N * (percent-grammar-2 / 100) - N2 > 0
  [ set N0 count turtles with [not contact?]
    let max-degree max [count link-neighbors] of turtles with [not contact?]
    ifelse max-degree < 3
    [set zero n-of (min list (N * (percent-grammar-2 / 100) - N2) N0) turtles with [not contact?]]
    [set zero one-of turtles with [not contact? and count link-neighbors > (max-degree - 2)]]

    if zero != nobody[
      ask zero [
        set prob-b 1.0
        set contact? true ]]]

  if N * ((100 - percent-grammar-1 - percent-grammar-2) / 100) - N3 > 0
  [ set N0 count turtles with [not contact?]
    let max-degree max [count link-neighbors] of turtles with [not contact?]
    ifelse max-degree < 3
    [set zero n-of (min list (N * ((100 - percent-grammar-1 - percent-grammar-2) / 100) - N3) N0) turtles with [not contact?]]
    [set zero one-of turtles with [not contact? and count link-neighbors > (max-degree - 2)]]

    if zero != nobody[
      ask zero [
        set prob-c 1.0
        set contact? true ]]]

  assign-color
end


to assign-color
  ask turtles [
    ifelse prob-a = 1
    [ set color green]
    [ ifelse prob-b = 1
      [set color red]
      [ifelse prob-c = 1
      [set color blue]
        [set color black]]]]
end

to initialization
  let nearby sort nw:turtles-in-radius 2
  set Sa 0
  set Sb 0
  set Sc 0
  let i 0
  set Na count (nw:turtles-in-radius 2) with [ prob-a > 0 ]
  set Nb count (nw:turtles-in-radius 2) with [ prob-b > 0 ]
  set Nc count (nw:turtles-in-radius 2) with [ prob-c > 0 ]
  while [ i < count (nw:turtles-in-radius 2) ] [
    let turtle-x item i nearby
    let d-x nw:distance-to turtle-x
    if d-x != 0 [
      set Sa Sa + (([prob-a] of turtle-x) / (( d-x ) ^ 2))
      set Sb Sb + (([prob-b] of turtle-x) / (( d-x ) ^ 2))
      set Sc Sc + (([prob-c] of turtle-x) / (( d-x ) ^ 2))]
      set i (i + 1) ]

  if Sa + Sb + Sb != 0 [
    set penalty-a 1 - ( Sa / (Sa + Sb + Sb))
    set penalty-b 1 - ( Sb / (Sa + Sb + Sb))
    set penalty-c 1 - ( Sc / (Sa + Sb + Sb))]

  if age = 1 [
    set prob-a 1 - penalty-a
    set prob-b 1 - penalty-b
    set prob-c 1 - penalty-c ]

  update-color
end


to create-network
  let partner nobody
  let first-node one-of turtles
  let second-node one-of turtles with [self != first-node]
  ask first-node [ create-link-with second-node [ set color white ] ]
  let new-node one-of turtles with [not any? link-neighbors]
  while [new-node != nobody] [
    set partner find-partner
    ask new-node [ create-link-with partner [ set color white ] ]
    layout
    set new-node one-of turtles with [not any? link-neighbors]
  ]
end


to rewire-all
    set number-rewired 0
    ask links [
      ;; whether to rewire it or not?
      if (random-float 1) < rewiring-probability
      [ let node1 end1
        if [ count link-neighbors ] of end1 < (count turtles - 1)
        [ let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
        ask node1 [ create-link-with node2 [ set rewired? true ]]
          set number-rewired number-rewired + 1  ;; counter for number of rewirings
          set rewired? true
        ]
      ]
      if (rewired?)
      [ die ]
    ]
   update-plots
end

to wire-them
  ;; iterate over the turtles
  let a 0
  while [a < count turtles]
  [
    ;; make edges with the next two neighbors
    ;; this makes a lattice with average degree of 4
    make-edge turtle a
              turtle ((a + 1) mod count turtles)
    make-edge turtle a
              turtle ((a + 2) mod count turtles)
    set a a + 1
  ]
end

;; connects the two turtles
to make-edge [node1 node2]
  ask node1 [ create-link-with node2  [
    set rewired? false
  ] ]
end


to update-color
  set color scale-color red lingual 0 3
end

to lingual-state
    let prob (list prob-a prob-b prob-c)
    let i 0
    set lingual 0
    let increase 0
    loop [
      if i > 2 [stop]
      let x item i prob
      ifelse x != 0 [
        set increase 1 ]
      [ set increase 0 ]
      set lingual lingual + increase
      set i i + 1]
end



;;;
;;; GO PROCEDURES
;;;

to go
  ask turtles [
  communicate-via update-algorithm
  lingual-state]
  ask turtles [update-color]
  if ticks > 0 and ticks mod 100 = 0 [
  ask turtles [
  set age age + 1
  initialization
  while [ age > 5 ]
  [ set age 1 ]]]
  plot-degree
  tick
end

to communicate-via [ algorithm ]
  if (algorithm = "reward")
  [ speak
    ask link-neighbors
    [ listen [spoken-state] of myself ]
]
end


to speak
  let filter-prob 0
  let state [ "a" "b" "c" ]
  let penalty (list (1 - penalty-a) (1 - penalty-b) (1 - penalty-c) )
  let prob (list prob-a prob-b prob-c )
  let pairs (map list state penalty)
  set select-state first rnd:weighted-one-of-list pairs [ [p] -> last p ]
  let filter-val item (position select-state state) prob
  ifelse filter-val = 1
  [ set filter-prob 1 ]
  [ set filter-prob  1 / (1 + exp(-(10 * filter-val - 5)))]
  ifelse random-float 1.0 <= filter-prob [
    set spoken-state select-state ]
  [ set spoken-state one-of state ]
end

;; Listening uses a linear reward/punish algorithm
to listen [heard-state]

  let gamma 0.002 * (1 - (1 / (1 + exp (-0.1 * ( age - 1 )))))
  let ind-3 (list preference 1 1 )
  let ind-4 (list (1 / preference) 1 1 )
  let rate-1 map [ i -> i * gamma ] ind-3
  let rate-2 map [ i -> i * gamma ] ind-4
  let state [ "a" "b" "c" ]
  let prob (list prob-a prob-b prob-c )
  let ind-1 [ 0 0 0 ]
  let ind-2 [ 0.5 0.5 0.5 ]

  let pairs (map list state prob)
  set choose-state first rnd:weighted-one-of-list pairs [ [p] -> last p ]
  ifelse heard-state = choose-state [
    let choose-rate item (position choose-state state) rate-1
    let ind_1 replace-item (position choose-state state) ind-1 1
    set prob (map [ [ P ind ] -> (1 - choose-rate) * P + (ind * choose-rate) ] prob ind_1) ]
  [ let choose-rate item (position choose-state state) rate-2
    let ind_2 replace-item (position choose-state state) ind-2 0
    set prob (map [ [ P ind ] -> (1 - choose-rate) * P + (ind * choose-rate) ] prob ind_2) ]
  set prob-a item 0 prob
  set prob-b item 1 prob
  set prob-c item 2 prob
end


;;
;; Making the network
;;
;; This code is borrowed from Lottery Example, from the Code Examples section of the Models Library.
;; The idea behind this procedure is as the following.
;; The sum of the sizes of the turtles is set as the number of "tickets" we have in our lottery.
;; Then we pick a random "ticket" (a random number), and we step through the
;; turtles to find which turtle holds that ticket.
to-report find-partner
  let pick random-float sum [count link-neighbors] of (turtles with [any? link-neighbors])
  let partner nobody
  ask turtles
  [ ;; if there's no winner yet
    if partner = nobody
    [ ifelse count link-neighbors > pick
      [ set partner self]
      [ set pick pick - (count link-neighbors)]
    ]
  ]
  report partner
end


to plot-degree
  let max-degree max [count link-neighbors] of turtles
  set-current-plot "Degree Distribution"
  plot-pen-reset
  set-plot-x-range 0 (max-degree + 1)
  histogram [count link-neighbors] of turtles
end


to layout
  layout-spring (turtles with [any? link-neighbors]) links 0.4 6 1
end





; Copyright 2007 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
405
10
838
444
-1
-1
4.67033
1
10
1
1
1
0
0
0
1
-45
45
-45
45
1
1
1
ticks
30.0

PLOT
880
290
1150
440
Mean state of language users in the network
Time
State
0.0
100.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [penalty-a] of turtles"
"pen-1" 1.0 0 -7500403 true "" "plot mean [penalty-b] of turtles"
"pen-2" 1.0 0 -2674135 true "" "plot mean [penalty-c] of turtles"

BUTTON
10
10
105
43
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
226
10
366
44
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
110
10
205
43
layout
layout\ndisplay
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
55
205
88
N
N
2
1000
500.0
1
1
NIL
HORIZONTAL

SLIDER
10
205
190
238
percent-grammar-2
percent-grammar-2
0
99 - percent-grammar-1
20.0
1
1
%
HORIZONTAL

CHOOSER
225
150
365
195
update-algorithm
update-algorithm
"individual" "threshold" "reward"
2

SLIDER
225
260
365
293
alpha
alpha
0
0.05
0.05
0.0050
1
NIL
HORIZONTAL

SLIDER
225
205
365
238
threshold-val
threshold-val
0
1
1.0
0.05
1
NIL
HORIZONTAL

BUTTON
225
60
365
94
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
10
100
202
145
network-type
network-type
"Preferential Attachment" "Small world"
0

SLIDER
10
160
190
193
percent-grammar-1
percent-grammar-1
0
100
20.0
1
1
%
HORIZONTAL

PLOT
875
10
1130
195
Degree Distribution
degree
# of nodes
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

MONITOR
880
220
1022
265
select-state
count turtles with [select-state = \"a\"]
6
1
11

SLIDER
10
370
187
403
rewiring-probability
rewiring-probability
0
1
1.0
0.01
1
NIL
HORIZONTAL

BUTTON
15
315
187
348
        rewire-all                
rewire-all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1040
220
1217
265
NIL
count turtles with [age = 1]
17
1
11

MONITOR
880
485
1077
530
NIL
count turtles with [lingual = 1]
17
1
11

MONITOR
220
315
382
360
NIL
mean [prob-a] of turtles
10
1
11

MONITOR
220
385
382
430
NIL
mean [prob-b] of turtles
10
1
11

MONITOR
220
450
382
495
NIL
mean [prob-c] of turtles
10
1
11

BUTTON
15
265
187
298
            cluster                 
cluster
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
20
440
192
473
preference
preference
0.5
2
1.5
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model explores how the properties of language users and the structure of their social networks can affect the course of language change.

In this model, there are two linguistic variants in competition within the social network -- one variant generated by grammar 0 and the other generated by grammar 1. Language users interact with each other based on whom they are connected to in the network. At each iteration, each individual speaks by passing an utterance using either grammar 0 or grammar 1 to the neighbors in the network. Individuals then listen to their neighbors and change their grammars based on what they heard.

## HOW IT WORKS

The networks in this model are constructed through the process of "preferential attachment" in which individuals enter the network one by one, and prefer to connect to those language users who already have many connections. This leads to the emergence of a few "hubs", or language users who are very well connected; most other language users have very few connections.

There are three different options to control how language users listen and learn from their neighbors, listed in the UPDATE-ALGORITHM chooser. For two of these options, INDIVIDUAL and THRESHOLD, language users can only access one grammar at a time. Those that can only access grammar 1 are white in color, and those that can access only grammar 0 are black. For the third option, REWARD, each grammar is associated with a weight, which determines the language user's probability of accessing that grammar. Because there are only two grammars in competition here, the weights are represented with a single value - the weight of grammar 1. The color of the nodes reflect this probability; the larger the weight of grammar 1, the lighter the node.

- INDIVIDUAL: Language users choose one of their neighbors randomly, and adopt that neighbor's grammar.

- THRESHOLD: Language users adopt grammar 1 if some proportion of their neighbors is already using grammar 1. This proportion is set with the THRESHOLD-VAL slider. For example, if THRESHOLD-VAL is 0.30, then an individual will adopt grammar 1 if at least 30% of his neighbors have grammar 1.

- REWARD: Language users update their probability of using one grammar or the other. In this algorithm, if an individual hears an utterance from grammar 1, the individual's weight of grammar 1 is increased, and they will be more likely to use that grammar in the next iteration. Similarly, hearing an utterance from grammar 0 increases the likelihood of using grammar 0 in the next iteration.

## HOW TO USE IT

The NUM-NODES slider determines the number of nodes (or individuals) to be included in the network population. PERCENT-GRAMMAR-1 determines the proportion of these language learners who will be initialized to use grammar 1. The remaining nodes will be initialized to use grammar 0.

Press SETUP-EVERYTHING to generate a new network based on NUM-NODES and PERCENT-GRAMMAR-1.

Press GO ONCE to allow all language users to "speak" and "listen" only once, according to the algorithm in the UPDATE-ALGORITHM dropdown menu (see the above section for more about these options). Press GO for the simulation to run continuously; pressing GO again will halt the simulation.

Press LAYOUT to move the nodes around so that the structure of the network easier to see.

When the HIGHLIGHT button is pressed, rolling over a node in the network will highlight the nodes to which it is connected. Additionally, the node's initial and current grammar state will be displayed in the output area.

Press REDISTRIBUTE-GRAMMARS to reassign grammars to all language users, under the same initial condition. For example, if 20% of the nodes were initialized with grammar 1, pressing REDISTRIBUTE-GRAMMARS will assign grammar 1 to a new sample of 20% of the population.

Press RESET-STATES to reinitialize all language users to their original grammars. This allows you to run the model multiple times without generating a new network structure.

The SINK-STATE-1? switch applies only for the INDIVIDUAL and THRESHOLD updating algorithms. If on, once an individual adopts grammar 1, then he can never go back to grammar 0.

The LOGISTIC? switch applies only for the REWARD updating algorithm. If on, an individual's probability of using one of the grammars is pushed to the extremes (closer to 0% or 100%), based on the output of the logistic function. For more details, see https://en.wikipedia.org/wiki/Logistic_function.

The ALPHA slider also applies only for the REWARD updating algorithm, and only when LOGISTIC? is turned on. ALPHA represents a bias in favor of grammar 1. Probabilities are pushed to the extremes, and shifted toward selecting grammar 1. The larger the value of ALPHA, the more likely a language user will speak using grammar 1.

The plot "Mean state of language users in the network" calculates the average weight of grammar 1 for all nodes in the network, at each iteration.

## THINGS TO NOTICE

Over time, language users tend to arrive at using just one grammar all of the time. However, they may not all converge to the same grammar. It is possible for sub-groups to emerge, which may be seen as the formation of different dialects.

## THINGS TO TRY

Under what conditions is it possible to get one grammar to spread through the entire network? Try manipulating PERCENT-GRAMMAR-1, the updating algorithm, and the various other parameters. Does the number of nodes matter?

## EXTENDING THE MODEL

Whether or not two language users interact with each other is determined by the network structure. How would the model behave if language users were connected by a small-world network rather than a preferential attachment network?

In this model, only two grammars are in competition in the network. Try extending the model to allow competition between three grammars.

The updating algorithm currently has agents updating asynchronously. Currently, the grammar may spread one step or several within one tick, depending on the links. Try implementing synchronous updating.

Regardless of the updating algorithm, language users always start out using one grammar categorically (that is, with a weight of 0 or 1). Edit the model to allow some language users to be initialized to an intermediate weight (e.g., 0.5).

## NETLOGO FEATURES

Networks are represented using turtles (nodes) and links.  In NetLogo, both turtles and links are agents.

## RELATED MODELS

Preferential Attachment

## CREDITS AND REFERENCES

This model was also described in Troutman, Celina; Clark, Brady; and Goldrick, Matthew (2008) "Social networks and intraspeaker variation during periods of language change," University of Pennsylvania Working Papers in Linguistics: Vol. 14: Issue 1, Article 25.
https://repository.upenn.edu/pwpl/vol14/iss1/25/

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Troutman, C. and Wilensky, U. (2007).  NetLogo Language Change model.  http://ccl.northwestern.edu/netlogo/models/LanguageChange.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2007 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2007 Cite: Troutman, C. -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment 1" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>rewire-all
go</go>
    <timeLimit steps="300"/>
    <metric>ratio-2</metric>
    <enumeratedValueSet variable="N">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;threshold&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;Small world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="percent-grammar-2" first="0" step="10" last="80"/>
    <enumeratedValueSet variable="threshold-val">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="N">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;threshold&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-2">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="N">
      <value value="150"/>
    </enumeratedValueSet>
    <steppedValueSet variable="rewiring-probability" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="alpha">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;individual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;Small world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-2">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="trilingual_1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>cluster</go>
    <final>go</final>
    <metric>mean [prob-a] of turtles</metric>
    <metric>mean [prob-b] of turtles</metric>
    <metric>mean [prob-c] of turtles</metric>
    <enumeratedValueSet variable="N">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;reward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="percent-grammar-1" first="5" step="5" last="75"/>
    <enumeratedValueSet variable="percent-grammar-2">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="preference" first="0.5" step="0.1" last="2"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
