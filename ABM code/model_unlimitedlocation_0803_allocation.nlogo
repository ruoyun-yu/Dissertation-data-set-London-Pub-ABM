extensions [gis]

breed [consumers consumer]
breed [pubs pub]

consumers-own [
  OAcode
  money
  reachable-pubs
  utility-list
  allocation-list
]

pubs-own [
  pubid
  name
  rating
  numrat
  daily_income
  total_income
  exit_probability
  pubrent
  daily_profit
  age
  pub_borough
  pub_OA
]

globals [
  boundary
  OA
  pubpoint
  rentprice
  num-consumers
  num-pubs
  total-exits
  initial-total-pubs
  initial-red-pubs
  tick-exit
  tick-exit-red
  exit-rate
  exit-rate-red
  remaining-total-pubs
  remaining-red-pubs
  possible_hundredexit_profit
  possible_oneexit_profit
  next-pubid

]

patches-own [
  borough
  inlondon
  insector?
  patchrent
  OAID
  spend
  initial-pubs       ;; 记录该 patch 初始 pub 数量
  dead-pubs          ;; 记录该 patch 死亡 pub 数量
  patch_borough
]

to setup
  clear-all
  set_gis
  set_boundary
  set_rentprice
  set_OA
  set_index
  create_consumers
  create_pubs
  ; 初始化导出文件
  set next-pubid (count pubs + 1)
  let filename (word
    "pub_evolution_run" behaviorspace-run-number
    "_a" constant_a
    "_b" constant_b
    "_c" constant_c
    "_reach" reach_distance
    ".csv")

  file-open filename
  file-print "tick,pubid,age,xcor,ycor,borough,OAID,spend,nearby_pubs,rating,numrat,income_per_tick,pubrent"
  file-close

  reset-ticks
end

to set_gis
  gis:load-coordinate-system "EPSG_27700_base.prj" 
  set boundary gis:load-dataset "londonboundary.shp"
  set OA gis:load-dataset "OAspend.shp"
  set pubpoint gis:load-dataset "pubpoint.shp" 
  set rentprice gis:load-dataset "housepricegrid.shp" 
  let env1 gis:envelope-of boundary 
  let env2 gis:envelope-of OA  
  let xmin min (list item 0 env1 item 0 env2) 
  let xmax max (list item 1 env1 item 1 env2) 
  let ymin min (list item 2 env1 item 2 env2) 
  let ymax max (list item 3 env1 item 3 env2)  
  gis:set-world-envelope (list xmin xmax ymin ymax)
end

to set_boundary
  gis:apply-coverage boundary "BOUNDARY" inlondon
  ask patches [
  ifelse inlondon = 1 [
    set insector? true
  ] [
    set insector? false
  ]
]
end

to set_rentprice
  gis:apply-coverage rentprice "RENT" patchrent
end

to set_OA
  gis:apply-coverage OA "OA21CD" OAID
  gis:apply-coverage OA "OASPEND21" spend
  gis:apply-coverage OA "BRGH_CL" patch_borough
end

to set_index
  set total-exits 0
  set possible_hundredexit_profit (-1 * ln 101) / (constant_b * ln constant_a)
  set possible_oneexit_profit (-1 * ln 1) / (constant_b * ln constant_a)
end

to create_consumers
  foreach gis:feature-list-of OA [ feature -> 
    let myName gis:property-value feature "OA21CD" 
    let myCentroid gis:location-of gis:centroid-of feature 
    let myMoney gis:property-value feature "OASPEND21" 
 
    create-consumers 1
    [ 
      set xcor item 0 myCentroid 
      set ycor item 1 myCentroid 
      set size 1 
      set color blue 
      set shape "triangle" 
      set OAcode myName 
      set money myMoney 
    ] 
  ]
end

to create_pubs
  foreach gis:feature-list-of pubpoint [ feature -> 
    let myid gis:property-value feature "place_id" 
    let myname gis:property-value feature "name" 
    let myCentroid gis:location-of gis:centroid-of feature 
    let myrating gis:property-value feature "rating"
    let numrating gis:property-value feature "USER_RATIN"  

    if length myCentroid = 2 and is-number? myrating [ 
      create-pubs 1 [ 
        set xcor item 0 myCentroid 
        set ycor item 1 myCentroid 
        set size 1 
        set color red 
        set shape "circle" 
        set pubid myid 
        set name myname 
        set rating myrating
        set numrat numrating
        set pubrent [patchrent] of patch-here
        set age 0
        set pub_borough [patch_borough] of patch-here
        set pub_OA [OAID] of patch-here
    ] 
    ]
    ]
  ask patches [
    set initial-pubs count pubs-here
    set dead-pubs 0
  ]
end



to go
  record_initial_pub
  reset_pubstate
  consumers_consumption
  pubs_competition
  report_pubs_income
  export_pub_state
  tick
end

to record_initial_pub
  set initial-total-pubs count pubs
  set initial-red-pubs count pubs with [color = red]
end


to reset_pubstate
  ask pubs [
    set daily_income 0
    set exit_probability 0
    set age age + 1
  ]
end

to consumers_consumption
  identify_reachablepubs
  calculate_utilitypubs
  calculate_allocationexpending
  expend
end

to identify_reachablepubs
  ask consumers [
    set reachable-pubs []
    let nearby pubs in-radius reach_distance
    set reachable-pubs sort nearby
  ]
end

to calculate_utilitypubs
  ask consumers [
    set utility-list []

    let sum-distance 0
    let sum-rating 0


    ;;calculate the sum of distance and rating
    foreach reachable-pubs [p ->
      let dij distance p
      let rij [rating] of p
      set sum-distance sum-distance + dij
      set sum-rating sum-rating + rij
    ]


    ;;normalize distance and rating
    if (sum-distance > 0) and (sum-rating > 0) [
      foreach reachable-pubs [p ->
        let dij distance p
        let rij [rating] of p

        let norm-d dij / sum-distance
        let norm-rating rij / sum-rating

        let uij (1 / norm-d) * (1 / norm-d) * norm-rating

        set utility-list lput (list p uij) utility-list
      ]
    ]
  ]
end


to calculate_allocationexpending
  ask consumers [
    set allocation-list []

    let total-attract 0

    ;;calculate the sum of attractiveness
    foreach utility-list [ p ->
      set total-attract total-attract + item 1 p
    ]

    if total-attract > 0 [
      foreach utility-list [ p ->
        let j item 0 p
        let uij item 1 p
        let pij uij / total-attract
        set allocation-list lput (list j pij) allocation-list
      ]
    ]
  ]
end


to expend
  ask consumers [
    foreach allocation-list [ p ->
      let target-pub item 0 p
      let pij item 1 p
      if is-agent? target-pub and is-number? pij and is-number? money [
      let sij pij * money
      ask target-pub [
      set daily_income daily_income + sij
      set total_income total_income + sij
        ]
      ]
    ]
  ]
end

to report_pubs_income
  ask pubs [
    show (word "Pub " who ": daily = " daily_income ", total = " total_income)
  ]
end

to pubs_competition
  calculate_profit
  calculate_probabilityexit_index
  execute_exit
end

to calculate_profit
  ask pubs [
    set daily_profit (daily_income - pubrent * constant_c)
  ]
end

to calculate_probabilityexit_index
  ask pubs [
    let k daily_profit
    if k < possible_hundredexit_profit [set k possible_hundredexit_profit]
    if k > possible_oneexit_profit [set k possible_oneexit_profit]
    let exit_probability_tempor constant_a ^ (- constant_b * k) - 1
    if exit_probability_tempor <= 0 [set exit_probability_tempor 0]
    set exit_probability exit_probability_tempor
  ]
end

to execute_exit
  let exit-count 0
  let red-exit-count 0
  ask pubs [
    if random-float 100 < exit_probability [
      if color = red [ set red-exit-count red-exit-count + 1 ]
      set exit-count exit-count + 1
      ask patch-here [ set dead-pubs dead-pubs + 1 ]
      die
    ]
  ]

  set remaining-total-pubs count pubs
  set remaining-red-pubs count pubs with [color = red]

  repeat exit-count [
    create-pubs 1 [
      initialize-new-pub
    ]
  ]

  set tick-exit exit-count
  set tick-exit-red red-exit-count
  set exit-rate tick-exit / initial-total-pubs * 1000
  set exit-rate-red tick-exit-red / initial-red-pubs * 1000
  set total-exits total-exits + exit-count
end

to initialize-new-pub
  let valid-patch one-of patches with [insector? = true]
  if valid-patch != nobody [
    move-to valid-patch
  setxy ([pxcor] of valid-patch) ([pycor] of valid-patch)
  set rating random-float 2.5 + 2.5
  set total_income 0
  set shape "triangle"
  set color yellow
  set pubrent [patchrent] of patch-here
  set age 0
  set pubid (word "new_" next-pubid)  ;; 给新酒吧分配唯一 ID
  set next-pubid next-pubid + 1       ;; 更新计数器
  set pub_borough [patch_borough] of patch-here
  set pub_OA [OAID] of patch-here
  ]
end


to researcharea
  ask patches [
    set pcolor ifelse-value insector? [yellow] [gray]
  ]
  wait 1
  ask patches [ set pcolor black ]
end

to rent
  ask patches [
    ifelse is-number? patchrent and patchrent >= 3500 and patchrent <= 15000 [
      let hererent patchrent
      let rent-class floor ((hererent - 3500) / 500)
      set rent-class max list 0 (min list rent-class 23)
      set pcolor scale-color pink rent-class 0 23
    ]
    [
      set pcolor gray
    ]
  ]
  wait 1
  ask patches [ set pcolor black ]
end


to export_pub_summary
  let filename (word
    "pub_summary_run" behaviorspace-run-number
    "_a" constant_a
    "_b" constant_b
    "_c" constant_c
    "_reach" reach_distance
    ".csv")

  file-open filename
  file-print "pubid,age,xcor,ycor,borough,OAID,spend,nearby_pubs,rating,numrat,income_per_tick,pubrent"

  ask pubs [
    let nearby count pubs in-radius reach_distance
    let boro-name ifelse-value is-string? [borough] of patch-here [ [borough] of patch-here ] [ "NA" ]
    let oa-id ifelse-value is-string? [OAID] of patch-here [ [OAID] of patch-here ] [ "NA" ]
    let patch-spend ifelse-value is-number? [spend] of patch-here [ [spend] of patch-here ] [ -1 ]

    let income-per-tick 0
    if age > 0 [
      set income-per-tick total_income / age
    ]

    file-print (word pubid "," age "," precision xcor 4 "," precision ycor 4 ","
                boro-name "," oa-id "," patch-spend "," nearby ","
                rating "," numrat "," precision income-per-tick 4 "," pubrent)
  ]

  file-close
end

to random-select-and-display-pub
  ; 随机选择一个酒吧
  let selected-pub one-of pubs

  ; 如果选到了酒吧（确保酒吧列表不为空）
  if selected-pub != nobody [
    ; 显示酒吧的所有属性
    show (word "Pub ID: " [pubid] of selected-pub)
    show (word "Name: " [name] of selected-pub)
    show (word "Rating: " [rating] of selected-pub)
    show (word "Number of Ratings: " [numrat] of selected-pub)
    show (word "Daily Income: " [daily_income] of selected-pub)
    show (word "Total Income: " [total_income] of selected-pub)
    show (word "Exit Probability: " [exit_probability] of selected-pub)
    show (word "Pub Rent: " [pubrent] of selected-pub)
    show (word "Daily Profit: " [daily_profit] of selected-pub)
    show (word "Age: " [age] of selected-pub)
  ]
end

to calculate_average_daily_income
  let total_in sum [daily_income] of pubs  ;; 计算所有 pub 的 daily_income 总和
  let num_pubs count pubs  ;; 计算 pub 的数量
  let average_income total_in / num_pubs  ;; 计算平均值
  print (word "Average daily income of all pubs: " average_income)  ;; 输出结果
end

to calculate_average_pubrent
  let total_rent sum [pubrent] of pubs  ;; 计算所有 pub 的 pubrent 总和
  let num_pubs count pubs  ;; 计算 pub 的数量
  let average_rent total_rent / num_pubs  ;; 计算平均值
  print (word "Average pubrent of all pubs: " average_rent)  ;; 输出结果
end

to export_pub_state
  let filename (word
    "pub_evolution_run" behaviorspace-run-number
    "_a" constant_a
    "_b" constant_b
    "_c" constant_c
    "_reach" reach_distance
    ".csv")

  file-open filename

  ask pubs [
    let nearby count pubs in-radius reach_distance
    let income-per-tick ifelse-value (age > 0) [ total_income / age ] [ 0 ]

    file-print (word ticks "," pubid "," age "," precision xcor 4 "," precision ycor 4 ","
                pub_borough "," pub_OA "," spend "," nearby ","
                rating "," numrat "," precision income-per-tick 4 "," pubrent)
  ]

  file-close
end



to export_patch_summary
  let filename (word
    "patch_summary_run" behaviorspace-run-number
    "_a" constant_a
    "_b" constant_b
    "_c" constant_c
    "_reach" reach_distance
    ".csv")

  file-open filename
  file-print "pxcor,pycor,patchrent,OAID,patch_borough,spend,reachable_pubs,reachable_consumers,reachable_spending,initial_pubs,dead_pubs,current_pubs"

  ask patches with [insector? = true] [
    ;; 计算可达范围内的 pubs 和 consumers
    let patch-reachable-pubs count pubs in-radius reach_distance
    let reachable-consumers consumers in-radius reach_distance
    let reachable-consumers-count count reachable-consumers
    let reachable-spending sum [money] of reachable-consumers
    let current-pubs count pubs-here

    file-print (word pxcor "," pycor "," patchrent "," OAID "," patch_borough "," spend ","
                patch-reachable-pubs "," reachable-consumers-count "," reachable-spending ","
                initial-pubs "," dead-pubs "," current-pubs)
  ]

  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
245
41
781
578
-1
-1
2.63
1
10
1
1
1
0
1
1
1
-100
100
-100
100
0
0
1
ticks
30.0

BUTTON
49
48
115
81
NIL
setup\n
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
50
113
113
146
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
52
194
130
239
NIL
total-exits
17
1
11

BUTTON
49
150
130
183
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
1

MONITOR
54
257
111
302
NIL
ticks
17
1
11

PLOT
799
10
999
160
total-exits
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-exits"

BUTTON
52
321
164
354
NIL
researcharea
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
800
167
1000
317
tick-exit
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot tick-exit"

SLIDER
6
407
178
440
constant_a
constant_a
1
5
1.005
0.001
1
NIL
HORIZONTAL

SLIDER
6
442
178
475
constant_b
constant_b
0
30
0.04
0.01
1
NIL
HORIZONTAL

SLIDER
6
478
178
511
constant_c
constant_c
0
300
80.0
10
1
NIL
HORIZONTAL

BUTTON
51
359
143
392
NIL
rent
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1036
170
1236
320
The number of original pubs
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [breed = pubs and color = red]"

SLIDER
6
520
177
553
reach_distance
reach_distance
0
20
10.0
1
1
NIL
HORIZONTAL

PLOT
800
323
1000
473
 exit-rate
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot exit-rate"

PLOT
802
483
1002
633
exit-rate-red
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot exit-rate-red"

BUTTON
1049
53
1228
86
NIL
export_patch_summary
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 25</exitCondition>
    <metric>total-exits</metric>
    <metric>initial-total-pubs</metric>
    <metric>initial-red-pubs</metric>
    <metric>tick-exit</metric>
    <metric>tick-exit-red</metric>
    <metric>exit-rate</metric>
    <metric>exit-rate-red</metric>
    <metric>remaining-total-pubs</metric>
    <metric>remaining-red-pubs</metric>
    <enumeratedValueSet variable="constant_a">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reach_distance">
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_b">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_c">
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment (copy)" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 25</exitCondition>
    <metric>total-exits</metric>
    <metric>initial-total-pubs</metric>
    <metric>initial-red-pubs</metric>
    <metric>tick-exit</metric>
    <metric>tick-exit-red</metric>
    <metric>exit-rate</metric>
    <metric>exit-rate-red</metric>
    <metric>remaining-total-pubs</metric>
    <metric>remaining-red-pubs</metric>
    <enumeratedValueSet variable="constant_a">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reach_distance">
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_b">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_c">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="try07192225" repetitions="8" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 30</exitCondition>
    <metric>total-exits</metric>
    <metric>initial-total-pubs</metric>
    <metric>initial-red-pubs</metric>
    <metric>tick-exit</metric>
    <metric>tick-exit-red</metric>
    <metric>exit-rate</metric>
    <metric>exit-rate-red</metric>
    <metric>remaining-total-pubs</metric>
    <metric>remaining-red-pubs</metric>
    <enumeratedValueSet variable="constant_a">
      <value value="1.001"/>
      <value value="1.003"/>
      <value value="1.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reach_distance">
      <value value="5"/>
      <value value="8"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_b">
      <value value="0.2"/>
      <value value="0.04"/>
      <value value="0.5"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_c">
      <value value="40"/>
      <value value="60"/>
      <value value="80"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="try" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 25</exitCondition>
    <metric>total-exits</metric>
    <metric>initial-total-pubs</metric>
    <metric>initial-red-pubs</metric>
    <metric>tick-exit</metric>
    <metric>tick-exit-red</metric>
    <metric>exit-rate</metric>
    <metric>exit-rate-red</metric>
    <metric>remaining-total-pubs</metric>
    <metric>remaining-red-pubs</metric>
    <enumeratedValueSet variable="constant_a">
      <value value="1.01"/>
      <value value="1.001"/>
      <value value="1.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reach_distance">
      <value value="10"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_b">
      <value value="0.2"/>
      <value value="0.04"/>
      <value value="0.5"/>
      <value value="0.1"/>
      <value value="0.4"/>
      <value value="0.3"/>
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_c">
      <value value="10"/>
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="34" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export_patch_summary</postRun>
    <exitCondition>ticks = 40</exitCondition>
    <metric>total-exits</metric>
    <metric>initial-total-pubs</metric>
    <metric>initial-red-pubs</metric>
    <metric>tick-exit</metric>
    <metric>tick-exit-red</metric>
    <metric>exit-rate</metric>
    <metric>exit-rate-red</metric>
    <metric>remaining-total-pubs</metric>
    <metric>remaining-red-pubs</metric>
    <enumeratedValueSet variable="constant_a">
      <value value="1.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reach_distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_b">
      <value value="0.2"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant_c">
      <value value="100"/>
    </enumeratedValueSet>
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
0
@#$#@#$#@
