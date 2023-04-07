## 1. Построение в виде таблицы истинности 
### 1. (псевдо)случайной булевой функции от заданного числа переменных 
```
  GenerateTable[n_] := Module[{table}, 
  table = Table[RandomInteger[], 2^n]]
```
#### Пример использования: <br />
<img width="227" alt="image" src="https://user-images.githubusercontent.com/80067024/230634061-ab0677f5-3ed1-4a87-b90d-ee8420db3062.png">

### 2.  (псевдо)случайной булевой функции заданного веса от заданного числа переменных
```
GenerateTableWeight[n_, weight_] := Module[{table},
  While[True, table = Table[RandomInteger[1], 2^n];
   If[Not[Total[table] != weight], Break[]]];
  Return[table]
  ]
```
#### Пример использования: <br />
<img width="261" alt="image" src="https://user-images.githubusercontent.com/80067024/230645188-56d9fbbc-10cb-4bca-9b4d-7543e468a26a.png">  <br />
### 3. (псевдо)случайной линейной булевой функции от заданного числа переменных  <br />
Линейная функция задана вектором из n коэфициентов  <br />

```
LinearBf[n_] := Module[{function = 0, table, coef, variables},
  coef = Table[RandomInteger[1], n];
  Print[coef];
  variables = Array[x, n];
  For[i = 1, i <= n, i++,
   If[coef[[i]] == 1, function = Xor[function, x[i]]]];
  If[Total[coef] != 0, final = Drop[function, 1], final = function];
  Print[final];
  table = BooleanTable[variables -> final, variables] // TableForm
  ]
  
```
#### Пример использования:
<img width="309" alt="image" src="https://user-images.githubusercontent.com/80067024/230645497-d5960471-db98-423a-92e0-ead26722ed25.png">

## 2. Разработка способов и реализация средствами САВ "Mathematica"  преобразований представлений булевых функций 
### 1. из многочлена Жегалкина в АНФ
АНФ - коэфициенты конъюнкций многочлена Жегалкина
```
FromZhToAnf[function_] := 
 Module[{variables = BooleanVariables[function], anf, size},
  If[Length[variables[[-1]][[-1]]] == 0,
   size = variables[[-1]][[-1]],
   size = variables[[-1]][[-1]][[-1]]
   ];
  Print[function];
  anf = Table[0, 2^size];
  For[i = 1, i <= Length[function], i ++,
   position = 1;
   mon = function[[i]];
   If [Length[mon] == 0, 
    anf = ReplacePart[anf, 1, position],
    If[Length[mon] == 1, 
     position = position + 2 ^(size - mon[[1]]);
     anf = ReplacePart[anf, 1, position],
     For[j = 1, j <= Length[mon], j ++,
      position = position + 2 ^ ( size - mon[[j]][[1]])
      ];
     anf = ReplacePart[anf, 1, position]
     ]
    ]
   ];
  Return[anf]
  ]
```
#### Пример использвоания:
<img width="806" alt="image" src="https://user-images.githubusercontent.com/80067024/230645974-5f42c351-6a39-4575-b2de-d3fb7af86cfa.png">

### 2. из таблицы истинности в многочлен Жегалкина и АНФ
Из таблицы истинности в многочлен Жегалкина: 
```
FromTtToZh[func_] := Module[{f, variables, x, i},
  variables = Array[x, Log[2, Length[func]]];
  f = BooleanFunction[func,  variables];
  f = BooleanConvert[f, "ANF"];
  Return[f]
  ]
  ```
#### Пример использвоания:
<img width="1099" alt="image" src="https://user-images.githubusercontent.com/80067024/230646332-2981bf63-17f2-4cb0-92b0-16ade9c5b19c.png">
