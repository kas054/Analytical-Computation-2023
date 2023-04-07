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
