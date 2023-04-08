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
#### Пример использвания:
<img width="806" alt="image" src="https://user-images.githubusercontent.com/80067024/230645974-5f42c351-6a39-4575-b2de-d3fb7af86cfa.png">

### 2. из таблицы истинности в многочлен Жегалкина и АНФ
Преобразование из многочлена в таблицу: <br />
<img width="711" alt="image" src="https://user-images.githubusercontent.com/80067024/230721689-925682ea-d957-4ebc-814b-920dc2a1fc0b.png">
 <br />
Преобразование из таблицы в вектор коэффициентов $a_f$
 <br />
<img width="760" alt="image" src="https://user-images.githubusercontent.com/80067024/230722338-2ab4e9b2-2f78-4bb8-bdbf-286f71f3baf0.png">

<br />

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
<br />
<img width="1099" alt="image" src="https://user-images.githubusercontent.com/80067024/230646332-2981bf63-17f2-4cb0-92b0-16ade9c5b19c.png">
<br />
Вспомогательная функция: <br />

```
XorVectors[v1_, v2_] := Module[{result },
  size = Length[v1];
  result = Table[0, size];
  For[i = 1, i <= size, i ++,
   result = ReplacePart[result, BitXor[v1[[i]], v2[[i]]], i]
   ];
  Return[result]
  ]
```
<br />
Из таблицы истинности в АНФ: <br />

```
FromTTToAnf[vector_] := Module[{mat = {{1, 0}, {1, 1}}, a1, a2, mul},
  If[Length[vector] == 2,
   mul = Dot[mat, vector];
   For[i = 1, i <= 2, i ++,
    If[mul[[i]] == 2, mul = ReplacePart[mul, 0, i]]
    ];
   Return[mul],
   a1 = FromTTToAnf[Take[vector, {1, Length[vector] / 2}]];
   a2 = XorVectors[a1, 
     FromTTToAnf[
      Take[vector, {Length[vector] / 2 + 1, Length[vector]}]]];
   Return[Join[a1, a2]]
   ]
  ]

```
#### Пример использования:
<img width="393" alt="image" src="https://user-images.githubusercontent.com/80067024/230646693-debfff35-d9fa-45f6-8baa-c5e3c909e8b4.png">

### 3. из многочлена Жегалкина в таблицу истинности
```
FromZhToTt[function_] := Module[{tmp},
  table = BooleanTable[BooleanConvert[function, "BFF"]];
  Return[Boole[table]];
  ]
```
#### Пример использования:
<img width="262" alt="image" src="https://user-images.githubusercontent.com/80067024/230722016-349e74d5-1be4-4e2b-b06b-5b82aa646e3a.png">

### 4. из таблицы истинности в действительный многочлен

```
FromTTToP[vector_] := Module[{mat = {{1, 0}, {-1, 1}}, a1, a2, mul},
  If[Length[vector] == 2,
   mul = Dot[mat, vector];
   Return[mul],
   a1 = FromTTToP[Take[vector, {1, Length[vector] / 2}]];
   a2 = FromTTToP[
      Take[vector, {Length[vector] / 2 + 1, Length[vector]}]] - a1 ;
   Return[Join[a1, a2]]
   ]
  ]
```

#### Пример использования:
<img width="354" alt="image" src="https://user-images.githubusercontent.com/80067024/230722065-aae82728-6419-4c79-88da-9d7f8f69ecc1.png">

<br />

### 5. вычисление списка спектральных коэффициентов (Фурье, Адамара-Уолша) по таблице истинности
<img width="726" alt="image" src="https://user-images.githubusercontent.com/80067024/230723287-31889dd0-2917-449c-bc59-e8c01e463962.png">

<br />
Вспомогательная функция: <br />


```

ChangeVector[vector_] := Module[{result},
  result = Table[1 - 2*vector[[i]], {i, Length[vector]}];
  Return[result]
  ]
  
```

Коэффициенты Адамара-Уолша: <br />

```

WalshHadamard[vector_] := 
 Module[{mat = {{1, 1}, {1, -1}}, a1, a2, mul},
  If[Length[vector] == 2,
   mul = Dot[mat, vector];
   Return[mul],
   a1 = WalshHadamard[Take[vector, {1, Length[vector] / 2}]];
   a2 = WalshHadamard[
     Take[vector, {Length[vector] / 2 + 1, Length[vector]}]];
   Return[Join[a1 + a2, a1 - a2]]
   ]
  ]
  
```

#### Пример использования:
<img width="371" alt="image" src="https://user-images.githubusercontent.com/80067024/230722523-3048692c-8c57-4bfc-a10c-3b801b0d45f2.png">


Коэффициенты Фурье:

<img width="692" alt="image" src="https://user-images.githubusercontent.com/80067024/230723647-fa6f5899-69bb-4f69-8324-1542e643fb77.png">


```
Fourier1[vector_]  := Module[{fwhd, fourier, n},
  n = Log[2, Length[vector]];
  fwhd = WalshHadamard[vector];
  fourier = {2^n - 2*fwhd[[1]]};
  For[ i = 2, i <= 2^n, i++,
   fourier = Append[fourier, -2*fwhd[[i]]];
   ];
  Return[fourier]
  ]
```
#### Пример использования:
<img width="331" alt="image" src="https://user-images.githubusercontent.com/80067024/230722530-fd613172-5b77-4f6c-80a4-ec64796cafb8.png">

### 6) получение таблицы истинности по спектральным коэффициентам </br>

Вспомогательная функция:
```
Clear[BaseTranslator];
Options[BaseTranslator] = {BTForm -> BaseForm};
BaseTranslator[number_, base1_, base2_, 
  OptionsPattern[]] := (OptionValue@BTForm)[
  FromDigits[ToString[number], base1], base2]
  
  
Calculate[vector_, cur_, size_] := 
 Module[{tmp, answer = vector[[1]], eq  = 0},
  For[i = 2, i <= Length[vector], i ++,
   tmp = BaseTranslator[i, 10, 2, BTForm -> IntegerDigits];
   While[Length[tmp] < Length[size],
    tmp = Insert[tmp, 1, 0];
    ];
   For[j = 1, j <= Length[tmp], j ++,
    If [tmp[[i]] == 1, eq = BitXor[eq, cur[[i]]]];
    ];
   If[vector[[i]] != 0, 
    answer += Power[vector[[i]], eq]];
   ];
  Return[answer ];
  ]
```

```
FromWalshToTt[vector_] := Module[{size, func, cur},
  size = Log[2, Length[vector]];
  func = Table[0, 2^size];
  For[i = 1; i <= 2^size, i ++,
   cur =  BaseTranslator[i, 10, 2, BTForm -> IntegerDigits];
   While[Length[cur] < size,
    cur = Insert[cur, 1, 0];
    ];
   ReplacePart[func, Calculate[vector, cur, size], i];
   ];
  Return[func];
  ]
```
#### Пример использования:
<img width="389" alt="image" src="https://user-images.githubusercontent.com/80067024/230724035-511038ed-c4dd-4bba-a57e-221f7a3dde76.png">

## 3. Разработка способов и реализация средствами САВ "Mathematica"  инструментов исследования булевых вектор-функций.

### 3.1 Построение вектор-функций для заданных размерностей ходных и выходных векторов:
1) из разрядных функций;
```
FFunction[m_, n_, functions_] := Module[{output},
  output = Boole[Table[BooleanTable[functions[[i]]], {i, m}]];
  Return[output];
  ]
```
2) (псевдо)случайным образом
```
SFunction[m_, n_] := Module[{output, i},
  output = 
   Boole[Table[
     BooleanTable[BooleanFunction[RandomInteger[100], n]], {i, m}]];
  Return[output]
  ]
```
#### Пример использования:
<img width="1264" alt="image" src="https://user-images.githubusercontent.com/80067024/230724236-bfda4df4-37eb-40a8-9a62-47f68a7e3cce.png">


### 3.2 Исследование разрядных функций вектор-функции.


1) Получение разрядных функций вектор-функции, заданной таблично, где входные и выходные вектора упакованы в (целые неотрицательные) числа.

```

GetFunction[m_, n_, func_] := Module[{i},
  For[i = 1, i <= m, i ++,
    Print[func[[i]]];
    ];
  ]
  
```

#### Пример использлвания:
<img width="231" alt="image" src="https://user-images.githubusercontent.com/80067024/230724331-2d44123e-ad0a-41d8-93b5-9902bf27bcd6.png">

2)Получение следующих характеристик разрядных функций:
-	вес;
Количество единиц в таблице истинности разрядной функции

```

WeightF[func_] := Module[{i},
  m = Length[func];
  For[i = 1, i <= m, i ++,
   Print[Total[func[[i]]]]
   ]
  ]
```
#### Пример использования:

<img width="774" alt="image" src="https://user-images.githubusercontent.com/80067024/230724498-653077d9-c9fb-4baa-acad-d63a9616811d.png">


-	число мономов для многочлена Жегалкина каждой функции;
Количество конъюнкций, входящих в многочлен Жегалкина (количество единиц в АНФ)

```

GetMon[func_] := Module[{ i},
  m = Length[func];
  For[i = 1, i <= m, i ++,
   Print[FromTTToAnf[func[[i]]]];
   Print[Total[FromTTToAnf[func[[i]]]]];
   ]
  ]
  
```
#### Пример использования:
<img width="226" alt="image" src="https://user-images.githubusercontent.com/80067024/230724561-11d8906b-652e-440d-87fd-9ef794d7d6ae.png">

-	число мономов во всех многочленах Жегалкина разрядных функциях (т.е. мощность объединения множеств мономов многочленов Жегалкина разрядных функций);

```

GetTotal[func_] := Module[{ i, sum = 0},
  m = Length[func];
  For[i = 1, i <= m, i ++,
   sum += Total[FromTTToAnf[func[[i]]]];
   ];
  Print[sum];
  ]
```
#### Пример использования:
<img width="160" alt="image" src="https://user-images.githubusercontent.com/80067024/230724613-c28fe2ec-839b-4fc0-b172-8e531e780fac.png">

-	коэффициенты Фурье и Адамара-Уолша, соответственно список  линейных аналогов и соответствующих вероятностей совпадения разрядной функции с линейной (аффинной);

$P(f(x) = \alpha^Tx) = \frac {1}{2} - \frac {\tilde f(\alpha)}{2^n}, \alpha \neq 0$

Впомогательная функциия:

```
NormalizeSN[normnum_, decnumber_] := 
 Module[{newarr, i, binarr}, 
  binarr = BaseTranslator[decnumber, 10, 2, BTForm -> IntegerDigits];
  newarr := binarr;
  For[i = Length[binarr], i < normnum, i++, 
   newarr = Insert[newarr, 0, 1]];
  Return[newarr];]

```
```
P[func_] := Module[{vector, coef, p, n, count, tmp, i},
  n  = Length[func[[1]]];
  count = Log[2, n];
  p = Table[0, {i, count}];
  For[i = 1, i <= Length[func], i ++,
   vector = func[[i]];
   coef = Fourier1[vector];
   p = ReplacePart[p, Table[1/2 + coef[[i]]/ 2^(count + 1), {i, n}], 
     i];
   ];
  Return[p];
  ]

LinearAnalog[func_] := Module[{prob, size, vect, variables},
  size = Log[2, Length[func[[1]]]];
  prob = P[func];
  variables = Array[{x}, size];
  For[j = 1, j <= Length[func], j ++, 
   For[i = 0, i <= Length[func[[1]]] - 1, i ++,
    vect = NormalizeSN[size, i];
    Print[Dot[vect, variables]];
    Print[prob[[j]][[i + 1]]]
    ]
   ]
  ]
```
#### Пример использолвания: 
<img width="742" alt="image" src="https://user-images.githubusercontent.com/80067024/230724933-c611c49e-f5a6-4953-8caf-b2d77bccc82f.png">
