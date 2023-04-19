## Факторизация
### Разложить числа методом Ферма.
Алгоритм:

<img width="1234" alt="image" src="https://user-images.githubusercontent.com/80067024/233144782-155c5cc0-bf3f-48ab-9eed-e88677da60a4.png">
Код:

```
Fermat1 (num_) := Module[{y , k = -1, s = Sqrt[num]},
  While[Mod[y, 10] != 0,
   k += 1;
   s = IntegerPart[s] + 1;
   y = (s + k)^ 2 - num;
   ];
  Return[{y^2, (s + k)^2, s + k - y, y + s + k}];
  ]

```
Пример использования (Вариант 1):



<img width="385" alt="image" src="https://user-images.githubusercontent.com/80067024/233145022-1753db43-b9ea-49c6-8fb0-89bd1494a05c.png">

p = 15485867, q = 15518449, u = 15502158, v = 16291


<img width="502" alt="image" src="https://user-images.githubusercontent.com/80067024/233145857-84c6e535-2f8b-4b8f-affd-f007390448d2.png">


p = 15487469, q = 15516719, u = 15502094, v = 14625

### Разложить числа ро-методом Полларда.

Алгоритм:



<img width="983" alt="image" src="https://user-images.githubusercontent.com/80067024/233170456-f5fef5d6-f495-4cd0-8064-21dc0d983c17.png">

<img width="486" alt="image" src="https://user-images.githubusercontent.com/80067024/233170132-51f30bf2-1046-40c9-b081-c05357d84bf9.png">

<img width="228" alt="image" src="https://user-images.githubusercontent.com/80067024/233162447-6afd6b55-dccb-40f5-ac95-9ab41f8e36d6.png">



```
G[x_, n_] := Module[{ans},
  ans = Mod[x^2 + 1, n];
  Return[ans]
  ]

Pollard[num_] := Module[{x = 2, y = 2, d = 1, len = 0},
  While[d == 1,
   len += 1;
   x = G[x, num];
   Print[x];
   y = G[G[y, num]];
   Print[y];
   d = GCD[Abs[x - y], num];
   Print[d];
   ];
  If[d == n,
   Return[False],
    Return[{d, num / d, len}]]
  ]
```


<img width="347" alt="image" src="https://user-images.githubusercontent.com/80067024/233160943-18c5ae59-4dd3-4c81-ae38-13d2a413e2a3.png">

p = 15485867, q = 15518449, длина последовательности: 3399


<img width="324" alt="image" src="https://user-images.githubusercontent.com/80067024/233161685-097a4d18-9363-4522-b3d7-072b59b94755.png">


p = 1569181, q = 15518449, длина последовательности: 3017
