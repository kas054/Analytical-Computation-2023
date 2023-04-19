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
