## 1. Построение в виде таблицы истинности 
### 1. (псевдо)случайной булевой функции от заданного числа переменных 
```
  GenerateTable[n_] := Module[{table}, 
  table = Table[RandomInteger[], 2^n]]
```
#### Пример использования: <br />
<img width="227" alt="image" src="https://user-images.githubusercontent.com/80067024/230634061-ab0677f5-3ed1-4a87-b90d-ee8420db3062.png">

