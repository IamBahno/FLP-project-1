#  FLP Haskell 2025 - CART decision tree
- login: xbahou00
- name: Ondřej Bahounek
- year: 2025

### Implementation of a FLP Haskell 2025 project - CART decision tree.
I have implemented the project in full, exactly as specified, with no additional features beyond the requirements.

The training algorithm works as follows: after loading the labeled data, I generate all possible split points by considering all data points across each attribute. For each potential split point, I partition the data and compute the Gini impurity for both subsets. Then, I calculate a weighted sum of the Gini impurities based on the sizes of the subsets. I choose the split point with the lowest score as the value in the tree node, and split the data for real. This process is recursively applied to both subsets until only a single class remains, at which point the node becomes a leaf node.



#### Building project:
Build on merlin:
`make` or `make build-merlin`
on other machines use:
`make build`

#### First part is loading a tree and classifying data:
`
./flp-fun -1 <file_with_tree> <file_with_data>
`

Tree in 'file_with_tree' should look like:
```
Node: 0, 5.5
  Leaf: TridaA
  Node: 1, 3.0
    Leaf: TridaB
    Leaf: TridaC
```

Data in 'file_with_data' should look like:
```
2.4,1.3
6.1,0.3
6.3,4.4
```

#### Second part is training a tree

`
flp-fun -2 <file_with_training_data> 
`

Data in 'file_with_training_data' is values seperated by comma and class label at the end:
```
2.4,1.3,TridaA 
6.1,0.3,TridaB 
6.3,4.4,TridaC 
2.9,4.4,TridaA 
3.1,2.9,TridaB
```

#### Tests
Binary `flp-fun` copy into folder `public`
```
cd public
python -m venv venv
source venv/bin/activate
pip install pandas scikit-learn termcolor tqdm
```

And run with:
`python test_flp.py --test_type b`

Test type either `i` for running test of first part, `t` for testing the second part or `b` for testing both (default.)