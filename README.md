# NER

## Build

```bash
cd src
make
```

## Run

```bash
./preprocess pages-articles.xml names links # output to names and links
./relatedness links rel # output to rel
./ner names links rel 4242 # listen on port 4242 after printing "ready"
curl localhost:4242 -d ""
```

## How it works

`preprocess` parses `pages-articles.xml` to extract links between pages and a mapping from text to entities.
The confidence in the mapping $a\mapsto b$ is $$conf(a\mapsto b)=\frac{count(a\mapsto b) - 1}{\sum_{c\in Pages}{count(a\mapsto c)}}$$

`relatedness` computes for each page $p$ the number $paths2(p, p)$ of paths of lenght 2 from $p$ to $p$ and $\sum_{q\in Pages}{rel(p,q)}$ with $$rel(p,q)=\frac{paths2(p, q) + paths2(q, p)}{paths2(p, p) + paths2(q, q)}$$

`ner` extract entities from text.
All candidate entities are extracted (longest match of text) and sorted by confidence.
Entities are selected greedily starting with the worst candidate.
If the worst candidate is the only remaining candidate for a piece of text, then it is selected, otherwise it is pruned and confidence of all other candidates is updated.

Confidence is $$conf(c_i)=prior(c_i)\sum_{j\not=i}{rel(c_i,c_j)}$$ where $prior(c_i)=conf(t_i\mapsto c_i)$ with $t_i$ the piece of text corresponding to $c_i$.
