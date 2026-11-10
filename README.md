## Structure

```text
root/
├── R/
│   ├── data/                 # code for data creation and preprocessing steps
│   ├── plots/                # code for graphics
│   ├── analysis/             # code for actual code analysis
│   └── utils/                # code for helper functions
├── data/
│   ├── raw/                  # original data 
│   ├── auxiliary/            # auxiliary data like coastline, ecozone, adjacency graph, ...
│   ├── cache/                # cached data
│   └── processed/            # processed data for analysis
├── assets/
│   ├── mesh/
│   ├── plots/
│   └── fits/    
└── conf/
    ├── general.yaml
    ├── fields.yaml
    ├── data.yaml
    ├── grid.yaml
    └── paths.yaml