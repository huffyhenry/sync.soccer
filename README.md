
I'm keeping the data in a directory: `data`, and for development purposes I'm keeping truncated data in `truncated-data`.
You can truncate data file with:

```
head -n 400 data/tracab/803174_Man\ City-Chelsea.dat > truncated-data/tracab/803174_ManCity-Chelsea.dat
```
assuming `truncated-data/tracab` exists.

# Building

```
stack build sync-soccer
```

# Visualise tracab

For this you will need the 'gloss' package which in turn relies on opengl.

```
stack build sync-soccer && stack exec visualise-tracab truncated-data/tracab/803174_ManCity-Chelsea.dat
````

