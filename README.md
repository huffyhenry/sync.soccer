
I'm keeping the data in a directory: `data`, and for development purposes I'm keeping truncated data in `truncated-data`.
You can truncate data file with:

```
head -n 400 data/tracab/803174_Man\ City-Chelsea.dat > truncated-data/tracab/803174_ManCity-Chelsea.dat
```
assuming `truncated-data/tracab` exists.

# Building

I installed stack very simply with the instructions here:

https://docs.haskellstack.org/en/stable/README/

```
stack build
```

# Visualise tracab

For this you will need the 'gloss' package which in turn relies on opengl.

```
stack build && stack exec visualise-tracab truncated-data/tracab/803174_ManCity-Chelsea.dat
````

# Beginnings of a synchronisation program

Assuming you have both appropriate datafiles after:

```
stack build
```

succeeds you can do:

```
stack exec sync-soccer f24filename tracabfilename
```

It currently just prints out how many lines/events there are. Even this, will take a while.

# Visualise f24


```
stack build
stack exec visualise-opta data/tracab/metadata/803174_Man\ City-Chelsea_metadata.xml data/f24/f24-8-2015-803174-eventdetails.xml
```