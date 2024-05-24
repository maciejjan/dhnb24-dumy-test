# Computational analysis of Ukrainian dumy

This repository contains the code and data used in the following
presentation and planned article:
* Olha Petrovych, Mari Sarv, Maciej Janicki and Kati Kallio. 2024. How to analyse variation in folklore: Finnic runosongs and Ukrainian dumas. *Digital Humanities in the Nordic & Baltic Countries*, Reykjavik, May 2024.

The file `dumy.csv` contains 74 versions of the duma "Escape of three
brothers from the Turkish captivity". The texts were transcribed from
the following book:
* Hanna Skrypnyk (Ed.). 2019. *Dumy XVI-XVII stolit*, in: *Ukraïnski narodni dumy*. V. 2. Kyiv.

`main.R` runs the following computational methods:
* text preprocessing (removal of punctuation, lowercasing etc.)
* computation of line similarity (cosine similarity of character bigrams)
* clustering lines by similarity using the Chinese Whispers algorithm
* calculating a co-occurrence measure on line clusters (Lexicographer's Mutual Information)

The calculations run on a standard laptop within several minutes.
The resulting table `dumy_gephi.csv` can be imported to Gephi for
further analysis.

The file `methods.R` contains some functions that are called in `main.R`,
namely the n-gram-based vectorization and Chinese Whispers clustering.

