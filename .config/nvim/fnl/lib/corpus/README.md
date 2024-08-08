# Corpus

A note taking vim utility. 

## How it works

A Corpus directory is a directory with a '.corpus' file. It should contain markdown files with TOML metadata. YAML metadata is automatically converted to TOML.

## commands

### Corpus 

The base command searches through files within a corpus directory. The search happens live, when the command is detected two buffers are open, one displaying a list of resulting files, the other displaying a preview with search terms highlighted.

When a search matches a file, enter will open that file in a new buffer. When no match is found, it will create a new file with the filename set to the search term. If a match is found but a '!' it will forcefully create a new file if the search term doesn't exactly match an existing file name.

### CorpusZet

Creates a new temp zettelkastran note file, when saved it will save to the markdown file in the corpus directory.


### CorpusAddTag

Add a tag to a corpus file. 
