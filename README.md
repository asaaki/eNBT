# Erlang NBT (Named Binary Tag) file library

This library is for reading and writing the NBT file format which is used by the sandbox game Minecraft.

File format is described [here (Named Binary Tag)](http://wiki.vg/NBT).
It is the most up to date version of the specification (includes type 11 = IntArray).

## Motivation

I like Erlang. I want to learn more (especially Erlang's binary processing and bit syntax).
Erlang is not only made for concurrency but also for binary data handling.

After reading the specs I thought, NBT is a very cool format to store structured data.
Although it has some limitations, it covers most used types.
And so it can save the data in a more compact way than `term_to_binary/1,2` can do.

_(Yes, I have already seen the other implementation in Erlang. I don't care; it's just for fun and learning.)_


## Usage

Compile `nbt.erl` in `src/`. (I will rebar'fy this project soon.)

```erlang
% Load a gzipped NBT file:
Data = nbt:read_gzip_file("path/to/file.nbt").
%% this is the combination of "nbt:open_gzip_file/1" and "nbt:parse_data/1"
%% other opener/reader: open_plain_file/1, read_plain_file/1
%% read_gzip_file/1 can also be used in shortened form read_file/1, because
%% gzipped nbt files are most common type so far

% Write the erlang nbt tuple to disk
% I use *.nrl as extension ("NBT in Erlang")
nbt:unconsult("path/to/file.nrl", Data).

% And read the nbt tuple again from .nrl:
ConsultedData = nbt:consult("path/to/file.nrl").
```


## Status

+ parse a nbt file (plain or gzipped) -> `{nbt, …NBT DATA…}`
+ save the nbt tuple to a file (`nbt:unconsult/2`, for debugging)
+ read the nbt tuple from file (`nbt:consult/2`, for debugging)


## TODO

+ refactoring!!!
+ writing a nbt file from erlang tuple
+ parser for MC region files in `Anvil file format` (*.mcr)
+ did I mention refactoring?