# IN REWORK #
**Tiril** is an open source software aiming to assist in a language learning process.  

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Showcase](#showcase)
  - [I enjoy watching movies. I wouldn't mind to learn while watching on occasion](#i-enjoy-watching-movies-i-wouldnt-mind-to-learn-while-watching-on-occasion)
  - [I want to remember every new word I encounter for good](#i-want-to-remember-every-new-word-i-encounter-for-good)
- [Backyard](#backyard)
  - [Structure](#structure)
    - [Siril](#siril)
    - [Viril](#viril)
  - [Build](#build)
    - [Prerequisites](#prerequisites)
    - [Windows](#windows)
    - [Linux](#linux)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Showcase

## I enjoy watching movies. I wouldn't mind to learn while watching on occasion

Watching a movie in a language of the original with subtitles turned on - what a nice way to boost your language skills! Indeed, the very thought of making your leisure time productive seems ever so tempting that it is hard to resist really. Only it's the necessity to look for translations of every new word is what makes the process ever so tiresome. "I wish there was an easier way", - you say. Well guess what - there is the easier way! 
</p>
Let us assume for the sake of the demonstration that you've decided to watch a Norwegian movie. In <a href="https://github.com/videolan">VLC</a> - a very powerful and popular open source video player - Tiril will give you this.
<details>
 <summary>Click to see the gif-image</summary>
  
![Tiril in VideoLAN](https://github.com/erithion/tiril/raw/master/doc/tiril.gif "Tiril in VideoLAN")
</details>

## I want to remember every new word I encounter for good

There are lots of automated solutions for efficient memorizing, like Anki, Memrise, Quizlet and alike. It won't take long for you to realize what a relief this is - to let the software to track the words you are to learn/recollect today. But also what a tremendous amount of effort it would need to maintain your own dictionary. 
With Tiril this is a problem no more. It knows your vocabulary well for it translated the movie pieces ~~and the web-pages~~ for you, remember? Below is an example of how Tiril would help you to export your dictionary to Memrise.
</p>
The left pane contains your recent words, while the right one gives you the content of various online dictionaries. They say you learn faster if you memorize words as parts of phrases or sentences. So you are free to choose what you preserve with the word
<details>
 <summary>Click to see the gif-image</summary>
  
![Creating a database of words](https://github.com/erithion/tiril/raw/master/doc/tiril.view.gif "Creating a database of words")
</details>
<br>
Memrise requires at least two columns. Use filtering by tags to split your export into the columns. Delete the elements you don't need or drag the elements with the mouse if you can't apply the proper filter
<details>
 <summary>Click to see the gif-image</summary>
  
![Formatting and exporting](https://github.com/erithion/tiril/raw/master/doc/tiril.export.memrise.gif "Formatting and exporting")
</details>
<br>
And finally importing the whole set to Memrise at one stroke
<details>
 <summary>Click to see the gif-image</summary>
  
![Importing to Memrise](https://github.com/erithion/tiril/raw/master/doc/tiril.export.memrise.second.gif "Importing to Memrise")
</details>

# Backyard

## Structure

Currently Tiril consists of      
* Siril - the server with an embedded UI. 
* Viril - a [VLC](https://github.com/videolan) agent to translate subtitles.
* ...

### Siril

Supports dictionaries

* Universal dictionaries
  * [x] [Google Translate](https://translate.google.com)
  * [ ] ...
* Specialized dictionaries
  * [x] [Lexin (Norwegian Bokmål)](http://lexin.udir.no)
  * [ ] [Teanglann (Irish)](https://www.teanglann.ie/)
  * [ ] ...

### Viril 

Supports subtitle formats

  * [x] Embedded and external SubRip 
  * [ ] ASS/SSA possibility has been confirmed and awaiting implementation
  * [ ] ...


## Build
### Prerequisites

The following must be installed prior to proceed with the build

* [CMake](https://cmake.org/download/) (>=3.12.0)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (>=1.6.3)
* C++ compiler whith C++17 support. For an instance, [MSVC Community](https://www.visualstudio.com/free-developer-offers/) will do
* [Boost library](https://www.boost.org/users/download/) (>=1.68)

### Windows
* Review *config.bat* and set the Boost path according to your environment. Tweak the other variables if needed.  
* Run *config.bat*
* Run *build.bat*

### Linux

*TBD*
