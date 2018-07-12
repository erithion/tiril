**Tiril** is an open source software aiming to assist in a language learning process. At the current stage of development when the project hasn't even reached an alpha yet, it doesn't have a stable set of features, for they change and merge into each other rather fast. Luckily, a display of learning approaches which become easier with Tiril serves as a much better showcase for the project. It is about practices, of which any learner has been thinking at least once while reflecting on his own learning process and the ways of its improvement. This is exactly where Tiril strives to back you up.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [The showcase](#the-showcase)
  - [I enjoy watching movies. I wouldn't mind to learn while watching on occasion](#i-enjoy-watching-movies-i-wouldnt-mind-to-learn-while-watching-on-occasion)
  - [I'm old school. I'd rather a good book instead](#im-old-school-id-rather-a-good-book-instead)
  - [I prefer none of the above. But I surf foreign websites a lot](#i-prefer-none-of-the-above-but-i-surf-foreign-websites-a-lot)
  - [I want to remember every new word I encounter for good](#i-want-to-remember-every-new-word-i-encounter-for-good)
- [The pantry](#the-pantry)
  - [Structure](#structure)
    - [Siril](#siril)
    - [Viril](#viril)
  - [Build](#build)
    - [Prerequisites](#prerequisites)
    - [Windows](#windows)
    - [Linux](#linux)
  - [ToDo](#todo)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# The showcase

## I enjoy watching movies. I wouldn't mind to learn while watching on occasion

Unless you won't have to be buried under piles of dictionaries, watching a movie in a language of the original sounds like a nice way to boost your language skills. Indeed there are tons of subtitles in the Internet. There are even well-thought websites which offer you to watch a subtitled movie and translate any word from it in one click at will. However most of them are for English and/or have a small film collection. That means you are restricted in your choice of either the language or the movie of your liking. What if you want to watch some Norwegian movie for example? Or the movie is on the disc(s)? See how you would solve this complication with Tiril and [VLC](https://github.com/videolan) - a very powerful and popular open source video player.

<details>
 <summary>Click to see the gif-image</summary>
  
![Tiril in VideoLAN](https://github.com/erithion/tiril/raw/master/doc/tiril.gif "Tiril in VideoLAN")
</details>

## I'm old school. I'd rather a good book instead

It's about reading the book in a language of the original alongside  some inserts or translation in your mother tongue. Here one finds far less options let alone the decent ones. One of them is Kindle. The other solution is a free application for Android devices called [SmartBook](https://play.google.com/store/apps/details?id=com.kursx.smartbook&hl=en). But again as soon as you step out beyond the "mainstream" set of European languages or books, you are on your own. Perhaps you would like to read Harry Potter in Irish-Georgian? Or maybe Sherlock Holmes in Norwegian-English? See how you could assemble yourself such a book with Tiril for SmartBook application.

Searching the text of the books in both languages is nearly the hardest part throughout the entire process. But once it is done, everything else is easy. Start from opening them in Tiril
<details>
 <summary>Click to see the gif-image</summary>
  
![Opening books to assemble a SmartBook in Tiril](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.open.gif "Opening books to assemble a SmartBook in Tiril")

</details>
<br>
The books need to be aligned in order for SmartBook application to show the proper translation under each paragraph. The rules are simple and intuitive - just keep the same number of chapters and paragraphs in both books or merge/create new paragraphs if needed. The number of empty rows between two paragraphs doesn't matter, for only non-empty ones are accounted, but this trick often improves visual comparison so feel free to use it.
<details>
 <summary>Click to see the gif-image</summary>
  
![Editing the SmartBook](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.edit.gif "Editing the SmartBook")

</details>
<br>
A final step. Optionally you could also check if you missed any chapter before saving
<details>
 <summary>Click to see the gif-image</summary>
  
![Saving the SmartBook](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.checknsave.gif "Saving the SmartBook")

</details>
<br>
In case you know what JSON is and you have some good JSON viewer you could also review the book's internals to make sure everything is aligned as it should
<details>
 <summary>Click to see the gif-image</summary>
  
![Review your SmartBook in unencrypted form](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.unencrypted.gif "Review your SmartBook in unencrypted form")

</details>
<br>
After the book was uploaded on your device ...
<details>
 <summary>Click to see the gif-image</summary>
  
![Copy the SmartBook on your device](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.copy.gif "Copy the SmartBook on your device")

</details>
<br>
... you can enjoy your reading. Mind you though that SmartBook application shows the author's translation for English-Russian books only. This is a limitation set by the creator of the application. Thus you will have to set this direction in the options to see the translation you have supplied with your book. Otherwise the application will show you a translation from Google Translate
<details>
 <summary>Click to see the gif-image</summary>
  
![Reading it on the device](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.device.gif "Reading it on the device")

</details>

## I prefer none of the above. But I surf foreign websites a lot

TBD

## I want to remember every new word I encounter for good

For the small percent of people who was blessed being born with eidetic memory this is probably not an issue at all. For the rest of us who wasn't the situation is rather different. Luckily there are lots of automated solutions for efficient memorizing. These are Anki, Memrise, Quizlet and alike. It won't take long for you to realize how convenient it would be to have your own dictionary there and let the software to decide for you which words you need recollect today. But also what a tremendous amount of effort it would require to implement and maintain all that. Furthermore it gets nearly impossible if you chose more than one such a program and/or more than one tongue. With Tiril this is a problem no more. It knows your dictionary well for it translated the movie pieces ~~and the web-pages~~ for you, remember? Below is an example of how Tiril would help you in case of using Memrise.

Typically there are lots of meanings, contexts and examples for each single word. You may decide which one to preserve
<details>
 <summary>Click to see the gif-image</summary>
  
![Creating a database of words](https://github.com/erithion/tiril/raw/master/doc/tiril.view.gif "Creating a database of words")
</details>
<br>
Memrise allows you to add your own custom columns. It is for you to decide what to put in every column
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

# The pantry

## Structure

Tiril was conceived as a client-server app where the server would take the burden of handling a continuously extending list of online dictionaries and maintaining the user database whereas its "client" would be a growing set of agents requesting translations from it. That guarantees Tiril's presence virtually everywhere so that the user might count upon the translation regardless of whether he is watching a movie in a media player, surfing the Internet in a browser or working with textual information of any other kind. 
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

Some of the project dependencies might seem unexpected, ~~like Electron JS for example~~. Yet the project has scripts that seek for making the build process as effortless as possible by automatically downloading every single library that the project depends on. However there are some steps one is manually to perform. The following must be installed prior to proceed with the build

* [CMake](https://cmake.org/download/) (>=3.12.0)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (>=1.6.3)
* C++ compiler whith C++17 support. For an instance, [MSVC Community](https://www.visualstudio.com/free-developer-offers/) will do
* [Boost library](https://www.boost.org/users/download/) (>=1.67)
* ~~[Electron](https://electronjs.org)~~

### Windows
* Review *config.bat* and set the Boost path according to your environment. Tweak the other variables if needed.  
* Run *config.bat*
* Run *build.bat*

### Linux

TBD

## ToDo
The section is mainly for the author to keep track of the emerging needs/ideas

* Siril server
  * [ ] Add Haskell Conduit HTTP streams to parse dictionaries on-demand and speed up UI page show ups
  * [ ] Research: Consider FRP model of Threepenny GUI and evaluate if it would  reduce the amount of type-unsafe Javascript on UI-side.
  * Dictionaries
    * [ ] Gaeilge support: add [Teanglann](https://www.teanglann.ie/) parser
    * [ ] Norsk bokmål support: add [bøyningsformer](http://inger.uib.no/perl/search/search.cgi?appid=72&tabid=1106) parser
  * Export
    * [ ] Implement exporting to [Anki](https://apps.ankiweb.net)
    * [ ] Introduce a sed-like syntax in export pages to format the output at will
    * [ ] Add single word editing
  * Tools
    * [ ] Add creation of [SmartBooks](https://play.google.com/store/apps/details?id=com.kursx.smartbook&hl=en)
  * [ ] Move UI to a standalone Electron-app
  * [ ] Implement the server as an OS daemon
  * [ ] Add Activity page
* Viril agent
  * [ ] Add ASS/SSA subitles support
  * [ ] Extended phrases selection
  * [ ] Mouse selection support
  * [ ] Add Options to control keyboard shortcuts etc
  * [ ] Siril no-connection screen logo
* Web agent
  * [ ] Opera
  * [ ] Chrome
  * [ ] Firefox
  * [ ] Saving Duolingo forum's notes with tags and searching
* Android agent (daemon)
  * [ ] [SmartBook library](http://smart-book.net) requests: intercept and allow to choose between a local books storage and the default one
* Linux build  
