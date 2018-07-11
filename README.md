Tiril is an open source software aiming to assist in a language learning process. At the current stage of development when the project hasn't even reached an alpha yet, it doesn't have a stable set of features, for they change and merge into each other rather fast. Luckily, a display of learning approaches which become much easier with using Tiril serves as a much better showcase for the project. This is about the practices, of which any learner has been thinking at least once in one way or another while reflecting and trying to improve own learning process. This is exactly where Tiril strives to back you up.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [The showcase](#the-showcase)
  - [I enjoy watching movies. I wouldn't mind to learn in the process on occasion](#i-enjoy-watching-movies-i-wouldnt-mind-to-learn-in-the-process-on-occasion)
  - [I'm old-school. I'd rather a good book instead](#im-old-school-id-rather-a-good-book-instead)
  - [I prefer none of the above. But I surf foreign segments of the Internet a lot](#i-prefer-none-of-the-above-but-i-surf-foreign-segments-of-the-internet-a-lot)
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

## I enjoy watching movies. I wouldn't mind to learn in the process on occasion

Unless you won't have to be buried under the piles of dictionaries, watching the movie in a language of the original sounds like fun and a pleasant way to boost your language skills. Indeed there are tons of subtitles in the Internet. There are also websites where you can translate every word in the subtitled video in one click while watching the movie. However most of them are for English. Usually that means you are limited in choosing either the language or the movie of your liking. What if you want to watch some Norwegian movie in Norwegian for example and you've even acquired the right disc(s)? See how to achieve that with Tiril and [VLC](https://github.com/videolan) - a very powerful and popular open source video player.  

<details>
 <summary>Click to see the gif-image</summary>
  
![Tiril in VideoLAN](https://github.com/erithion/tiril/raw/master/doc/tiril.gif "Tiril in VideoLAN")
</details>

## I'm old-school. I'd rather a good book instead

This is about reading a book in a language of the original along with some hints or translations in your mother tongue. Here we have far less available solutions let alone the decent ones. One of them is Kindle. The other is a free solution for Android devices called [SmartBook](https://play.google.com/store/apps/details?id=com.kursx.smartbook&hl=en). But again as soon as you step beyond the "classical" set of European languages or books, it is hard to find what you need. How nice it would be if you were able to read Harry Potter in Irish-Norwegian or Lord of the Rings in Norwegian-Russian or any other book in any other tongues of your choice. Below you will find a visual demo of how you could assemble yourself such a book for SmartBook application with Tiril.

* Having the text of the books in both languages, this is the start
<details>
 <summary>Click to see the gif-image</summary>
  
![Opening books to assemble a SmartBook in Tiril](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.open.gif "Opening books to assemble a SmartBook in Tiril")

</details>

* The books need to be aligned in order for the SmartBook application to show them properly. The rules are simple and intuitive - just keep the same number of chapters and paragraphs in both books or merge/create new paragraphs at will
<details>
 <summary>Click to see the gif-image</summary>
  
![Editing the SmartBook](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.edit.gif "Editing the SmartBook")

</details>

* A final step. Optionally you could also check if you missed any chapter before saving
<details>
 <summary>Click to see the gif-image</summary>
  
![Saving the SmartBook](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.checknsave.gif "Saving the SmartBook")

</details>

* In case you know what JSON format is and you have some good JSON viewer you could also review the book's internals to make sure everything is placed where it should be
<details>
 <summary>Click to see the gif-image</summary>
  
![Review your SmartBook in unencrypted form](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.unencrypted.gif "Review your SmartBook in unencrypted form")

</details>

* After the book was uploaded on your device ...
<details>
 <summary>Click to see the gif-image</summary>
  
![Copy the SmartBook on your device](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.copy.gif "Copy the SmartBook on your device")

</details>

* ... you can enjoy your reading
<details>
 <summary>Click to see the gif-image</summary>
  
![Reading it on the device](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.device.gif "Reading it on the device")

</details>

## I prefer none of the above. But I surf foreign segments of the Internet a lot

## I want to remember every new word I encounter for good

For the small percent of people who was blessed being born with eidetic memory this is probably not an issue at all. The situation is rather different for the rest of us who wasn't. Luckily there are lots of automated solutions for efficient memorizing which claim to use the latest scientific developments in their learning algorithms. This is about such software like Anki, Memrise, Quizlet and alike. It won't take long for you to realize how great it would be to have your own dictionary there and allow the software to decide which word you should recall today. At the same time how much efforts it would require to even write down all the words not to mention how tedious it would be to maintain such a database. Especially if you are learning more than one language or have chosen more than one such program. With Tiril it stops being a problem. It knows your dictionary well for it translated the movie pieces ~~and the web-pages~~ for you, remember? It is rather logical to ask it to create the database for you, isn't it? Below is an example of such process for Memrise.

* Typically there are lots of meanings for each single word, lots of contexts and examples. You may decide which one of them to preserve with the word
<details>
 <summary>Click to see the gif-image</summary>
  
![Creating a database of words](https://github.com/erithion/tiril/raw/master/doc/tiril.view.gif "Creating a database of words")
</details>


* Memrise allows you to add your own custom columns. It is for you to decide what will be in every column
<details>
 <summary>Click to see the gif-image</summary>
  
![Formatting and exporting](https://github.com/erithion/tiril/raw/master/doc/tiril.export.memrise.gif "Formatting and exporting")
</details>


And finally importing the whole set to Memrise at one stroke
<details>
 <summary>Click to see the gif-image</summary>
  
![Importing to Memrise](https://github.com/erithion/tiril/raw/master/doc/tiril.export.memrise.second.gif "Importing to Memrise")
</details>

# The pantry

## Structure

Tiril was conceived as a server part which takes the burden of handling online dictionaries and maintaining the user database, and an unlimited number of agents which would ask a translation from the server. This would guarantee Tiril's presence virtually everywhere which means the user could count on the translation regardless of whether he is watching the movie, surfing the Internet in a browser or working with a textual information in any other form. Currently Tiril consists of  
* Siril - the server with an embedded UI. 
* Viril - a [VideoLAN](https://github.com/videolan) agent to translate subtitles.

### Siril

Supported dictionaries

  * [x] [Google Translate](https://translate.google.com)
  * [x] [Lexin (Norwegian Bokmål)](http://lexin.udir.no)
  * [ ] [Teanglann (Irish)](https://www.teanglann.ie/)
  * [ ] ...

### Viril 

Supported subtitle formats
  * [x] Embedded and external SubRip 
  * [ ] ASS/SSA possibility has been confirmed and awaiting implementation
  * [ ] ...


## Build
### Prerequisites
The project has dependencies that might seem unexpected, ~~Electron JS for example~~. While currently available build scripts seek for making the build process as easy and effortless as possible by attempting to automatically find and download every single libary that the project depends on, yet there are some steps one is manually to perform. Thus the following must be installed prior to proceed with the build process itself

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
