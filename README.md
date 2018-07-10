# About Tiril
Tiril is an open source software aiming to assist in a language learning process. At the current stage of development when the project hasn't even reached an alpha yet, it doesn't have a stable set of features, for they change and merge into each other rather fast. Luckily, a display of learning approaches which become much easier with using Tiril serves as a much better showcase for the project. This is about the practices, of which any learner has been thinking at least once in one way or another while reflecting and trying to improve the own learning process. This is exactly where Tiril strives to back you up.


# The showcase

## I enjoy watching movies. I wouldn't mind to learn in the process on occasion

Unless you won't have to be buried under the piles of dictionaries, watching the movie in a language of the original sounds like fun and a pleasant way to boost your language skills. Indeed there are tons of subtitles in the Internet. There are also websites where you can translate every word of the subtitles in one click while watching the movie. However most of them are for English. Usually that means you are limited in choosing either the language or the movie of your liking. What if you want to watch some Georgian movie in Georgian for example and you've even acquired the right disc? See how to achieve that with Tiril and [VLC](https://github.com/videolan) - a very powerful and popular open source video player.  


<details>
 <summary>Click to see the gif-image</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.gif "VideoLAN demo")
</details>

## I'm old-school. I'd rather a good book instead

This is about reading a book in a language of the original along with some hints or translations in your mother tongue. Here we have far less available solutions let alone the decent ones. One of them is Kindle. The other is a free solution for Android devices called [SmartBook](https://play.google.com/store/apps/details?id=com.kursx.smartbook&hl=en). But again as soon as you step beyond the "classical" set of European languages or books, it is hard to find what you need. How nice it would be if you were able to read Harry Potter in Irish-Norwegian or Lord of the Rings in Norwegian-Russian or any other book in any other tongues of your choice. Below you will find a visual demo of the steps that Tiril would require from you to assemble yourself such a book for SmartBook application.

1. Having the text of the books in both languages, this is how you would start
<details>
 <summary>Click to see the gif-image</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/smartbook/tiril.smartbook.open.gif "Tiril SmartBook demo")

</details>

2. After that you need to align them in order SmartBook to show them properly
<details>
 <summary>Click to see the gif-image</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/ "Tiril SmartBook demo")

</details>

The rules for that are very intuitive
    * Just try to keep the same number of paragraphs within a chapter and the same number of chapters for both books
    * 

 
## I want to remember every new word I encounter for good

Tiril is an open source software aiming to assist in a language learning process. While it is on its early stage of development and hasn't even reached an alpha yet, it is reasonable to use a term "the tool-set" while referring it and talk more about use-cases it can cover rather than about concepts and ideas it strives for

While it still exists in a form of Proof-Of-Concept with lots of ideas to be yet implemented, the following components have already been added
* Siril server with an embedded UI. To date it operates merely dictionaries and handles just a few of them for now:
  * [x] [Google Translate](https://translate.google.com)
  * [x] [Lexin](http://lexin.udir.no)
* Viril agent - a [VideoLAN](https://github.com/videolan) plugin to translate subtitles. Among supported formats are:
  * [x] Embedded and external SubRip 
  * [ ] ASS/SSA possibility has been confirmed and awaits implementation

# Demo  

## Enjoy watching a subtitled video with translation

Below you can see a demonstration of how Tiril already strives to be helpful by having taught VLC media player to do something new in spite of having grown so small yet. Translate and save the words that are little-known to you in one fell swoop.

<details>
 <summary>Click to see the image</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.gif "VideoLAN demo")
</details>

## Learn from your favourite movies on Memrise

Again, while Tiril still might seem "sketchy" and lacking certain features, nonetheless it already might track, collect and keep the words for you that you have encountered and would readily assist you in exporting them in to your favourite learning tool. Below you can review how this process would look in the case of Memrise. Mind you though, the user interface and features will evolve and change.

This is how you would work with available online translations and save them along with your words.
<details>
 <summary>Review the words</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.view.gif "View demo")
</details>


This is how you would format and batch-export them after you added all the translations and examples you wanted.
<details>
 <summary>Export to Memrise format</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.export.memrise.gif "Memrise export demo")
</details>


And finally is how you would import the whole set to Memrise at one stroke.
<details>
 <summary>Import on Memrise</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.export.memrise.second.gif "Memrise import demo")
</details>

# Build
## Prerequisites
The project has some dependencies, at times even unexpected ones, ~~like Electron JS for example~~. And while the currently available build scripts seek for making the build process as easy and effortless as possible by attempting to automatically find and download every single libary that the project depends on, yet there are some steps one is manually to perform. Thus the following must be installed prior to proceed with the build process itself

* [CMake](https://cmake.org/download/) (>=3.11.0)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (>=1.6.3)
* C++ compiler whith C++17 support. For an instance, [MSVC Community](https://www.visualstudio.com/free-developer-offers/) will do
* [Boost library](https://www.boost.org/users/download/) (>=1.67)
* ~~[Electron](https://electronjs.org)~~

## Windows
* Review *config.bat* and set the Boost path according to your environment. Tweak the other variables if needed.  
* Run *config.bat*
* Run *build.bat*

## Linux

# ToDo
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
