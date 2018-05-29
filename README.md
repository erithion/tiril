# Tiril
Tiril is an open source software aiming to assist in a language learning process.

While it still exists in a form of Proof-Of-Concept with lots of ideas still to be implemented, the following components have already been added
* Siril server with an embedded UI. To date it operates merely dictionaries and handles just a few of them for now:
  * [x] [Google Translate](https://translate.google.com)
  * [x] [Lexin](http://lexin.udir.no)
* Viril agent - a [VideoLAN](https://github.com/videolan) plugin to translate subtitles. Among supported formats are:
  * [x] Embedded and external SubRip 
  * [ ] ASS/SSA possibility has been confirmed and awaits implementation

# Demo  

## Enjoy watching a subtitled video with translation

Below you can see a demonstration of how Tiril already strives to be helpful to you by having taught VLC media player to do something new in spite of having grown so small yet.

<details>
 <summary>Click to see the image</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.gif "VideoLAN demo")
</details>

## Learn from your favourite movies on Memrise

It also helps you to gather the new words you have encountered so far and export them to your favourite learning tool

<details>
 <summary>Review the words</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.view.gif "View demo")
</details>

<details>
 <summary>Export to Memrise format</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.export.memrise.gif "Memrise export demo")
</details>

<details>
 <summary>Import on Memrise</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.export.memrise.second.gif "Memrise import demo")
</details>


# Build
The project has some dependencies, at times even unexpected ones, ~~like Electron JS for example~~. And while the currently available build scripts seek for making the build process as easy and effortless as possible by attempting to automatically find and download every single libary that the project depends on, yet there are some steps one is manually to perform prior to proceed with the build process.
## Windows prerequisites

* [CMake](https://cmake.org/download/) (>=3.11.0)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (>=1.6.3)
* C++ compiler whith C++17 support. For an instance, [MSVC Community](https://www.visualstudio.com/free-developer-offers/) will do
* [Boost library](https://www.boost.org/users/download/) (>=1.67)
* ~~[Electron](https://electronjs.org)~~

## Linux

# ToDo
The section is mainly for the author to keep track of the emerging needs/ideas

* Siril server
  * [ ] Add Haskell Conduit HTTP streams to parse dictionaries on-demand and speed up UI page show ups
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
* Opera agent
* Linux build  
