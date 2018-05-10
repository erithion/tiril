# Tiril
Tiril is an open source software aiming to assist in a language learning process.

While it still exists in a form of Proof-Of-Concept with lots of ideas still to be implemented, the following components have already been added
* Siril - a server. To date it operates merely dictionaries and handles just a few of them for now:
  * [x] [Google Translate](https://translate.google.com)
  * [x] [Lexin](http://lexin.udir.no)
* Viril - a [VideoLAN](https://github.com/videolan) plugin to translate subtitles. Among supported formats are:
  * [x] Embedded and external SubRip 
  * [ ] ASS/SSA possibility has been confirmed and awaits implementation
  
Below you can see a demonstration of how Tiril already strives to be helpful to you by having taught VLC media player to do something new in spite of having grown so small yet.

<details>
 <summary>Click to see the image</summary>
  
![alt text](https://github.com/erithion/tiril/raw/master/doc/tiril.gif "Demo")
</details>

# Build
The project has some dependencies, at times even unexpected ones, like Electron JS for example. And while the currently available build scripts seek for making the build process as easy and effortless as possible, attempting to automatically find and download every single libary that the project depends on, yet there are some steps one manually to perform prior to proceed with the build.
## Windows prerequisites

* [CMake](https://cmake.org/download/) (>=3.11.0)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (>=1.6.3)
* C++ compiler which supports C++17. For an instance, [MSVC Community](https://www.visualstudio.com/free-developer-offers/)
* [Boost library](https://www.boost.org/users/download/) (>=1.66)
* [Electron](https://electronjs.org)

## Linux
