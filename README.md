# Paladin

Paladin is a simple, self-contained program that automatically
generates a stack scaffold and git repository for new Haskell
projects.  It is a fork of Yann Esposito's project, 'Holy Haskell
Starter', but modified for my own specific needs.

* It creates a project directory and stack project file using sane
  versions of the provided project name.  For example, if you specify
  your project as "My uber awesome project" the directory (and
  project) will be named 'my-uber-awesome-project'.

* It initializes stack for you

* It provides an initial source file, 'src/Main.hs' and gives it a
  module name derived from your project.   Following the example
  above, the module will be 'MyUberAwesomeProject'

* It initializes a git repo for you.

* It dumps a README containing a few details of your project, as well as
  an MIT license.

* Unlike Holy, which is a cabal package, Paladin's templates are built
  into the executable and thus only the executable is needed to
  generate the scaffolding.  At this point, the scaffolding is not customizable.

Paladin is based on Yann Esposito's 'Holy-Haskell-Starter', which is here:

   http://yannesposito.com/Scratch/en/blog/Holy-Haskell-Starter/

With only customizations because my needs are different (and admittedly,
much simpler!).  It doesn't have any testing frameworks set up, and
unlike holy, it isn't multithreaded.  

Example of use:

~~~
➜  Haskell  paladin


          /*
         />
       @/<    --==:: PALADIN ::==--
[\\\\\\(O):::<oooooooooooooooooooooooooooooooooooooooo-
       @\<
         \>
          \*

Creating project structure...
What is your project name? My Project
Checking my-project
What is your name? (Ysgard)
What is your email? (ysgard@gmail.com)
What is your github account? ("ysgard")
What is your project in less than a dozen words? This is a test
Initialized empty Git repository in /Users/juytven/Dropbox/Haskell/my-project/.git/
Writing default config file to: /Users/juytven/Dropbox/Haskell/my-project/stack.yaml
Basing on cabal files:
- /Users/juytven/Dropbox/Haskell/my-project/my-project.cabal

Checking against build plan lts-3.12
Selected resolver: lts-3.12
Wrote project config to: /Users/juytven/Dropbox/Haskell/my-project/stack.yaml
my-project-0.1.0.0: configure
Configuring my-project-0.1.0.0...
my-project-0.1.0.0: build
Preprocessing executable 'my-project' for my-project-0.1.0.0...
[1 of 1] Compiling Main             ( src/Main.hs, .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/my-project/my-project-tmp/Main.o )
Linking .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/my-project/my-project ...
my-project-0.1.0.0: install
Installing executable(s) in
/Users/juytven/Dropbox/Haskell/my-project/.stack-work/install/x86_64-osx/lts-3.12/7.10.2/bin
You fight with the strength of many men, sir Knight...
--==:: Done! ::==--
➜  Haskell
~~~

Copyright: (c) Jan Van Uytven, 2015
License: MIT
Maintainer : ysgard@gmail.com
Stability: experimental
Portability: POSIX
