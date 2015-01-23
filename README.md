# Paladin

Paladin is a simple, self-contained program that automatically
generates a cabal scaffold and git repository for new Haskell
projects.  It is a fork of Yann Esposito's project, 'Holy Haskell
Starter', but modified for my own specific needs.

* It creates a project directory and cabal project file using sane
  versions of the provided project name.  For example, if you specify
  your project as "My uber awesome project" the directory (and
  project) will be named 'my-uber-awesome-project'.

* It initializes a cabal sandbox for you

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
➜  paladin git:(master) ✗ ./paladin


          /*
         />
       @/<    --==:: PALADIN ::==--
[\\\\\\(O):::<oooooooooooooooooooooooooooooooooooooooo-
       @\<
         \>
          \*

Creating project structure...
What is your project name? My Project
What is your name? (ysgard)
What is your email? (ysgard@gmail.com)
What is your github account? ("ysgard")
What is your project in less than a dozen words? This is a test
Initialized empty Git repository in /Users/ysgard/Haskell/paladin/my-project/.git/
Writing a default package environment file to
/Users/ysgard/Haskell/paladin/my-project/cabal.sandbox.config
Creating a new sandbox at
/Users/ysgard/Haskell/paladin/my-project/.cabal-sandbox
Resolving dependencies...
Notice: installing into a sandbox located at
/Users/ysgard/Haskell/paladin/my-project/.cabal-sandbox
Configuring my-project-0.1.0.0...
Building my-project-0.1.0.0...
Installed my-project-0.1.0.0
--==:: Done! ::==--
➜  paladin git:(master) ✗➜  paladin git:(master) ✗ ./paladin
~~~

Copyright: (c) Jan Van Uytven, 2015
License: MIT
Maintainer : ysgard@gmail.com
Stability: experimental
Portability: POSIX
