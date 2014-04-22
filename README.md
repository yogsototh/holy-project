# Holy Project

Holy Project is an application which ask the user some questions
and create files to help you starting a new Haskell project.

There are example for tests using HUnit and SmallCheck.
It initialize git, use cabal sandboxes, and provide two useful scripts:
`auto-update` and `interact`.

You could check this [blog post](http://yannesposito.com/Scratch/en/blog/Holy-Haskell-Starter/) to learn how it was made.

## Install

~~~
cabal update && cabal install holy-project
~~~

Then simply:

~~~
cd ~/my-projects
holy-projects
~~~~

And follows the instructions.

## Thanks

Thanks to [Kirill](https://github.com/qrilka) to have taken the time to fix it.
