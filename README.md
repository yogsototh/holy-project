> This project has two homes.
> It is ok to work in github, still, for a better decentralized web
> please consider contributing (issues, PR, etc...) throught:
>
> https://gitlab.esy.fun/yogsototh/holy-project

---


# Holy Project

<a href="http://hackage.haskell.org/package/holy-project">
<img alt="Hackage" src="https://img.shields.io/hackage/v/holy-project.svg"/>
</a>

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
