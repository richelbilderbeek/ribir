# ribir

Branch|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.com)|[![AppVeyor logo](pics/AppVeyor.png)](https://www.appveyor.com)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---|---
master|[![Build Status](https://travis-ci.com/richelbilderbeek/ribir.svg?branch=master)](https://travis-ci.com/richelbilderbeek/ribir)|[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/richelbilderbeek/ribir?branch=master&svg=true)](https://ci.appveyor.com/project/richelbilderbeek/ribir)|[![codecov.io](https://codecov.io/github/richelbilderbeek/ribir/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/ribir/branch/master)
develop|[![Build Status](https://travis-ci.com/richelbilderbeek/ribir.svg?branch=develop)](https://travis-ci.com/richelbilderbeek/ribir)|[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/richelbilderbeek/ribir?branch=develop&svg=true)](https://ci.appveyor.com/project/richelbilderbeek/ribir)|[![codecov.io](https://codecov.io/github/richelbilderbeek/ribir/coverage.svg?branch=develop)](https://codecov.io/github/richelbilderbeek/ribir/branch/develop)

This package tries to bring phylogenetics code closer to real English.

For example:

```r
if (class(p) == "phylo") print("p is a phylogeny")
```

becomes:

```r
if (is_phylogeny(p)) print("p is a phylogeny")
```

This package 
 * tries to follow all [R-CodingStandard](https://github.com/richelbilderbeek/R-CodingStandard) guidelines
 * can be built without warnings and/or notes
 * will not trigger any warning by `lintr`


## Build status of builds that `ribir` relies on

master|develop|project
---|---|---
[![Build Status](https://travis-ci.com/rsetienne/PBD.svg?branch=master)](https://travis-ci.com/rsetienne/PBD) [![codecov.io](https://codecov.io/github/rsetienne/PBD/coverage.svg?branch=master)](https://codecov.io/github/rsetienne/PBD?branch=master) | [![Build Status](https://travis-ci.com/rsetienne/PBD.svg?branch=develop)](https://travis-ci.com/rsetienne/PBD) [![codecov.io](https://codecov.io/github/rsetienne/PBD/coverage.svg?branch=master)](https://codecov.io/github/rsetienne/PBD?branch=master) | [PBD](https://github.com/rsetienne/PBD)

## I want to collaborate

Great! These are your options:

 * Add an Issue
 * Submit a Pull Request

Pull Requests should
 * try to follow the [R-CodingStandard](https://github.com/richelbilderbeek/R-CodingStandard) guidelines
 * keep the package to be built without warnings and/or notes
 * not trigger any warning by `lintr`

## I think I have found a bug

Awesome! These are your options:

 * Add an Issue, with the test that fails
 * Submit a Pull Request, where the test is added to the `tests/testthat` folder

Pull Requests should
 * try to follow the [R-CodingStandard](https://github.com/richelbilderbeek/R-CodingStandard) guidelines
 * keep the package to be built without warnings and/or notes
 * not trigger any warning by `lintr`

## There's something else I want to say

Sure, just add an Issue. Or send an email.
