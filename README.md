
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edyn

Emotion Dynamics in Movie Dialogues

Compute Utterance Emotion Dynamics (UED) from a series of individual’s
or multiple individuals’ text.

[Hipson, W., & Mohammad, S. (2021). Emotion Dynamics in Movie Dialogues.
arXiv preprint arXiv:2103.01345.](https://arxiv.org/abs/2103.01345)

## Installation

``` r
# install.packages("devtools")
devtools::install_github("whipson/edyn")
```

## Usage

Load library and use sample data of IMSDb (script database).

``` r
library(edyn)
#> Warning: replacing previous import 'dplyr::collapse' by 'glue::collapse' when
#> loading 'edyn'
#> Warning: replacing previous import 'purrr::list_along' by 'rlang::list_along'
#> when loading 'edyn'
#> Warning: replacing previous import 'purrr::invoke' by 'rlang::invoke' when
#> loading 'edyn'
#> Warning: replacing previous import 'purrr::flatten_raw' by 'rlang::flatten_raw'
#> when loading 'edyn'
#> Warning: replacing previous import 'purrr::modify' by 'rlang::modify' when
#> loading 'edyn'
#> Warning: replacing previous import 'purrr::as_function' by 'rlang::as_function'
#> when loading 'edyn'
#> Warning: replacing previous import 'purrr::flatten_dbl' by 'rlang::flatten_dbl'
#> when loading 'edyn'
#> Warning: replacing previous import 'purrr::flatten_lgl' by 'rlang::flatten_lgl'
#> when loading 'edyn'
#> Warning: replacing previous import 'purrr::flatten_int' by 'rlang::flatten_int'
#> when loading 'edyn'
#> Warning: replacing previous import 'purrr::%@%' by 'rlang::%@%' when loading
#> 'edyn'
#> Warning: replacing previous import 'purrr::flatten_chr' by 'rlang::flatten_chr'
#> when loading 'edyn'
#> Warning: replacing previous import 'purrr::splice' by 'rlang::splice' when
#> loading 'edyn'
#> Warning: replacing previous import 'purrr::flatten' by 'rlang::flatten' when
#> loading 'edyn'
#> Warning: replacing previous import 'purrr::prepend' by 'rlang::prepend' when
#> loading 'edyn'
#> Warning: replacing previous import 'dplyr::filter' by 'stats::filter' when
#> loading 'edyn'
#> Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading
#> 'edyn'
#> Warning: replacing previous import 'stats::filter' by 'tibbletime::filter' when
#> loading 'edyn'

data("imsdb")
head(imsdb)
#> # A tibble: 6 x 7
#>   title      turn speaker text                 genre     writers            date
#>   <chr>     <dbl> <chr>   <chr>                <chr>     <chr>             <dbl>
#> 1 In the L…   118 SIMON   "But I was going to… Comedy    Jesse Armstrong,…  2009
#> 2 Watchmen     61 DAN     "No, uh, just tell … Action,F… David Hayter,Ale…    NA
#> 3 Rise of …   320 TOOTH   "It's not the teeth… Animatio… David Lindsay-Ab…    NA
#> 4 Beauty a…   194 GASTON  "Don't thank me, Ma… Family,F… Stephen Chbosky,…  2016
#> 5 Siege, T…   879 ELISE   "What are you doing… Action,T… Lawrence Wright,…    NA
#> 6 Event Ho…   386 WEIR    "What you saw could… Horror,S… Philip Eisner,     1997
```

Compute UED

``` r
ued <- ued(imsdb, id = speaker, time = turn, text = text, roll_avg = 10, min_count = 200, summarise = TRUE)
head(ued[, c("speaker", "avg_emo_var", "rise_rate", "disp_count")])
#> # A tibble: 6 x 4
#>   speaker avg_emo_var rise_rate disp_count
#>   <chr>         <dbl>     <dbl>      <dbl>
#> 1 AARON        0.0675    0.0487         29
#> 2 ABBY         0.0670    0.0416         37
#> 3 ABEL         0.0617    0.0731         18
#> 4 ACE          0.0699    0.0380         38
#> 5 ADAM         0.0647    0.0443        110
#> 6 ADELE        0.0588    0.0487         26
```
