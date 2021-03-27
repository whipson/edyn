
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Utterance Emotion Dynamics

This is an R package for computing Utterance Emotion Dynamics. For
details on Utterance Emotion Dynamics, please see this paper in arXiv:

[Hipson, W., & Mohammad, S. (2021). Emotion Dynamics in Movie Dialogues.
arXiv preprint arXiv:2103.01345.](https://arxiv.org/abs/2103.01345)

Steps for computing UED with user-supplied data:

1.  Download and install R (<https://cran.r-project.org/>)
2.  Install dependencies: dplyr, purrr, readr, stringr, tidytext,
    tibbletime.
3.  Open file eacl-ued-code.Rmd
4.  Replace line 38 with a .csv file. Each row must be an utterance
    (e.g., tweet, dialogue, etc.).
5.  If applicable, supply an id (e.g., participant id, speaker id, etc.)
    and a time variable.
6.  Choose summarise = TRUE if you only want summarised output for each
    id. Otherwise, each row in output will be VA word uttered.

The IMSDb movie dialogue data set is free to use for educational
purposes.
