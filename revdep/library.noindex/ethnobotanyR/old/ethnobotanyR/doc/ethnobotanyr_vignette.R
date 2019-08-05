## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

load("ethnobotanydata.rda")


## ---- echo= FALSE--------------------------------------------------------
knitr::kable(head(ethnobotanydata), digits = 2, caption = "First six rows of the example ethnobotany data included with ethnobotanyR")

## ------------------------------------------------------------------------
ethnobotanyR::URs(ethnobotanydata)

## ---- output_format = 'style="border:none"'------------------------------
ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::URs)

## ------------------------------------------------------------------------
ethnobotanyR::URsum(ethnobotanydata)

## ------------------------------------------------------------------------
ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::CIs)

## ------------------------------------------------------------------------
ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::FCs)

## ------------------------------------------------------------------------
ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::NUs)

## ------------------------------------------------------------------------
ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::RFCs)

## ------------------------------------------------------------------------
ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::RIs)

## ------------------------------------------------------------------------
ethnobotanyR::Radial_plot(ethnobotanydata, ethnobotanyR::UVs)

## ------------------------------------------------------------------------
ethnobotanyR::FLs(ethnobotanydata)

## ---- fig.width=7, fig.height=7------------------------------------------
ethnobotanyR::ethnoChord(ethnobotanydata)

## ---- fig.width=7, fig.height=7------------------------------------------
ethnobotanyR::ethnoChordUser(ethnobotanydata)

