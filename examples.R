#'===============================================================================
#' You will need to have the tidyverse, raw and janitor packages installed. We 
#' only use one function from janitor, so don't need to load it into memory.
library(tidyverse)
library(raw)

#'===============================================================================
#' Here we're setting some defaults for ggplot. The sizes are to make the display
#' easier to see in a large conference setting. You may want to omit this for 
#' display on a laptop.
old_theme <- theme_set(theme_minimal(base_size = 16))
theme_update(
    panel.grid.minor = element_blank()
)
update_geom_defaults("point", list(size = 3))

#'===============================================================================
#' This function will calculate incremental paid amounts, some LDFs, construct
#' factor variables for lag and accident year and set a flag for the upper part
#' of the triangle. Creating factors prevents ggplot from treating the lag and 
#' accident years as real numbers. In these examples, we want them to behave
#' like categories.
augment_triangle <- function(tbl_in) {

    tbl_in |> 
        janitor::clean_names() |> 
        group_by(accident_year) |> 
        arrange(lag, .by_group = TRUE) |> 
        mutate(
            lag_factor = as.factor(lag),
            ay_factor = as.factor(accident_year),
            prior_paid = lag(cumulative_paid),
            incremental_paid = coalesce(
                cumulative_paid - prior_paid,
                cumulative_paid
            ),
            ldf_paid = incremental_paid / prior_paid,
            upper = development_year <= 1997
        ) |> 
        ungroup()
    
}

tbl_taylor_mcguire <- raw::NJM_WC |> 
    augment_triangle()

#'===============================================================================
#' Some exploratory visualization. The last one illustrates the model we want to 
#' fit. Only 12-24 is shown, but it generalizes to other lags.
tbl_taylor_mcguire |> 
    filter(upper) |> 
    ggplot(aes(lag_factor, cumulative_paid, group = ay_factor, color = ay_factor)) + 
    geom_point() + 
    geom_line() + 
    scale_y_continuous(labels = scales::comma) + 
    labs(
        title = "Cumulative paid loss development",
        x = "Development lag",
        y = "Cumulative paid loss"
    ) + 
    theme(legend.position = 'none')

tbl_taylor_mcguire |> 
    filter(upper, lag <= 2, accident_year < 1997) |> 
    ggplot(aes(lag_factor, cumulative_paid, group = ay_factor)) + 
    geom_point() + 
    geom_line() + 
    scale_y_continuous(labels = scales::comma) + 
    labs(
        title = "Cumulative paid loss development 12-24 months",
        x = "Development lag",
        y = "Cumulative paid loss"
    ) + 
    theme(legend.position = 'none')

tbl_taylor_mcguire |> 
    filter(upper, lag == 2) |> 
    ggplot(aes(prior_paid, cumulative_paid)) + 
    geom_point() + 
    scale_y_continuous(labels = scales::comma, limits = c(75e3, 125e3)) + 
    scale_x_continuous(labels = scales::comma) + 
    labs(
        title = "Cumulative paid loss development 12-24 months",
        x = "Prior cumulative paid",
        y = "Cumulative paid loss"
    ) + 
    theme(legend.position = 'none')

tbl_taylor_mcguire |> 
    filter(upper, lag == 2) |> 
    ggplot(aes(prior_paid, cumulative_paid)) + 
    geom_point() + 
    geom_smooth(formula = y ~ 0 + x, method = lm, se = FALSE) +
    scale_y_continuous(labels = scales::comma, limits = c(75e3, 125e3)) + 
    scale_x_continuous(labels = scales::comma) + 
    labs(
        title = "Cumulative paid loss development 12-24 months",
        x = "Prior cumulative paid",
        y = "Cumulative paid loss"
    ) + 
    theme(legend.position = 'none')

#'==============================================================================
#' And here's our model. It's the chain ladder, but it's also a GLM!!!
model_one <- tbl_taylor_mcguire |> 
    filter(upper, lag > 1) |> 
    glm(
        formula = cumulative_paid ~ 0 + prior_paid:lag_factor,
        family = gaussian)

summary(model_one)

## We can also use a Poisson distribution
model_two <- tbl_taylor_mcguire |> 
    filter(upper, lag > 1) |> 
    glm(formula = cumulative_paid ~ 0 + lag_factor,
        offset = log(prior_paid),
        family = poisson)

## It's the volume-weighted average!!
model_two |> 
    coef() |> 
    exp()

## And we can use different predicts
model_three <- tbl_taylor_mcguire |> 
    filter(upper, lag > 1) |> 
    glm(formula = cumulative_paid ~ 0 + lag_factor,
        offset = log(net_ep),
        family = poisson)

summary(model_three)

#'==============================================================================
#' Here, we visualize an intercept. It does not appear all that useful for this
#' set of data. (Note that we're using data visualization to posit a model!)

tbl_taylor_mcguire |> 
    filter(upper, lag >= 2) |> 
    ggplot(aes(prior_paid, incremental_paid, color = lag_factor)) + 
    geom_point() + 
    geom_smooth(method = lm, se = FALSE, linetype = 'dashed') +
    geom_smooth(method = lm, se = FALSE, formula = y ~ 0 + x) +
    facet_wrap(~ lag_factor, scales = 'free') +
    scale_y_continuous(labels = scales::comma) + 
    scale_x_continuous(labels = scales::comma) + 
    labs(
        title = "Incremental payments against prior cumulative paid",
        subtitle = 'Dashed line contains an intercept',
        x = "Prior cumulative paid",
        y = "Incremental paid loss"
    ) + 
    theme(legend.position = 'none')

## Explore another data set
tbl_other <- raw::wkcomp |>
    filter(str_starts(Company, 'Allstate')) |> 
    augment_triangle()

tbl_other |> 
    filter(upper, lag >= 2) |> 
    ggplot(aes(prior_paid, incremental_paid, color = lag_factor)) + 
    geom_point() + 
    geom_smooth(method = lm, se = FALSE, linetype = 'dashed') +
    geom_smooth(method = lm, se = FALSE, formula = y ~ 0 + x) +
    facet_wrap(~ lag_factor, scales = 'free') +
    scale_y_continuous(labels = scales::comma) + 
    scale_x_continuous(labels = scales::comma) + 
    labs(
        title = "Incremental payments against prior cumulative paid",
        subtitle = 'Dashed line contains an intercept',
        x = "Prior cumulative paid",
        y = "Incremental paid loss"
    ) + 
    theme(legend.position = 'none')

## Do we really need a separate coefficient for each lag? They kinda look the same
tbl_other |> 
    filter(upper, lag >= 7) |> 
    ggplot(aes(prior_paid, incremental_paid)) + 
    geom_point(aes(color = lag_factor)) + 
    geom_smooth(method = lm, se = FALSE, linetype = 'dashed') +
    geom_smooth(method = lm, se = FALSE, formula = y ~ 0 + x) +
    scale_y_continuous(labels = scales::comma) + 
    scale_x_continuous(labels = scales::comma) + 
    labs(
        title = "Incremental payments against prior cumulative paid",
        subtitle = "Lags 7 and higher, dashed line contains an intercept",
        x = "Prior cumulative paid",
        y = "Incremental paid loss"
    ) + 
    theme(legend.position = 'none')

## Our last model (not shown during the presentation) has a flat/additive factor
## for lags 7 and higher. Might work better than chain ladder for the whole
## triangle!
model_four <- tbl_other |> 
    filter(upper, lag >= 7) |> 
    glm(formula = cumulative_paid ~ 1,
        family = poisson)

summary(model_four)

#'==============================================================================
#' I had some supplementary material about treating the incremental or cumulative
#' as a response. TL;DR you should use incremental. Hit me up on LinkedIn if 
#' you're curious about that. Hint: hypothesis testing will default to having
#' coefficients equal to zero, not one. Another hint: A + B is _always_ 
#' correlated with B.
tbl_taylor_mcguire |> 
    filter(upper, lag == 2) |> 
    ggplot(aes(prior_paid, incremental_paid)) + 
    geom_point() + 
    scale_y_continuous(labels = scales::comma, limits = c(30e3, 60e3)) + 
    scale_x_continuous(labels = scales::comma) + 
    labs(
        title = "Incremental paid loss development",
        x = "Prior cumulative paid",
        y = "Incremental paid loss"
    ) + 
    theme(legend.position = 'none')

tbl_taylor_mcguire |> 
    filter(upper, lag == 2) |> 
    ggplot(aes(prior_paid, cumulative_paid)) + 
    geom_point() + 
    geom_smooth(formula = y ~ 0 + x, method = lm) +
    scale_y_continuous(labels = scales::comma, limits = c(75e3, 125e3)) + 
    scale_x_continuous(labels = scales::comma) + 
    labs(
        title = "Cumulative paid loss development",
        x = "Prior cumulative paid",
        y = "Cumulative paid loss"
    ) + 
    theme(legend.position = 'none')

tbl_taylor_mcguire |> 
    filter(upper, lag == 2) |> 
    ggplot(aes(prior_paid, incremental_paid)) + 
    geom_point() + 
    geom_smooth(formula = y ~ 0 + x, method = lm) +
    scale_y_continuous(labels = scales::comma, limits = c(30e3, 60e3)) + 
    scale_x_continuous(labels = scales::comma) + 
    labs(
        title = "Incremental paid loss development",
        x = "Prior cumulative paid",
        y = "Incremental paid loss"
    ) + 
    theme(legend.position = 'none')
