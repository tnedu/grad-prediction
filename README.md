## Background

The Department has a policy emphasis on ensuring that all high school graduates in Tennessee are prepared to
pursue a post-secondary credential. To that end, it has introduced a ready graduates metric in
accountability which currently captures on-time graduates with an ACT composite score of 21 or higher. In
future years, it will also credit students for completing early postsecondary opportunities, earning
industry certification, or meeting specified criteria on the AFQT.

In light of the Department's emphasis on ready graduates, I use machine learning to
predict ready graduates based on observable information about students as they are in middle school.

This work follows a similar initiative in [Wisconsin](https://dpi.wi.gov/ews/dropout) to create a Dropout Early Warning System.

## Predictors and Outcomes

We use the following information about students in 6th, 7th, 8th grades:

- TCAP Scores
- Disciplinary Issues (Suspensions/Expulsions/Reasons)
- Absenteeism (# days)
- Mobility (# of schools enrolled)
- School aggregates of above

The outcome of interest is on-time graduation (within four years and a summer) with an ACT composite score
of 21 or higher. As data become available over time via the state's longitudinal data system, we will also
look to predict measures of enrollment and persistence in postsecondary education.

## Models

I currently use the following models (available via R's `caret` package):

- `gbm` (gradient boosting machine)
- `rpart` (recursive partitioning/decision tree)
- `rlda` (regularized linear discriminant analysis)
- `nnet` (neural network)
- `xgbLinear` (extreme gradient boosting w/ logistic regression)
- `xgbTree` (extreme gradient boosting w/ trees)

For a more detailed look at methods and results, please see my latest
[presentation](https://github.com/tnedu/grad-prediction/blob/master/presentations/presentation.pdf).
