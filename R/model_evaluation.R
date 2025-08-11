#' ModelEvaluator
#'
#' An R6 class to evaluate non-linear regression models.
#' Includes tools to generate tables of statistics and visualizations
#' to compare models against data.
#'
#' @export
ModelEvaluator <- R6::R6Class(
  "ModelEvaluator",
  public = list(
    #' @field fit_results A list of fitted model objects.
    fit_results = NULL,

    #' @field evaluation_metrics A data.table containing model evaluation metrics.
    evaluation_metrics = NULL,

    #' @field plots A list of visualizations comparing models against data.
    plots = list(),

    #' @field data The original dataset used for fitting models.
    data = NULL,

    #' Initialize the ModelEvaluator class
    #'
    #' @param fit_results A list of fitted model objects (e.g., output from NonLinearFitter).
    #' @param data The original dataset used for fitting models.
    #' @return A new instance of the ModelEvaluator class.
    initialize = function(fit_results, data) {
      if (!is.list(fit_results)) {
        message("fit_results must be a list of model objects.")
        return(NULL)
      }
      if (!data.table::is.data.table(data)) {
        message("data must be a data.table.")
        return(NULL)
      }
      self$fit_results <- fit_results
      self$data <- data
    },

    #' @description Computes and summarizes key performance metrics for all fitted models,
    #' including goodness-of-fit statistics, residual standard errors, and information criteria.
    #' The metrics provide a comprehensive evaluation of each model's performance on the
    #' given dataset.
    #'
    #' @details This method evaluates each fitted model by calculating various metrics such as
    #' Akaike Information Criterion (AIC), Bayesian Information Criterion (BIC), residual
    #' standard error, and R-squared values. It ensures compatibility across all models
    #' and gracefully handles cases where a model fails to fit properly by excluding it from
    #' the final summary.
    #'
    #' @param y_col target variable
    #' @param x_col x variable
    #' @return A data.table of evaluation metrics with fitted equations.
    #' @export
    generate_metrics = function(
    y_col,
    x_col,
    group_vars = NULL,      # NULL => global only; "auto" => infer from model; character() => explicit
    combine_groups = TRUE   # when multiple group vars: TRUE => group by their full combination
    ) {
      if (is.null(self$fit_results) || length(self$fit_results) == 0) {
        message("No fitted models to evaluate."); return(NULL)
      }
      stopifnot(all(c(y_col, x_col) %in% names(self$data)))

      DTall <- data.table::as.data.table(self$data)

      # ---- helpers ----
      wmean <- function(x, w) sum(w * x) / sum(w)

      detect_group_vars <- function(fit) {
        if (is.null(fit$cat)) return(character(0))
        cand <- c(
          if (!is.null(fit$cat$shift_vars)) fit$cat$shift_vars else character(0),
          if (!is.null(fit$cat$param_raw_vars)) unlist(fit$cat$param_raw_vars, use.names = FALSE) else character(0)
        )
        unique(cand[cand %in% names(DTall)])
      }

      compute_metrics_for_idx <- function(fit, idx, model_name, extra_cols = character(0), extra_vals = NULL) {
        ND_slice <- DTall[idx]
        ND_slice <- private$ensure_required_features(fit = fit, ND = ND_slice)  # strict; errors if missing

        # scale x and predict (decomposed: baselines + param-mods + shifts)
        x_scaled <- (ND_slice[[x_col]] - fit$scale_params$min_x) / fit$scale_params$scale_factor_x
        pred_std <- private$predict_std_from_decomposed(fit = fit, data = ND_slice, x_scaled = x_scaled)
        predicted <- fit$back_transform(pred_std, fit$scale_params)

        observed  <- ND_slice[[y_col]]
        residuals <- observed - predicted

        # weights (if present on fit)
        w <- fit$weights
        if (!is.null(w)) {
          w <- as.numeric(w)[idx]
          if (length(w) != length(observed)) w <- NULL
          if (!is.null(w)) {
            bad <- !is.finite(w) | w < 0
            if (any(bad)) w[bad] <- 0
            if (sum(w) == 0) w <- NULL
          }
        }

        # metrics
        if (is.null(w)) {
          rss  <- sum(residuals^2)
          n    <- length(observed)
          ybar <- mean(observed)
          tss  <- sum((observed - ybar)^2)
          rmse <- sqrt(mean(residuals^2))
          mae  <- mean(abs(residuals))
          mape <- mean(ifelse(observed == 0, NA_real_, abs(residuals / observed) * 100))
          smape<- mean(200 * abs(residuals) / (abs(observed) + abs(predicted)))
        } else {
          rss  <- sum(w * residuals^2)
          n    <- length(observed)
          ybar <- wmean(observed, w)
          tss  <- sum(w * (observed - ybar)^2)
          rmse <- sqrt(wmean(residuals^2, w))
          mae  <- wmean(abs(residuals), w)
          mape <- wmean(ifelse(observed == 0, NA_real_, abs(residuals / observed) * 100), w)
          smape<- wmean(200 * abs(residuals) / (abs(observed) + abs(predicted)), w)
        }

        r2 <- if (tss == 0) NA_real_ else 1 - rss / tss
        k  <- length(fit$coefficients)
        s2 <- rss / n
        loglik <- if (s2 <= 0 || !is.finite(s2)) NA_real_ else -0.5 * n * (log(2 * pi) + log(s2) + 1)
        aic <- if (is.na(loglik)) NA_real_ else -2 * loglik + 2 * k
        bic <- if (is.na(loglik)) NA_real_ else -2 * loglik + log(n) * k
        adjr2 <- if (is.na(r2) || n - k - 1 <= 0) NA_real_ else 1 - (1 - r2) * (n - 1) / (n - k - 1)

        row <- data.table::data.table(
          `Model Name`   = model_name,
          Formula        = gsub("\\s+", " ", paste(deparse(fit$formula), collapse = "")),
          AIC            = aic,
          BIC            = bic,
          Resid_Std_Err  = rmse,
          MAE            = mae,
          MAPE           = mape,
          sMAPE          = smape,
          R_Sq           = r2,
          Adj_R2         = adjr2,
          N              = n
        )

        # prepend group columns if provided
        if (length(extra_cols) > 0) {
          if (is.null(extra_vals)) {
            extra_vals <- ND_slice[1, extra_cols, with = FALSE]
          }
          row <- cbind(extra_vals, row)
          data.table::setcolorder(row, c(names(extra_vals), setdiff(names(row), names(extra_vals))))
        }

        row
      }

      # ---- build metrics (global + optional grouped) ----
      out_all_models <- lapply(names(self$fit_results), function(model_name) {
        fit <- self$fit_results[[model_name]]
        if (is.null(fit)) return(NULL)

        # global metrics
        res_list <- list(
          compute_metrics_for_idx(fit, idx = seq_len(nrow(DTall)), model_name = model_name)
        )

        # grouped metrics
        gv <- group_vars
        if (is.character(group_vars) && length(group_vars) == 1L && identical(group_vars, "auto")) {
          gv <- detect_group_vars(fit)
        }
        if (!is.null(gv) && length(gv) > 0) {
          gv <- gv[gv %in% names(DTall)]
          if (length(gv) == 1L) {
            # one group variable
            # group by that column, collect indices as list
            idx_dt <- DTall[, .(idx = list(.I)), by = get(gv)]
            data.table::setnames(idx_dt, old = names(idx_dt)[1], new = gv)

            # iterate groups
            grp_rows <- lapply(seq_len(nrow(idx_dt)), function(i) {
              idx_vec <- idx_dt$idx[[i]]
              extra   <- idx_dt[i, gv, with = FALSE]
              compute_metrics_for_idx(fit, idx_vec, model_name, extra_cols = gv, extra_vals = extra)
            })
            res_list[[length(res_list) + 1L]] <- data.table::rbindlist(grp_rows, fill = TRUE)

          } else {
            if (isTRUE(combine_groups)) {
              # group by full combination of group_vars
              idx_dt <- DTall[, .(idx = list(.I)), by = mget(gv)]

              grp_rows <- lapply(seq_len(nrow(idx_dt)), function(i) {
                idx_vec <- idx_dt$idx[[i]]
                extra   <- idx_dt[i, setdiff(names(idx_dt), "idx"), with = FALSE]
                compute_metrics_for_idx(fit, idx_vec, model_name, extra_cols = names(extra), extra_vals = extra)
              })
              res_list[[length(res_list) + 1L]] <- data.table::rbindlist(grp_rows, fill = TRUE)
            } else {
              # per-variable metrics, then stack
              per_var <- lapply(gv, function(v) {
                idt <- DTall[, .(idx = list(.I)), by = get(v)]
                data.table::setnames(idt, old = names(idt)[1], new = v)
                rows <- lapply(seq_len(nrow(idt)), function(i) {
                  idx_vec <- idt$idx[[i]]
                  extra   <- idt[i, v, with = FALSE]
                  compute_metrics_for_idx(fit, idx_vec, model_name, extra_cols = v, extra_vals = extra)
                })
                data.table::rbindlist(rows, fill = TRUE)
              })
              res_list[[length(res_list) + 1L]] <- data.table::rbindlist(per_var, fill = TRUE)
            }
          }
        }

        data.table::rbindlist(res_list, fill = TRUE)
      })

      self$evaluation_metrics <- data.table::rbindlist(out_all_models, fill = TRUE)
      self$evaluation_metrics
    },

    #' @description Creates visualizations comparing the fitted models against the observed data
    #' to assess their fit and predictive behavior. The plots include the fitted curves
    #' overlaid on the original data points for easy comparison.
    #'
    #' @details This method generates a comparison plot for each fitted model, allowing
    #' users to visually assess how well the models align with the observed data. It
    #' supports customization options such as theming and dynamic adjustment of the
    #' x-axis range. Models that fail to fit are gracefully excluded, ensuring clean
    #' and informative outputs.
    #'
    #' @param data A `data.table` or `data.frame` containing the dataset used for evaluation.
    #' @param x_col A string specifying the name of the x variable in the dataset.
    #' @param y_col A string specifying the name of the y variable in the dataset.
    #' @param group_vars Character vector of column names
    #' @param theme Echarts theme
    #' @return An `echarts4r` plot showing observed vs. predicted data, with weighted comparisons if available.
    #' @export
    generate_comparison_plot = function(
    data,
    x_col,
    y_col,
    group_vars = NULL,
    theme = "westeros"
    ) {
      if (is.null(self$fit_results) || length(self$fit_results) == 0) {
        message("No fitted models to evaluate.")
        return(NULL)
      }
      if (!all(c(x_col, y_col) %in% names(data))) {
        message("x_col and y_col must exist in the dataset.")
        return(NULL)
      }

      # refresh metrics so R^2 aligns with current scaling/weights
      metrics <- tryCatch(self$generate_metrics(y_col = y_col, x_col = x_col), error = function(e) NULL)
      if (is.null(metrics) || nrow(metrics) == 0) {
        message("No metrics available for the models.")
        return(NULL)
      }

      # observed data (copy to avoid by-ref issues)
      DT_obs <- data[, .SD, .SDcols = c(x_col, y_col, group_vars)]

      self$plots <- setNames(lapply(names(self$fit_results), function(model_name) {
        fit <- self$fit_results[[model_name]]
        if (is.null(fit)) return(NULL)

        # 1) Build engineered features on the provided data
        ND <- tryCatch({
          private$ensure_required_features(fit = fit, ND = data)
          # ensure_required_features(fit = fit, ND = data)
        }, error = function(e) {
          message(sprintf("Error preparing features for model '%s': %s", model_name, e$message))
          return(NULL)
        })
        if (is.null(ND)) return(NULL)

        # 2) Scale x exactly as fitter did
        x_scaled <- (DT_obs[[x_col]] - fit$scale_params$min_x) / fit$scale_params$scale_factor_x

        # 3) Predict via decomposed params (baseline + param mods + shifts)
        preds_std <- tryCatch({
          private$predict_std_from_decomposed(
          # predict_std_from_decomposed(
            fit  = fit,
            data = ND,
            x_scaled = x_scaled
          )
        }, error = function(e) {
          message(sprintf("Error predicting for model '%s': %s", model_name, e$message))
          return(NULL)
        })
        if (is.null(preds_std)) return(NULL)

        y_pred <- fit$back_transform(predictions = preds_std, scale_params = fit$scale_params)

        # 4) Assemble combined and sort by x
        combined <- cbind(DT_obs, y_pred)

        # 5) Pull R^2 safely
        r_sq <- tryCatch({
          val <- metrics[`Model Name` == model_name, R_Sq]
          if (length(val)) val[[1]] else NA_real_
        }, error = function(e) NA_real_)

        # 6) Build plot
        if (is.null(group_vars)) {
          plt <- combined |>
            echarts4r::e_charts_(x_col) |>
            echarts4r::e_scatter_(y_col, name = "Observed") |>
            echarts4r::e_line(y_pred, name = model_name, smooth = TRUE, showSymbol = FALSE) |>
            echarts4r::e_theme(name = theme) |>
            echarts4r::e_title(
              text = paste("Model Fit:", model_name),
              subtext = paste0("R-Sq: ", ifelse(is.na(r_sq), "NA", format(round(r_sq, 4), nsmall = 4)))
            ) |>
            echarts4r::e_datazoom(x_index = c(0, 1)) |>
            echarts4r::e_datazoom(y_index = c(0, 1)) |>
            echarts4r::e_toolbox_feature(feature = c("saveAsImage", "dataZoom")) |>
            echarts4r::e_brush() |>
            echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
            echarts4r::e_axis_(
              axis = "y",
              name = y_col,
              nameLocation = "middle",
              nameTextStyle = list(
                fontSize = 16,
                padding = 50)) |>
            echarts4r::e_axis_(
              axis = "x",
              name = x_col,
              nameLocation = "middle",
              nameTextStyle = list(
                fontSize = 16,
                padding = 25))
          plt

        } else {

          if (length(group_vars) == 1) {
            plt <- echarts4r::e_charts_(data = combined |> dplyr::group_by(get(group_vars)), x = x_col) |>
              echarts4r::e_scatter_(y_col, name = "Observed") |>
              echarts4r::e_line(y_pred, name = model_name, smooth = TRUE, showSymbol = FALSE) |>
              echarts4r::e_theme(name = theme) |>
              echarts4r::e_title(
                text = paste("Model Fit:", model_name),
                subtext = paste0("R-Sq: ", ifelse(is.na(r_sq), "NA", format(round(r_sq, 4), nsmall = 4)))
              ) |>
              echarts4r::e_datazoom(x_index = c(0, 1)) |>
              echarts4r::e_datazoom(y_index = c(0, 1)) |>
              echarts4r::e_toolbox_feature(feature = c("saveAsImage", "dataZoom")) |>
              echarts4r::e_brush() |>
              echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
              echarts4r::e_legend(show = TRUE, orient = "vertical", right = 0, top = "middle", padding = 50) |>
              echarts4r::e_axis_(
                axis = "y",
                name = y_col,
                nameLocation = "middle",
                nameTextStyle = list(
                  fontSize = 16,
                  padding = 50)) |>
              echarts4r::e_axis_(
                axis = "x",
                name = x_col,
                nameLocation = "middle",
                nameTextStyle = list(
                  fontSize = 16,
                  padding = 25))
            plt

          } else {

            combined[, CombinedGroups := do.call(paste, c(.SD, sep = " - ")), .SDcols = group_vars]
            plt <- echarts4r::e_charts_(data = combined |> dplyr::group_by(CombinedGroups), x = x_col) |>
              echarts4r::e_scatter_(y_col) |>
              echarts4r::e_line(y_pred, smooth = TRUE, showSymbol = FALSE) |>
              echarts4r::e_theme(name = theme) |>
              echarts4r::e_title(
                text = paste("Model Fit:", model_name),
                subtext = paste0("R-Sq: ", ifelse(is.na(r_sq), "NA", format(round(r_sq, 4), nsmall = 4)))
              ) |>
              echarts4r::e_datazoom(x_index = c(0, 1)) |>
              echarts4r::e_datazoom(y_index = c(0, 1)) |>
              echarts4r::e_toolbox_feature(feature = c("saveAsImage", "dataZoom")) |>
              echarts4r::e_brush() |>
              echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
              echarts4r::e_legend(show = TRUE, orient = "vertical", right = 0, top = "middle", padding = 50) |>
              echarts4r::e_axis_(
                axis = "y",
                name = y_col,
                nameLocation = "middle",
                nameTextStyle = list(
                  fontSize = 16,
                  padding = 50)) |>
              echarts4r::e_axis_(
                axis = "x",
                name = x_col,
                nameLocation = "middle",
                nameTextStyle = list(
                  fontSize = 16,
                  padding = 25))
            plt
          }
        }

      }), names(self$fit_results))

      self$plots
    }
  ),

  private = list(
    # Build engineered features needed by this fit on ND (strict: raw cats must exist)
    ensure_required_features = function(fit, ND) {
      ND <- data.table::as.data.table(ND)

      # engineered cols required by coef names (text after first ":")
      cn <- names(fit$coefficients)
      required_eng <- unique(sub("^[^:]+:", "", grep("^(shift:|[A-Za-z]+:)", cn, value = TRUE)))
      required_eng <- required_eng[required_eng != "baseline"]
      if (length(required_eng) == 0) return(ND)

      # Prefer attached encoders
      if (!is.null(fit$encoders) && length(fit$encoders) > 0) {
        for (enc in fit$encoders) ND <- enc$transform(ND)
      }

      # Fallback: use maps saved in fit$cat (same cols your categorical_encoding() produced)
      still <- setdiff(required_eng, names(ND))
      if (length(still) > 0 && !is.null(fit$cat)) {
        ## SHIFT
        if (!is.null(fit$cat$shift_vars) && !is.null(fit$cat$shift_names) && !is.null(fit$cat$shift_maps)) {
          for (i in seq_along(fit$cat$shift_vars)) {
            raw <- fit$cat$shift_vars[[i]]
            out <- fit$cat$shift_names[[i]]
            if (!(out %in% still)) next
            if (!(raw %in% names(ND))) stop(sprintf("Missing required raw categorical column '%s' in data.", raw))
            mp <- fit$cat$shift_maps[[raw]]
            if (is.null(mp)) next
            mp <- data.table::as.data.table(mp)
            key_col   <- if (raw %in% names(mp)) raw else names(mp)[1L]
            value_col <- if (out %in% names(mp)) out else setdiff(names(mp), key_col)[1L]
            idx  <- match(ND[[raw]], mp[[key_col]])
            vals <- mp[[value_col]][idx]; vals[is.na(vals)] <- 0
            ND[, (out) := vals]
          }
        }
        ## PARAM MODS
        if (!is.null(fit$cat$param_enc_names) && !is.null(fit$cat$param_maps) && !is.null(fit$cat$param_raw_vars)) {
          for (p in names(fit$cat$param_enc_names)) {
            out <- fit$cat$param_enc_names[[p]]
            if (!(out %in% still)) next
            raw <- fit$cat$param_raw_vars[[p]]
            if (is.null(raw)) next
            if (!(raw %in% names(ND))) stop(sprintf("Missing required raw categorical column '%s' in data.", raw))
            mp  <- fit$cat$param_maps[[p]]
            if (is.null(mp)) next
            mp <- data.table::as.data.table(mp)
            key_col   <- if (raw %in% names(mp)) raw else names(mp)[1L]
            value_col <- if (out %in% names(mp)) out else setdiff(names(mp), key_col)[1L]
            idx  <- match(ND[[raw]], mp[[key_col]])
            vals <- mp[[value_col]][idx]; vals[is.na(vals)] <- 0
            ND[, (out) := vals]
          }
        }
      }

      # final strict check
      miss <- setdiff(required_eng, names(ND))
      if (length(miss) > 0) stop(sprintf("Missing engineered feature(s) for evaluation: %s", paste(miss, collapse = ", ")))
      ND
    },

    # Predict on standardized scale using decomposed params + shifts (name-driven)
    predict_std_from_decomposed = function(fit, data, x_scaled, override_coef = NULL) {
      coefs <- if (is.null(override_coef)) fit$coefficients else override_coef
      if (is.null(names(coefs)) || any(names(coefs) == "")) {
        stop("Coefficients lack names; cannot decompose.")
      }

      param_names <- fit$param_names
      if (is.null(param_names) || length(param_names) == 0) {
        stop("fit$param_names is missing or empty.")
      }

      # pick a callable core function, else defer to fitted.values if possible
      core_fun <- fit$model_function
      has_core_fun <- is.function(core_fun)
      if (!has_core_fun && is.list(core_fun)) {
        if (!is.null(core_fun$fun) && is.function(core_fun$fun)) {
          core_fun <- core_fun$fun
          has_core_fun <- TRUE
        } else if (!is.null(core_fun$model_function) && is.function(core_fun$model_function)) {
          core_fun <- core_fun$model_function
          has_core_fun <- TRUE
        }
      }

      inv_link <- function(nm) switch(nm, log = exp, logit = plogis, identity = identity, identity)

      n <- length(x_scaled)
      params_list <- vector("list", length(param_names)); names(params_list) <- param_names

      # per-parameter baseline + optional modifier
      for (p in param_names) {
        base_name <- paste0(p, ":baseline")
        if (!(base_name %in% names(coefs))) stop(sprintf("Missing baseline for parameter '%s'", p))
        eta <- rep(unname(coefs[[base_name]]), n)

        enc_name <- if (!is.null(fit$cat) && !is.null(fit$cat$param_enc_names)) fit$cat$param_enc_names[[p]] else NULL
        if (!is.null(enc_name)) {
          if (!(enc_name %in% names(data))) stop(sprintf("Engineered column '%s' missing in data for parameter '%s'", enc_name, p))
          mod_coef_name <- paste0(p, ":", enc_name)
          if (!(mod_coef_name %in% names(coefs))) stop(sprintf("Missing coefficient '%s' for parameter '%s'", mod_coef_name, p))
          eta <- eta + data[[enc_name]] * unname(coefs[[mod_coef_name]])
        }

        link_name <- if (!is.null(fit$param_links) && !is.null(fit$param_links[[p]])) fit$param_links[[p]] else "identity"
        params_list[[p]] <- inv_link(link_name)(eta)
      }

      # If we can't call a core function, try to fall back to training fitted values
      if (!has_core_fun) {
        if (!is.null(fit$fitted.values) && length(fit$fitted.values) == n) {
          return(fit$fitted.values)
        }
        stop("fit$model_function is not callable and no compatible fitted.values fallback is available.")
      }

      # Call the core function
      nl <- core_fun(x = x_scaled, params = params_list)

      # additive shift terms
      if (!is.null(fit$cat) && !is.null(fit$cat$shift_names) && length(fit$cat$shift_names) > 0) {
        for (sn in fit$cat$shift_names) {
          beta_name <- paste0("shift:", sn)
          if (!(beta_name %in% names(coefs))) next
          if (!(sn %in% names(data))) stop(sprintf("Engineered shift column '%s' missing in data", sn))
          nl <- nl + data[[sn]] * unname(coefs[[beta_name]])
        }
      }

      nl
    }
  )
)
