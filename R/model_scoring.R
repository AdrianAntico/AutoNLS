#' ModelScorer
#'
#' An R6 class to score non-linear regression models on new data
#' and visualize the results.
#' @export
ModelScorer <- R6::R6Class(
  "ModelScorer",
  public = list(
    fit_results = NULL,
    scored_data = list(),
    score_plots = list(),

    initialize = function(fit_results) {
      if (!is.list(fit_results)) {
        message("fit_results must be a list of model objects.")
        return(NULL)
      }
      self$fit_results <- fit_results
    },

    # Score new data with all fitted models
    # - new_data must include any feature columns used by param mods (e.g., channel_Credibility) and shift terms
    score_new_data = function(
    new_data,
    x_col,
    get_prediction_bounds = FALSE,
    lower_bound = 0.025,
    upper_bound = 0.975,
    n_sim = 1000,
    return_cols = NULL,
    id_col = NULL
    ) {
      if (!data.table::is.data.table(new_data)) {
        stop("new_data must be a data.table.")
      }
      if (!x_col %in% names(new_data)) {
        stop(sprintf("x_col '%s' not found in new_data.", x_col))
      }
      if (!is.null(id_col) && !id_col %in% names(new_data)) {
        stop(sprintf("id_col '%s' not found in new_data.", id_col))
      }
      if (!is.null(return_cols)) {
        missing_rc <- setdiff(return_cols, names(new_data))
        if (length(missing_rc)) {
          stop(sprintf("return_cols not found in new_data: %s", paste(missing_rc, collapse = ", ")))
        }
      }

      # Stable order
      ND0 <- data.table::copy(new_data)
      ND0[, row_id := .I]

      self$scored_data <- lapply(self$fit_results, function(fit) {
        if (is.null(fit)) return(NULL)
        tryCatch({
          ND <- data.table::copy(ND0)

          # Materialize engineered features (strict: throws if raw cats missing)
          ef_info <- private$ensure_required_features(fit = fit, ND = ND)
          ND <- ef_info$ND
          required_eng <- ef_info$required_eng   # engineered cols referenced by coefficients
          needed_raw   <- ef_info$needed_raw     # raw categorical cols required

          # Scale x
          scaled_x <- (ND[[x_col]] - fit$scale_params$min_x) / fit$scale_params$scale_factor_x

          # Predict on standardized scale (handles decomposed params + shift)
          preds_std <- private$predict_std_from_decomposed(
            fit  = fit,
            data = ND,
            x_scaled = scaled_x
          )
          y_pred <- fit$back_transform(preds_std, fit$scale_params)

          # Optional intervals
          y_lower <- y_upper <- NULL
          if (isTRUE(get_prediction_bounds)) {
            bounds_std <- private$confidence_intervals(
              fit = fit,
              data = ND,
              x_scaled = scaled_x,
              lower_bound = lower_bound,
              upper_bound = upper_bound,
              n_sim = n_sim
            )
            if (!is.null(bounds_std)) {
              y_lower <- fit$back_transform(bounds_std$lower, fit$scale_params)
              y_upper <- fit$back_transform(bounds_std$upper, fit$scale_params)
            }
          }

          # ---- Assemble output ----
          # “All variables used in the model” = predictor + needed raw cats + engineered cols
          model_vars <- unique(c(x_col, needed_raw, required_eng))

          out <- data.table::data.table(
            y_pred = y_pred
          )
          if (!is.null(y_lower)) out[, y_lower := y_lower]
          if (!is.null(y_upper)) out[, y_upper := y_upper]

          # Bring model vars (keep their original order from ND)
          if (length(model_vars) > 0) {
            out <- cbind(ND[, ..model_vars], out)
          }

          # Carry-through columns
          if (!is.null(return_cols)) {
            out <- cbind(ND[, ..return_cols], out)
          }
          if (!is.null(id_col)) {
            out <- cbind(ND[, ..id_col], out)
            if (id_col %in% names(out)) data.table::setnames(out, id_col, "id")
          }

          # Restore row order and drop temp
          out[, row_id := ND[["row_id"]]]
          data.table::setorder(out, row_id)
          out[, row_id := NULL]

          out
        }, error = function(e) {
          stop("Error scoring model: ", e$message)
        })
      })

      names(self$scored_data) <- names(self$fit_results)
      self$scored_data
    },


    # Plot scored predictions for one model
    generate_score_plot = function(model_name, x_col, theme = "westeros", title = NULL) {
      if (!model_name %in% names(self$fit_results)) {
        message("Model '", model_name, "' not found in fit_results. Available models: ",
                paste(names(self$fit_results), collapse = ", "))
        return(NULL)
      }
      if (is.null(self$scored_data[[model_name]])) {
        message("Model '", model_name, "' has not been scored. Please run score_new_data() first.")
        return(NULL)
      }

      predictions <- data.table::copy(self$scored_data[[model_name]])
      data.table::setorder(predictions, x)

      plot <- echarts4r::e_charts(data = predictions, x) |>
        echarts4r::e_line(y_pred, name = "Predicted", smooth = TRUE, showSymbol = FALSE) |>
        echarts4r::e_title(text = if (!is.null(title)) title else paste("Scored Data: Model -", model_name)) |>
        echarts4r::e_x_axis(name = x_col) |>
        echarts4r::e_y_axis(name = "Predicted Values") |>
        echarts4r::e_legend(right = 120) |>
        echarts4r::e_datazoom(x_index = c(0,1)) |>
        echarts4r::e_datazoom(y_index = c(0, 1)) |>
        echarts4r::e_toolbox_feature(feature = c("saveAsImage","dataZoom")) |>
        echarts4r::e_theme(name = theme) |>
        echarts4r::e_brush() |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross"))

      if (all(c("y_lower", "y_upper") %in% names(predictions))) {
        plot <- plot |>
          echarts4r::e_line(y_lower, name = "Lower Bound", smooth = TRUE, showSymbol = FALSE,
                            lineStyle = list(type = "dotted")) |>
          echarts4r::e_line(y_upper, name = "Upper Bound", smooth = TRUE, showSymbol = FALSE,
                            lineStyle = list(type = "dotted"))
      }

      self$score_plots[[model_name]] <- plot
      plot
    }
  ),

  private = list(
    # Rebuild per-row params from decomposed coefficients and compute standardized predictions
    predict_std_from_decomposed = function(fit, data, x_scaled, override_coef = NULL) {
      # coefs (must be named like "a:baseline", "b:baseline", "b:<enc>", "shift:<enc>")
      coefs <- if (is.null(override_coef)) fit$coefficients else override_coef
      if (is.null(names(coefs)) || any(names(coefs) == "")) {
        stop("Coefficients are missing names; cannot decompose parameters.")
      }

      param_names <- fit$param_names
      if (is.null(param_names) || length(param_names) == 0) {
        stop("fit$param_names is missing or empty.")
      }

      inv_link <- function(nm) switch(nm, log = exp, logit = plogis, identity = identity, identity)

      n <- length(x_scaled)
      params_list <- vector("list", length(param_names))
      names(params_list) <- param_names

      # Build per-row parameter vectors: baseline + (optional) encoded modifier
      for (p in param_names) {
        base_name <- paste0(p, ":baseline")
        if (!(base_name %in% names(coefs))) {
          stop(sprintf("Missing baseline for parameter '%s'", p))
        }
        eta <- rep(unname(coefs[[base_name]]), n)

        # if this parameter has a categorical modifier, add it
        enc_name <- NULL
        if (!is.null(fit$cat) && !is.null(fit$cat$param_enc_names)) {
          enc_name <- fit$cat$param_enc_names[[p]]
        }
        if (!is.null(enc_name)) {
          # engineered feature must be present in 'data' already (scorer ensures this)
          if (!(enc_name %in% names(data))) {
            stop(sprintf("Engineered column '%s' missing in scoring data for parameter '%s'", enc_name, p))
          }
          mod_coef_name <- paste0(p, ":", enc_name)
          if (!(mod_coef_name %in% names(coefs))) {
            stop(sprintf("Missing coefficient '%s' for parameter '%s'", mod_coef_name, p))
          }
          eta <- eta + data[[enc_name]] * unname(coefs[[mod_coef_name]])
        }

        link_name <- if (!is.null(fit$param_links) && !is.null(fit$param_links[[p]])) fit$param_links[[p]] else "identity"
        params_list[[p]] <- inv_link(link_name)(eta)
      }

      # Nonlinear core
      nl <- fit$model_function(x = x_scaled, params = params_list)

      # Additive shift: sum_j X_j * beta_j for all engineered shift columns present
      if (!is.null(fit$cat) && !is.null(fit$cat$shift_names) && length(fit$cat$shift_names) > 0) {
        for (sn in fit$cat$shift_names) {
          beta_name <- paste0("shift:", sn)
          if (!(beta_name %in% names(coefs))) next  # if model didn’t include this shift, skip
          if (!(sn %in% names(data))) {
            stop(sprintf("Engineered shift column '%s' missing in scoring data", sn))
          }
          nl <- nl + data[[sn]] * unname(coefs[[beta_name]])
        }
      }
      nl
    },

    # Simulate CIs using decomposed params (vectorized when possible)
    confidence_intervals = function(fit, data, x_scaled, lower_bound, upper_bound, n_sim = 1000) {
      coef <- fit$coefficients
      se_params <- fit$confidence_intervals$SE
      if (is.null(se_params)) {
        message("se_params is NULL")
        return(NULL)
      }

      pnames <- names(coef)
      mu <- unlist(coef)[pnames]
      sdv <- unlist(se_params)[pnames]
      sdv[!is.finite(sdv) | sdv < 0] <- 0

      # Simulate predictions on standardized scale using decomposed logic
      sim_matrix <- replicate(n_sim, {
        draws <- stats::rnorm(length(mu), mean = mu, sd = sdv)
        names(draws) <- pnames
        private$predict_std_from_decomposed(
          fit = fit,
          data = data,
          x_scaled = x_scaled,
          override_coef = as.list(draws)
        )
      })

      # Rowwise quantiles
      if (is.matrix(sim_matrix)) {
        lower_std <- apply(sim_matrix, 1, stats::quantile, probs = lower_bound, na.rm = TRUE)
        upper_std <- apply(sim_matrix, 1, stats::quantile, probs = upper_bound, na.rm = TRUE)
      } else {
        # happens if nrow(data)==1 -> sim_matrix is a vector
        lower_std <- stats::quantile(sim_matrix, probs = lower_bound, na.rm = TRUE)[[1]]
        upper_std <- stats::quantile(sim_matrix, probs = upper_bound, na.rm = TRUE)[[1]]
      }

      list(lower = lower_std, upper = upper_std)
    },

    # Back-transform encodings
    # Create any required encoded features in ND using saved artifacts
    ensure_required_features = function(fit, ND) {
      ND <- data.table::as.data.table(ND)

      # Engineered features required by this model (everything after the first ":")
      coef_names <- names(fit$coefficients)
      required_eng <- grep("^(shift:|[A-Za-z]+:)", coef_names, value = TRUE)
      required_eng <- unique(sub("^[^:]+:", "", required_eng))
      required_eng <- required_eng[required_eng != "baseline"]

      if (length(required_eng) == 0) {
        return(list(ND = ND, required_eng = character(0), needed_raw = character(0)))
      }

      # Determine which RAW columns are needed
      needed_raw <- character(0)

      # Prefer attached encoders (have explicit $requires and $produced_features)
      if (!is.null(fit$encoders) && length(fit$encoders) > 0) {
        for (enc in fit$encoders) {
          if (length(intersect(enc$produced_features, required_eng)) > 0) {
            needed_raw <- unique(c(needed_raw, enc$requires))
          }
        }
      }

      # Fallback: infer from fit$cat (shift + param mods)
      if (!is.null(fit$cat)) {
        # shift: map shift_names -> shift_vars
        if (!is.null(fit$cat$shift_names) && !is.null(fit$cat$shift_vars)) {
          take <- intersect(required_eng, fit$cat$shift_names)
          if (length(take) > 0) {
            idx <- match(take, fit$cat$shift_names)
            needed_raw <- unique(c(needed_raw, fit$cat$shift_vars[idx]))
          }
        }
        # param mods: map param_enc_names -> param_raw_vars
        if (!is.null(fit$cat$param_enc_names) && !is.null(fit$cat$param_raw_vars)) {
          for (p in names(fit$cat$param_enc_names)) {
            enc_name <- fit$cat$param_enc_names[[p]]
            if (!is.null(enc_name) && enc_name %in% required_eng) {
              needed_raw <- unique(c(needed_raw, fit$cat$param_raw_vars[[p]]))
            }
          }
        }
      }

      needed_raw <- unique(stats::na.omit(needed_raw))

      # STRICT: raw columns must exist
      missing_raw <- setdiff(needed_raw, names(ND))
      if (length(missing_raw) > 0) {
        stop(sprintf("Missing required raw categorical column(s) in new_data: %s",
                     paste(missing_raw, collapse = ", ")))
      }

      # Try encoders first
      if (!is.null(fit$encoders) && length(fit$encoders) > 0) {
        for (enc in fit$encoders) {
          ND <- enc$transform(ND)
        }
      }

      # Fallback: use fit$cat maps directly (no renaming; use map as returned by categorical_encoding())
      still_missing <- setdiff(required_eng, names(ND))
      if (length(still_missing) > 0 && !is.null(fit$cat)) {
        # SHIFT maps
        if (!is.null(fit$cat$shift_vars) && !is.null(fit$cat$shift_names) && !is.null(fit$cat$shift_maps)) {
          for (i in seq_along(fit$cat$shift_vars)) {
            raw    <- fit$cat$shift_vars[[i]]
            outcol <- fit$cat$shift_names[[i]]
            if (!(outcol %in% still_missing)) next
            mp <- fit$cat$shift_maps[[raw]]
            if (is.null(mp)) next
            mp <- data.table::as.data.table(mp)
            key_col   <- if (raw %in% names(mp)) raw else names(mp)[1L]
            value_col <- if (outcol %in% names(mp)) outcol else setdiff(names(mp), key_col)[1L]
            idx  <- match(ND[[raw]], mp[[key_col]])
            vals <- mp[[value_col]][idx]
            vals[is.na(vals)] <- 0
            ND[, (outcol) := vals]
          }
        }

        # PARAM modifier maps
        if (!is.null(fit$cat$param_enc_names) && !is.null(fit$cat$param_maps) && !is.null(fit$cat$param_raw_vars)) {
          for (p in names(fit$cat$param_enc_names)) {
            outcol <- fit$cat$param_enc_names[[p]]
            if (!(outcol %in% still_missing)) next
            raw <- fit$cat$param_raw_vars[[p]]
            mp  <- fit$cat$param_maps[[p]]
            if (is.null(raw) || is.null(mp)) next
            mp <- data.table::as.data.table(mp)
            key_col   <- if (raw %in% names(mp)) raw else names(mp)[1L]
            value_col <- if (outcol %in% names(mp)) outcol else setdiff(names(mp), key_col)[1L]
            idx  <- match(ND[[raw]], mp[[key_col]])
            vals <- mp[[value_col]][idx]
            vals[is.na(vals)] <- 0
            ND[, (outcol) := vals]
          }
        }
      }

      # Final strict check
      missing_eng <- setdiff(required_eng, names(ND))
      if (length(missing_eng) > 0) {
        stop(sprintf("Missing engineered feature column(s) after encoding: %s",
                     paste(missing_eng, collapse = ", ")))
      }

      list(ND = ND, required_eng = required_eng, needed_raw = needed_raw)
    }
  )
)
