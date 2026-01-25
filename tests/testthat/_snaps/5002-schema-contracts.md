# round_log_schema columns are stable

    c("round_id", "iter_at_refit", "mode", "model_variant", "n_items", 
    "total_pairs", "hard_cap_threshold", "n_unique_pairs_seen", 
    "scheduled_pairs", "completed_pairs", "backlog_unjudged", "new_pairs", 
    "proposed_pairs", "batch_size", "window_W", "exploration_rate", 
    "mean_degree", "min_degree", "pos_balance_mean", "pos_balance_sd", 
    "epsilon_mean", "epsilon_p2.5", "epsilon_p5", "epsilon_p50", 
    "epsilon_p95", "epsilon_p97.5", "b_mean", "b_p2.5", "b_p5", 
    "b_p50", "b_p95", "b_p97.5", "divergences", "max_rhat", "min_ess_bulk", 
    "diagnostics_pass", "reliability_EAP", "theta_sd_eap", 
    "rho_theta_lag", "delta_sd_theta_lag", "rho_rank_lag", 
    "rank_stability_pass", "stop_passes", "stop_eligible", 
    "stop_decision", "stop_reason", "starve_rate_since_last_refit", 
    "fallback_rate_since_last_refit", "fallback_used_mode", 
    "starvation_reason_mode", "mcmc_chains", "mcmc_parallel_chains", 
    "mcmc_core_fraction", "mcmc_cores_detected_physical", 
    "mcmc_cores_detected_logical", "mcmc_threads_per_chain", 
    "mcmc_cmdstanr_version")

# item_summary_schema columns are stable

    c("ID", "theta_mean", "theta_sd", "theta_ci90_lo", "theta_ci90_hi", 
    "theta_ci95_lo", "theta_ci95_hi", "rank_mean", "rank_sd", "deg", 
    "posA_prop")
