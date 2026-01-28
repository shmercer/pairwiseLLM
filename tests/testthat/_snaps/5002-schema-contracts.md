# round_log_schema columns are stable

    c("round_id", "iter_at_refit", "mode", "model_variant", "n_items", 
    "total_pairs", "hard_cap_threshold", "n_unique_pairs_seen", 
    "scheduled_pairs", "completed_pairs", "backlog_unjudged", "new_pairs", 
    "proposed_pairs", "batch_size", "window_W", "exploration_rate", 
    "mean_degree", "min_degree", "pos_balance_mean", "pos_balance_sd", 
    "mean_degree_scheduled", "min_degree_scheduled", "pos_balance_sd_scheduled", 
    "epsilon_mean", "epsilon_p2.5", "epsilon_p5", "epsilon_p50", 
    "epsilon_p95", "epsilon_p97.5", "beta_mean", "beta_p2.5", "beta_p5", 
    "beta_p50", "beta_p95", "beta_p97.5", "divergences", "max_rhat", "min_ess_bulk", 
    "diagnostics_pass", "reliability_EAP", "eap_pass", "theta_sd_eap", 
    "rho_theta_lag", "theta_corr_pass", "delta_sd_theta_lag", 
    "delta_sd_theta_pass", "rho_rank_lag", "rho_rank_pass", 
    "rank_stability_pass", "lag_eligible", "stop_decision", "stop_reason", 
    "starve_rate_since_last_refit", "fallback_rate_since_last_refit", 
    "fallback_used_mode", "starvation_reason_mode", "mcmc_chains", 
    "mcmc_parallel_chains", "mcmc_core_fraction", 
    "mcmc_cores_detected_physical", "mcmc_cores_detected_logical", 
    "mcmc_threads_per_chain", "mcmc_cmdstanr_version")

# item_summary_schema columns are stable

    c("ID", "deg", "posA_prop", "theta_mean", "theta_p2.5", "theta_p5", 
    "theta_p50", "theta_p95", "theta_p97.5", "theta_sd", "rank_mean", 
    "rank_p2.5", "rank_p5", "rank_p50", "rank_p95", "rank_p97.5", 
    "rank_sd")

