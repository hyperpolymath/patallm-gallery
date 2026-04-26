// SPDX-License-Identifier: MIT
//! Echo State Network Benchmark
//!
//! Reproducible demo and benchmark using the Mackey-Glass time series,
//! a classic chaotic dynamical system for evaluating ESN performance.
//!
//! Run with: cargo run --release --example benchmark

use esn::{Activation, EchoStateNetwork, EsnConfig};
use ndarray::Array1;
use rand::SeedableRng;
use rand_chacha::ChaCha8Rng;
use std::time::Instant;

/// Mackey-Glass time series parameters
const MG_BETA: f64 = 0.2;
const MG_GAMMA: f64 = 0.1;
const MG_N: f64 = 10.0;
const MG_TAU: usize = 17;
const MG_DT: f64 = 1.0;

/// Generate Mackey-Glass time series
///
/// The Mackey-Glass equation:
///   dx/dt = β * x(t-τ) / (1 + x(t-τ)^n) - γ * x(t)
///
/// This is a chaotic system commonly used to benchmark time series prediction.
fn generate_mackey_glass(length: usize, seed: u64) -> Vec<f64> {
    let mut rng = ChaCha8Rng::seed_from_u64(seed);
    use rand::Rng;

    // Initialize history with small random perturbations around 0.9
    let mut history: Vec<f64> = (0..MG_TAU + 1)
        .map(|_| 0.9 + rng.random::<f64>() * 0.2 - 0.1)
        .collect();

    let mut series = Vec::with_capacity(length);

    for _ in 0..length {
        let x_t = *history.last().unwrap();
        let x_tau = history[history.len() - MG_TAU - 1];

        // Mackey-Glass discrete update
        let dx = MG_BETA * x_tau / (1.0 + x_tau.powf(MG_N)) - MG_GAMMA * x_t;
        let x_next = x_t + dx * MG_DT;

        series.push(x_next);
        history.push(x_next);

        // Keep history bounded
        if history.len() > MG_TAU + 100 {
            history.remove(0);
        }
    }

    series
}

/// Normalize data to zero mean and unit variance
fn normalize(data: &[f64]) -> (Vec<f64>, f64, f64) {
    let mean: f64 = data.iter().sum::<f64>() / data.len() as f64;
    let variance: f64 = data.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / data.len() as f64;
    let std_dev = variance.sqrt();

    let normalized: Vec<f64> = data.iter().map(|x| (x - mean) / std_dev).collect();

    (normalized, mean, std_dev)
}

/// Calculate Mean Squared Error
fn mse(predictions: &[f64], targets: &[f64]) -> f64 {
    predictions
        .iter()
        .zip(targets.iter())
        .map(|(p, t)| (p - t).powi(2))
        .sum::<f64>()
        / predictions.len() as f64
}

/// Calculate Normalized Root Mean Squared Error
fn nrmse(predictions: &[f64], targets: &[f64]) -> f64 {
    let mse_val = mse(predictions, targets);
    let target_variance: f64 =
        targets.iter().map(|t| t.powi(2)).sum::<f64>() / targets.len() as f64;
    (mse_val / target_variance).sqrt()
}

fn main() {
    println!("═══════════════════════════════════════════════════════════════");
    println!("       Echo State Network (ESN) Benchmark");
    println!("       Mackey-Glass Time Series Prediction");
    println!("═══════════════════════════════════════════════════════════════");
    println!();

    // Configuration
    const SEED: u64 = 42;
    const TOTAL_LENGTH: usize = 5000;
    const TRAIN_LENGTH: usize = 3000;
    const WASHOUT: usize = 100;
    const PREDICTION_HORIZON: usize = 1; // 1-step ahead prediction

    println!("Data Generation");
    println!("───────────────────────────────────────────────────────────────");
    println!("  Seed:               {}", SEED);
    println!("  Total samples:      {}", TOTAL_LENGTH);
    println!("  Training samples:   {}", TRAIN_LENGTH);
    println!("  Test samples:       {}", TOTAL_LENGTH - TRAIN_LENGTH);
    println!("  Washout period:     {}", WASHOUT);
    println!("  Prediction horizon: {}-step ahead", PREDICTION_HORIZON);
    println!();

    // Generate Mackey-Glass data
    let start = Instant::now();
    let raw_data = generate_mackey_glass(TOTAL_LENGTH + PREDICTION_HORIZON, SEED);
    let (data, mean, std_dev) = normalize(&raw_data);
    println!(
        "  Generated {} samples in {:.2?}",
        TOTAL_LENGTH,
        start.elapsed()
    );
    println!("  Mean: {:.4}, Std: {:.4}", mean, std_dev);
    println!();

    // Prepare inputs and targets for training
    // Input: x(t), Target: x(t+1)
    let train_inputs: Vec<Array1<f32>> = data[..TRAIN_LENGTH]
        .iter()
        .map(|&x| Array1::from_vec(vec![x as f32]))
        .collect();

    let train_targets: Vec<Array1<f32>> = data
        [PREDICTION_HORIZON..TRAIN_LENGTH + PREDICTION_HORIZON]
        .iter()
        .map(|&x| Array1::from_vec(vec![x as f32]))
        .collect();

    // ESN Configuration
    println!("ESN Configuration");
    println!("───────────────────────────────────────────────────────────────");
    let config = EsnConfig {
        reservoir_size: 500,
        spectral_radius: 0.95,
        leaking_rate: 0.3,
        input_scale: 0.5,
        feedback_scale: 0.0,
        sparsity: 0.9,
        ridge_param: 1e-6,
        use_bias: true,
        noise_level: 1e-8,
    };

    println!("  Reservoir size:     {}", config.reservoir_size);
    println!("  Spectral radius:    {}", config.spectral_radius);
    println!("  Leaking rate:       {}", config.leaking_rate);
    println!("  Input scale:        {}", config.input_scale);
    println!("  Sparsity:           {:.0}%", config.sparsity * 100.0);
    println!("  Ridge parameter:    {:.0e}", config.ridge_param);
    println!("  Activation:         Tanh");
    println!();

    // Create and train ESN
    println!("Training");
    println!("───────────────────────────────────────────────────────────────");
    let start = Instant::now();
    let mut esn = EchoStateNetwork::new(config, 1, Activation::Tanh).expect("Failed to create ESN");

    let train_mse = esn
        .train(&train_inputs, &train_targets, WASHOUT)
        .expect("Training failed");

    let train_time = start.elapsed();
    println!("  Training time:      {:.2?}", train_time);
    println!("  Training MSE:       {:.6e}", train_mse);
    println!();

    // Evaluate on test set
    println!("Evaluation");
    println!("───────────────────────────────────────────────────────────────");
    let start = Instant::now();
    esn.reset();

    // Warm up the reservoir with training data (no washout needed, already done)
    for input in &train_inputs {
        esn.step(input);
    }

    // Test predictions
    let test_start = TRAIN_LENGTH;
    let test_end = TOTAL_LENGTH;
    let mut predictions = Vec::with_capacity(test_end - test_start);
    let mut targets = Vec::with_capacity(test_end - test_start);

    for i in test_start..test_end {
        let input = Array1::from_vec(vec![data[i] as f32]);
        esn.step(&input);
        let pred = esn.predict().expect("Prediction failed");
        predictions.push(pred[0] as f64);
        targets.push(data[i + PREDICTION_HORIZON]);
    }

    let eval_time = start.elapsed();
    let test_mse = mse(&predictions, &targets);
    let test_nrmse = nrmse(&predictions, &targets);

    println!("  Evaluation time:    {:.2?}", eval_time);
    println!("  Test samples:       {}", predictions.len());
    println!("  Test MSE:           {:.6e}", test_mse);
    println!("  Test NRMSE:         {:.4}", test_nrmse);
    println!();

    // Summary
    println!("═══════════════════════════════════════════════════════════════");
    println!("                        RESULTS SUMMARY");
    println!("═══════════════════════════════════════════════════════════════");
    println!();
    println!("  Total time:         {:.2?}", train_time + eval_time);
    println!("  Training MSE:       {:.6e}", train_mse);
    println!("  Test MSE:           {:.6e}", test_mse);
    println!("  Test NRMSE:         {:.4}", test_nrmse);
    println!();

    // Performance assessment
    if test_nrmse < 0.1 {
        println!("  Status:             EXCELLENT (NRMSE < 0.1)");
    } else if test_nrmse < 0.3 {
        println!("  Status:             GOOD (NRMSE < 0.3)");
    } else if test_nrmse < 0.5 {
        println!("  Status:             FAIR (NRMSE < 0.5)");
    } else {
        println!("  Status:             NEEDS TUNING (NRMSE >= 0.5)");
    }
    println!();

    // Sample predictions
    println!("Sample Predictions (first 10 test samples):");
    println!("───────────────────────────────────────────────────────────────");
    println!(
        "  {:>8}  {:>12}  {:>12}  {:>12}",
        "Sample", "Target", "Prediction", "Error"
    );
    for i in 0..10.min(predictions.len()) {
        let error = (predictions[i] - targets[i]).abs();
        println!(
            "  {:>8}  {:>12.6}  {:>12.6}  {:>12.6}",
            i, targets[i], predictions[i], error
        );
    }
    println!();

    println!(
        "Benchmark complete. Results are reproducible with seed={}.",
        SEED
    );
}
