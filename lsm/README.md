# LSM - Liquid State Machine

A Rust implementation of Liquid State Machines (spiking neural network reservoirs) for temporal pattern recognition.

## Features

- 3D grid of Leaky Integrate-and-Fire (LIF) neurons
- Distance-dependent connectivity
- Excitatory/inhibitory neuron balance
- Real-time spike processing
- Synaptic plasticity (STDP)
- State encoding for downstream processing

## Installation

```toml
[dependencies]
lsm = "0.1"
```

## Quick Start

```rust
use lsm::{LiquidStateMachine, LsmConfig};
use ndarray::Array1;

// Create LSM with 8x8x8 = 512 neurons
let config = LsmConfig {
    grid_size: (8, 8, 8),
    connectivity_radius: 2.0,
    excitatory_ratio: 0.8,
    ..Default::default()
};

let mut lsm = LiquidStateMachine::new(config)?;

// Process input spikes
let input = Array1::from_vec(vec![1.0, 0.0, 1.0, 0.0]);
let state = lsm.step(&input);

// Get spike counts for downstream processing
let spike_count = lsm.get_spike_count();
```

## Configuration

| Parameter | Default | Description |
|-----------|---------|-------------|
| `grid_size` | (8,8,8) | 3D neuron grid dimensions |
| `connectivity_radius` | 2.0 | Connection probability radius |
| `excitatory_ratio` | 0.8 | Fraction of excitatory neurons |
| `membrane_tau` | 20.0 | Membrane time constant (ms) |
| `threshold` | 1.0 | Spike threshold |
| `reset_potential` | 0.0 | Post-spike reset voltage |
| `refractory_period` | 2.0 | Refractory period (ms) |

## Use Cases

- Real-time sensor processing
- Temporal pattern recognition
- Spike-based signal processing
- Neuromorphic computing simulations
- Speech and audio processing

## References

- Maass, W. (2002). "Real-time computing without stable states"
- Maass, W. et al. (2004). "Computational aspects of feedback in neural circuits"

## Related

- [esn](https://github.com/hyperpolymath/esn) - Echo State Network (rate-based reservoir)

## License

MIT License
