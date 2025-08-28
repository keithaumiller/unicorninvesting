# LEAN Integration Layer

This directory contains integration code to connect Unicorn algorithms with the QuantConnect LEAN framework.

## Components

- `lean_bridge.py` - Bridge between Unicorn and LEAN
- `data_bridge.py` - Data sharing between systems
- `algorithm_wrapper.py` - Wrapper for Unicorn algorithms in LEAN
- `config_manager.py` - Configuration management for integration

## Purpose

This integration layer allows:
- Unicorn machine learning models to provide signals to LEAN
- LEAN to execute trades based on Unicorn algorithm decisions
- Shared data access between both systems
- Unified configuration management

## Dependencies

- QuantConnect LEAN framework
- Unicorn backend services
- Shared data models
