# Brain PubSub Review

**Date:** 2025-12-08
**Status:** Documentation / No Changes Required

## Current Implementation

The `brain_pubsub` module in faber-tweann uses OTP `pg` (process groups) for brain-level events.

### Topic Structure

```erlang
{brain_pubsub, BrainId, Topic}
```

Where Topic is one of:
- `evaluated` - brain evaluation complete
- `reward_received` - reward signal received
- `weights_updated` - weights changed
- `activation` - network activation
- `error` - error occurred

### Key Functions

```erlang
%% Subscribe to brain events
brain_pubsub:subscribe(BrainId, Topic) -> ok

%% Publish brain event
brain_pubsub:publish(BrainId, Topic, Payload) -> ok

%% Get all subscribers
brain_pubsub:subscribers(BrainId, Topic) -> [pid()]
```

### Integration with Neuroevolution

The brain_pubsub is used at the individual network level (within exoself/cortex).
It is **separate** from the neuroevolution event system which operates at population level.

## Relationship to Event Architecture

| Layer | Module | Scope | Frequency |
|-------|--------|-------|-----------|
| Network | brain_pubsub | Single brain | Very high |
| Population | neuroevolution_events | Entire realm | Low-Medium |

## State Classification

### brain.erl State Fields

| Field | Category | Notes |
|-------|----------|-------|
| `id` | ESSENTIAL | Brain identity |
| `brain_id` | ESSENTIAL | Required for pubsub |
| `network` | ESSENTIAL | The actual network (write model) |
| `last_inputs` | VISUALIZATION | JIT candidate - only for viz |
| `last_outputs` | VISUALIZATION | JIT candidate - only for viz |
| `last_activations` | VISUALIZATION | JIT candidate - expensive to store |
| `subscribers` | ESSENTIAL | Subscription management |
| `input_labels` | VISUALIZATION | Only for rendering |
| `viz_enabled` | ESSENTIAL | Control flag |

### JIT Projection Opportunity

The `last_inputs`, `last_outputs`, and `last_activations` fields could be computed lazily when `get_viz()` is called instead of storing them after every evaluation. This would:
- Eliminate storage of 100s-1000s of floats per brain
- Reduce memory pressure during evolution
- Only compute visualization data when requested

## Recommendation

**No changes needed** to brain_pubsub itself. It correctly handles high-frequency,
per-network events at the appropriate layer. The event architecture refactoring
in neuroevolution_events is orthogonal to brain_pubsub.

However, consider refactoring `brain.erl` to compute visualization state lazily
(JIT Projection pattern) rather than storing it after every inference cycle.
