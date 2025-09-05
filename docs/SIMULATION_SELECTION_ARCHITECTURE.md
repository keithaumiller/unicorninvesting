# Multi-Simulation Dashboard Architecture

## Overview
Extension of current unicornmetrics module to support multiple simulation/portfolio selection while reusing existing dashboard code.

## Recommended Implementation Strategy

### 1. **URL Parameter Approach** (Primary Recommendation)
```
/admin/metrics?simulation=Myportolio
/admin/metrics?simulation=ETH_Strategy_A  
/admin/metrics?simulation=BTC_Conservative
```

**Advantages:**
- ✅ Reuses existing controller code
- ✅ Bookmarkable URLs
- ✅ SEO-friendly
- ✅ Simple implementation
- ✅ Backward compatible

### 2. **Dynamic Route Approach** (Alternative)
```
/admin/metrics/simulation/{simulation_id}
/admin/metrics/simulation/Myportolio
/admin/metrics/simulation/ETH_Strategy_A
```

## Implementation Plan

### Phase 1: Service Layer Enhancement
1. **PortfolioApiService Updates**
   - Add `getAvailableSimulations()` method
   - Enhance all methods to accept simulation parameter
   - Add simulation validation
   - Add fallback handling

### Phase 2: Controller Enhancement  
1. **DashboardController Updates**
   - Add simulation selection UI component
   - Update all dashboard methods to use simulation parameter
   - Add simulation validation and error handling
   - Maintain backward compatibility

### Phase 3: Frontend UI Enhancement
1. **Simulation Selector Component**
   - Dropdown/tab interface for simulation selection
   - Active simulation indicator
   - Simulation metadata display
   - Auto-refresh on selection change

### Phase 4: Routing Enhancement (Optional)
1. **Dynamic Routes**
   - Add parameterized routes if needed
   - Maintain URL parameter support
   - Add route validation

## Technical Requirements

### Backend Structure Expected
```
/BackendPython/unicorn/4_portfolios/
├── Myportolio/                    # Current simulation
├── ETH_Strategy_A/                # New simulation
├── BTC_Conservative/              # New simulation  
└── utilities/                     # Shared components
```

### Service Methods to Update
- `getPortfolioStatus($simulation_id)`
- `getPortfolioConfig($simulation_id)` 
- `getLatestStatusReport($simulation_id)`
- `getLatestRiskReport($simulation_id)`
- `getEthAlgorithmStatus($simulation_id)`
- `getAvailableSimulations()` (new)

### Controller Methods to Update
- `dashboard($simulation_id = 'Myportolio')`
- `leanPortfolio($simulation_id = null)`
- `leanHoldings($simulation_id = null)`
- `performanceMetrics($simulation_id = null)`

## Code Reuse Strategy

### 1. **Template Inheritance**
- Create base dashboard template
- Pass simulation data to existing templates
- No template duplication needed

### 2. **Service Method Parameterization**
- All existing service methods accept simulation parameter
- Default to 'Myportolio' for backward compatibility
- Consistent error handling across simulations

### 3. **Controller Method Enhancement**
- Extract simulation ID from URL parameter
- Pass to all service calls
- Maintain existing display logic

## User Experience Design

### Simulation Selector UI
```html
<div class="simulation-selector">
  <label>Select Simulation:</label>
  <select id="simulation-dropdown">
    <option value="Myportolio" selected>Myportolio (Default)</option>
    <option value="ETH_Strategy_A">ETH Strategy A</option>
    <option value="BTC_Conservative">BTC Conservative</option>
  </select>
</div>
```

### URL Update on Selection
```javascript
// Auto-update URL when simulation changes
document.getElementById('simulation-dropdown').addEventListener('change', function() {
  const simulation = this.value;
  const url = new URL(window.location);
  url.searchParams.set('simulation', simulation);
  window.location.href = url.toString();
});
```

## Benefits of This Approach

1. **Code Reuse**: 95% of existing code remains unchanged
2. **Backward Compatibility**: Myportolio remains default
3. **Scalability**: Easy to add new simulations
4. **Performance**: No additional database queries
5. **Maintenance**: Single codebase for all simulations
6. **User Experience**: Intuitive simulation switching

## Implementation Priority

1. **High Priority**: Service layer updates, simulation detection
2. **Medium Priority**: Controller parameter handling, UI selector
3. **Low Priority**: Dynamic routing (if needed)

## Testing Strategy

1. **Unit Tests**: Service methods with different simulation IDs
2. **Integration Tests**: Controller with various simulation parameters  
3. **Frontend Tests**: UI selection and URL updating
4. **Regression Tests**: Ensure Myportolio default behavior unchanged

## Migration Path

1. **Phase 1**: Add simulation parameter support (backward compatible)
2. **Phase 2**: Add simulation selector UI
3. **Phase 3**: Create additional simulation directories
4. **Phase 4**: Test and validate multi-simulation functionality

This architecture maximizes code reuse while providing flexible simulation selection capabilities.
