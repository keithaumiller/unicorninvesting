"""
Emergency Stop Mechanism for Myportolio
Implements automatic emergency stops and manual override controls
"""

import json
import os
import sys
from datetime import datetime, timedelta
from typing import Dict, List, Optional
import logging

# Add paths for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(current_dir)
sys.path.append(os.path.dirname(current_dir))

logger = logging.getLogger(__name__)

class EmergencyStopManager:
    """
    Emergency Stop Management System
    
    Provides:
    1. Automatic emergency stops based on risk conditions
    2. Manual emergency stop triggers
    3. Emergency position liquidation
    4. Risk condition monitoring
    5. Recovery procedures
    """
    
    def __init__(self, config_path: str = None):
        """Initialize emergency stop manager"""
        
        self.emergency_stop_file = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
            'emergency_stop.json'
        )
        
        # Load emergency stop configuration
        if config_path is None:
            config_path = os.path.join(
                os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                'risk_parameters.json'
            )
        
        with open(config_path, 'r') as f:
            self.risk_config = json.load(f)
        
        # Emergency stop state
        self.emergency_state = self._load_emergency_state()
        
        # Emergency triggers
        self.emergency_triggers = {
            'max_drawdown_breach': self.risk_config['max_drawdown'] * 1.2,  # 20% above limit
            'extreme_var': self.risk_config['var_limit_1day'] * 2.0,  # 2x daily VaR limit
            'system_failure': True,  # Can be triggered manually
            'market_crash': True,  # External market condition trigger
            'manual_override': True  # Manual emergency stop
        }
        
        logger.info("Emergency Stop Manager initialized")
    
    def _load_emergency_state(self) -> Dict:
        """Load emergency stop state from file"""
        
        if os.path.exists(self.emergency_stop_file):
            try:
                with open(self.emergency_stop_file, 'r') as f:
                    return json.load(f)
            except Exception as e:
                logger.error(f"Error loading emergency state: {e}")
        
        # Default state
        return {
            'emergency_stop_active': False,
            'stop_reason': None,
            'stop_timestamp': None,
            'recovery_conditions_met': False,
            'manual_override_active': False,
            'emergency_history': []
        }
    
    def _save_emergency_state(self):
        """Save emergency stop state to file"""
        
        try:
            with open(self.emergency_stop_file, 'w') as f:
                json.dump(self.emergency_state, f, indent=2, default=str)
        except Exception as e:
            logger.error(f"Error saving emergency state: {e}")
    
    def check_emergency_conditions(self, risk_metrics: Dict) -> Dict:
        """Check if emergency stop conditions are met"""
        
        emergency_conditions = []
        emergency_severity = 0
        
        # 1. Extreme drawdown
        current_drawdown = risk_metrics.get('current_drawdown', 0)
        if current_drawdown > self.emergency_triggers['max_drawdown_breach']:
            emergency_conditions.append({
                'trigger': 'extreme_drawdown',
                'severity': 'critical',
                'value': current_drawdown,
                'threshold': self.emergency_triggers['max_drawdown_breach'],
                'description': f"Extreme drawdown {current_drawdown:.1%} exceeds emergency threshold {self.emergency_triggers['max_drawdown_breach']:.1%}"
            })
            emergency_severity += 3
        
        # 2. Extreme VaR
        var_1day = risk_metrics.get('var_1day', 0)
        if var_1day > self.emergency_triggers['extreme_var']:
            emergency_conditions.append({
                'trigger': 'extreme_var',
                'severity': 'critical',
                'value': var_1day,
                'threshold': self.emergency_triggers['extreme_var'],
                'description': f"Extreme VaR {var_1day:.1%} exceeds emergency threshold {self.emergency_triggers['extreme_var']:.1%}"
            })
            emergency_severity += 2
        
        # 3. Portfolio volatility spike
        portfolio_vol = risk_metrics.get('portfolio_volatility', 0)
        vol_threshold = self.risk_config['max_portfolio_volatility'] * 2
        if portfolio_vol > vol_threshold:
            emergency_conditions.append({
                'trigger': 'volatility_spike',
                'severity': 'high',
                'value': portfolio_vol,
                'threshold': vol_threshold,
                'description': f"Portfolio volatility {portfolio_vol:.1%} exceeds emergency threshold {vol_threshold:.1%}"
            })
            emergency_severity += 1
        
        # 4. Multiple simultaneous violations
        if len(emergency_conditions) >= 2:
            emergency_severity += 2
        
        emergency_stop_required = emergency_severity >= 3 or len(emergency_conditions) >= 2
        
        return {
            'emergency_conditions': emergency_conditions,
            'emergency_severity': emergency_severity,
            'emergency_stop_required': emergency_stop_required,
            'timestamp': datetime.now()
        }
    
    def trigger_emergency_stop(self, reason: str, trigger_data: Dict = None, manual: bool = False):
        """Trigger emergency stop"""
        
        if self.emergency_state['emergency_stop_active']:
            logger.warning("Emergency stop already active")
            return
        
        timestamp = datetime.now()
        
        # Update emergency state
        self.emergency_state.update({
            'emergency_stop_active': True,
            'stop_reason': reason,
            'stop_timestamp': timestamp,
            'recovery_conditions_met': False,
            'manual_override_active': manual,
            'trigger_data': trigger_data or {}
        })
        
        # Add to history
        self.emergency_state['emergency_history'].append({
            'timestamp': timestamp,
            'reason': reason,
            'manual': manual,
            'trigger_data': trigger_data
        })
        
        # Save state
        self._save_emergency_state()
        
        # Execute emergency procedures
        self._execute_emergency_procedures(reason, manual)
        
        logger.critical(f"EMERGENCY STOP TRIGGERED: {reason}")
        
        return {
            'status': 'emergency_stop_activated',
            'reason': reason,
            'timestamp': timestamp,
            'manual': manual
        }
    
    def _execute_emergency_procedures(self, reason: str, manual: bool):
        """Execute emergency stop procedures"""
        
        procedures_executed = []
        
        try:
            # 1. Create emergency notification
            self._create_emergency_notification(reason, manual)
            procedures_executed.append('notification_created')
            
            # 2. Log emergency event
            self._log_emergency_event(reason, manual)
            procedures_executed.append('event_logged')
            
            # 3. Prepare liquidation plan (but don't execute automatically)
            liquidation_plan = self._prepare_liquidation_plan()
            procedures_executed.append('liquidation_plan_prepared')
            
            # 4. Disable automated trading
            self._disable_automated_trading()
            procedures_executed.append('automated_trading_disabled')
            
            # 5. Send alerts (would integrate with notification system)
            self._send_emergency_alerts(reason)
            procedures_executed.append('alerts_sent')
            
        except Exception as e:
            logger.error(f"Error executing emergency procedures: {e}")
        
        # Update state with executed procedures
        self.emergency_state['procedures_executed'] = procedures_executed
        self._save_emergency_state()
    
    def _create_emergency_notification(self, reason: str, manual: bool):
        """Create emergency stop notification file"""
        
        notification = {
            'type': 'EMERGENCY_STOP',
            'timestamp': datetime.now(),
            'reason': reason,
            'manual_trigger': manual,
            'portfolio': 'Myportolio',
            'severity': 'CRITICAL',
            'action_required': True,
            'instructions': [
                "1. Review current portfolio positions",
                "2. Assess market conditions",
                "3. Consider position liquidation if necessary",
                "4. Review risk parameters",
                "5. Execute recovery procedures when conditions improve"
            ]
        }
        
        notification_file = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
            f'emergency_notification_{datetime.now().strftime("%Y%m%d_%H%M%S")}.json'
        )
        
        with open(notification_file, 'w') as f:
            json.dump(notification, f, indent=2, default=str)
    
    def _log_emergency_event(self, reason: str, manual: bool):
        """Log emergency event"""
        
        log_entry = f"EMERGENCY STOP - Reason: {reason} - Manual: {manual} - Time: {datetime.now()}"
        logger.critical(log_entry)
    
    def _prepare_liquidation_plan(self) -> Dict:
        """Prepare emergency liquidation plan"""
        
        # Note: This would integrate with actual portfolio data
        liquidation_plan = {
            'timestamp': datetime.now(),
            'plan_type': 'emergency_liquidation',
            'priority_order': ['ETH', 'BTC'],  # Liquidate in this order
            'liquidation_method': 'market_order',  # Fast execution
            'risk_considerations': [
                'Execute during market hours for better liquidity',
                'Use market orders for speed',
                'Monitor execution for slippage',
                'Preserve cash position after liquidation'
            ]
        }
        
        # Save liquidation plan
        plan_file = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
            f'liquidation_plan_{datetime.now().strftime("%Y%m%d_%H%M%S")}.json'
        )
        
        with open(plan_file, 'w') as f:
            json.dump(liquidation_plan, f, indent=2, default=str)
        
        return liquidation_plan
    
    def _disable_automated_trading(self):
        """Disable automated trading systems"""
        
        # Create a flag file to disable automated trading
        disable_file = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
            'trading_disabled.flag'
        )
        
        with open(disable_file, 'w') as f:
            f.write(json.dumps({
                'disabled_timestamp': datetime.now(),
                'reason': 'emergency_stop',
                'status': 'automated_trading_disabled'
            }, default=str))
    
    def _send_emergency_alerts(self, reason: str):
        """Send emergency alerts (placeholder for notification system)"""
        
        # This would integrate with actual notification systems
        alert_message = f"EMERGENCY STOP ACTIVATED - Portfolio: Myportolio - Reason: {reason}"
        
        # For now, just log the alert
        logger.critical(f"EMERGENCY ALERT: {alert_message}")
    
    def check_recovery_conditions(self, risk_metrics: Dict) -> Dict:
        """Check if conditions allow for emergency stop recovery"""
        
        if not self.emergency_state['emergency_stop_active']:
            return {'recovery_allowed': True, 'reason': 'no_emergency_stop_active'}
        
        recovery_checks = []
        recovery_score = 0
        
        # 1. Drawdown back within limits
        current_drawdown = risk_metrics.get('current_drawdown', 0)
        if current_drawdown <= self.risk_config['max_drawdown']:
            recovery_checks.append('drawdown_within_limits')
            recovery_score += 1
        
        # 2. VaR back within limits
        var_1day = risk_metrics.get('var_1day', 0)
        if var_1day <= self.risk_config['var_limit_1day']:
            recovery_checks.append('var_within_limits')
            recovery_score += 1
        
        # 3. Volatility stabilized
        portfolio_vol = risk_metrics.get('portfolio_volatility', 0)
        if portfolio_vol <= self.risk_config['max_portfolio_volatility']:
            recovery_checks.append('volatility_stabilized')
            recovery_score += 1
        
        # 4. Time buffer (at least 1 hour since emergency stop)
        if self.emergency_state['stop_timestamp']:
            stop_time = datetime.fromisoformat(self.emergency_state['stop_timestamp']) if isinstance(self.emergency_state['stop_timestamp'], str) else self.emergency_state['stop_timestamp']
            time_since_stop = datetime.now() - stop_time
            if time_since_stop > timedelta(hours=1):
                recovery_checks.append('time_buffer_met')
                recovery_score += 1
        
        recovery_allowed = recovery_score >= 3 and not self.emergency_state.get('manual_override_active', False)
        
        return {
            'recovery_allowed': recovery_allowed,
            'recovery_checks': recovery_checks,
            'recovery_score': recovery_score,
            'required_score': 3,
            'manual_override_required': self.emergency_state.get('manual_override_active', False)
        }
    
    def execute_recovery(self, manual_override: bool = False) -> Dict:
        """Execute emergency stop recovery"""
        
        if not self.emergency_state['emergency_stop_active']:
            return {'status': 'error', 'message': 'No emergency stop active'}
        
        if self.emergency_state.get('manual_override_active', False) and not manual_override:
            return {'status': 'error', 'message': 'Manual override required for recovery'}
        
        timestamp = datetime.now()
        
        # Update emergency state
        self.emergency_state.update({
            'emergency_stop_active': False,
            'recovery_timestamp': timestamp,
            'recovery_conditions_met': True,
            'manual_override_active': False
        })
        
        # Re-enable automated trading
        self._enable_automated_trading()
        
        # Log recovery
        logger.info(f"Emergency stop recovery executed at {timestamp}")
        
        # Save state
        self._save_emergency_state()
        
        return {
            'status': 'recovery_successful',
            'timestamp': timestamp,
            'manual_override': manual_override
        }
    
    def _enable_automated_trading(self):
        """Re-enable automated trading systems"""
        
        disable_file = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
            'trading_disabled.flag'
        )
        
        if os.path.exists(disable_file):
            os.remove(disable_file)
    
    def get_emergency_status(self) -> Dict:
        """Get current emergency stop status"""
        
        return {
            'emergency_stop_active': self.emergency_state['emergency_stop_active'],
            'stop_reason': self.emergency_state.get('stop_reason'),
            'stop_timestamp': self.emergency_state.get('stop_timestamp'),
            'manual_override_active': self.emergency_state.get('manual_override_active', False),
            'recovery_conditions_met': self.emergency_state.get('recovery_conditions_met', False),
            'emergency_history_count': len(self.emergency_state.get('emergency_history', []))
        }
    
    def manual_emergency_stop(self, reason: str = "Manual override"):
        """Manually trigger emergency stop"""
        
        return self.trigger_emergency_stop(reason, manual=True)
    
    def manual_recovery(self, reason: str = "Manual recovery"):
        """Manually execute recovery"""
        
        return self.execute_recovery(manual_override=True)
