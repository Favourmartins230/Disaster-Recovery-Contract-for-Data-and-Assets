;; Recovery Status Monitor for Disaster Recovery Contract
;; Provides proactive monitoring and alerting for users approaching recovery eligibility

(define-constant ERR_NOT_AUTHORIZED (err u500))
(define-constant ERR_INVALID_THRESHOLD (err u501))
(define-constant ERR_SUBSCRIPTION_EXISTS (err u502))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u503))
(define-constant ERR_MAX_SUBSCRIPTIONS_REACHED (err u504))
(define-constant ERR_INVALID_USER (err u505))

;; Configuration constants
(define-constant MAX_SUBSCRIPTIONS_PER_MONITOR u20)
(define-constant MIN_ALERT_THRESHOLD u10) ;; Minimum 10% of recovery period
(define-constant MAX_ALERT_THRESHOLD u95) ;; Maximum 95% of recovery period

;; Contract owner for administrative functions
(define-data-var contract-owner principal tx-sender)

;; Monitor subscription structure
(define-map monitor-subscriptions
  { monitor: principal, monitored-user: principal }
  {
    alert-threshold-percentage: uint, ;; Percentage of recovery period when to alert
    subscription-block: uint,
    is-active: bool,
    last-alert-block: uint,
    alert-count: uint
  }
)

;; Track subscription counts for each monitor
(define-map monitor-subscription-counts
  { monitor: principal }
  { active-subscriptions: uint }
)

;; Alert configuration for users
(define-map user-alert-preferences
  { user: principal }
  {
    allow-monitoring: bool,
    default-threshold-percentage: uint,
    self-monitoring-enabled: bool
  }
)

;; Recovery status cache to reduce computation
(define-map recovery-status-cache
  { user: principal }
  {
    last-checked-block: uint,
    is-approaching-recovery: bool,
    is-recovery-eligible: bool,
    cached-recovery-percentage: uint
  }
)

;; Read-only functions

;; Get monitor subscription details
(define-read-only (get-monitor-subscription (monitor principal) (monitored-user principal))
  (map-get? monitor-subscriptions { monitor: monitor, monitored-user: monitored-user })
)

;; Get user alert preferences
(define-read-only (get-user-alert-preferences (user principal))
  (default-to 
    { allow-monitoring: true, default-threshold-percentage: u80, self-monitoring-enabled: true }
    (map-get? user-alert-preferences { user: user })
  )
)

;; Get subscription count for a monitor
(define-read-only (get-monitor-subscription-count (monitor principal))
  (default-to { active-subscriptions: u0 }
    (map-get? monitor-subscription-counts { monitor: monitor })
  )
)

;; Calculate recovery status for a user (integrates with main DRCA contract)
(define-read-only (calculate-recovery-status (user principal))
  (let (
    ;; This would integrate with the main DRCA contract to get user info
    ;; For demonstration, using mock values
    (recovery-period u1440) ;; Mock: 1 day recovery period
    (last-active-block (- stacks-block-height u1000)) ;; Mock: user inactive for 1000 blocks
    (time-since-active (- stacks-block-height last-active-block))
    (recovery-percentage (/ (* time-since-active u100) recovery-period))
  )
    {
      time-since-active: time-since-active,
      recovery-period: recovery-period,
      recovery-percentage: (if (> recovery-percentage u100) u100 recovery-percentage),
      is-approaching-recovery: (>= recovery-percentage u80), ;; Default threshold
      is-recovery-eligible: (>= recovery-percentage u100),
      blocks-until-recovery: (if (< time-since-active recovery-period) 
                              (- recovery-period time-since-active) 
                              u0)
    }
  )
)

;; Check if user allows monitoring
(define-read-only (user-allows-monitoring (user principal))
  (get allow-monitoring (get-user-alert-preferences user))
)

;; Public functions

;; Set user alert preferences
(define-public (set-alert-preferences (allow-monitoring bool) (default-threshold uint) (self-monitoring bool))
  (let ((user tx-sender))
    (asserts! (and (>= default-threshold MIN_ALERT_THRESHOLD) 
                   (<= default-threshold MAX_ALERT_THRESHOLD)) 
              ERR_INVALID_THRESHOLD)
    
    (map-set user-alert-preferences
      { user: user }
      {
        allow-monitoring: allow-monitoring,
        default-threshold-percentage: default-threshold,
        self-monitoring-enabled: self-monitoring
      }
    )
    (ok true)
  )
)

;; Subscribe to monitor a user's recovery status
(define-public (subscribe-to-monitor (monitored-user principal) (alert-threshold uint))
  (let (
    (monitor tx-sender)
    (existing-subscription (get-monitor-subscription monitor monitored-user))
    (monitor-count (get-monitor-subscription-count monitor))
    (user-preferences (get-user-alert-preferences monitored-user))
  )
    ;; Validate inputs
    (asserts! (not (is-eq monitor monitored-user)) ERR_INVALID_USER)
    (asserts! (is-none existing-subscription) ERR_SUBSCRIPTION_EXISTS)
    (asserts! (< (get active-subscriptions monitor-count) MAX_SUBSCRIPTIONS_PER_MONITOR) 
              ERR_MAX_SUBSCRIPTIONS_REACHED)
    (asserts! (and (>= alert-threshold MIN_ALERT_THRESHOLD) 
                   (<= alert-threshold MAX_ALERT_THRESHOLD)) 
              ERR_INVALID_THRESHOLD)
    (asserts! (get allow-monitoring user-preferences) ERR_NOT_AUTHORIZED)
    
    ;; Create subscription
    (map-set monitor-subscriptions
      { monitor: monitor, monitored-user: monitored-user }
      {
        alert-threshold-percentage: alert-threshold,
        subscription-block: stacks-block-height,
        is-active: true,
        last-alert-block: u0,
        alert-count: u0
      }
    )
    
    ;; Update subscription count
    (map-set monitor-subscription-counts
      { monitor: monitor }
      { active-subscriptions: (+ (get active-subscriptions monitor-count) u1) }
    )
    
    (ok true)
  )
)

;; Unsubscribe from monitoring a user
(define-public (unsubscribe-from-monitor (monitored-user principal))
  (let (
    (monitor tx-sender)
    (subscription (unwrap! (get-monitor-subscription monitor monitored-user) ERR_SUBSCRIPTION_NOT_FOUND))
    (monitor-count (get-monitor-subscription-count monitor))
  )
    ;; Remove subscription
    (map-delete monitor-subscriptions { monitor: monitor, monitored-user: monitored-user })
    
    ;; Update subscription count
    (map-set monitor-subscription-counts
      { monitor: monitor }
      { active-subscriptions: (- (get active-subscriptions monitor-count) u1) }
    )
    
    (ok true)
  )
)

;; Check recovery status and trigger alerts if needed
(define-public (check-and-alert (monitored-user principal))
  (let (
    (monitor tx-sender)
    (subscription (unwrap! (get-monitor-subscription monitor monitored-user) ERR_SUBSCRIPTION_NOT_FOUND))
    (recovery-status (calculate-recovery-status monitored-user))
  )
    (asserts! (get is-active subscription) ERR_NOT_AUTHORIZED)
    
    ;; Check if alert should be triggered
    (if (>= (get recovery-percentage recovery-status) (get alert-threshold-percentage subscription))
      (begin
        ;; Update subscription with alert information
        (map-set monitor-subscriptions
          { monitor: monitor, monitored-user: monitored-user }
          (merge subscription {
            last-alert-block: stacks-block-height,
            alert-count: (+ (get alert-count subscription) u1)
          })
        )
        
        ;; Update cache
        (map-set recovery-status-cache
          { user: monitored-user }
          {
            last-checked-block: stacks-block-height,
            is-approaching-recovery: (get is-approaching-recovery recovery-status),
            is-recovery-eligible: (get is-recovery-eligible recovery-status),
            cached-recovery-percentage: (get recovery-percentage recovery-status)
          }
        )
        
        (ok { alert-triggered: true, recovery-status: recovery-status })
      )
      (ok { alert-triggered: false, recovery-status: recovery-status })
    )
  )
)

;; Batch check multiple users (gas efficient)
(define-public (batch-check-recovery-status (users (list 10 principal)))
  (let ((monitor tx-sender))
    (ok (map check-single-user-status users))
  )
)

;; Helper function for batch checking
(define-private (check-single-user-status (user principal))
  (let (
    (subscription (get-monitor-subscription tx-sender user))
    (recovery-status (calculate-recovery-status user))
  )
    (match subscription
      sub-data
      {
        user: user,
        has-subscription: true,
        recovery-percentage: (get recovery-percentage recovery-status),
        should-alert: (>= (get recovery-percentage recovery-status) 
                          (get alert-threshold-percentage sub-data)),
        is-recovery-eligible: (get is-recovery-eligible recovery-status)
      }
      {
        user: user,
        has-subscription: false,
        recovery-percentage: u0,
        should-alert: false,
        is-recovery-eligible: false
      }
    )
  )
)

;; Toggle subscription active status
(define-public (toggle-subscription (monitored-user principal) (active bool))
  (let (
    (monitor tx-sender)
    (subscription (unwrap! (get-monitor-subscription monitor monitored-user) ERR_SUBSCRIPTION_NOT_FOUND))
  )
    (map-set monitor-subscriptions
      { monitor: monitor, monitored-user: monitored-user }
      (merge subscription { is-active: active })
    )
    (ok true)
  )
)
