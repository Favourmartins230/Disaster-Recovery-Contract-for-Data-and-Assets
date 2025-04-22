(define-constant err-unauthorized (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-not-registered (err u102))
(define-constant err-invalid-beneficiary (err u103))
(define-constant err-beneficiary-exists (err u104))
(define-constant err-beneficiary-not-found (err u105))
(define-constant err-invalid-recovery-period (err u106))
(define-constant err-recovery-in-progress (err u107))
(define-constant err-recovery-not-initiated (err u108))
(define-constant err-recovery-period-not-elapsed (err u109))
(define-constant err-self-appointment (err u110))
(define-constant err-max-beneficiaries-reached (err u111))

(define-constant max-beneficiaries u5)
(define-constant min-recovery-period u1440) ;; minimum 1 day (in blocks)
(define-constant max-recovery-period u525600) ;; maximum ~1 year (in blocks)

(define-data-var contract-owner principal tx-sender)

(define-map users
  { user: principal }
  { 
    registered: bool,
    recovery-period: uint,
    last-active-block: uint
  }
)

(define-map beneficiaries
  { user: principal, beneficiary: principal }
  { 
    authorized: bool,
    recovery-initiated: bool,
    recovery-initiation-block: uint
  }
)

(define-map assets
  { user: principal, asset-id: (string-utf8 36) }
  { 
    asset-name: (string-utf8 64),
    asset-url: (optional (string-utf8 256)),
    asset-data: (optional (string-utf8 1024))
  }
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (get-user-info (user principal))
  (default-to 
    { registered: false, recovery-period: u0, last-active-block: u0 }
    (map-get? users { user: user })
  )
)

(define-read-only (get-beneficiary-info (user principal) (beneficiary principal))
  (default-to 
    { authorized: false, recovery-initiated: false, recovery-initiation-block: u0 }
    (map-get? beneficiaries { user: user, beneficiary: beneficiary })
  )
)

(define-read-only (get-asset (user principal) (asset-id (string-utf8 36)))
  (map-get? assets { user: user, asset-id: asset-id })
)

(define-read-only (is-beneficiary (user principal) (potential-beneficiary principal))
  (default-to false (some (get authorized (get-beneficiary-info user potential-beneficiary))))
)

;; (define-read-only (list-assets (user principal))
;;   (map-get? assets { user: user })
;; )

(define-read-only (can-recover (user principal) (beneficiary principal))
  (let (
    (user-info (get-user-info user))
    (beneficiary-info (get-beneficiary-info user beneficiary))
  )
    (and
      (get registered user-info)
      (get authorized beneficiary-info)
      (get recovery-initiated beneficiary-info)
      (>= stacks-block-height (+ (get recovery-initiation-block beneficiary-info) (get recovery-period user-info)))
    )
  )
)

(define-public (register (recovery-period uint))
  (let (
    (user tx-sender)
    (user-info (get-user-info user))
  )
    (asserts! (not (get registered user-info)) err-already-registered)
    (asserts! (and (>= recovery-period min-recovery-period) (<= recovery-period max-recovery-period)) err-invalid-recovery-period)
    
    (map-set users
      { user: user }
      { 
        registered: true,
        recovery-period: recovery-period,
        last-active-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (update-activity)
  (let (
    (user tx-sender)
    (user-info (get-user-info user))
  )
    (asserts! (get registered user-info) err-not-registered)
    
    (map-set users
      { user: user }
      (merge user-info { last-active-block: stacks-block-height })
    )
    (ok true)
  )
)

(define-public (add-beneficiary (beneficiary principal))
  (let (
    (user tx-sender)
    (user-info (get-user-info user))
    (current-beneficiaries (get-beneficiary-count user))
  )
    (asserts! (get registered user-info) err-not-registered)
    (asserts! (not (is-eq user beneficiary)) err-self-appointment)
    (asserts! (< current-beneficiaries max-beneficiaries) err-max-beneficiaries-reached)
    (asserts! (not (is-beneficiary user beneficiary)) err-beneficiary-exists)
    
    (map-set beneficiaries
      { user: user, beneficiary: beneficiary }
      { 
        authorized: true,
        recovery-initiated: false,
        recovery-initiation-block: u0
      }
    )
    (ok true)
  )
)

(define-public (remove-beneficiary (beneficiary principal))
  (let (
    (user tx-sender)
    (user-info (get-user-info user))
  )
    (asserts! (get registered user-info) err-not-registered)
    (asserts! (is-beneficiary user beneficiary) err-beneficiary-not-found)
    
    (map-delete beneficiaries { user: user, beneficiary: beneficiary })
    (ok true)
  )
)

(define-public (add-asset (asset-id (string-utf8 36)) (asset-name (string-utf8 64)) (asset-url (optional (string-utf8 256))) (asset-data (optional (string-utf8 1024))))
  (let (
    (user tx-sender)
    (user-info (get-user-info user))
  )
    (asserts! (get registered user-info) err-not-registered)
    
    (map-set assets
      { user: user, asset-id: asset-id }
      { 
        asset-name: asset-name,
        asset-url: asset-url,
        asset-data: asset-data
      }
    )
    (ok true)
  )
)

(define-public (remove-asset (asset-id (string-utf8 36)))
  (let (
    (user tx-sender)
    (user-info (get-user-info user))
  )
    (asserts! (get registered user-info) err-not-registered)
    
    (map-delete assets { user: user, asset-id: asset-id })
    (ok true)
  )
)

(define-public (initiate-recovery (user principal))
  (let (
    (beneficiary tx-sender)
    (user-info (get-user-info user))
    (beneficiary-info (get-beneficiary-info user beneficiary))
  )
    (asserts! (get registered user-info) err-not-registered)
    (asserts! (get authorized beneficiary-info) err-unauthorized)
    (asserts! (not (get recovery-initiated beneficiary-info)) err-recovery-in-progress)
    
    (map-set beneficiaries
      { user: user, beneficiary: beneficiary }
      (merge beneficiary-info 
        { 
          recovery-initiated: true,
          recovery-initiation-block: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-public (cancel-recovery (user principal))
  (let (
    (beneficiary tx-sender)
    (beneficiary-info (get-beneficiary-info user beneficiary))
  )
    (asserts! (get authorized beneficiary-info) err-unauthorized)
    (asserts! (get recovery-initiated beneficiary-info) err-recovery-not-initiated)
    
    (map-set beneficiaries
      { user: user, beneficiary: beneficiary }
      (merge beneficiary-info 
        { 
          recovery-initiated: false,
          recovery-initiation-block: u0
        }
      )
    )
    (ok true)
  )
)

(define-public (recover-asset (user principal) (asset-id (string-utf8 36)))
  (let (
    (beneficiary tx-sender)
    (asset-data (get-asset user asset-id))
  )
    (asserts! (can-recover user beneficiary) err-recovery-period-not-elapsed)
    (asserts! (is-some asset-data) err-not-registered)
    
    (ok (unwrap-panic asset-data))
  )
)

(define-read-only (get-beneficiary-count (user principal))
  (fold + (map check-if-beneficiary (list-beneficiaries user)) u0)
)

;; mock function to simulate the list of beneficiaries
;; in a real-world scenario, this would be replaced with actual logic

(define-read-only (list-beneficiaries (user principal))
  (list 
    tx-sender
    'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
    'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG
    'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC
    'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND
  )
)

(define-private (check-if-beneficiary (potential-beneficiary principal))
  (if (is-beneficiary tx-sender potential-beneficiary)
    u1
    u0
  )
)