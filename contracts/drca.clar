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
(define-constant err-emergency-contact-exists (err u200))
(define-constant err-emergency-contact-not-found (err u201))
(define-constant err-cannot-freeze-own-recovery (err u202))
(define-constant err-recovery-already-frozen (err u203))
(define-constant err-recovery-not-frozen (err u204))
(define-constant err-freeze-expired (err u205))
(define-constant err-max-emergency-contacts-reached (err u206))

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



(define-map recovery-notifications
  { beneficiary: principal }
  { pending-recoveries: (list 25 { user: principal, asset-id: (string-utf8 36) }) }
)

(define-read-only (get-pending-recoveries (beneficiary principal))
  (default-to 
    { pending-recoveries: (list) }
    (map-get? recovery-notifications { beneficiary: beneficiary })
  )
)

(define-public (notify-recovery (beneficiary principal) (asset-id (string-utf8 36)))
  (let (
    (user tx-sender)
    (current-notifications (get-pending-recoveries beneficiary))
    (new-notification { user: user, asset-id: asset-id })
    (current-list (get pending-recoveries current-notifications))
  )
    (asserts! (< (len current-list) u25) (err u112))
    (map-set recovery-notifications
      { beneficiary: beneficiary }
      { pending-recoveries: (unwrap! (as-max-len? (append current-list new-notification) u25) (err u112)) }
    )
    (ok true)
  )
)

(define-public (clear-notifications (beneficiary principal))
  (let (
    (user tx-sender)
    (current-notifications (get-pending-recoveries beneficiary))
  )
    (asserts! (is-eq user beneficiary) err-unauthorized)
    
    (map-set recovery-notifications
      { beneficiary: beneficiary }
      { pending-recoveries: (list) }
    )
    (ok true)
  )
)
(define-public (get-recovery-notifications (beneficiary principal))
  (let (
    (user tx-sender)
    (current-notifications (get-pending-recoveries beneficiary))
  )
    (asserts! (is-eq user beneficiary) err-unauthorized)
    
    (ok (get pending-recoveries current-notifications))
  )
)
(define-public (get-recovery-notification-count (beneficiary principal))
  (let (
    (user tx-sender)
    (current-notifications (get-pending-recoveries beneficiary))
  )
    (asserts! (is-eq user beneficiary) err-unauthorized)
    
    (ok (len (get pending-recoveries current-notifications)))
  )
)



(define-constant max-emergency-contacts u3)
(define-constant freeze-duration u2016)

(define-map emergency-contacts
  { user: principal, contact: principal }
  { 
    active: bool,
    added-block: uint
  }
)

(define-map recovery-freezes
  { user: principal, beneficiary: principal }
  { 
    frozen: bool,
    freeze-block: uint,
    frozen-by: principal
  }
)

(define-read-only (get-emergency-contact-info (user principal) (contact principal))
  (default-to 
    { active: false, added-block: u0 }
    (map-get? emergency-contacts { user: user, contact: contact })
  )
)

(define-read-only (get-recovery-freeze-info (user principal) (beneficiary principal))
  (default-to 
    { frozen: false, freeze-block: u0, frozen-by : user }
    (map-get? recovery-freezes { user: user, beneficiary: beneficiary })
  )
)

(define-read-only (is-emergency-contact (user principal) (contact principal))
  (get active (get-emergency-contact-info user contact))
)

(define-read-only (is-recovery-frozen (user principal) (beneficiary principal))
  (let (
    (freeze-info (get-recovery-freeze-info user beneficiary))
  )
    (and
      (get frozen freeze-info)
      (< stacks-block-height (+ (get freeze-block freeze-info) freeze-duration))
    )
  )
)

(define-read-only (get-emergency-contact-count (user principal))
  (fold + (map check-if-emergency-contact (list-potential-contacts user)) u0)
)

(define-read-only (list-potential-contacts (user principal))
  (list 
    tx-sender
    'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
    'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG
    'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC
    'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND
  )
)

(define-private (check-if-emergency-contact (potential-contact principal))
  (if (is-emergency-contact tx-sender potential-contact)
    u1
    u0
  )
)

(define-public (add-emergency-contact (contact principal))
  (let (
    (user tx-sender)
    (current-contacts (get-emergency-contact-count user))
  )
    (asserts! (not (is-eq user contact)) err-unauthorized)
    (asserts! (< current-contacts max-emergency-contacts) err-max-emergency-contacts-reached)
    (asserts! (not (is-emergency-contact user contact)) err-emergency-contact-exists)
    
    (map-set emergency-contacts
      { user: user, contact: contact }
      { 
        active: true,
        added-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (remove-emergency-contact (contact principal))
  (let (
    (user tx-sender)
  )
    (asserts! (is-emergency-contact user contact) err-emergency-contact-not-found)
    
    (map-delete emergency-contacts { user: user, contact: contact })
    (ok true)
  )
)

(define-public (freeze-recovery (user principal) (beneficiary principal))
  (let (
    (contact tx-sender)
    (freeze-info (get-recovery-freeze-info user beneficiary))
  )
    (asserts! (is-emergency-contact user contact) err-unauthorized)
    (asserts! (not (is-eq contact beneficiary)) err-cannot-freeze-own-recovery)
    (asserts! (not (get frozen freeze-info)) err-recovery-already-frozen)
    
    (map-set recovery-freezes
      { user: user, beneficiary: beneficiary }
      { 
        frozen: true,
        freeze-block: stacks-block-height,
        frozen-by: contact
      }
    )
    (ok true)
  )
)

(define-public (unfreeze-recovery (user principal) (beneficiary principal))
  (let (
    (caller tx-sender)
    (freeze-info (get-recovery-freeze-info user beneficiary))
  )
    (asserts! (get frozen freeze-info) err-recovery-not-frozen)
    (asserts! (or 
      (is-eq caller user)
      (is-eq caller (get frozen-by freeze-info))
    ) err-unauthorized)
    
    (map-set recovery-freezes
      { user: user, beneficiary: beneficiary }
      { 
        frozen: false,
        freeze-block: u0,
        frozen-by: 'ST000000000000000000002AMW42H
      }
    )
    (ok true)
  )
)

(define-public (check-recovery-status (user principal) (beneficiary principal))
  (let (
    (freeze-info (get-recovery-freeze-info user beneficiary))
  )
    (if (and 
      (get frozen freeze-info)
      (>= stacks-block-height (+ (get freeze-block freeze-info) freeze-duration))
    )
      (begin
        (map-set recovery-freezes
          { user: user, beneficiary: beneficiary }
          { 
            frozen: false,
            freeze-block: u0,
            frozen-by: 'ST000000000000000000002AMW42H
          }
        )
        (ok { frozen: false, auto-expired: true })
      )
      (ok { frozen: (is-recovery-frozen user beneficiary), auto-expired: false })
    )
  )
)

(define-read-only (can-recover-with-freeze-check (user principal) (beneficiary principal))
  (and
    (not (is-recovery-frozen user beneficiary))
    true
  )
)

(define-constant err-vault-not-found (err u300))
(define-constant err-vault-still-locked (err u301))
(define-constant err-vault-already-exists (err u302))
(define-constant err-insufficient-balance (err u303))
(define-constant err-invalid-unlock-time (err u304))

(define-constant min-lock-duration u144)
(define-constant max-vaults-per-user u10)

(define-map time-locked-vaults
  { user: principal, vault-id: (string-utf8 32) }
  {
    amount: uint,
    unlock-block: uint,
    created-block: uint,
    beneficiary: (optional principal)
  }
)

(define-read-only (get-vault-info (user principal) (vault-id (string-utf8 32)))
  (map-get? time-locked-vaults { user: user, vault-id: vault-id })
)

(define-read-only (get-vault-count (user principal))
  (fold + (map check-vault-exists (generate-vault-ids)) u0)
)

(define-read-only (generate-vault-ids)
  (list 
    u"vault-1" u"vault-2" u"vault-3" u"vault-4" u"vault-5"
    u"vault-6" u"vault-7" u"vault-8" u"vault-9" u"vault-10"
  )
)

(define-private (check-vault-exists (vault-id (string-utf8 32)))
  (if (is-some (get-vault-info tx-sender vault-id))
    u1
    u0
  )
)

(define-read-only (is-vault-unlocked (user principal) (vault-id (string-utf8 32)))
  (match (get-vault-info user vault-id)
    vault-data (>= stacks-block-height (get unlock-block vault-data))
    false
  )
)

(define-public (create-time-locked-vault (vault-id (string-utf8 32)) (amount uint) (lock-duration uint) (beneficiary (optional principal)))
  (let (
    (user tx-sender)
    (unlock-block (+ stacks-block-height lock-duration))
    (current-vaults (get-vault-count user))
  )
    (asserts! (is-none (get-vault-info user vault-id)) err-vault-already-exists)
    (asserts! (>= lock-duration min-lock-duration) err-invalid-unlock-time)
    (asserts! (< current-vaults max-vaults-per-user) err-max-beneficiaries-reached)
    (asserts! (>= (stx-get-balance user) amount) err-insufficient-balance)
    
    (try! (stx-transfer? amount user (as-contract tx-sender)))
    
    (map-set time-locked-vaults
      { user: user, vault-id: vault-id }
      {
        amount: amount,
        unlock-block: unlock-block,
        created-block: stacks-block-height,
        beneficiary: beneficiary
      }
    )
    (ok true)
  )
)

(define-public (withdraw-from-vault (vault-id (string-utf8 32)))
  (let (
    (user tx-sender)
    (vault-data (unwrap! (get-vault-info user vault-id) err-vault-not-found))
  )
    (asserts! (>= stacks-block-height (get unlock-block vault-data)) err-vault-still-locked)
    
    (try! (as-contract (stx-transfer? (get amount vault-data) tx-sender user)))
    
    (map-delete time-locked-vaults { user: user, vault-id: vault-id })
    (ok (get amount vault-data))
  )
)

(define-public (claim-vault-as-beneficiary (user principal) (vault-id (string-utf8 32)))
  (let (
    (beneficiary tx-sender)
    (vault-data (unwrap! (get-vault-info user vault-id) err-vault-not-found))
    (vault-beneficiary (unwrap! (get beneficiary vault-data) err-unauthorized))
  )
    (asserts! (is-eq beneficiary vault-beneficiary) err-unauthorized)
    (asserts! (>= stacks-block-height (get unlock-block vault-data)) err-vault-still-locked)
    
    (try! (as-contract (stx-transfer? (get amount vault-data) tx-sender beneficiary)))
    
    (map-delete time-locked-vaults { user: user, vault-id: vault-id })
    (ok (get amount vault-data))
  )
)

(define-public (update-vault-beneficiary (vault-id (string-utf8 32)) (new-beneficiary (optional principal)))
  (let (
    (user tx-sender)
    (vault-data (unwrap! (get-vault-info user vault-id) err-vault-not-found))
  )
    (map-set time-locked-vaults
      { user: user, vault-id: vault-id }
      (merge vault-data { beneficiary: new-beneficiary })
    )
    (ok true)
  )
)

(define-read-only (get-vault-status (user principal) (vault-id (string-utf8 32)))
  (match (get-vault-info user vault-id)
    vault-data 
    (ok {
      exists: true,
      amount: (get amount vault-data),
      unlock-block: (get unlock-block vault-data),
      blocks-remaining: (if (> (get unlock-block vault-data) stacks-block-height)
                         (- (get unlock-block vault-data) stacks-block-height)
                         u0),
      is-unlocked: (>= stacks-block-height (get unlock-block vault-data)),
      beneficiary: (get beneficiary vault-data)
    })
    (ok { exists: false, amount: u0, unlock-block: u0, blocks-remaining: u0, is-unlocked: false, beneficiary: none })
  )
)

;; Asset Verification & Attestation System
;; Provides cryptographic proofs and third-party attestations for asset ownership

(define-constant err-verification-not-found (err u400))
(define-constant err-verification-exists (err u401))
(define-constant err-invalid-proof (err u402))
(define-constant err-attestation-not-found (err u403))
(define-constant err-attestation-exists (err u404))
(define-constant err-cannot-attest-own-asset (err u405))
(define-constant err-insufficient-attestations (err u406))
(define-constant err-challenge-not-found (err u407))
(define-constant err-challenge-exists (err u408))
(define-constant err-challenge-period-active (err u409))

(define-constant max-attestations-per-asset u10)
(define-constant min-attestations-required u2)
(define-constant challenge-period u1008) ;; ~1 week in blocks
(define-constant attestor-stake-amount u1000000) ;; 1 STX in microSTX

;; Store cryptographic proofs for asset ownership
(define-map asset-verifications
  { user: principal, asset-id: (string-utf8 36) }
  {
    proof-hash: (buff 32), ;; Hash of ownership proof
    proof-timestamp: uint,
    verification-score: uint,
    is-verified: bool,
    challenge-count: uint
  }
)

;; Store third-party attestations for assets
(define-map asset-attestations
  { user: principal, asset-id: (string-utf8 36), attestor: principal }
  {
    attestation-hash: (buff 32), ;; Hash of attestation data
    attestation-timestamp: uint,
    stake-amount: uint,
    is-active: bool
  }
)

;; Store challenges against asset verifications
(define-map verification-challenges
  { user: principal, asset-id: (string-utf8 36), challenger: principal }
  {
    challenge-reason: (string-utf8 256),
    challenge-timestamp: uint,
    challenge-stake: uint,
    is-resolved: bool,
    resolution: (optional bool) ;; true = challenge upheld, false = challenge rejected
  }
)

;; Track attestor reputation scores
(define-map attestor-reputation
  { attestor: principal }
  {
    total-attestations: uint,
    successful-attestations: uint,
    reputation-score: uint,
    total-stake: uint
  }
)

;; Read-only functions for verification system
(define-read-only (get-asset-verification (user principal) (asset-id (string-utf8 36)))
  (map-get? asset-verifications { user: user, asset-id: asset-id })
)

(define-read-only (get-asset-attestation (user principal) (asset-id (string-utf8 36)) (attestor principal))
  (map-get? asset-attestations { user: user, asset-id: asset-id, attestor: attestor })
)

(define-read-only (get-verification-challenge (user principal) (asset-id (string-utf8 36)) (challenger principal))
  (map-get? verification-challenges { user: user, asset-id: asset-id, challenger: challenger })
)

(define-read-only (get-attestor-reputation (attestor principal))
  (default-to 
    { total-attestations: u0, successful-attestations: u0, reputation-score: u0, total-stake: u0 }
    (map-get? attestor-reputation { attestor: attestor })
  )
)

;; Check if asset meets minimum verification requirements
(define-read-only (is-asset-sufficiently-verified (user principal) (asset-id (string-utf8 36)))
  (let (
    (verification-data (get-asset-verification user asset-id))
    (attestation-count (get-attestation-count user asset-id))
  )
    (and
      (is-some verification-data)
      (get is-verified (unwrap-panic verification-data))
      (>= attestation-count min-attestations-required)
      (not (has-active-challenges user asset-id))
    )
  )
)

;; Count active attestations for an asset
(define-read-only (get-attestation-count (user principal) (asset-id (string-utf8 36)))
  (fold + (map check-attestation-active (generate-attestor-list)) u0)
)

;; Generate list of potential attestors (simplified for demonstration)
(define-read-only (generate-attestor-list)
  (list 
    tx-sender
    'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
    'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG
    'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC
    'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND
  )
)

;; Helper function to check if attestation is active
(define-private (check-attestation-active (attestor principal))
  (match (get-asset-attestation tx-sender u"temp-asset" attestor)
    attestation-data (if (get is-active attestation-data) u1 u0)
    u0
  )
)

;; Check if asset has active challenges
(define-read-only (has-active-challenges (user principal) (asset-id (string-utf8 36)))
  (fold or (map check-challenge-active (generate-attestor-list)) false)
)

;; Helper function to check if challenge is active
(define-private (check-challenge-active (challenger principal))
  (match (get-verification-challenge tx-sender u"temp-asset" challenger)
    challenge-data 
    (and
      (not (get is-resolved challenge-data))
      (< stacks-block-height (+ (get challenge-timestamp challenge-data) challenge-period))
    )
    false
  )
)

;; Submit cryptographic proof of asset ownership
(define-public (submit-asset-proof (asset-id (string-utf8 36)) (proof-hash (buff 32)))
  (let (
    (user tx-sender)
    (existing-verification (get-asset-verification user asset-id))
  )
    ;; Check if asset exists in the system
    (asserts! (is-some (get-asset user asset-id)) err-not-registered)
    ;; Prevent duplicate verifications
    (asserts! (is-none existing-verification) err-verification-exists)
    
    ;; Store the verification proof
    (map-set asset-verifications
      { user: user, asset-id: asset-id }
      {
        proof-hash: proof-hash,
        proof-timestamp: stacks-block-height,
        verification-score: u100, ;; Initial score
        is-verified: true,
        challenge-count: u0
      }
    )
    (ok true)
  )
)

;; Third parties can attest to asset ownership validity
(define-public (attest-asset-ownership (user principal) (asset-id (string-utf8 36)) (attestation-hash (buff 32)))
  (let (
    (attestor tx-sender)
    (existing-attestation (get-asset-attestation user asset-id attestor))
    (attestor-rep (get-attestor-reputation attestor))
  )
    ;; Verify asset has initial verification
    (asserts! (is-some (get-asset-verification user asset-id)) err-verification-not-found)
    ;; Prevent self-attestation
    (asserts! (not (is-eq attestor user)) err-cannot-attest-own-asset)
    ;; Prevent duplicate attestations
    (asserts! (is-none existing-attestation) err-attestation-exists)
    ;; Check attestor has sufficient stake
    (asserts! (>= (stx-get-balance attestor) attestor-stake-amount) err-insufficient-balance)
    
    ;; Transfer stake to contract
    (try! (stx-transfer? attestor-stake-amount attestor (as-contract tx-sender)))
    
    ;; Record the attestation
    (map-set asset-attestations
      { user: user, asset-id: asset-id, attestor: attestor }
      {
        attestation-hash: attestation-hash,
        attestation-timestamp: stacks-block-height,
        stake-amount: attestor-stake-amount,
        is-active: true
      }
    )
    
    ;; Update attestor reputation
    (map-set attestor-reputation
      { attestor: attestor }
      {
        total-attestations: (+ (get total-attestations attestor-rep) u1),
        successful-attestations: (get successful-attestations attestor-rep),
        reputation-score: (+ (get reputation-score attestor-rep) u10),
        total-stake: (+ (get total-stake attestor-rep) attestor-stake-amount)
      }
    )
    
    (ok true)
  )
)

;; Challenge an asset verification if suspected to be fraudulent
(define-public (challenge-asset-verification (user principal) (asset-id (string-utf8 36)) (challenge-reason (string-utf8 256)))
  (let (
    (challenger tx-sender)
    (verification-data (unwrap! (get-asset-verification user asset-id) err-verification-not-found))
    (existing-challenge (get-verification-challenge user asset-id challenger))
    (challenge-stake (/ attestor-stake-amount u2)) ;; Half of attestation stake
  )
    ;; Prevent duplicate challenges from same challenger
    (asserts! (is-none existing-challenge) err-challenge-exists)
    ;; Ensure challenger has sufficient stake
    (asserts! (>= (stx-get-balance challenger) challenge-stake) err-insufficient-balance)
    
    ;; Transfer challenge stake to contract
    (try! (stx-transfer? challenge-stake challenger (as-contract tx-sender)))
    
    ;; Record the challenge
    (map-set verification-challenges
      { user: user, asset-id: asset-id, challenger: challenger }
      {
        challenge-reason: challenge-reason,
        challenge-timestamp: stacks-block-height,
        challenge-stake: challenge-stake,
        is-resolved: false,
        resolution: none
      }
    )
    
    ;; Update verification to reflect challenge
    (map-set asset-verifications
      { user: user, asset-id: asset-id }
      (merge verification-data { challenge-count: (+ (get challenge-count verification-data) u1) })
    )
    
    (ok true)
  )
)

;; Resolve a challenge (simplified - in practice would need governance mechanism)
(define-public (resolve-challenge (user principal) (asset-id (string-utf8 36)) (challenger principal) (challenge-upheld bool))
  (let (
    (resolver tx-sender)
    (challenge-data (unwrap! (get-verification-challenge user asset-id challenger) err-challenge-not-found))
    (verification-data (unwrap! (get-asset-verification user asset-id) err-verification-not-found))
  )
    ;; Simple authorization check (in practice would use governance)
    (asserts! (is-eq resolver (var-get contract-owner)) err-unauthorized)
    ;; Ensure challenge is not already resolved
    (asserts! (not (get is-resolved challenge-data)) err-challenge-exists)
    
    ;; Resolve the challenge
    (map-set verification-challenges
      { user: user, asset-id: asset-id, challenger: challenger }
      (merge challenge-data { is-resolved: true, resolution: (some challenge-upheld) })
    )
    
    ;; If challenge upheld, mark verification as invalid and return stake
    (if challenge-upheld
      (begin
        (map-set asset-verifications
          { user: user, asset-id: asset-id }
          (merge verification-data { is-verified: false, verification-score: u0 })
        )
        ;; Return stake to challenger
        (try! (as-contract (stx-transfer? (get challenge-stake challenge-data) tx-sender challenger)))
        (ok true)
      )
      ;; If challenge rejected, forfeit challenger's stake
      (ok false)
    )
  )
)

;; Get comprehensive verification status for an asset
(define-read-only (get-asset-verification-status (user principal) (asset-id (string-utf8 36)))
  (let (
    (verification-data (get-asset-verification user asset-id))
    (attestation-count (get-attestation-count user asset-id))
    (has-challenges (has-active-challenges user asset-id))
  )
    (match verification-data
      verification-info
      (ok {
        has-proof: true,
        is-verified: (get is-verified verification-info),
        verification-score: (get verification-score verification-info),
        attestation-count: attestation-count,
        meets-requirements: (is-asset-sufficiently-verified user asset-id),
        has-active-challenges: has-challenges,
        challenge-count: (get challenge-count verification-info)
      })
      (ok {
        has-proof: false,
        is-verified: false,
        verification-score: u0,
        attestation-count: u0,
        meets-requirements: false,
        has-active-challenges: false,
        challenge-count: u0
      })
    )
  )
)

