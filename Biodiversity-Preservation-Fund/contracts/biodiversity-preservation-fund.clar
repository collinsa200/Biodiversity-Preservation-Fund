;; Biodiversity Preservation Fund Smart Contract
;; Tokenize endangered species protection with transparent funding

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-project-inactive (err u105))
(define-constant err-project-completed (err u106))
(define-constant err-already-voted (err u107))

;; Data Variables
(define-data-var total-projects uint u0)
(define-data-var total-funds-raised uint u0)
(define-data-var conservation-token-supply uint u1000000000000) ;; 1M tokens with 6 decimals

;; Data Maps
(define-map conservation-projects
  uint
  {
    species-name: (string-ascii 100),
    location: (string-ascii 100),
    funding-goal: uint,
    funds-raised: uint,
    project-owner: principal,
    status: (string-ascii 20), ;; "active", "completed", "cancelled"
    created-at: uint,
    description: (string-ascii 500)
  }
)

(define-map project-contributors
  {project-id: uint, contributor: principal}
  {amount: uint, timestamp: uint}
)

(define-map user-token-balance
  principal
  uint
)

(define-map project-votes
  {project-id: uint, voter: principal}
  {vote-type: (string-ascii 10), timestamp: uint} ;; "approve" or "reject"
)

(define-map project-vote-counts
  uint
  {approve: uint, reject: uint}
)

(define-map species-conservation-status
  (string-ascii 100)
  {
    threat-level: (string-ascii 20), ;; "critical", "endangered", "vulnerable"
    population-estimate: uint,
    last-updated: uint,
    verified: bool
  }
)

(define-map authorized-conservationists
  principal
  bool
)

;; Token Management Functions
(define-public (mint-conservation-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (let ((current-balance (default-to u0 (map-get? user-token-balance recipient))))
      (map-set user-token-balance recipient (+ current-balance amount))
      (ok amount)
    )
  )
)

(define-public (transfer-tokens (sender principal) (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender sender) err-unauthorized)
    (let ((sender-balance (default-to u0 (map-get? user-token-balance sender))))
      (asserts! (>= sender-balance amount) err-insufficient-funds)
      (let ((recipient-balance (default-to u0 (map-get? user-token-balance recipient))))
        (map-set user-token-balance sender (- sender-balance amount))
        (map-set user-token-balance recipient (+ recipient-balance amount))
        (ok amount)
      )
    )
  )
)

;; Project Management Functions
(define-public (create-conservation-project 
  (species-name (string-ascii 100))
  (location (string-ascii 100))
  (funding-goal uint)
  (description (string-ascii 500))
)
  (let ((project-id (+ (var-get total-projects) u1)))
    (asserts! (> funding-goal u0) err-invalid-amount)
    (map-set conservation-projects project-id
      {
        species-name: species-name,
        location: location,
        funding-goal: funding-goal,
        funds-raised: u0,
        project-owner: tx-sender,
        status: "active",
        created-at: block-height,
        description: description
      }
    )
    (map-set project-vote-counts project-id {approve: u0, reject: u0})
    (var-set total-projects project-id)
    (ok project-id)
  )
)

(define-public (contribute-to-project (project-id uint) (amount uint))
  (let ((project (unwrap! (map-get? conservation-projects project-id) err-not-found)))
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-eq (get status project) "active") err-project-inactive)
    (let ((user-balance (default-to u0 (map-get? user-token-balance tx-sender))))
      (asserts! (>= user-balance amount) err-insufficient-funds)
      (let ((new-funds-raised (+ (get funds-raised project) amount)))
        ;; Transfer tokens from user to contract
        (try! (transfer-tokens tx-sender contract-owner amount))
        ;; Update project funds
        (map-set conservation-projects project-id
          (merge project {funds-raised: new-funds-raised})
        )
        ;; Record contribution
        (map-set project-contributors
          {project-id: project-id, contributor: tx-sender}
          {amount: amount, timestamp: block-height}
        )
        ;; Update total funds raised
        (var-set total-funds-raised (+ (var-get total-funds-raised) amount))
        ;; Check if funding goal is reached
        (if (>= new-funds-raised (get funding-goal project))
          (map-set conservation-projects project-id
            (merge project {status: "completed", funds-raised: new-funds-raised})
          )
          true
        )
        (ok amount)
      )
    )
  )
)

;; Voting Functions
(define-public (vote-on-project (project-id uint) (vote-type (string-ascii 10)))
  (let ((project (unwrap! (map-get? conservation-projects project-id) err-not-found)))
    (asserts! (or (is-eq vote-type "approve") (is-eq vote-type "reject")) err-invalid-amount)
    (asserts! (is-none (map-get? project-votes {project-id: project-id, voter: tx-sender})) err-already-voted)
    (let ((vote-counts (unwrap! (map-get? project-vote-counts project-id) err-not-found)))
      ;; Record vote
      (map-set project-votes
        {project-id: project-id, voter: tx-sender}
        {vote-type: vote-type, timestamp: block-height}
      )
      ;; Update vote counts
      (if (is-eq vote-type "approve")
        (map-set project-vote-counts project-id
          (merge vote-counts {approve: (+ (get approve vote-counts) u1)})
        )
        (map-set project-vote-counts project-id
          (merge vote-counts {reject: (+ (get reject vote-counts) u1)})
        )
      )
      (ok true)
    )
  )
)

;; Conservation Status Management
(define-public (update-species-status
  (species-name (string-ascii 100))
  (threat-level (string-ascii 20))
  (population-estimate uint)
)
  (begin
    (asserts! (default-to false (map-get? authorized-conservationists tx-sender)) err-unauthorized)
    (map-set species-conservation-status species-name
      {
        threat-level: threat-level,
        population-estimate: population-estimate,
        last-updated: block-height,
        verified: true
      }
    )
    (ok true)
  )
)

(define-public (authorize-conservationist (conservationist principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-conservationists conservationist true)
    (ok true)
  )
)

;; Emergency Functions
(define-public (cancel-project (project-id uint))
  (let ((project (unwrap! (map-get? conservation-projects project-id) err-not-found)))
    (asserts! (or (is-eq tx-sender (get project-owner project)) (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (is-eq (get status project) "active") err-project-inactive)
    (map-set conservation-projects project-id
      (merge project {status: "cancelled"})
    )
    (ok true)
  )
)

(define-public (withdraw-funds (project-id uint) (amount uint))
  (let ((project (unwrap! (map-get? conservation-projects project-id) err-not-found)))
    (asserts! (is-eq tx-sender (get project-owner project)) err-unauthorized)
    (asserts! (is-eq (get status project) "completed") err-project-completed)
    (asserts! (<= amount (get funds-raised project)) err-insufficient-funds)
    (try! (transfer-tokens contract-owner tx-sender amount))
    (ok amount)
  )
)

;; Read-only Functions
(define-read-only (get-project-info (project-id uint))
  (map-get? conservation-projects project-id)
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (map-get? user-token-balance user))
)

(define-read-only (get-contribution (project-id uint) (contributor principal))
  (map-get? project-contributors {project-id: project-id, contributor: contributor})
)

(define-read-only (get-project-votes (project-id uint))
  (map-get? project-vote-counts project-id)
)

(define-read-only (get-species-status (species-name (string-ascii 100)))
  (map-get? species-conservation-status species-name)
)

(define-read-only (get-total-projects)
  (var-get total-projects)
)

(define-read-only (get-total-funds-raised)
  (var-get total-funds-raised)
)

(define-read-only (is-authorized-conservationist (user principal))
  (default-to false (map-get? authorized-conservationists user))
)

;; Initialize contract with initial token allocation
(begin
  (map-set user-token-balance contract-owner (var-get conservation-token-supply))
  (map-set authorized-conservationists contract-owner true)
)