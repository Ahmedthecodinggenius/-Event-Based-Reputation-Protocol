(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-RATING (err u101))
(define-constant ERR-EVENT-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-RATED (err u103))
(define-constant ERR-SELF-RATING (err u104))
(define-constant ERR-BADGE-NOT-FOUND (err u105))
(define-constant ERR-BADGE-ALREADY-AWARDED (err u106))
(define-constant ERR-ALREADY-REGISTERED (err u108))
(define-constant ERR-NOT-REGISTERED (err u109))
(define-constant ERR-REGISTRATION-CLOSED (err u110))
(define-constant ERR-EVENT-FULL (err u111))
(define-constant ERR-EVENT-CANCELLED (err u112))
(define-constant ERR-EVENT-NOT-ACTIVE (err u113))
(define-constant ERR-EVENT-NOT-COMPLETED (err u114))
(define-constant ERR-LIST-FULL (err u115))
(define-constant ERR-CATEGORY-NOT-FOUND (err u116))

(define-data-var next-event-id uint u1)
(define-data-var next-badge-id uint u1)
(define-data-var next-category-id uint u1)

(define-map events
    { event-id: uint }
    {
        organizer: principal,
        title: (string-ascii 50),
        description: (string-ascii 200),
        date: uint,
        status: (string-ascii 20),
        rating-sum: uint,
        rating-count: uint,
        max-participants: uint,
        registration-count: uint,
        category-id: uint
    }
)

(define-map user-ratings
    { event-id: uint, rater: principal }
    { rating: uint }
)

(define-map user-reputation
    { user: principal }
    {
        total-events: uint,
        total-rating: uint,
        avg-rating: uint
    }
)

(define-map badges
    { badge-id: uint }
    {
        creator: principal,
        name: (string-ascii 30),
        description: (string-ascii 100),
        event-id: uint,
        awarded-to: (list 100 principal)
    }
)

(define-map user-badges
    { user: principal, badge-id: uint }
    { awarded-by: principal, timestamp: uint }
)

(define-map event-registrations
    { event-id: uint, user: principal }
    { registered-at: uint }
)

(define-map user-event-registrations
    { user: principal, event-id: uint }
    { registration-status: bool }
)

(define-map categories
    { category-id: uint }
    {
        name: (string-ascii 30),
        events: (list 100 uint)
    }
)

(define-public (create-event (title (string-ascii 50)) (description (string-ascii 200)) (date uint) (max-participants uint) (category-id uint))
    (let ((event-id (var-get next-event-id)) (category (unwrap! (map-get? categories {category-id: category-id}) ERR-CATEGORY-NOT-FOUND)))
        (map-set events
            { event-id: event-id }
            {
                organizer: tx-sender,
                title: title,
                description: description,
                date: date,
                status: "active",
                rating-sum: u0,
                rating-count: u0,
                max-participants: max-participants,
                registration-count: u0,
                category-id: category-id
            }
        )
        (var-set next-event-id (+ event-id u1))
        (map-set categories {category-id: category-id} (merge category {events: (unwrap! (as-max-len? (append (get events category) event-id) u100) ERR-LIST-FULL)}))
        (ok event-id)
    )
)

(define-public (create-category (name (string-ascii 30)))
    (let ((category-id (var-get next-category-id)))
        (map-set categories
            { category-id: category-id }
            {
                name: name,
                events: (list)
            }
        )
        (var-set next-category-id (+ category-id u1))
        (ok category-id)
    )
)

(define-public (complete-event (event-id uint))
    (let ((event (unwrap! (map-get? events {event-id: event-id}) ERR-EVENT-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status event) "active") ERR-EVENT-NOT-ACTIVE)
        (map-set events
            { event-id: event-id }
            (merge event { status: "completed" })
        )
        (ok true)
    )
)

(define-public (cancel-event (event-id uint))
    (let ((event (unwrap! (map-get? events {event-id: event-id}) ERR-EVENT-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status event) "active") ERR-EVENT-NOT-ACTIVE)
        (map-set events {event-id: event-id} (merge event {status: "cancelled"}))
        (ok true)
    )
)

(define-public (rate-event (event-id uint) (rating uint))
    (let (
        (event (unwrap! (map-get? events {event-id: event-id}) ERR-EVENT-NOT-FOUND))
        (organizer (get organizer event))
    )
        (asserts! (not (is-eq tx-sender organizer)) ERR-SELF-RATING)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (is-eq (get status event) "completed") ERR-EVENT-NOT-COMPLETED)
        (asserts! (is-none (map-get? user-ratings {event-id: event-id, rater: tx-sender})) ERR-ALREADY-RATED)
        
        (map-set user-ratings {event-id: event-id, rater: tx-sender} {rating: rating})
        
        (map-set events
            {event-id: event-id}
            (merge event {
                rating-sum: (+ (get rating-sum event) rating),
                rating-count: (+ (get rating-count event) u1)
            })
        )
        
        (update-user-reputation organizer rating)
        (ok true)
    )
)

(define-private (update-user-reputation (user principal) (new-rating uint))
    (let (
        (current-rep (default-to 
            {total-events: u0, total-rating: u0, avg-rating: u0}
            (map-get? user-reputation {user: user})
        ))
    )
        (map-set user-reputation
            {user: user}
            {
                total-events: (+ (get total-events current-rep) u1),
                total-rating: (+ (get total-rating current-rep) new-rating),
                avg-rating: (/ (+ (get total-rating current-rep) new-rating) 
                             (+ (get total-events current-rep) u1))
            }
        )
    )
)

(define-read-only (get-event (event-id uint))
    (map-get? events {event-id: event-id})
)

(define-read-only (get-category (category-id uint))
    (map-get? categories {category-id: category-id})
)

(define-read-only (get-events-by-category (category-id uint))
    (let ((category (unwrap! (map-get? categories {category-id: category-id}) ERR-CATEGORY-NOT-FOUND)))
        (ok (get events category))
    )
)

(define-read-only (get-user-reputation (user principal))
    (map-get? user-reputation {user: user})
)

(define-read-only (get-event-rating (event-id uint))
    (let ((event (unwrap! (map-get? events {event-id: event-id}) ERR-EVENT-NOT-FOUND)))
        (ok {
            rating-sum: (get rating-sum event),
            rating-count: (get rating-count event),
            avg-rating: (if (is-eq (get rating-count event) u0)
                u0
                (/ (get rating-sum event) (get rating-count event)))
        })
    )
)

(define-public (create-badge (name (string-ascii 30)) (description (string-ascii 100)) (event-id uint))
    (let (
        (badge-id (var-get next-badge-id))
        (event (unwrap! (map-get? events {event-id: event-id}) ERR-EVENT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
        (map-set badges
            { badge-id: badge-id }
            {
                creator: tx-sender,
                name: name,
                description: description,
                event-id: event-id,
                awarded-to: (list)
            }
        )
        (var-set next-badge-id (+ badge-id u1))
        (ok badge-id)
    )
)

(define-public (award-badge (badge-id uint) (recipient principal))
    (let (
        (badge (unwrap! (map-get? badges {badge-id: badge-id}) ERR-BADGE-NOT-FOUND))
        (current-awarded (get awarded-to badge))
    )
        (asserts! (is-eq tx-sender (get creator badge)) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? user-badges {user: recipient, badge-id: badge-id})) ERR-BADGE-ALREADY-AWARDED)
        
        (map-set user-badges
            {user: recipient, badge-id: badge-id}
            {awarded-by: tx-sender, timestamp: burn-block-height}
        )
        
        (map-set badges
            {badge-id: badge-id}
            (merge badge {awarded-to: (unwrap! (as-max-len? (append current-awarded recipient) u100) ERR-LIST-FULL)})
        )
        (ok true)
    )
)

(define-read-only (get-badge (badge-id uint))
    (map-get? badges {badge-id: badge-id})
)

(define-read-only (get-user-badge-count (user principal))
    (len (filter has-badge-for-user (list
        u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20
        u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40
        u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59 u60
        u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79 u80
        u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99 u100
    )))
)

(define-private (has-badge-for-user (badge-id uint))
    (is-some (map-get? user-badges {user: tx-sender, badge-id: badge-id}))
)

(define-public (register-for-event (event-id uint))
    (let (
        (event (unwrap! (map-get? events {event-id: event-id}) ERR-EVENT-NOT-FOUND))
        (current-registrations (get registration-count event))
        (max-participants (get max-participants event))
    )
        (asserts! (is-eq (get status event) "active") ERR-REGISTRATION-CLOSED)
        (asserts! (< current-registrations max-participants) ERR-EVENT-FULL)
        (asserts! (is-none (map-get? user-event-registrations {user: tx-sender, event-id: event-id})) ERR-ALREADY-REGISTERED)
        
        (map-set event-registrations
            {event-id: event-id, user: tx-sender}
            {registered-at: burn-block-height}
        )
        
        (map-set user-event-registrations
            {user: tx-sender, event-id: event-id}
            {registration-status: true}
        )
        
        (map-set events
            {event-id: event-id}
            (merge event {registration-count: (+ current-registrations u1)})
        )
        (ok true)
    )
)

(define-public (unregister-from-event (event-id uint))
    (let (
        (event (unwrap! (map-get? events {event-id: event-id}) ERR-EVENT-NOT-FOUND))
        (current-registrations (get registration-count event))
    )
        (asserts! (is-eq (get status event) "active") ERR-REGISTRATION-CLOSED)
        (asserts! (is-some (map-get? user-event-registrations {user: tx-sender, event-id: event-id})) ERR-NOT-REGISTERED)
        
        (map-delete event-registrations {event-id: event-id, user: tx-sender})
        (map-delete user-event-registrations {user: tx-sender, event-id: event-id})
        
        (map-set events
            {event-id: event-id}
            (merge event {registration-count: (- current-registrations u1)})
        )
        (ok true)
    )
)

(define-read-only (get-event-registrations (event-id uint))
    (let (
        (event (unwrap! (map-get? events {event-id: event-id}) ERR-EVENT-NOT-FOUND))
    )
        (ok {
            registration-count: (get registration-count event),
            max-participants: (get max-participants event),
            slots-available: (- (get max-participants event) (get registration-count event))
        })
    )
)

(define-read-only (is-user-registered (user principal) (event-id uint))
    (is-some (map-get? user-event-registrations {user: user, event-id: event-id}))
)

(define-read-only (get-user-registrations (user principal))
    (len (filter is-registered-for-event (list
        u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20
        u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40
        u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59 u60
        u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79 u80
        u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99 u100
    )))
)

(define-private (is-registered-for-event (event-id uint))
    (is-some (map-get? user-event-registrations {user: tx-sender, event-id: event-id})))
