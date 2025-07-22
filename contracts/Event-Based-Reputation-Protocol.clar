(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-RATING (err u101))
(define-constant ERR-EVENT-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-RATED (err u103))
(define-constant ERR-SELF-RATING (err u104))

(define-data-var next-event-id uint u1)

(define-map events 
    { event-id: uint }
    {
        organizer: principal,
        title: (string-ascii 50),
        description: (string-ascii 200),
        date: uint,
        status: (string-ascii 20),
        rating-sum: uint,
        rating-count: uint
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

(define-public (create-event (title (string-ascii 50)) (description (string-ascii 200)) (date uint))
    (let ((event-id (var-get next-event-id)))
        (map-set events
            { event-id: event-id }
            {
                organizer: tx-sender,
                title: title,
                description: description,
                date: date,
                status: "active",
                rating-sum: u0,
                rating-count: u0
            }
        )
        (var-set next-event-id (+ event-id u1))
        (ok event-id)
    )
)

(define-public (complete-event (event-id uint))
    (let ((event (unwrap! (map-get? events {event-id: event-id}) (err u100))))
        (asserts! (is-eq tx-sender (get organizer event)) (err u101))
        (map-set events
            { event-id: event-id }
            (merge event { status: "completed" })
        )
        (ok true)
    )
)

(define-public (rate-event (event-id uint) (rating uint))
    (let (
        (event (unwrap! (map-get? events {event-id: event-id}) (err u404)))
        (organizer (get organizer event))
    )
        (asserts! (not (is-eq tx-sender organizer)) (err u401))
        (asserts! (and (>= rating u1) (<= rating u5)) (err u400))
        (asserts! (is-eq (get status event) "completed") (err u403))
        (asserts! (is-none (map-get? user-ratings {event-id: event-id, rater: tx-sender})) (err u409))
        
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