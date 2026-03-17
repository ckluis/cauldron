;;;; src/forge/integrations.lisp — Forge integration health dashboard
(in-package :cauldron.forge)

(defun integrations-view (conn config)
  "Render the integrations health dashboard.
Table: Name | Status | Success Rate | Avg Latency | Last Call | Last Error"
  (let* ((all-health (cauldron.integration:all-integration-health))
         (integrations (cauldron.integration:list-integrations))
         (body
           (forge-layout "Integrations"
             (cauldron.alembic:html
               (:div :class "forge-section"
                 (:p (format nil "~D registered integration~:P" (length integrations)))
                 (if integrations
                     `(:table :class "forge-table"
                        (:thead
                          (:tr (:th "Name") (:th "Status") (:th "Success Rate")
                               (:th "Avg Latency") (:th "Last Call") (:th "Last Error")))
                        (:tbody
                          ,@(mapcar
                              (lambda (name)
                                (let* ((stats (cdr (assoc name all-health)))
                                       (total (if stats (cauldron.integration:health-stats-total-calls stats) 0))
                                       (successes (if stats (cauldron.integration:health-stats-success-count stats) 0))
                                       (rate (if (plusp total) (* 100.0 (/ successes total)) 0.0))
                                       (avg-ms (if (plusp total)
                                                   (round (cauldron.integration:health-stats-total-duration-ms stats) total)
                                                   0))
                                       (status-class (cond
                                                       ((zerop total) "forge-status-neutral")
                                                       ((> rate 95) "forge-status-green")
                                                       ((> rate 80) "forge-status-yellow")
                                                       (t "forge-status-red")))
                                       (status-label (cond
                                                       ((zerop total) "No data")
                                                       ((> rate 95) "Healthy")
                                                       ((> rate 80) "Degraded")
                                                       (t "Unhealthy")))
                                       (last-error (when stats
                                                     (cauldron.integration:health-stats-last-error stats))))
                                  `(:tr
                                     (:td ,(string-downcase (string name)))
                                     (:td (:span :class ,status-class ,status-label))
                                     (:td ,(format nil "~,1F%" rate))
                                     (:td ,(format nil "~Dms" avg-ms))
                                     (:td ,(if (and stats (plusp (cauldron.integration:health-stats-last-call-at stats)))
                                               "Active"
                                               "Never"))
                                     (:td ,(or (when last-error
                                                 (subseq (format nil "~A" last-error)
                                                         0 (min 80 (length (format nil "~A" last-error)))))
                                               "—")))))
                              integrations)))
                     `(:p "No integrations registered."))))
             :config config)))
    (cauldron.crucible:conn-put-resp-body
     (cauldron.crucible:conn-put-resp-header
      (cauldron.crucible:conn-put-status conn 200)
      "Content-Type" "text/html; charset=utf-8")
     body)))
