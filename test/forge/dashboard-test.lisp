;;;; test/forge/dashboard-test.lisp — Forge dashboard tests
(in-package :cauldron.test)

(defsuite :forge-dashboard)

;;; ============================================================
;;; forge-layout (pure HTML generator)
;;; ============================================================

(deftest test-forge-layout-returns-string
  "forge-layout returns a non-nil string for minimal input."
  (let ((result (cauldron.forge::forge-layout "Test Page" '(:p "hello"))))
    (is-not-nil result)
    (is (stringp result))))

(deftest test-forge-layout-contains-title
  "forge-layout output contains the passed title text."
  (let ((result (cauldron.forge::forge-layout "My Title" '(:p "content"))))
    (is (search "My Title" result))))

(deftest test-forge-layout-contains-dashboard-link
  "forge-layout sidebar contains Dashboard link."
  (let ((result (cauldron.forge::forge-layout "Test" '(:p "x"))))
    (is (search "Dashboard" result))))

(deftest test-forge-layout-contains-tool-links
  "forge-layout sidebar contains SQL Console, Schema Browser, Audit Log, System."
  (let ((result (cauldron.forge::forge-layout "Test" '(:p "x"))))
    (is (search "SQL Console" result))
    (is (search "Schema Browser" result))
    (is (search "Audit Log" result))
    (is (search "System" result))))

(deftest test-forge-layout-default-title-without-config
  "Without config, forge-layout defaults to 'Forge Admin' app title."
  (let ((result (cauldron.forge::forge-layout "Page" '(:p "x"))))
    (is (search "Forge Admin" result))))

(deftest test-forge-layout-custom-title-with-config
  "With config, forge-layout uses the config title."
  (let* ((config (cauldron.forge:derive-forge-config
                  :title "My App Admin"
                  :resources '()))
         (result (cauldron.forge::forge-layout "Page" '(:p "x") :config config)))
    (is (search "My App Admin" result))))

(deftest test-forge-layout-contains-content
  "Content passed to forge-layout appears in output."
  (let ((result (cauldron.forge::forge-layout "Test" '(:p "unique-sentinel-text"))))
    (is (search "unique-sentinel-text" result))))

(deftest test-forge-layout-resource-links-with-config
  "With config containing resources, sidebar has resource links."
  (let* ((config (cauldron.forge:derive-forge-config
                  :resources (list (list :name 'user :attributes '(name email) :actions '())
                                   (list :name 'post :attributes '(title body) :actions '()))))
         (result (cauldron.forge::forge-layout "Test" '(:p "x") :config config)))
    (is (search "User" result))
    (is (search "Post" result))))

;;; ============================================================
;;; dashboard-view (view function)
;;; ============================================================

(deftest test-dashboard-view-returns-conn
  "dashboard-view returns a non-nil conn."
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge"))
         (result (cauldron.forge:dashboard-view conn nil)))
    (is-not-nil result)))

(deftest test-dashboard-view-status-200
  "dashboard-view sets response status to 200."
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge"))
         (result (cauldron.forge:dashboard-view conn nil)))
    (is-equal 200 (cauldron.crucible:conn-status result))))

(deftest test-dashboard-view-contains-dashboard
  "dashboard-view body contains 'Dashboard'."
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge"))
         (result (cauldron.forge:dashboard-view conn nil))
         (body (cauldron.crucible:conn-resp-body result)))
    (is (search "Dashboard" body))))

(deftest test-dashboard-view-nil-config-renders
  "dashboard-view with nil config renders without resource cards."
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge"))
         (result (cauldron.forge:dashboard-view conn nil))
         (body (cauldron.crucible:conn-resp-body result)))
    (is-not-nil body)
    (is (search "System" body))))

(deftest test-dashboard-view-with-resources
  "dashboard-view with config containing resources shows resource names."
  (let* ((config (cauldron.forge:derive-forge-config
                  :resources (list (list :name 'product :attributes '(name price) :actions '()))))
         (conn (cauldron.crucible:make-conn :method :get :path "/forge"))
         (result (cauldron.forge:dashboard-view conn config))
         (body (cauldron.crucible:conn-resp-body result)))
    (is (search "Product" body))))

(deftest test-dashboard-view-content-type-header
  "dashboard-view sets Content-Type header."
  (let* ((conn (cauldron.crucible:make-conn :method :get :path "/forge"))
         (result (cauldron.forge:dashboard-view conn nil))
         (headers (cauldron.crucible:conn-resp-headers result))
         (ct (cdr (assoc "Content-Type" headers :test #'string=))))
    (is-not-nil ct)))
