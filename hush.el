;;; hush.el --- Pluggable secret manager -*- lexical-binding: t -*-

;; Author: Theodor-Alexandru Irimia
;; Maintainer: Theodor-Alexandru Irimia
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/tirimia/hush
;; Keywords: extensions lisp local tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; hush.el helps abstract getting data out of external password managers
;; By default, supports manual input and the 1password-cli tool.
;; You can add support for your own preferred tool by writing your own
;; handler function and appending it together with its name to the hush-engine-alist

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup hush nil
  "Settings for `hush.el'."
  :link '(url-link "https://github.com/tirimia/hush")
  :prefix "hush-"
  :group 'external)

(defcustom hush-cache #'hush--default-cache
  "Caching function to use.
See function `hush--default-cache' for how to build your own cache."
  :type '(choice (function-item :tag "Enabled" :value #'hush--default-cache)
                 (function :tag "Your own cache function")
                 (const :tag "Disabled" nil))
  :group 'hush)

(defcustom hush-cache-timeout 3600
  "Number of seconds secrets are considered valid."
  :type 'integer
  :group 'hush)

(defcustom hush-default-engine "prompt"
  "Default engine to use when fetching secrets."
  :type 'string
  :group 'hush)

(defcustom hush-engine-alist `(("prompt" . hush--prompt-engine)
                               ("1password" . hush--onepassword-engine))
  "Collection of secret backends.
Keys are the names and values are their callback functions."
  :type '(alist :key-type string :value-type function)
  :group 'hush)

(defvar hush--default-cache (make-hash-table :test #'equal)
  "Default cache for `hush.el'.")

(defun hush--prompt-engine (prompt)
  "Get secret by asking the user using PROMPT."
  (read-string (format "%s: " prompt)))

(defun hush--onepassword-engine (parameters)
  "Get secret out of 1password using PARAMETERS.
PARAMETERS is a plist containing keys :vault and :path.
Requires the 1password-cli command line tool to be installed and configured."
  (unless (locate-file "op" exec-path) (user-error "Could not find `op` in `exec-path'"))
  (let ((vault (plist-get parameters :vault))
        (path (plist-get parameters :path)))
    (shell-command-to-string (format "op read op://%s/%s -n" vault path))))

(defun hush--default-cache (action &optional secret engine value)
  "Run ACTION on the `hush-cache'.
ACTION can be one of `get', `set' or `flush'.
`get' gets the SECRET based on ENGINE if not expired.
`set' puts the VALUE in the cache.
`flush' just flushes the cache."

  (if (eq action 'flush) (clrhash hush--default-cache)
    (let* ((current-time (time-convert nil 'integer))
           (key (list secret engine))
           (expiration-time (+ current-time hush-cache-timeout)))
      (pcase action
        ('get (if-let* ((cached (gethash key hush--default-cache))
                        (value-timestamp (plist-get cached :expires))
                        (value-fresh (< current-time value-timestamp)))
                  (plist-get cached :value)
                (remhash key hush--default-cache)))
        ('set (plist-get (puthash key `(:expires ,expiration-time :value ,value) hush--default-cache) :value))
        (_ (user-error "Wrong ACTION provided to `hush--default-cache': %s" action))))))

(defun hush-get (secret &optional engine)
  "Return secret string based on SECRET.
SECRET may be any type as long as the ENGINE supports it in its callback.
If ENGINE given, use it to fetch the secret instead of `hush-default-engine'."
  (if-let* ((engine (or engine hush-default-engine))
            (hush-engine (assoc-string engine hush-engine-alist))
            (callback (cdr hush-engine)))
      (let* ((cached-value (when hush-cache (funcall hush-cache 'get secret engine)))
             (secret-value (or cached-value (funcall callback secret))))
        (when hush-cache (funcall hush-cache 'set secret engine secret-value))
        (or cached-value secret-value))
    (user-error "Wrong secret engine (%s) or misconfigured `hush-engine-alist'" engine)))

;;;###autoload
(defun hush-clear-cache ()
  "Clears the secret cache."
  (interactive)
  (unless hush-cache (user-error "`hush-cache' is not configured"))
  (funcall hush-cache 'flush)
  (message "Cache of %s cleared successfully" hush-cache))

(provide 'hush)
;;; hush.el ends here
