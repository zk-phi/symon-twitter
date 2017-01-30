(require 'button)
(require 'cl-lib)
(require 'json)
(require 'jit-lock)

(require 'oauth)

;; + customs / constants
;;   + core

(defgroup symon-twitter nil
  "Twitter notification-based Twitter client."
  :group 'symon)

(defgroup symon-twitter-timeline nil
  "Timeline settings for symon-twitter.el."
  :group 'symon-twitter)

(defcustom symon-twitter-access-token-file nil
  "File to save access tokens in."
  :group 'symon-twitter)

(defcustom symon-twitter-consumer-key nil
  "Consumer key for accessing twitter API."
  :group 'symon-twitter)

(defcustom symon-twitter-consumer-secret nil
  "Consumer secret for accessing twitter API."
  :group 'symon-twitter)

(defcustom symon-twitter-enable-avatars t
  "When non-nil, avatar images are displayed in timelines."
  :group 'symon-twitter-timeline)

(defcustom symon-twitter-enable-media-thumbnails t
  "When non-nil, media thumbnails are displayed in timelines."
  :group 'symon-twitter-timeline)

(defcustom symon-twitter-indent-conversation-string
  (concat (propertize " " 'face '(:inherit symon-twitter-button-face :inverse-video t)) " ")
  "String used to indent conversations."
  :group 'symon-twitter-timeline)

;;   + icons

(defconst symon-twitter--fontawesome-available-p
  (if (member "FontAwesome" (font-family-list)) t nil)
  "Non-nil iff `FontAwesome' font is available.")

(defconst symon-twitter-logo
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#5e9cdd" :height 1.35)))
  "The Twitter bird.")

(defconst symon-twitter-reply-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#b2b2b2" :height 1.35)))
  "Icon used to display reply button.")

(defconst symon-twitter-retweeted-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#77b255" :height 1.35)))
  "Icon used to indicate retweeted status.")

(defconst symon-twitter-retweet-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#b2b2b2" :height 1.35)))
  "Icon used to display retweet button.")

(defconst symon-twitter-favorited-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#ffac33" :height 1.35)))
  "Icon used to indicate favorited status.")

(defconst symon-twitter-favorite-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#b2b2b2" :height 1.35)))
  "Icon used to display favorite button.")

(defconst symon-twitter-quote-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#b2b2b2" :height 1.35)))
  "Icon used to display quote button.")

(defconst symon-twitter-delete-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#b2b2b2" :height 1.35)))
  "Icon used to display delete button.")

(defconst symon-twitter-verified-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#55acee")))
  "Icon used to indicate verified users.")

(defconst symon-twitter-protected-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#b2b2b2")))
  "Icon used to indicate protected users.")

(defconst symon-twitter-unavailable-icon
  (when symon-twitter--fontawesome-available-p
    (propertize "" 'face `(:family "FontAwesome" :foreground "#eda6a6" :height 1.35)))
  "Icon used to indicate protected users.")

;;   + faces

(defface symon-twitter-too-long-tweet-face
  '((t (:inherit warning)))
  "Face used to highlight tweet longer than 140 characters."
  :group 'symon-twitter)

(defface symon-twitter-button-face
  '((((background light)) (:foreground "#a0a0a0"))
    (t (:foreground "#606060")))
  "Face used to display reply/RT/Fav buttons."
  :group 'symon-twitter-timeline)

(defface symon-twitter-retweet-face
  '((t (:foreground "#77b255")))
  "Face used to highlight retweets in symon-twitter timelines."
  :group 'symon-twitter-timeline)

(defface symon-twitter-favorite-face
  '((t (:foreground "#ffac33")))
  "Face used to highlight favorites in symon-twitter timelines."
  :group 'symon-twitter-timeline)

(defface symon-twitter-link-face
  '((t (:inherit link)))
  "Face used to highlight links in tweets."
  :group 'symon-twitter-timeline)

(defface symon-twitter-screenname-face
  '((((background light)) (:foreground "#7e7765" :bold t))
    (t (:foreground "#faf5ee" :bold t)))
  "Face used to highlight @screenname in symon-twitter timelines."
  :group 'symon-twitter-timeline)

(defface symon-twitter-name-face
  '((((background light)) (:foreground "#aea89a"))
    (t (:foreground "#e2c69f")))
  "Face used to highlight names in symon-twitter timelines."
  :group 'symon-twitter-timeline)

(defface symon-twitter-verified-user-face
  '((t (:foreground "#55acee")))
  "Face used to display verified user indicators."
  :group 'symon-twitter-timeline)

(defface symon-twitter-protected-user-face
  '((t (:foreground "#b2b2b2")))
  "Face used to display protected user indicators."
  :group 'symon-twitter-timeline)

(defface symon-twitter-time-face
  '((((background light)) (:foreground "#aea89a"))
    (t (:foreground "#e2c69f")))
  "Face used to highlight tweet time in symon-twitter timelines."
  :group 'symon-twitter-timeline)

(defface symon-twitter-user-description-face
  '((((background light)) (:background "#e4e3de"))
    (t (:background "#4c4c4c")))
  "Face used to highlight user description in symon-twitter
timelines."
  :group 'symon-twitter-timeline)

(defface symon-twitter-quoted-tweet-face
  '((((background light)) (:background "#e4e3de"))
    (t (:background "#4c4c4c")))
  "Face used to highlight quoted tweets in symon-twitter
timelines."
  :group 'symon-twitter-timeline)

;; + utilities

(defun symon-twitter--plist-to-alist (plst)
  "Convert (K1 V1 K2 V2 ...) to ((K1 . V1) (K2 . V2) ...)."
  (when plst
    (cons (cons (car plst) (cadr plst))
          (symon-twitter--plist-to-alist (cddr plst)))))

(defun symon-twitter--make-button (str action &rest props)
  "Make a string button and return it. This is NOT a destructive
function unlike `make-text-button'."
  (declare (indent 1))
  (let ((str (copy-sequence str)))
    (apply 'make-text-button str 0 'action action props)
    str))

(defun symon-twitter--make-link-button (str url &rest props)
  "Make a string that links to URL and return it. This is NOT a
destructive function unlike `make-text-button'."
  (declare (indent 1))
  (apply 'symon-twitter--make-button str
         (lambda (b) (browse-url (button-get b 'url))) 'url url props))

(defun symon-twitter--read-tweet-text (&optional prefix suffix)
  "Like `read-from-minibuffer' but highlights when the text is
over 140 characters."
  (minibuffer-with-setup-hook
      (lambda ()
        (when prefix (insert prefix))
        (when suffix (save-excursion (insert suffix)))
        (add-hook 'post-command-hook
                  (lambda ()
                    (let ((beg (minibuffer-prompt-end))
                          (end (point-max)))
                      (put-text-property
                       beg end 'face
                       (when (> (- end beg) 140) 'symon-twitter-too-long-tweet-face))))
                  nil t))
    (read-from-minibuffer "Tweet: ")))

;; + fix for oauth.el

;; *TODO* MIGRATE TO request.el, WHICH OFFICIALLY SUPPORTS curl

;; 1. Advice `oauth-url-retrieve' to use curl also for async requests,
;; when `oauth-use-curl' is non-nil.

(defun symon-twitter--oauth-curl-retrieve-async (url cb cbdata)
  "Like `oauth-curl-retrieve' but asynchronous. CB is a function,
called with STATUS and CBDATA when finished. STATUS is either nil
for succes, or (:error (SYMBOL . DATA)) for failure."
  ;; almost a copy-paste of `oauth-curl-retrieve'
  (url-gc-dead-buffers)
  (with-current-buffer (generate-new-buffer " *oauth-request*")
    (let* ((curl-args `("-s" ,(when oauth-curl-insecure "-k")
                        "-X" ,url-request-method
                        "-i" ,url
                        "-N"              ; added (zk_phi)
                        ,@(when oauth-post-vars-alist
                            (apply
                             'append
                             (mapcar
                              (lambda (pair)
                                (list
                                 "-d"
                                 (concat (car pair) "="
                                         (oauth-hexify-string (cdr pair)))))
                              oauth-post-vars-alist)))
                        ,@(oauth-headers-to-curl url-request-extra-headers)))
           (proc (apply 'start-process "curl" (current-buffer) "curl" curl-args)))
      (set-process-sentinel
       proc
       `(lambda (p s)
          (with-current-buffer (process-buffer p)
            (url-mark-buffer-as-dead (current-buffer))
            (apply ',cb (unless (string= "finished\n" s) `(:error (error ,s))) ',cbdata))))
      (current-buffer))))

(defadvice oauth-url-retrieve (around symon-twitter--oauth-workaround activate)
  (let ((original-definition (symbol-function 'url-retrieve)))
    (unwind-protect
        (progn
          ;; locally redefine `url-retrieve' (why should `flet' be obsolete BTW ?)
          (when oauth-use-curl
            (fset 'url-retrieve 'symon-twitter--oauth-curl-retrieve-async))
          ad-do-it)
      ;; ... and restore `url-retrieve'
      (fset 'url-retrieve original-definition))))

(defun symon-twitter--oauth-post-url-async (token url vars-alist &optional cb cbdata)
  "Like `oauth-url-retrieve' but use the POST method."
  (let ((url-request-method "POST")
        (oauth-post-vars-alist vars-alist))
    (oauth-url-retrieve token url cb cbdata)))

;; 2. When `oauth-nonce-function' is unset, set it.

(unless oauth-nonce-function
  (if (require 'sasl nil t)
      (setq oauth-nonce-function 'sasl-unique-id)
    (setq oauth-nonce-function 'oauth-internal-make-nonce)))

;; + twitter API core

(put 'twitter-error 'error-conditions '(error twitter-error))
(put 'twitter-error 'error-message "Twitter error")

(defun symon-twitter--parse-response ()
  "Read current response buffer as JSON. Return nil on parse
error."
  (save-excursion
    (goto-char (point-max))
    (ignore-errors
      (search-backward-regexp "^[[{]")
      (let ((json-object-type 'hash-table)
            (json-key-type 'symbol)
            (json-array-type 'list))
        (json-read-from-string
         (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8))))))

(defun symon-twitter--call-api (url token &optional method async-cb &rest params)
  "Request URL by METHOD with PARAMS and returns the parsed
response on success, or nil iff Twitter did not return a
JSON. This function may raise an error on connection failure, or
signal `twitter-error' if Twitter returned an error
status.

 TOKEN can be either an OAuth access token or an alist which at
least has `token' as its key, associated with an OAuth access
token (which may usually be obtained with
`symon-twitter--verify-twitter-account').

 METHOD can be either `get' (default), `post'.

 ASYNC-CB can be a (pair of) function(s). If ASYNC-CB is
specified, this function immediately returns with the process
buffer and the (first) function is called with the response
later. When the second function is specified, it's called with a
signal of the form (ERROR-SYMBOL DATA ...) on failure. If the
second function is omitted, errors are demoted to a simple
message and never raised.

 PARAMS must be a plist of the form (\"KEY\"
\"VALUE\" \"KEY\" \"VALUE\" ...)."
  (let* ((token (if (oauth-access-token-p token) token (gethash 'token token)))
         (params (symon-twitter--plist-to-alist params))
         (url (if (or (null params) (eq method 'post))
                  url
                (concat
                 url "?"
                 (mapconcat (lambda (p) (format "%s=%s" (car p) (cdr p))) params "&"))))
         (callback (if (functionp async-cb) async-cb (car async-cb)))
         (errorback (if (functionp async-cb)
                        (lambda (e) (message "%s" (error-message-string e)))
                      (cdr async-cb))))
    (if (null async-cb)
        ;; synchronous call
        (let* ((res (with-current-buffer (if (eq method 'post)
                                             (oauth-post-url token url params)
                                           (oauth-url-retrieve token url))
                      (symon-twitter--parse-response)))
               (errors (and (hash-table-p res) (gethash 'errors res))))
          (if errors (signal 'twitter-error (list errors)) res))
      ;; asynchronous call
      (let ((cb
             (lambda (s cb eb)
               (cond ((null s)
                      (let* ((res (symon-twitter--parse-response))
                             (errors (and (hash-table-p res) (gethash 'errors res))))
                        (if errors
                            (funcall eb `(twitter-error ,errors))
                          (funcall cb res))))
                     ((eq (caar s) :error)
                      (funcall eb (cl-cadar s)))
                     (t
                      (funcall eb '(error "Twitter: Unexpected redirection detected."))))))
            (cbargs (list callback errorback)))
        (condition-case err
            (if (not (eq method 'post))
                (oauth-url-retrieve token url cb cbargs)
              (symon-twitter--oauth-post-url-async token url params cb cbargs))
          (error (funcall errorback err)))))))

(defun symon-twitter--call-rest-api (api token &optional method async-cb &rest params)
  "Call Twitter *REST API* API by METHOD with
PARAMS. Arguments (except for API) and returned value are the
same as `symon-twitter--call-api'."
  (apply 'symon-twitter--call-api
         (concat "https://api.twitter.com/1.1/" api ".json")
         token method async-cb params))

(defun symon-twitter--call-stream-api
  (api token callback &optional method sentinel &rest params)
  "Open a stream by calling Twitter Stream API API with
PARAMS. When a new message is arrived, CALLBACK is called with
the message.

 SENTINEL is a (pair of) function(s) and (the first one is)
called with the first message when finished. When the second
function is specified, it's called with the status object on an
error."
  (let* ((filter `(lambda (p s)
                    (with-current-buffer (process-buffer p)
                      (goto-char (point-max))
                      (insert s)
                      (save-match-data
                        (when (string-match "\n" s)
                          (let* ((end (progn (search-backward "\n") (point)))
                                 (beg (progn (search-backward "\n") (point)))
                                 (json-object-type 'hash-table)
                                 (json-key-type 'symbol)
                                 (json-array-type 'list)
                                 (obj (ignore-errors (json-read))))
                            (delete-region (point-min) beg)
                            (when obj (funcall ',callback obj))))))))
         (buf (apply 'symon-twitter--call-api
                     (concat "https://userstream.twitter.com/1.1/" api ".json")
                     token
                     method
                     (or sentinel (lambda (s) (message "Twitter: Disconnected.")))
                     params))
         (proc (get-buffer-process buf)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc filter)))

;; + account management

(defvar symon-twitter--twitter-accounts nil
  "List of verified twitter accounts. A twitter account is a hash
table of various account informations, including an OAuth access
token. An OAuth token can be verified and converted to a twitter
account object with `symon-twitter--verify-twitter-account'.")

(defun symon-twitter--read-twitter-account ()
  "Prompt a twitter account with minibuffer."
  (let ((accounts (mapcar (lambda (a) (cons (gethash 'screen_name a) a))
                          symon-twitter--twitter-accounts)))
    (cl-case (length symon-twitter--twitter-accounts)
      ((0) (error "Not logged in to twitter."))
      ((1) (car symon-twitter--twitter-accounts))
      (t   (cdr (assoc (completing-read "Account: " accounts) accounts))))))

;; account/verify_credentials
(defun symon-twitter--verify-twitter-account (token &optional async-cb)
  "Verify access token TOKEN and return a twitter account
object. When ASYNC-CB is non-nil, this function immediately
returns and call ASYNC-CB with the response later. [This function
calls `account/verify_credentials' once]"
  (message "Twitter: Verifying access token ...")
  (if async-cb
      (symon-twitter--call-rest-api
       "account/verify_credentials" token 'get
       `(lambda (s)
          (message "Twitter: Verifying access tokens ... Done (%s)."
                   (gethash 'screen_name s))
          (puthash 'token ',token s)
          (funcall ',async-cb s)))
    (let ((s (symon-twitter--call-rest-api "account/verify_credentials" token)))
      (message "Twitter: Verifying access tokens ... Done (%s)." (gethash 'screen_name s))
      (puthash 'token token s)
      s)))

(defun symon-twitter-login (&optional cont -tokens -tokens-loaded-p)
  "Load and verify access tokens in
`symon-twitter-access-token-file' asynchronously, if the file
exists. When CONT is non-nil, the thunk is called when
succeeded. -TOKENS and -TOKENS-LOADED-P are internal variables
and should not be used. [This function calls
`account/verify_credentials' once per a token]"
  (interactive)
  (when (and (not -tokens-loaded-p)
             (null symon-twitter--twitter-accounts)
             symon-twitter-access-token-file
             (file-exists-p symon-twitter-access-token-file))
    (with-temp-buffer
      (insert-file-contents symon-twitter-access-token-file)
      (goto-char 1)
      (setq -tokens-loaded-p t
            -tokens          (read (current-buffer)))))
  (cond (-tokens
         (symon-twitter--verify-twitter-account
          (car -tokens)
          `(lambda (s)
             (push s symon-twitter--twitter-accounts)
             (symon-twitter-login ',cont ',(cdr -tokens) t))))
        (t
         (when cont (funcall cont)))))

(defun symon-twitter--save-access-tokens ()
  "Save access tokens in `symon-twitter-access-token-file', if
the variable is set."
  (when symon-twitter-access-token-file
    (with-temp-buffer
      (prin1 (mapcar (lambda (a) (gethash 'token a))
                     (reverse symon-twitter--twitter-accounts))
             (current-buffer))
      (write-region (point-min) (point-max) symon-twitter-access-token-file))))

(defun symon-twitter-add-account ()
  "Add and authorize a twitter account. This function will update
`symon-twitter-access-token-file'. [This function calls
`account/verify_credentials' once]"
  (interactive)
  (unless (and symon-twitter-consumer-key symon-twitter-consumer-secret)
    (error "Consumer-key and consumer-secret are not set."))
  (unless symon-twitter--twitter-accounts
    (symon-twitter-login))
  (let* ((oauth-enable-browse-url window-system)
         (token (oauth-authorize-app symon-twitter-consumer-key
                                     symon-twitter-consumer-secret
                                     "https://api.twitter.com/oauth/request_token"
                                     "https://api.twitter.com/oauth/access_token"
                                     "https://api.twitter.com/oauth/authenticate"))
         (account (cond ((not (oauth-access-token-p token))
                         (error "Authentication failed."))
                        ((member token (mapcar (lambda (a) (gethash 'token a))
                                               symon-twitter--twitter-accounts))
                         (error "You've already logged in with this account."))
                        (t
                         (symon-twitter--verify-twitter-account token)))))
    (push account symon-twitter--twitter-accounts)
    (symon-twitter--save-access-tokens)
    (message "Twitter: Logged in to twitter as @%s (%s)."
             (gethash 'screen_name account) (gethash 'name account))))

(defun symon-twitter-remove-account ()
  "Remove a twitter account. This function may update
`symon-twitter-access-token-file'."
  (interactive)
  (let ((account (symon-twitter--read-twitter-account)))
    (setq symon-twitter--twitter-accounts
          (delq account symon-twitter--twitter-accounts))
    (symon-twitter--save-access-tokens)
    (message "Twitter: Logged out.")))

;; + twitter API wrappers

;; statuses/update
(defun symon-twitter-tweet (&optional str account in-reply-to quote)
  "Tweet STR with ACCOUNT asynchronously. When STR or ACCOUNT is
omitted or nil, prompt it. When IN-REPLY-TO is a tweet, tell
Twitter that the new tweet is a reply to the Tweet. When QUOTE is
a tweet, quote the tweet. [This function calls `statuses/update'
once]"
  (interactive)
  (unless account
    (setq account (symon-twitter--read-twitter-account)))
  (unless str
    (setq str (symon-twitter--read-tweet-text
               (when in-reply-to
                 (format "@%s " (gethash 'screen_name (gethash 'user in-reply-to))))
               (when quote
                 (format
                  " http://twitter.com/%s/status/%s"
                  (gethash 'screen_name (gethash 'user quote)) (gethash 'id_str quote))))))
  (message "Twitter: Posting a Tweet ...")
  (if in-reply-to
      (symon-twitter--call-rest-api
       "statuses/update" account 'post
       (lambda (s) (message "Twitter: Posting a Tweet ... Done."))
       "status" str
       "in_reply_to_status_id" (gethash 'id_str in-reply-to))
    (symon-twitter--call-rest-api
     "statuses/update" account 'post
     (lambda (s) (message "Twitter: Posting a Tweet ... Done."))
     "status" str)))

;; users/show
(defun symon-twitter--fetch-user-profile (screenname account &optional async-cb)
  "Get user object from SCREENNAME. When ASYNC-CB is non-nil,
this function immediately returns and call ASYNC-CB with the
response later. [This function calls `users/show' once]"
  (message "Twitter: Requesting user profile ... (%s)" screenname)
  (symon-twitter--call-rest-api
   "users/show" account 'get async-cb "screen_name" screenname))

;; statuses/destroy/:id
(defun symon-twitter--delete-tweet (tweet account &optional cont)
  "Delete TWEET tweeted by ACCOUNT asynchronously. When CONT is
non-nil, the thunk is called after succeeded. [This function
calls `statuses/destroy/:id' once]"
  (let ((id-str (gethash 'id_str tweet)))
    (message "Twitter: Deleting a Tweet ...")
    (symon-twitter--call-rest-api
     (concat "statuses/destroy/" id-str) account 'post
     `(lambda (s)
        (message "Twitter: Deleting a Tweet ... Done.")
        ,(when cont `(funcall ',cont)))
     "id" id-str)))

;; statuses/show/:id
(defun symon-twitter--fetch-tweet-from-id (id account &optional async-cb)
  "Return a tweet object whose id is equal to ID, if there's
one. This function forces Twitter to include
`current_user_retweet' field in the response. When ASYNC-CB is
non-nil, this function immediately returns and call ASYNC-CB with
the response later. [This function calls `statuses/show/:id'
once]"
  (message "Twitter: Requesting tweet details ...")
  (symon-twitter--call-rest-api
   (format "statuses/show/%s" id) account 'get async-cb
   "include_my_retweet" "true"))

;; statuses/show/:id
(defun symon-twitter--get-my-retweet (tweet account &optional async-cb)
  "Return ACCOUNT's retweet of TWEET if there's one. Otherwise
return nil. When ASYNC-CB is non-nil, this function immediately
returns and call ASYNC-CB with the response later. [This function
calls `statuses/show/:id' once]"
  (if async-cb
      (symon-twitter--fetch-tweet-from-id
       (gethash 'id_str tweet)
       account
       `(lambda (s)
          (funcall ',async-cb (gethash 'current_user_retweet s))))
    (gethash 'current_user_retweet
             (symon-twitter--fetch-tweet-from-id (gethash 'id_str tweet) account))))

;; statuses/retweet/:id
(defun symon-twitter--retweet-tweet (tweet account &optional cont)
  "Retweet TWEET with ACCOUNT asynchronously. When CONT is
non-nil, the thunk is called when succeeded. [This function calls
`statuses/retweet/:id' once]"
  (let ((id-str (gethash 'id_str tweet)))
    (message "Twitter: Retweeting ...")
    (symon-twitter--call-rest-api
     (concat "statuses/retweet/" id-str) account 'post
     `(lambda (s)
        (message "Twitter: Retweeting ... Done.")
        ,(when cont `(funcall ',cont)))
     "id" id-str)))

;; statuses/show/:id
(defun symon-twitter--tweet-favorited-p (tweet account &optional async-cb)
  "Return non-nil if TWEET is favorited by ACCOUNT. When ASYNC-CB
is non-nil, this function immediately returns and call ASYNC-CB
with the response later. [This function calls `statuses/show/:id'
once]"
  (message "Twitter: Requesting favorite status ...")
  (if async-cb
      (symon-twitter--call-rest-api
       (concat "statuses/show/" (gethash 'id_str tweet))
       account 'get
       `(lambda (s)
          (funcall ',async-cb (eq t (gethash 'favorited s)))))
    (eq t (gethash 'favorited
                   (symon-twitter--call-rest-api
                    (concat "statuses/show/" (gethash 'id_str tweet))
                    account 'get nil)))))

;; favorites/create
(defun symon-twitter--favorite-tweet (tweet account &optional cont)
  "Favorite TWEET with ACCOUNT asynchronously. When CONT is
non-nil, the thunk is called when succeeded. [This function calls
`favorites/create' once]"
  (message "Twitter: Favoriting ... ")
  (symon-twitter--call-rest-api
   "favorites/create" account 'post
   `(lambda (s)
      (message "Twitter: Favoriting ... Done.")
      ,(when cont `(funcall ',cont)))
   "id" (gethash 'id_str tweet)))

;; favorites/destroy
(defun symon-twitter--unfavorite-tweet (tweet account &optional cont)
  "Unfavorite TWEET with ACCOUNT. When CONT is non-nil, the thunk
is called when succeeded. [This function calls
`favorites/destroy' once]"
  (message "Twitter: Unfavoriting ...")
  (symon-twitter--call-rest-api
   "favorites/destroy" account 'post
   `(lambda (s)
      (message "Twitter: Unfavoriting ... Done.")
      ,(when cont `(funcall ',cont)))
   "id" (gethash 'id_str tweet))
  (message "Twitter: Unfavoriting ... Done."))

;; search/tweets
(defun symon-twitter--search-tweets (query account &optional async-cb)
  "Search tweets that match QUERY with ACCOUNT. When ASYNC-CB is
non-nil, this function immediately returns and call ASYNC-CB with
the response later. [This function calls `search/tweets' once]"
  (message "Twitter: Requesting search result ... (%s)" query)
  (if async-cb
      (symon-twitter--call-rest-api
       "search/tweets" account 'get
       `(lambda (s)
          (funcall ',async-cb (gethash 'statuses s)))
       "q" query "count" "100" "result_type" "mixed")
    (gethash 'statuses
             (symon-twitter--call-rest-api
              "search/tweets" account
              'get nil
              "q" query "count" "100" "result_type" "mixed"))))

;; statuses/home_timeline
(defun symon-twitter--fetch-home-timeline (account &optional async-cb)
  "Fetch ACCOUNT's home timeline. When ASYNC-CB is non-nil, this
function immediately returns and call ASYNC-CB with the response
later. [This function calls `statuses/home_timeline' once]"
  (message "Twitter: Requesting the home timeline ...")
  (symon-twitter--call-rest-api
   "statuses/home_timeline" account 'get async-cb "count" "200"))

;; statuses/mentions_timeline
(defun symon-twitter--fetch-mentions-timeline (account &optional async-cb)
  "Fetch ACCOUNT's mentions timeline. When ASYNC-CB is non-nil,
this function immediately returns and call ASYNC-CB with the
response later. [This function calls `statuses/mentions_timeline'
once]"
  (message "Twitter: Requesting the mentions timeline ...")
  (symon-twitter--call-rest-api
   "statuses/mentions_timeline" account 'get async-cb "count" "200"))

;; direct_messages
(defun symon-twitter--fetch-messages (account &optional async-cb)
  "Fetch ACCOUNT's direct messages. When ASYNC-CB is non-nil,
this function immediately returns and call ASYNC-CB with the
response later. [This function calls `direct_messages' once]"
  (message "Twitter: Requesting direct messages ...")
  (symon-twitter--call-rest-api
   "direct_messages" account 'get async-cb "count" "200"))

;; statuses/user_timeline
(defun symon-twitter--fetch-user-tweets (user-or-account account &optional async-cb)
  "Fetch USER's recent tweets with ACCOUNT. When ASYNC-CB is
non-nil, this function immediately returns and call ASYNC-CB with
the response later. [This function calls `statuses/user_timeline'
once]"
  (message "Twitter: Requesting the user timeline ... (%s)"
           (gethash 'screen_name user-or-account))
  (symon-twitter--call-rest-api
   "statuses/user_timeline" account 'get async-cb
   "user_id" (gethash 'id_str user-or-account)
   "count" "200" "include_rts" "true"))

;; user
(defun symon-twitter--open-userstream (account consumer-fn &optional track-keywords)
  "Open an userstream for ACCOUNT. When new messages are arrived
to the stream, they're passed to CONSUMER-FN. TRACK-KEYWORDS can
be a list of words, and tweets that match one of the word will be
delivered in addition. [This function calls `user' once]"
  (message "Twitter: Opening an userstream ...")
  (symon-twitter--call-stream-api
   "user" account consumer-fn 'get
   (lambda (p s) (message "Twitter: Disconnected."))
   "track" (mapconcat 'identity track-keywords ",")))

;; (* uncomment me, when the activity API got available *)
;; activity/about_me
;; (defun symon-twitter--fetch-user-activities (account &optional async-cb)
;;   (when async-cb (funcall async-cb nil)))
(defun symon-twitter--fetch-user-activities (account &optional async-cb)
  "Fetch ACCOUNT's activities. When ASYNC-CB is non-nil,
this function immediately returns and call ASYNC-CB with the
response later. [This function calls `activity/about_me' once]"
  (message "Twitter: Requesting user activities.")
  (symon-twitter--call-rest-api "activity/about_me" account 'get async-cb))

;; + timeline rendering
;;   + text / time

(defun symon-twitter--format-tweet-text (tweet &optional indent-str)
  "Generate a formatted hypertext from TWEET."
  (with-temp-buffer
    (insert (gethash 'text tweet))
    (let ((entities (gethash 'entities tweet)))
      ;; mark hashtags
      (dolist (obj (gethash 'hashtags entities))
        (let* ((indices (gethash 'indices obj))
               (ov (make-overlay (1+ (car indices)) (1+ (cadr indices))))
               (tag (gethash 'text obj)))
          (overlay-put ov 'display (concat "#" tag))
          (overlay-put ov 'url     (concat "http://twitter.com/hashtag/" tag))))
      ;; mark medias
      (dolist (obj (gethash 'media entities))
        (let* ((indices (gethash 'indices obj))
               (ov (make-overlay (1+ (car indices)) (1+ (cadr indices)))))
          (overlay-put ov 'display (gethash 'display_url obj))
          (overlay-put ov 'url     (gethash 'url         obj))))
      ;; mark urls
      (dolist (obj (gethash 'urls entities))
        (let* ((indices (gethash 'indices obj))
               (ov (make-overlay (1+ (car indices)) (1+ (cadr indices)))))
          (overlay-put ov 'display (gethash 'display_url obj))
          (overlay-put ov 'url     (gethash 'url         obj))))
      ;; mark mentions
      (dolist (obj (gethash 'user_mentions entities))
        (let* ((indices (gethash 'indices obj))
               (ov (make-overlay (1+ (car indices)) (1+ (cadr indices))))
               (screen-name (gethash 'screen_name obj)))
          (overlay-put ov 'display (concat "@" screen-name))
          (overlay-put ov 'url (concat "http://twitter.com/" screen-name)))))
    ;; linkify'em
    (dolist (ov (overlays-in (point-min) (point-max)))
      (goto-char (overlay-start ov))
      (delete-region (overlay-start ov) (overlay-end ov))
      (insert
       (symon-twitter--make-link-button
           (propertize (overlay-get ov 'display) 'face 'symon-twitter-link-face)
         (overlay-get ov 'url)))
      (delete-overlay ov))
    ;; format
    (fill-region (point-min) (point-max))
    (mapconcat (lambda (s) (concat (or indent-str "") s))
               (split-string (buffer-string) "\n") "\n")))

(defun symon-twitter--get-tweet-time-str (tweet)
  "Generate a string that represents the creation time of TWEET."
  (let* ((created-time (date-to-time (gethash 'created_at tweet)))
         (diff (time-subtract (current-time) created-time))
         (secs (+ (* (car diff) 65536.0) (cadr diff))))
    (symon-twitter--make-link-button
        (cond ((> secs 86400)              ; >= 24h
               (cl-destructuring-bind (_ __ ___ d m . ____) (decode-time created-time)
                 (concat (int-to-string d) " "
                         (cl-case m
                           ((1) "Jan") ((4) "Apr") ((7) "Jul") ((10) "Oct")
                           ((2) "Feb") ((5) "May") ((8) "Aug") ((11) "Nov")
                           ((3) "Mar") ((6) "Jun") ((9) "Sep") ((12) "Dec")))))
              ((> secs 3600) (format "%dh" (/ secs 60 60))) ; >= 1h
              ((> secs 60)   (format "%dm" (/ secs 60)))    ; >= 1m
              ((> secs 1)    (format "%ds" secs))           ; >= 1s
              (t             "now"))                        ; = 0s
      (format "http://twitter.com/%s/status/%s"
              (gethash 'screen_name (gethash 'user tweet))
              (gethash 'id_str tweet)))))

;;   + media

(defun symon-twitter--get-media-thumbnails (tweet)
  "Generate a string line containing thumbnails of all medias in
TWEET."
  (mapconcat
   (lambda (media)
     (symon-twitter--make-link-button
         (propertize "[img]" 'image-url (concat (gethash 'media_url media) ":thumb"))
       (gethash 'url media)))
   (let ((ex-entities (gethash 'extended_entities tweet)))
     (and ex-entities (gethash 'media ex-entities)))
   " "))

;;   + author

(defun symon-twitter--get-user-name-str (user-or-account &optional with-avatar)
  "Generate a hypertext for displaying name of USER-OR-ACCOUNT."
  (symon-twitter--make-link-button
      (concat (when with-avatar
                (ignore-errors
                  (concat
                   (propertize "[icon]"
                               'image-url (replace-regexp-in-string
                                           "_normal" "_mini"
                                           (gethash 'profile_image_url user-or-account))
                               'image-enable-cache t
                               'face 'symon-twitter-screenname-face)
                   (propertize " " 'face 'symon-twitter-screenname-face))))
              (propertize (format "@%s" (gethash 'screen_name user-or-account))
                          'face 'symon-twitter-screenname-face)
              (propertize (format " (%s)" (gethash 'name user-or-account))
                          'face 'symon-twitter-name-face)
              (when (eq (gethash 'verified user-or-account) t)
                (let ((str (propertize  " [v]" 'face 'symon-twitter-verified-user-face)))
                  (put-text-property 1 4 'display symon-twitter-verified-icon str)
                  str))
              (when (eq (gethash 'protected user-or-account) t)
                (let ((str (propertize  " [p]" 'face 'symon-twitter-protected-user-face)))
                  (put-text-property 1 4 'display symon-twitter-protected-icon str)
                  str)))
    (concat "http://twitter.com/" (gethash 'screen_name user-or-account))))

;;   + buttons

(defun symon-twitter--get-reply-button (tweet)
  "Generate a reply button for TWEET."
  (symon-twitter--make-button "[Reply]"
    (lambda (b) (symon-twitter-tweet nil nil (button-get b 'tweet)))
    'display symon-twitter-reply-icon
    'face 'symon-twitter-button-face
    'tweet tweet 'button-category 'reply))

(defun symon-twitter--get-retweet-button (tweet)
  "Generate a retweet button for TWEET."
  (let ((on (eq (gethash 'retweeted tweet) t)))
    (symon-twitter--make-button "[RT]"
      (lambda (b)
        (let ((tweet (button-get b 'tweet))
              (account (symon-twitter--read-twitter-account)))
          (symon-twitter--get-my-retweet
           tweet account
           `(lambda (s)
              (cond (s                  ; retweeted -> undo
                     (symon-twitter--delete-tweet
                      s ',account
                      (lambda ()
                        (with-current-buffer ,(current-buffer)
                          (button-put ',b 'display symon-twitter-retweet-icon)
                          (button-put ',b 'face 'symon-twitter-button-face)))))
                    (t                  ; otherwise -> retweet
                     (symon-twitter--retweet-tweet
                      ',tweet ',account
                      (lambda ()
                        (with-current-buffer ,(current-buffer)
                          (button-put ',b 'display symon-twitter-retweeted-icon)
                          (button-put ',b 'face 'symon-twitter-retweet-face))))))))))
      'display (if on symon-twitter-retweeted-icon symon-twitter-retweet-icon)
      'face (if on 'symon-twitter-retweet-face 'symon-twitter-button-face)
      'tweet tweet
      'button-category 'retweet)))

(defun symon-twitter--get-favorite-button (tweet)
  "Generate a favorite button for TWEET."
  (let ((on (eq (gethash 'favorited tweet) t)))
    (symon-twitter--make-button "[Fav]"
      (lambda (b)
        (let* ((account (symon-twitter--read-twitter-account))
               (tweet (button-get b 'tweet)))
          (symon-twitter--tweet-favorited-p
           tweet account
           `(lambda (s)
              (cond (s                  ; favorited -> unfav
                     (symon-twitter--unfavorite-tweet
                      ',tweet ',account
                      (lambda ()
                        (with-current-buffer ,(current-buffer)
                          (button-put ',b 'display symon-twitter-favorite-icon)))))
                    (t                      ; otherwise -> fav
                     (symon-twitter--favorite-tweet
                      ',tweet ',account
                      (lambda ()
                        (with-current-buffer ,(current-buffer)
                          (button-put ',b 'display symon-twitter-favorited-icon))))))))))
      'display (if on symon-twitter-favorited-icon symon-twitter-favorite-icon)
      'face (if on 'symon-twitter-favorite-face 'symon-twitter-button-face)
      'tweet tweet
      'button-category 'favorite)))

(defun symon-twitter--get-quote-button (tweet)
  "Generate a quote button for TWEET."
  (symon-twitter--make-button "[Quote]"
    (lambda (b) (symon-twitter-tweet nil nil nil (button-get b 'tweet)))
    'display symon-twitter-quote-icon
    'face 'symon-twitter-button-face
    'tweet tweet
    'button-category 'quote))

(defun symon-twitter--get-delete-button (tweet account)
  "Generate a delete button for TWEET."
  (symon-twitter--make-button "[Delete]"
    (lambda (b)
      (when (y-or-n-p "really delete ? ")
        (let ((account (button-get b 'account))
              (tweet (button-get b 'tweet)))
          (symon-twitter--delete-tweet
           tweet account
           `(lambda ()
              (with-current-buffer ,(current-buffer)
                (save-excursion
                  (goto-char (button-start ',b))
                  (delete-region (button-start ',b) (button-end ',b))
                  (insert "(Deleted)"))))))))
    'display symon-twitter-delete-icon
    'face 'symon-twitter-button-face
    'tweet tweet
    'account account
    'button-category 'delete))

(defun symon-twitter--get-in-reply-to-button (tweet &optional indent-str)
  "Generate a button to insert `in-reply-to' tweet for
TWEET. When INDENT-STR is non-nil, the `in-reply-to' tweet will
be indented with the string."
  (symon-twitter--make-button
      (format "> [in reply to @%s]" (gethash 'in_reply_to_screen_name tweet))
    (lambda (b)
      (let ((tweet (button-get b 'tweet))
            (indent-str (button-get b 'indent-str)))
        (symon-twitter--fetch-tweet-from-id
         (gethash 'in_reply_to_status_id_str tweet)
         (symon-twitter--read-twitter-account)
         `(lambda (s)
            (with-current-buffer ,(current-buffer)
              (save-excursion
                (goto-char (button-start ',b))
                (beginning-of-line)
                (save-excursion
                  (delete-region (point) (progn (forward-line 1) (point))))
                (symon-twitter--insert-tweet s ',indent-str)))))))
    'face 'symon-twitter-button-face
    'tweet tweet
    'indent-str indent-str
    'button-category 'in-reply-to))

;;   + fetch/insert images

(defvar symon-twitter--image-cache (make-hash-table :test 'equal))

(defun symon-twitter--download-image-async (url cb)
  "Download an image from URL and call CB with the image object
on success, or nil on failure."
  (let ((url-show-status nil)
        (callback
         `(lambda (s)
            (unless s
              (goto-char (point-min))
              (when (search-forward "\n\n" nil t)
                (funcall ,cb (create-image (buffer-substring (point) (point-max))
                                           nil t :ascent 'center)))))))
    (message "Twitter: Retrieving images ... (%s)" url)
    (url-retrieve url callback)))

(defun symon-twitter--insert-image-async (buf b e url &optional use-cache)
  "Replace text between B and E in BUF, with an image downloaded
from URL. When USE-CACHE is non-nil, cache the downloaded
image. When the image is unavailable, display
`symon-twitter-unavailable-icon' instead."
  (let ((cached-image (gethash url symon-twitter--image-cache))
        (callback `(lambda (image)
                     (setq image (or image symon-twitter-unavailable-icon))
                     ,(when use-cache
                        `(puthash ,url image symon-twitter--image-cache))
                     (when (buffer-live-p ,buf)
                       (with-current-buffer ,buf
                         (put-text-property ,b ,e 'display image)
                         (put-text-property ,b ,e 'image-url nil)
                         (put-text-property ,b ,e 'image-enable-cache nil))))))
    (if cached-image
        (with-current-buffer buf
          (put-text-property b e 'display cached-image)
          (put-text-property b e 'image-url nil)
          (put-text-property b e 'image-enable-cache nil))
      (condition-case nil
          (symon-twitter--download-image-async url callback)
        (error (funcall callback nil))))))

(defun symon-twitter--jit-lock-inline-image (b e)
  "Substitutes thumbnails for each regions between B and E with
`image-url' property."
  (goto-char b)
  (let (beg url cache)
    (while (and (setq beg (if (get-text-property (point) 'image-url)
                              (or (previous-single-property-change (1+ (point)) 'image-url)
                                  (point-min))
                            (next-single-property-change (point) 'image-url nil e)))
                ;; if `next-single-property-change' returned E, no
                ;; images are found (as described in the docstring of
                ;; the function)
                (not (= beg e)))
      (setq url   (get-text-property beg 'image-url)
            cache (get-text-property beg 'image-enable-cache))
      (goto-char (or (next-single-property-change beg 'image-url) (point-max)))
      (symon-twitter--insert-image-async (current-buffer) beg (point) url cache))))

;;   + timeline renderer

(defun symon-twitter--insert-timeline-header (title)
  "Insert a timeline header titled TITLE."
  (let ((logo (propertize "[t]" 'display symon-twitter-logo)))
    (insert
     (propertize (concat "\n" logo " " title "\n\n") 'face '(:foreground "#55acee")))))

(defun symon-twitter--insert-user-description (user-or-account)
  "Insert formatted description of USER at the point."
  (let ((description (with-temp-buffer
                       (insert (gethash 'description user-or-account))
                       (fill-region (point-min) (point-max))
                       (buffer-string))))
    (insert
     (propertize
      (concat
       (symon-twitter--get-user-name-str user-or-account symon-twitter-enable-avatars) "\n"
       (mapconcat (lambda (s) (concat "  " s "\n")) (split-string description "\n") "")
       (format "Followed:%6d | Following:%6d | Listed:%4d\n"
               (gethash 'followers_count user-or-account)
               (gethash 'friends_count user-or-account)
               (gethash 'listed_count user-or-account))
       (format "Tweets:  %6d | Favorites:%6d |\n"
               (gethash 'statuses_count user-or-account)
               (gethash 'favourites_count user-or-account)))
      'face 'symon-twitter-user-description-face))))

(defun symon-twitter--insert-tweet (tweet &optional indent-str)
  "Insert TWEET at the point, formatted as suggested in Twitter's
\"Display Requirements\"."
  (let ((retweeted_status (gethash 'retweeted_status tweet))
        (quoted_status (gethash 'quoted_status tweet))
        (indent-str (or indent-str ""))
        str)
    ;; retweet author / date
    (when retweeted_status
      (let* ((str (symon-twitter--get-tweet-time-str tweet))
             (size (- fill-column (length str))))
        (insert indent-str
                (propertize "[RT]" 'display symon-twitter-retweeted-icon
                            'face 'symon-twitter-retweet-face)
                " "
                (symon-twitter--get-user-name-str (gethash 'user tweet))
                (propertize " retweeted" 'face 'symon-twitter-retweet-face)
                " :"
                (propertize " " 'display `(space :align-to ,size))
                (propertize str 'face 'symon-twitter-time-face) "\n"))
      (setq tweet retweeted_status))
    ;; tweet author / date
    (let* ((str (symon-twitter--get-tweet-time-str tweet))
           (size (- fill-column (length str))))
      (insert indent-str
              (symon-twitter--get-user-name-str
               (gethash 'user tweet)
               symon-twitter-enable-avatars)
              " :"
              (propertize " " 'display `(space :align-to ,size))
              (propertize str 'face 'symon-twitter-time-face) "\n"))
    ;; text
    (insert (symon-twitter--format-tweet-text tweet (concat indent-str " ")) "\n")
    ;; quoted status
    (when quoted_status
      (put-text-property
       (point)
       (progn
         (symon-twitter--insert-tweet quoted_status "   ")
         (point))
       'face 'symon-twitter-quoted-tweet-face))
    ;; media
    (when symon-twitter-enable-media-thumbnails
      (let ((thumbnails (symon-twitter--get-media-thumbnails tweet)))
        (unless (string= thumbnails "") (insert indent-str " " thumbnails "\n"))))
    ;; buttons
    (insert (if (gethash 'in_reply_to_status_id tweet)
                (concat indent-str " "
                        (symon-twitter--get-in-reply-to-button
                         tweet (concat indent-str symon-twitter-indent-conversation-string))
                        "\n")
              "")
            indent-str "   "
            (symon-twitter--get-reply-button tweet) " "
            (symon-twitter--get-quote-button tweet) " "
            (symon-twitter--get-retweet-button tweet)
            (let ((retweet_count (gethash 'retweet_count tweet)))
              (if (zerop retweet_count) " "
                (format "%d " retweet_count)))
            (symon-twitter--get-favorite-button tweet)
            (let ((favorite_count (gethash 'favorite_count tweet)))
              (if (zerop favorite_count) " "
                (format "%d " favorite_count)))
            (let* ((id-str (gethash
                            'id_str (gethash 'user tweet)))
                   (account (cl-some (lambda (account)
                                       (when (string= id-str (gethash 'id_str account))
                                         account))
                                     symon-twitter--twitter-accounts)))
              (if account (symon-twitter--get-delete-button tweet account) ""))
            (propertize "\n" 'line-height 1.5))))

(defvar symon-twitter--timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'forward-paragraph)
    (define-key map (kbd "k") 'backward-paragraph)
    map)
  "Keymap for symon-twitter timelines.")

(define-derived-mode symon-twitter--timeline-mode text-mode "Twitter"
  "Major mode for symon-twitter timelines."
  :group 'symon-twitter-timeline
  :keymap symon-twitter--timeline-mode-map
  (set (make-local-variable 'paragraph-start) "\\(?:\\(?:\\[icon\\]\\|⇄\\) \\)?@\\|$")
  (set (make-local-variable 'paragraph-separate) "$")
  (jit-lock-mode 1)
  (jit-lock-register 'symon-twitter--jit-lock-inline-image))

(defun symon-twitter--display-timeline (title tweets &optional user)
  "Display tweet timeline titled TITLE. When USER is given,
display USER's description at the top of timeline. This may be
useful to display an user-timeline."
  (message "Twitter: Rendering the timeline ...")
  (with-current-buffer (get-buffer-create "*symon-twitter*")
    (fundamental-mode)
    (erase-buffer)
    (symon-twitter--insert-timeline-header title)
    (when user
      (symon-twitter--insert-user-description user)
      (insert "\n"))
    (mapc (lambda (tweet)
            (symon-twitter--insert-tweet tweet)
            (insert "\n"))
          tweets)
    (goto-char (point-min))
    (symon-twitter--timeline-mode))
  (select-window (display-buffer "*symon-twitter*")))

;; + provide

(provide 'symon-twitter)

;;; symon-twitter.el ends here
