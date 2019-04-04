(require 'with-editor)

(defun ensure-spack ()
  (if-let ((spack (executable-find "spack")))
      spack
    (error "Command `spac+k` not found in exec-path")))

(defun spack-get-envs ()
  (interactive)
  (let ((spack (ensure-spack)))
    (with-temp-buffer
      (process-file "spack" nil (list t nil) nil "env" "ls")
      (split-string (buffer-string) split-string-default-separators t))))

(defun spack-env-location (env)
  (with-temp-buffer
    (process-file "spack" nil (list t nil) nil "location" "-e" env)
    (string-trim (buffer-string))))


(defun spack-loadenv ()
  (interactive)
  (let* ((envs (spack-get-envs))
         (env (completing-read "environment: " envs))
         (env-path (spack-env-location env)))
    (setenv "SPACK_ENV" env-path)))

(defun spack-current-env ()
  (interactive)
  (let* ((spack (ensure-spack)))
    (with-temp-buffer
      (process-file "spack" nil (list t nil) nil "env" "st"))))

(defun spack-edit ()
  (with-editor (process-file "spack" nil 0 nil "config" "edit")))


(provide 'spack)
