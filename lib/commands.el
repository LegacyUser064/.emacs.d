;;; commands.el -*- lexical-binding: t -*-

;;;###autoload
(defun my-create-python-project (dir main-file-name venv-dir-name)
  "Creates a new Python project in dir"
  (interactive "DDirectory: \nsName of main file: \nsVenv directory name: ")
  (make-empty-file (expand-file-name ".projectile" dir))
  (make-empty-file (expand-file-name "__init__.py" dir))

  (with-temp-file (expand-file-name "pyrightconfig.json" dir)
    (insert (format "{\"venv\": \"%s\"}" venv-dir-name)))

  (with-temp-file (expand-file-name main-file-name dir)
    (insert "#!/usr/bin/env python"))

  (with-temp-file (expand-file-name ".dir-locals.el" dir)
    (insert (format "((nil . ((pyvenv-activate . \"%s\"))))" venv-dir-name)))

  (shell-command (concat "python3 -m venv " venv-dir-name)))

;;;###autoload
(defun my-create-exploit-project (dir &optional link)
  "Creates a project for exploiting binaries"
  (interactive "DDirectory: ")
  
  (my-create-python-project dir "asd.py" ".venv")
  (shell-command "chmod +x ./asd.py")
  
  (let ((sh-cmd "source .venv/bin/activate && pip install pwntools && 
pip install ropper"))
    (shell-command sh-cmd))
  
  (when link
    (shell-command (format "wget %s" link)))

  (with-temp-file (expand-file-name "profile.rr2" dir)
    (insert "#!/usr/bin/rarun2")))

(provide 'commands)
