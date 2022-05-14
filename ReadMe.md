* ���ܽ���
��������ΪWindows Emacs�û����Ƶ�С���߼���Ŀǰ��Ҫʵ�ֵĹ��ܶ��������йأ�����
1. `pop-select/gui-set-transparent`
����Emacs����͸�������ô��룺
```
(ignore-errors (module-load "pop-select.dllȫ·�������û�м���bin·���Ļ�"))
(when (functionp 'pop-select/gui-set-transparent)
  (defvar cur-transparent 255)
  (defconst step-transparent 20)
  (pop-select/gui-set-transparent cur-transparent)
  (defun dec-transparent()
    (interactive)
    (setq cur-transparent (min 255 (+ cur-transparent step-transparent)))
    (pop-select/gui-set-transparent cur-transparent))
  (defun inc-transparent()
    (interactive)
    (setq cur-transparent (max 0 (- cur-transparent step-transparent)))
    (pop-select/gui-set-transparent cur-transparent))
  (global-set-key (kbd "<C-wheel-up>") 'dec-transparent)
  (global-set-key (kbd "<C-wheel-down>") 'inc-transparent)
  )
```
2. `pop-select/pop-select`
����һ�������б��ڣ�Ȼ����԰�ctrl+tab�л�����һ�ctrl+tab+shift�л�����һ��ͷŰ����󷵻���ѡ���emacs
```
(when (fboundp 'pop-select/pop-select)
  (defun my-pop-select(&optional backward)
    (interactive)
    (let* ((myswitch-buffer-list (copy-sequence (ep-tabbar-buffer-list)
					                            )
                                 )  (vec_name [])
                                    sel
                                    )
      (cl-dolist (buf myswitch-buffer-list)
        (setq vec_name (vconcat vec_name (list (buffer-name buf)))))
      ;; �������
      (setq sel (pop-select/pop-select vec_name (if backward
                                                    (1- (length vec_name))
                                                  1
                                                  )))
      (let ((buf (switch-to-buffer (nth sel myswitch-buffer-list))))
        (when (and (bufferp buf) (featurep 'wcy-desktop))
	      (with-current-buffer buf
	        (when (eq major-mode 'not-loaded-yet)
	          (wcy-desktop-load-file))))
        )
      )
    )
  (global-set-key (kbd "<C-tab>") 'my-pop-select)
  (global-set-key (if (string-equal system-type "windows-nt")
		              (kbd "<C-S-tab>")
	                (kbd "<C-S-iso-lefttab>"))
                  (lambda ()(interactive)
                    (my-pop-select t)))
  )
```
3. `pop-select/beacon-blink`��`pop-select/beacon-set-parameters`
�����滻beacon����˸Ч������ȫ����Emacs���ڣ���Ϊ��������һ��ר�ŵ�ui�߳�����beacon
```
(when (fboundp 'pop-select/beacon-set-parameters)
  ;; 51afef
  (pop-select/beacon-set-parameters 300 20 #x51 #xaf #xef 50)
  (use-package beacon
    :defer 1.5
    :init
    (setq beacon-blink-when-focused t)
    (setq beacon-blink-delay 0.01)
    (setq beacon-blink-duration 0.2)
    (setq beacon-blink-when-window-scrolls nil) ; ������auto save������ʱ�������ʶ�����
    :config
    (beacon-mode 1)
    (defadvice beacon-blink (around my-beacon-blink activate)
      ;; Ŀǰż������emacsʱҲ����
      ;; (message (concat (symbol-name this-command) " " (symbol-name last-command)))
      (when (frame-visible-p (window-frame)) ;; ������ֹ��С��ʱ����
        (let ((p (window-absolute-pixel-position)))
          (when p
            (pop-select/beacon-blink (car p) ; x
                                     (cdr p) ; y
                                     (truncate (* beacon-blink-duration 1000)) ; timer
                                     (truncate (* beacon-blink-delay 1000)) ; delay
                                     ))))))
  )
```
