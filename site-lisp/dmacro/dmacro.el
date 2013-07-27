;;
;;      dmacro.el - �������η��֤����� & �¹�
;;
;;      1993 4/14        original idea by �����Ƿ�����㡼��
;;                         implemented by ����������Ĺ��������
;;                          refinement by �����Ƿ�����㡼��
;;	1995 3/30 modified for Emacs19 by �����Ƿ�����㡼��
;;
;;	2002 3              XEmacs�б� by ��Ȫ�ѻ� obata@suzuki.kuee.kyoto-u.ac.jp
;;                                        ������ zah07175@rose.zero.ad.jp
;;
;;

;;
;; dmacro.el �ϡ������֤���륭������󤫤鼡������ͽ¬���¹Ԥ�����
;; ����Υץ����Ǥ������η��֤��θ��ФȤ��μ¹Ԥ���᤹�뤿���
;; *dmacro-key* �ǻ��ꤹ�����̤Ρַ��֤������פ���Ѥ��ޤ���
;;
;; �㤨�Х桼����
;;     abcabc
;; �����Ϥ�����ַ��֤������פ򲡤��ȡ�dmacro.el �� "abc" ����������
;; ���֤��򸡽Ф��Ƥ����¹Ԥ������η�̥ƥ����Ȥ�
;;     abcabcabc
;; �Ȥʤ�ޤ����ޤ���
;;     abcdefab
;; �����Ϥ�����ַ��֤������פ򲡤��ȡ�dmacro.el �Ϥ���� "abcdef" ��
;; ���Ϥη��֤���Ƚ�Ǥ������֤��λĤ����ʬ��ͽ¬�¹Ԥ��� "cdef" �����Ϥ���
;; �ƥ����Ȥ�
;;     abcdefabcdef
;; �Ȥʤ�ޤ��������Ǥ⤦���١ַ��֤������פ򲡤��ȡ�"abcdef" ������
;; �������֤���ơ��ƥ����Ȥ�
;;     abcdefabcdefabcdef
;; �Ȥʤ�ޤ���
;;
;; �����륭�����η��֤���ǧ�����¹Ԥ���뤿�ᡢ�㤨��
;;     line1
;;     line2
;;     line3
;;     line4
;; �Ȥ����ƥ����Ȥ�
;;     % line1
;;     % line2
;;     line3
;;     line4
;; �Τ褦���Խ�������ַ��֤������פ򲡤��ȥƥ����Ȥ�
;;     % line1
;;     % line2
;;     % line3
;;     line4
;; �Τ褦�ˤʤꡢ���θ岡�����Ӥ˼��ι�Ƭ�� "% "���ɲä���Ƥ����ޤ���
;;
;; ���Τ褦�ʵ�ǽ�ϡ����֤��ѥ����ǧ���ˤ�ꥭ���ܡ��ɥޥ����ưŪ��
;; ������Ƥ���ȹͤ��뤳�Ȥ�Ǥ��ޤ��������ܡ��ɥޥ���ξ�������
;; ���Ϥ�������ˤ��Τ��Ȥ�桼����ǧ�����ƥޥ������Ͽ����ɬ�פ�����
;; �ޤ�����dmacro.el �Ǥϼºݤ˷��֤����򤷤Ƥ��ޤä���Ǥ��Τ��Ȥ�
;; �����Ĥ������Ǥ�ַ��֤������פ򲡤������Ǥ�������ޤ��¹Ԥ�����
;; ���Ȥ��Ǥ��ޤ����ޤ��ޥ���������ˡ(���θ�ǡַ��֤������פ򲡤�
;; ����)�⥭���ܡ��ɥޥ���ξ��(�ޥ���γ��ϤȽ�λ����ꤹ��)����٤�
;; ñ��ˤʤäƤ��ޤ���
;;
;; �� ������
;;
;; ��ʸ�����ִ�
;;
;; �ƥ�����������ƤΡ�abc�פ��def]�˽����������ͤ��Ƥߤޤ���
;; ��abc�פ򸡺����륭������ "Ctrl-S a b c ESC" �ǡ������
;; "DEL DEL DEL d e f" �ǡ�def�פ˽������뤳�Ȥ��Ǥ��ޤ���
;; ����³�����Ρ�abc�פ򸡺����� "Ctrl-S a b c ESC" �����Ϥ������
;; �ַ��֤������פ򲡤��� "DEL DEL DEL d e f" ��ͽ¬�¹Ԥ��졢������
;; �������줿��abc�פ���def�פ˽�������ޤ��������Ǥޤ��ַ��֤�������
;; �򲡤��ȼ��Ρ�abc�פ���def�פ˽�������ޤ���
;; ���Τ褦�ˡַ��֤������פ򲡤��Ƥ������Ȥˤ��硹��ʸ�����
;; �ִ����Ƥ������Ȥ��Ǥ��ޤ���
;;
;; �������ˤ�뤪����
;;
;; ���֤���ޤ೨���ñ�˽񤯤��Ȥ��Ǥ��ޤ����㤨�С�
;;   ��������������������������������������������������������
;;     ����������������������������������������������������
;; �Τ褦�ʳ���񤭤������ϡ�keisen.el �ʤɤ�Ȥä�
;;   ��������
;;     ����
;; �Ƚ񤤤���ǡַ��֤��ץ����򲡤��ȡ�
;;   ��������
;;     ��������
;; �Ȥʤꡢ�⤦���١ַ��֤������פ򲡤���
;;   ������������
;;     ������������
;; �Ȥʤ�ޤ���Ʊ�ͤ�
;;  ������������������������������������������������
;;  ������������������������������������������������
;; �Τ褦�ʳ���
;;  ������  ��
;;  ������
;; �������Ϥ�����ַ��֤������פ�Ϣ³���Ʋ����������������Ȥ��Ǥ��ޤ���
;;
;; �� ���֤�ͽ¬����ˡ
;;
;; ���Ϥη��֤���ͽ¬��ˡ�Ϥ�����ͤ����ޤ�����dmacro.el�Ǥ�
;; �ʲ��Τ褦��ͥ���٤�⤿���Ƥ��ޤ���
;;
;;  (1) Ʊ�����ϥѥ���ͽ¬��ľ����2�ٷ��֤���Ƥ�����Ϥ����
;;      ͥ�褹�롣���֤��ѥ���ʣ���������Ĺ����Τ�ͥ�褹�롣
;;
;;      �㤨�С��֤��襤�����襤���פȤ������ϤǤϡ֤��襤���פ�
;;      �����ѥ��󤬷����֤��줿�Ȥ������ȡ��֤��פȤ����ѥ���
;;      �����֤��줿�Ȥ�������ξ������ǽ�Ǥ��������ξ��
;;      �֤��襤���פ�ͥ�褷�ޤ���
;;
;;  (2) (1)�ξ��ˤ��ƤϤޤ餺��ľ����������<s>������������������
;;      �����ˤʤäƤ�����(ľ�������Ϥ�<s> <t> <s>�Τ褦�ʷ���
;;      �ʤäƤ�����)�ϡ��ޤ�<t>��ͽ¬�������μ�����<s> <t>��ͽ¬
;;      ���롣���ΤȤ�<s>��Ĺ����Τ�ͥ�褷��������Ǥ�<t>��û�����
;;      ��ͥ�褹�롣
;;
;;      �㤨�С�abracadabra�פȤ������ϤǤϡ�<s>=��abra�פ���Ĺ�ʤΤ�
;;      <s> <t>=��cadabra�פ�ͽ¬��ͥ�褵��ޤ���
;;
;; �� XEmacs �б���Super, Hyper, Alt �������б��ˤĤ���
;;
;; �����ǤǤ� XEmacs �ˤ��б����ޤ�����
;; ���ߤΤȤ��� GNU Emacs 18, 19, 20, 21, XEmacs 21 ��
;; ư��뤳�Ȥ���ǧ�Ǥ��Ƥ��ޤ���
;; �ޤ������ dmacro �Ǥ� Super, Hyper, Alt �Υ������Ϥ�
;; �������������Ȥ��Ǥ��ޤ���Ǥ����������ΥС������Ǥ�
;; ������褦�ˤʤäƤ��ޤ���
;; �����֤��Υ����Ȥ��� *dmacro-key* �� Super, Hyper, Alt, Meta
;; ��ޤ᤿������Ȥ����Ȥ�Ǥ��ޤ��������������κݤ�
;; �ʲ�����դ˽��äƲ�������
;;
;; �� *dmacro-key* �λ���
;;
;; GNU Emacs �ξ��
;;   Modifier key �Ȥ��� Control �Τߤ��Ȥ������ "\C-t" �Τ褦��
;;   ʸ����Ȥ��ƻ���Ǥ��ޤ���Meta, Super, Hyper, Alt �����Ѥ�����ˤ�
;;   ���줾�� [?\M-t], [?\s-t], [?\H-t], [?\A-t] �Τ褦�˻��ꤷ�Ʋ�������
;;
;; XEmacs �ξ��
;;   Meta key ��Ȥ����Ǥ�嵭�Τ褦�����¤Ϥ���ޤ���Super ����Ȥ�
;;   ���ˤ� [(super t)] �Τ褦�˻��ꤷ�Ʋ�������
;;
;; �� ������ˡ
;;
;;  .emacs�ʤɤ˰ʲ��ιԤ�����Ʋ�������
;;
;; (defconst *dmacro-key* "\C-t" "���֤����ꥭ��")
;; (global-set-key *dmacro-key* 'dmacro-exec)
;; (autoload 'dmacro-exec "dmacro" nil t)
;;
;; ���ꥸ�ʥ��Ϣ����:
;; �����Ƿ
;; ���㡼�׳������ ���եȥ����������
;; masui@shpcsl.sharp.co.jp
;;
;; 2002/6/3���ߤ�Ϣ����:
;; �����Ƿ
;; (��)���ˡ�����ԥ塼���������󥹸����
;; masui@acm.org
;;

(defvar dmacro-array-type
  (if (and (boundp 'emacs-major-version)
	   (>= emacs-major-version 19))
      'vector 'string)
  "dmacro �������ǽ�����������μ��ࡣ
emacs 19 �ʾ�ʤ�ǥե���Ȥ� vector �ˤ��롣
string �Ǥ� hyper, super, alt ��ޤ�����Ϥη����֤���
�����������Ǥ��ʤ��Τ���ա�
GNU Emacs 18 (Nemacs) ��ȤäƤ������ʳ��� vector �����ꤢ��ޤ���")

(fset 'dmacro-concat
      (cond ((eq dmacro-array-type 'string) 'concat)
	    ((eq dmacro-array-type 'vector) 'vconcat)))

(fset 'dmacro-subseq
      (cond ((featurep 'xemacs) 'subseq)
            ((and (eq dmacro-array-type 'vector)
                  (boundp 'emacs-major-version)
                  (eq emacs-major-version 19))
             (require 'cl)
             'subseq)
            (t 'substring)))

(defvar *dmacro-arry* nil "���֤���������")
(defvar *dmacro-arry-1* nil "���֤���������ʬ����")

(setq dmacro-key
      (cond ((eq dmacro-array-type 'string)
             *dmacro-key*)
            (t
             (let ((key *dmacro-key*))
               (cond ((featurep 'xemacs)
                      (if (arrayp key)
                          (mapvector 'character-to-event key)
                        (vector (character-to-event key))))
                     (t
                      (vconcat key)))))))

(setq dmacro-keys (dmacro-concat dmacro-key dmacro-key))

(defun dmacro-exec ()
  "�������η��֤��򸡽Ф��¹Ԥ���"
  (interactive)
  (let ((s (dmacro-get)))
    (if (null s)
	(message "���η��֤������Ĥ���ޤ���")
      (execute-kbd-macro s)
      )
    ))

(defun dmacro-event (e)
  (cond
   ((integerp e) e)
   ((eq e 'backspace) 8)
   ((eq e 'tab) 9)
   ((eq e 'enter) 13)
   ((eq e 'return) 13)
   ((eq e 'escape) 27)
   ((eq e 'delete) 127)
   (t 0)
   ))

(defun dmacro-recent-keys ()
  (cond ((eq dmacro-array-type 'vector) (recent-keys))
	((eq dmacro-array-type 'string)
	 (let ((s (recent-keys)) )
	   (if (stringp s) s
	     (concat (mapcar 'dmacro-event s))
	     )))))

(defun dmacro-get ()
  (let ((rkeys (dmacro-recent-keys)) arry)
    (if (if (featurep 'xemacs)
            (let ((keys (vconcat dmacro-key
                                 (or *dmacro-arry-1* *dmacro-arry*)
                                 dmacro-key)))
              (equal keys
                     (subseq rkeys (- (length keys)))))
          (equal dmacro-keys (dmacro-subseq rkeys (- (length dmacro-keys)))))
        (progn
          (setq *dmacro-arry-1* nil)
          *dmacro-arry*)
      (setq arry (dmacro-search (dmacro-subseq rkeys 0 (- (length dmacro-key)))))
      (if (null arry)
          (setq *dmacro-arry* nil)
        (let ((s1 (car arry)) (s2 (cdr arry)))
          (setq *dmacro-arry* (dmacro-concat s2 s1)
                *dmacro-arry-1* (if (equal s1 "") nil s1))
          (setq last-kbd-macro *dmacro-arry*)
          (if (equal s1 "") *dmacro-arry* s1))
        ))))

(defun dmacro-search (array)
  (let* ((arry (dmacro-array-reverse array))
         (sptr  1)
         (dptr0 (dmacro-array-search (dmacro-subseq arry 0 sptr) arry sptr))
         (dptr dptr0)
         maxptr)
    (while (and dptr0
                (not (dmacro-array-search dmacro-key (dmacro-subseq arry sptr dptr0))))
      (if (= dptr0 sptr)
          (setq maxptr sptr))
      (setq sptr (1+ sptr))
      (setq dptr dptr0)
      (setq dptr0 (dmacro-array-search (dmacro-subseq arry 0 sptr) arry sptr))
      )
    (if (null maxptr)
        (let ((predict-arry (dmacro-array-reverse (dmacro-subseq arry (1- sptr) dptr))))
          (if (dmacro-array-search dmacro-key predict-arry)
              nil
            (cons predict-arry (dmacro-array-reverse (dmacro-subseq arry 0 (1- sptr)))))
          )
      (cons "" (dmacro-array-reverse (dmacro-subseq arry 0 maxptr)))
      )
    ))

(defun dmacro-array-reverse (arry)
  (dmacro-concat (reverse (mapcar 'identity arry))))

(defun dmacro-array-search (pat arry &optional start)
  (let* ((len (length pat))
	 (max (- (length arry) len))
	 p found
	 )
    (setq p (if start start 0))
    (while (and (not found) (<= p max))
      (setq found (equal pat (dmacro-subseq arry p (+ p len))))
      (if (not found) (setq p (1+ p)))
      )
    (if found p nil)
    ))

(provide 'dmacro)
