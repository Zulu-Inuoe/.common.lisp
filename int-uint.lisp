(in-package #:cl-user)

(declaim (inline :uint->int))
(defun :uint->int (size value)
  "unsigned integer -> signed integer conversion"
  (declare (type unsigned-byte size value))
  (logior value (- (mask-field (byte 1 (1- size)) value))))

(declaim (inline :int->uint))
(defun :int->uint (size value)
  "signed integer -> unsigned integer conversion"
  (declare (type unsigned-byte size)
           (type signed-byte value))
  (ldb (byte size 0) value))

(declaim (inline :uint8->int))
(defun :uint8->int (value)
  (declare (type (unsigned-byte 8) value))
  (the (signed-byte 8) (:uint->int 8 value)))

(declaim (inline :uint16->int))
(defun :uint16->int (value)
  (declare (type (unsigned-byte 16) value))
  (the (signed-byte 16) (:uint->int 16 value)))

(declaim (inline :uint32->int))
(defun :uint32->int (value)
  (declare (type (unsigned-byte 32) value))
  (the (signed-byte 32) (:uint->int 32 value)))

(declaim (inline :uint64->int))
(defun :uint64->int (value)
  (declare (type (unsigned-byte 64) value))
  (the (signed-byte 64) (:uint->int 64 value)))

(declaim (inline :int8->uint))
(defun :int8->uint (value)
  (declare (type (signed-byte 8) value))
  (the (unsigned-byte 8) (:int->uint 8 value)))

(declaim (inline :int16->uint))
(defun :int16->uint (value)
  (declare (type (signed-byte 16) value))
  (the (unsigned-byte 16) (:int->uint 16 value)))

(declaim (inline :int32->uint))
(defun :int32->uint (value)
  (declare (type (signed-byte 32) value))
  (the (unsigned-byte 32) (:int->uint 32 value)))

(declaim (inline :int64->uint))
(defun :int64->uint (value)
  (declare (type (signed-byte 64) value))
  (the (unsigned-byte 64) (:int->uint 64 value)))
