;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FMI-OBSERVATIONS; Base: 10 -*-
;;;; Copyright (c) 2021, Atte Hinkka <atte.hinkka@iki.fi>
;;;; 
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package #:weather-utilitae)

(defun kelvin-to-celsius (value) (- value 273.15))
(defun celsius-to-kelvin (value) (+ value 273.15))
(defun fahrenheit-to-celsius (value) (/ (- value 32) 1.8))
(defun celsius-to-fahrenheit (value) (+ (* value 1.8) 32))

(defun meters-per-second-to-kilometers-per-hour (value) (* value 3.6))
(defun kilometers-per-hour-to-meters-per-second (value) (/ value 3.6))

;; References:
;;  - http://ritvalawx.com/wxtermit.php
;;  - http://fi.wikipedia.org/wiki/Pakkasen_purevuus
;;  - https://en.wikipedia.org/wiki/Wind_chill
;; 
;;  - HI, SSI http://www.gorhamschaffler.com/humidity_formulas.htm
;;  - Humidex http://climate.weatheroffice.gc.ca/prods_servs/normals_documentation_e.html
;;  - AT http://www.bom.gov.au/info/thermal_stress/#apparent
;;  - THW http://www.ehow.com/how_12028315_calculate-thw-index.html

(defun summer-simmer-index (fahrenheit-temperature relative-humidity)
  (-
   (* 1.98
      (- fahrenheit-temperature
	 (*
	  (- 0.55 (* 0.0055 relative-humidity))
	  (- fahrenheit-temperature 58))))
   56.83))

(defun wind-chill-index-original (celsius-temperature wind-velocity-m-per-s-squared)
  "This is an old unit returning not an equivalent temperature, but a value
   with unit of kilocalories per hour per square metre. Frostbite is e.g. 1400."
  (*
   (+ (-
       (* 10 (sqrt wind-velocity-m-per-s-squared))
       wind-velocity-m-per-s-squared)
      10.5)
   (- 33 celsius-temperature)))

(defun wind-chill-environment-canada-nws (celsius-air-temperature wind-speed-in-km-per-h)
  "This unit tries to capture the effect of wind to cold temperatures,
   approximating how cold it really feels when there's wind.

   This is defined only for temperatures at or below 10°C and wind speeds
   above 4.8 km/h. This is also an approximation to an accuracy of one degree,
   hence the rounding."
  (declare (ignore leftover))
  (multiple-value-bind (rounded leftover)
      (round
       (+
	(-
	 (+ 13.12 (* 0.6215 celsius-air-temperature))
	 (* 11.37 (expt wind-speed-in-km-per-h 0.16)))
	(* 0.3965 celsius-air-temperature (expt wind-speed-in-km-per-h 0.16))))
    rounded))

