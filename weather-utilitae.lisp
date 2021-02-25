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

(defmacro defconversionfun (from to formula)
  (let* ((function-name (intern (format nil "~A-TO-~A" from to)))
	 (argument-name (intern (format nil "VALUE-IN-~A" (symbol-name from))))
	 (effective-formula
	   (subst-if
	    argument-name
	    #'(lambda (x) (eq x :arg))
	    formula)))
    `(defun ,function-name (,argument-name) ,effective-formula)))

(defconversionfun :kelvin :celsius (- :arg 273.15))
(defconversionfun :celsius :kelvin (+ :arg 273.15))
(defconversionfun :fahrenheit :celsius (/ (- :arg 32) 1.8))
(defconversionfun :celsius :fahrenheit (+ (* :arg 1.8) 32))

(defconversionfun :meters-per-second :kilometers-per-hour (* :arg 3.6))
(defconversionfun :kilometers-per-hour :meters-per-second (/ :arg 3.6))

(defparameter *nautical-mile-in-meters* 1852)
(defconversionfun :knots :meters-per-second (* *nautical-mile-in-meters* (/ :arg 3600)))
(defconversionfun :meters-per-second :knots (/ (* :arg 3600) *nautical-mile-in-meters*))

(defparameter *mile-in-meters* 1609.344)
(defconversionfun :miles-per-hour :meters-per-second (* *mile-in-meters* (/ :arg 3600)))
(defconversionfun :meters-per-second :miles-per-hour  (/ (* :arg 3600) *mile-in-meters*))

;; References:
;;  - http://ritvalawx.com/wxtermit.php
;;  - https://donsnotes.com/reference/temperature-index.html
;;  - http://fi.wikipedia.org/wiki/Pakkasen_purevuus
;;  - https://en.wikipedia.org/wiki/Wind_chill
;; 
;;  - HI, SSI http://www.gorhamschaffler.com/humidity_formulas.htm
;;  - Humidex http://climate.weatheroffice.gc.ca/prods_servs/normals_documentation_e.html
;;  - AT http://www.bom.gov.au/info/thermal_stress/#apparent
;;  - THW http://www.ehow.com/how_12028315_calculate-thw-index.html

(defun summer-simmer-index (temperature-in-celsius relative-humidity)
  "Defined from 21°C (70 F) up."
  (let ((fahrenheit-temperature (celsius-to-fahrenheit temperature-in-celsius)))
    (fahrenheit-to-celsius
     (-
      (* 1.98
	 (- fahrenheit-temperature
	    (*
	     (- 0.55 (* 0.0055 relative-humidity))
	     (- fahrenheit-temperature 58))))
      56.83))))

(defun wind-chill (air-temperature-in-celsius wind-speed-in-meters-per-second)
  "This unit tries to capture the effect of wind to cold temperatures,
   approximating how cold it really feels when there's wind.

   This is the wind chill formula as specified by Environment Canada and NWS.

   This is defined only for temperatures at or below 10°C and wind speeds
   above 1.33 m/s (4.8 km/h). This is also an approximation to an accuracy of one
   degree, hence the rounding."
  (multiple-value-bind (rounded)
      (round
       (let ((wind-speed (meters-per-second-to-kilometers-per-hour wind-speed-in-meters-per-second)))
	 (+
	  (-
	   (+ 13.12 (* 0.6215 air-temperature-in-celsius))
	   (* 11.37 (expt wind-speed 0.16)))
	  (* 0.3965 air-temperature-in-celsius (expt wind-speed 0.16)))))
    rounded))

(defun temperature-and-humidity-to-dew-point-temperature (temperature-in-celsius relative-humidity)
  "Lawrence, Mark G., 2005: The relationship between relative humidity and the dewpoint temperature in moist air: A simple conversion and applications. Bull. Amer. Meteor. Soc., 86, 225-233. doi: http;//dx.doi.org/10.1175/BAMS-86-2-225"
  (- temperature-in-celsius (/ (- 100 relative-humidity) 5)))

(defun humidex-from-relative-humidity (temperature-in-celsius relative-humidity)
  (humidex temperature-in-celsius (temperature-and-humidity-to-dew-point-temperature temperature-in-celsius relative-humidity)))

(defun humidex (temperature-in-celsius dew-point-temperature-in-celsius)
  "This follows the formula as specified in https://en.wikipedia.org/wiki/Humidex."
  (multiple-value-bind (rounded)
      (round
       (+
	temperature-in-celsius
	(*
	 (/ 5 9)
	 (-
	  (* 6.11 (expt 2.71828
			(*
			 5417.7530
			 (-
			  (/ 1 273.16)
			  (/ 1 (+ 273.15 dew-point-temperature-in-celsius))))))
	  10))))
    rounded))
