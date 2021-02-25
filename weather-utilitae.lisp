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

(defun heat-index-rothfusz (temperature-in-fahrenheit relative-humidity)
  ;; HI = -42.379 + 2.04901523*T + 10.14333127*RH - .22475541*T*RH - .00683783*T*T - .05481717*RH*RH + .00122874*T*T*RH + .00085282*T*RH*RH - .00000199*T*T*RH*RH
  (+
   -42.379
   (* 2.04901523 temperature-in-fahrenheit)
   (* 10.14333127 relative-humidity)
   (* -0.22475541 temperature-in-fahrenheit relative-humidity)
   (* -0.00683783 temperature-in-fahrenheit temperature-in-fahrenheit)
   (* -0.00683783 temperature-in-fahrenheit temperature-in-fahrenheit)
   (* -0.05481717 relative-humidity relative-humidity)
   (* 0.00122874 temperature-in-fahrenheit temperature-in-fahrenheit relative-humidity)
   (* 0.00085282 temperature-in-fahrenheit relative-humidity relative-humidity)
   (* -0.00000199 temperature-in-fahrenheit temperature-in-fahrenheit relative-humidity relative-humidity)))

(defun heat-index-adjust-subtract (temperature-in-fahrenheit relative-humidity)
  "The result of this is subtracted from HI when RH is less than 13% and the
   temperature is between 80 and 112 degrees F, then the following adjustment
   is subtracted from HI.

   ADJUSTMENT = [(13-RH)/4]*SQRT{[17-ABS(T-95.)]/17}"
  (if (and
       (< relative-humidity 13)
       (>= temperature-in-fahrenheit 80)
       (< temperature-in-fahrenheit 112))
      (*
       (/ (- 13 relative-humidity) 4)
       (sqrt
	(/
	 (- 17 (abs (- temperature-in-fahrenheit 95)))
	 17)))
      0))

(defun heat-index-adjust-add (temperature-in-fahrenheit relative-humidity)
  "The result of this is added to HI when RH is greater than 85% and the
   temperature is between 80 and 87 degrees F, then the following adjustment
   is subtracted from HI.

   ADJUSTMENT = [(RH-85)/10] * [(87-T)/5]"
  (if (and
       (> relative-humidity 85)
       (>= temperature-in-fahrenheit 80)
       (< temperature-in-fahrenheit 87))
      (*
       (/ (- relative-humidity 85) 10)
       (/ (- 87 temperature-in-fahrenheit) 5))
      0))

(defun heat-index-simple (temperature-in-fahrenheit relative-humidity)
  ;; HI = 0.5 * {T + 61.0 + [(T-68.0)*1.2] + (RH*0.094)}
  (*
   0.5
   (+
    temperature-in-fahrenheit
    61.0
    (* (- temperature-in-fahrenheit 68.0) 1.2)
    (* relative-humidity 0.094))))

(defun heat-index (temperature-in-celsius relative-humidity)
  "Heat Index (HI) as defined by NWS in
   https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml."
  (fahrenheit-to-celsius
   (let* ((temperature-in-fahrenheit (celsius-to-fahrenheit temperature-in-celsius))
	  (simple-result (heat-index-simple temperature-in-fahrenheit relative-humidity)))
     (if (>= (/ (+ temperature-in-fahrenheit simple-result) 2) 80.0)
	 (+
	  (heat-index-rothfusz temperature-in-fahrenheit relative-humidity)
	  (* -1 (heat-index-adjust-subtract temperature-in-fahrenheit relative-humidity))
	  (heat-index-adjust-add temperature-in-fahrenheit relative-humidity))
	 simple-result))))
