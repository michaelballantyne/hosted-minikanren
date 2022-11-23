#lang racket

(provide (all-defined-out))

(define nodes
  '(aguascalientes
    baja-california
    baja-california-sur
    campeche
    chiapas
    chihuahua
    coahuila
    colima
    durango
    guanajuato
    guerrero
    hidalgo
    jalisco
    mexico
    mexico-city
    michoacan
    morelos
    nayarit
    nuevo-leon
    oaxaca
    puebla
    queretaro
    quintana-roo
    san-luis-potosi
    sinaloa
    sonora
    tabasco
    tamaulipas
    tlaxcala
    veracruz
    yucatan
    zacatecas))

(define edges
  '((aguascalientes jalisco)
    (aguascalientes zacatecas)
    (baja-california baja-california-sur)
    (baja-california sonora)
    (campeche quintana-roo)
    (campeche yucatan)
    (campeche tabasco)
    (chiapas oaxaca)
    (chiapas tabasco)
    (chiapas veracruz)
    (chihuahua coahuila)
    (chihuahua durango)
    (chihuahua sinaloa)
    (chihuahua sonora)
    (coahuila zacatecas)
    (coahuila durango)
    (coahuila nuevo-leon)
    (colima michoacan)
    (colima jalisco)
    (durango nayarit)
    (durango zacatecas)
    (durango sinaloa)
    (guanajuato queretaro)
    (guanajuato jalisco)
    (guanajuato san-luis-potosi)
    (guerrero oaxaca)
    (guerrero puebla)
    (guerrero morelos)
    (guerrero mexico)
    (guerrero michoacan)
    (hidalgo puebla)
    (hidalgo tlaxcala)
    (hidalgo veracruz)
    (hidalgo san-luis-potosi)
    (hidalgo mexico)
    (hidalgo queretaro)
    (jalisco michoacan)
    (jalisco nayarit)
    (jalisco zacatecas)
    (mexico puebla)
    (mexico morelos)
    (mexico tlaxcala)
    (mexico mexico-city)
    (mexico queretaro)
    (mexico michoacan)
    (mexico-city morelos)
    (michoacan queretaro)
    (morelos puebla)
    (nayarit sinaloa)
    (nuevo-leon san-luis-potosi)
    (nuevo-leon tamaulipas)
    (oaxaca puebla)
    (oaxaca veracruz)
    (puebla veracruz)
    (puebla tlaxcala)
    (queretaro san-luis-potosi)
    (quintana-roo yucatan)
    (san-luis-potosi veracruz)
    (san-luis-potosi tamaulipas)
    (san-luis-potosi zacatecas)
    (sinaloa sonora)
    (tamaulipas veracruz)))
